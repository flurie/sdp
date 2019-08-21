{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.STUnlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    This module provides service type Unlist - mutable lazy boxed unrolled
    linked list for SDP.Unrolled.Unlist.
-}

module SDP.Unrolled.STUnlist
(
  module SDP.IndexedM,
  
  STUnlist (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Linear

import GHC.Base
  (
    MutableArray#, Int (..),
    
    newArray#, writeArray#, readArray#,
    
    isTrue#, sameMutableArray#, (+#), (-#), (==#)
  )

import GHC.ST ( ST (..), STRep )

import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | This STUnlist is mutable version of Unlist.
data STUnlist s e = STUNEmpty | STUnlist {-# UNPACK #-} !Int (MutableArray# s e) (STUnlist s e)

type role STUnlist nominal representational

--------------------------------------------------------------------------------

{- Eq instance. -}

instance (Eq e) => Eq (STUnlist s e)
  where
    STUNEmpty == STUNEmpty = True
    (STUnlist n1 marr1# marr1) == (STUnlist n2 marr2# marr2) = res
      where
        res  = n1 == n2 && (n1 == 0 || same)
        same = isTrue# (sameMutableArray# marr1# marr2#) && marr1 == marr2
    _ == _ = False

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance BorderedM (ST s) (STUnlist s e) Int e
  where
    getLower  _  = return 0
    getUpper  es = return $ case es of {STUnlist n _ _ -> max 0 n - 1; _ -> -1}
    getSizeOf es = return $ case es of {STUnlist n _ _ -> max 0 n; _ -> 0}
    
    getIndices es   = return $ case es of {STUnlist n _ _ -> [0 .. n - 1]; _ -> []}
    getIndexOf es i = return $ case es of {STUnlist n _ _ -> i >= 0 && i < n; _ -> False}

instance LinearM (ST s) (STUnlist s e) e
  where
    newLinear = fromFoldableM
    
    {-# INLINE fromFoldableM #-}
    fromFoldableM es = foldr (\ x y -> toChunk' err lim x .++ y) rest' chunks
      where
        (chunks :< rest)  = take count [ lim, lim .. ] `splits` toList es
        (count, restSize) = length es `divMod` lim
        
        rest' = toChunk' err restSize rest
        err   = unreachEx "fromFoldableM"
    
    getLeft       STUNEmpty      = return []
    getLeft  es@(STUnlist n _ _) = mapM (es >!) [0 .. n - 1]
    
    getRight      STUNEmpty      = return []
    getRight es@(STUnlist n _ _) = mapM (es >!) [n - 1, n - 2 .. 0]
    
    {-# INLINE reversed #-}
    reversed es = liftA2 zip (getIndices es) (getRight es) >>= overwrite es
    
    {-# INLINE filled #-}
    -- Note: STUnlist.filled is not so efficient as Unlist.replicate.
    filled n e
        | n <  1  = return STUNEmpty
        | n < lim = ST $ \ s1# -> case newArray# l# e s1# of
          (# s2#, marr# #) -> (# s2#, STUnlist n marr# STUNEmpty #)
        |  True   = filled (n - lim) e >>= \ marr -> ST $
          \ s1# -> case newArray# l# e s1# of
            (# s2#, marr# #) -> (# s2#, STUnlist n marr# marr #)
      where
        !(I# l#) = lim

--------------------------------------------------------------------------------

{- IndexedM instance. -}

instance IndexedM (ST s) (STUnlist s e) Int e
  where
    {-# INLINE fromAssocs' #-}
    fromAssocs' bnds defvalue ascs = size bnds `filled` defvalue >>= (`overwrite` ascs)
    
    es !#> i = es >! i
    
    {-# INLINE (>!) #-}
    (STUnlist n marr# marr) >! i@(I# i#) = i >= n ? marr !> (i - n) $ ST (readArray# marr# i#)
    _ >! _ = error "SDP.Unrolled.STUnlist.(>!) tried to find element in empty STUnlist"
    
    STUNEmpty           !> _ = throw $ EmptyRange "in SDP.Unrolled.STUnlist.(!>)"
    es@(STUnlist n _ _) !> i
      | n < 1 = throw $ EmptyRange     msg
      | i < 0 = throw $ IndexUnderflow msg
      | i < n = es >! i
      | True  = throw $ IndexOverflow  msg
      where
        msg = "in SDP.STUnlist.(!>)"
    
    writeM STUNEmpty _ _ = return ()
    writeM (STUnlist n marr# marr) i@(I# i#) e
      | i < 0 = return ()
      | i < n = ST $ \ s1# -> case writeArray# marr# i# e s1# of s2# -> (# s2#, () #)
      | True  = writeM marr (i - n) e
    
    {-# INLINE overwrite #-}
    overwrite STUNEmpty ascs = isNull ascs ? return STUNEmpty $ fromAssocs (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    overwrite es@(STUnlist n marr# marr) ascs = writes >> overwrite marr others' >> return es
      where
        (curr, others) = partition (inRange (0, n - 1) . fst) ascs
        writes  = ST $ foldr (fill marr#) (done n marr# marr) curr
        others' = [ (i - n, e) | (i, e) <- others ]

--------------------------------------------------------------------------------

{-# INLINE toChunk' #-}
toChunk' :: e -> Int -> [e] -> ST s (STUnlist s e)
toChunk' e n@(I# n#) es = ST $ \ s1# -> case newArray# n# e s1# of
  (# s2#, marr# #) ->
    let go x r = \ i# s3# -> case writeArray# marr# i# x s3# of
          s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
    in case ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# ) of
      s5# -> (# s5#, STUnlist n marr# STUNEmpty #)

{-# INLINE (.++) #-}
(.++) :: ST s (STUnlist s e) -> ST s (STUnlist s e) -> ST s (STUnlist s e)
xs' .++ ys' = liftA2 cat xs' ys'
  where
    cat (STUnlist n1 marr1# marr1) y = STUnlist n1 marr1# (cat marr1 y)
    cat STUNEmpty y = y

{-# INLINE done #-}
done :: Int -> MutableArray# s e -> STUnlist s e -> STRep s (STUnlist s e)
done n marr# rest = \ s1# -> (# s1#, STUnlist n marr# rest #)

{-# INLINE fill #-}
fill :: MutableArray# s e -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) nxt = \ s1# -> case writeArray# marr# i# e s1# of s2# -> nxt s2#

unreachEx     :: String -> a
unreachEx msg =  throw . UnreachableException $ "in SDP.STUnlist." ++ msg

lim :: Int
lim =  1024


