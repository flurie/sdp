{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.STUblist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    Stability   :  stable
    
    This module provides service type Ublist - mutable strict unboxed unrolled
    linked list for SDP.ByteList.Ublist.
-}

module SDP.ByteList.STUblist
(
  module SDP.IndexedM,
  
  STUblist (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Unboxed
import SDP.Linear

import GHC.Base
  (
    MutableByteArray#, Int (..),
    
    isTrue#, sameMutableByteArray#, (+#), (-#), (==#)
  )

import GHC.ST ( ST (..), STRep )

import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | This STUblist type definition is mutable version of Ublist.
data STUblist s e = STUBEmpty | STUblist {-# UNPACK #-} !Int (MutableByteArray# s) (STUblist s e)

type role STUblist nominal representational

--------------------------------------------------------------------------------

{- Eq instance. -}

instance (Unboxed e) => Eq (STUblist s e)
  where
    STUBEmpty == STUBEmpty = True
    (STUblist n1 marr1# marr1) == (STUblist n2 marr2# marr2) = n1 == n2 && (n1 == 0 || isTrue# (sameMutableByteArray# marr1# marr2#) && marr1 == marr2)
    _ == _ = False

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance (Unboxed e) => BorderedM (ST s) (STUblist s e) Int e
  where
    getLower  _ = return 0
    
    getUpper     STUBEmpty     = return $ -1
    getUpper  (STUblist n _ _) = return $ max 0 n - 1
    
    getSizeOf    STUBEmpty     = return 0
    getSizeOf (STUblist n _ _) = return $ max 0 n
    
    getIndices    STUBEmpty     = return []
    getIndices (STUblist n _ _) = return [0 .. n - 1]
    
    getIndexOf     STUBEmpty    _ = return False
    getIndexOf (STUblist n _ _) i = return $ i >= 0 && i < n

instance (Unboxed e) => LinearM (ST s) (STUblist s e) e
  where
    newLinear es = fromFoldableM es
    
    {-# INLINE fromFoldableM #-}
    fromFoldableM es = foldr (\ x y -> toChunk err lim x .++ y) rest' chunks
      where
        (chunks :< rest)  = take count [ lim, lim .. ] `splits` toList es
        (count, restSize) = length es `divMod` lim
        
        rest' = toChunk err restSize rest
        err   = unreachEx "fromFoldableM"
    
    getLeft       STUBEmpty      = return []
    getLeft  es@(STUblist n _ _) = sequence [ es >! i | i <- range (0, n - 1) ]
    
    getRight      STUBEmpty      = return []
    getRight es@(STUblist n _ _) = sequence [ es >! i | i <- reverse $ range (0, n - 1) ]
    
    {-# INLINE reversed #-}
    reversed es = liftA2 (\ bnds -> zip $ range bnds) (getBounds es) (getRight es) >>= overwrite es
    
    {-# INLINE filled #-}
    -- Note: STUblist.filled is not so efficient as Ublist.replicate.
    filled n e
        | n <  1  = return STUBEmpty
        | n < lim = ST $ \ s1# -> case newUnboxed' e l# s1# of
          (# s2#, marr# #) -> (# s2#, STUblist n marr# STUBEmpty #)
        |  True   = filled (n - lim) e >>= \ marr -> ST $
          \ s1# -> case newUnboxed' e l# s1# of
            (# s2#, marr# #) -> (# s2#, STUblist n marr# marr #)
      where
        !(I# l#) = lim

--------------------------------------------------------------------------------

{- IndexedM instance. -}

instance (Unboxed e) => IndexedM (ST s) (STUblist s e) Int e
  where
    {-# INLINE fromAssocs #-}
    fromAssocs bnds ascs = size bnds `filled_` err >>= (`overwrite` ascs)
      where
        filled_ :: (Unboxed e) => Int -> e -> ST s (STUblist s e)
        filled_ n e
            | n <  1  = return STUBEmpty
            | n < lim = ST $ \ s1# -> case newUnboxed e l# s1# of
              (# s2#, marr# #) -> (# s2#, STUblist n marr# STUBEmpty #)
            |  True   = filled_ (n - lim) e >>= \ marr -> ST $
              \ s1# -> case newUnboxed e l# s1# of
                (# s2#, marr# #) -> (# s2#, STUblist n marr# marr #)
        
        err      = throw $ UndefinedValue "in SDP.ByteList.STUblist.fromAssocs"
        !(I# l#) = lim
    
    {-# INLINE fromAssocs' #-}
    fromAssocs' bnds defvalue ascs = size bnds `filled` defvalue >>= (`overwrite` ascs)
    
    {-# INLINE (>!) #-}
    (STUblist n marr# marr) >! i@(I# i#)
      | i >= n = marr !> (i - n)
      |  True  = ST $ marr# !># i#
    _ >! _ = error "SDP.ByteList.STUblist.(>!) tried to find element in empty STUblist"
    
    STUBEmpty           !> _ = throw $ EmptyRange "in SDP.ByteList.STUnlist.(!>)"
    es@(STUblist n _ _) !> i
      | n < 1 = throw $ EmptyRange     msg
      | i < 0 = throw $ IndexUnderflow msg
      | i < n = es >! i
      | True  = throw $ IndexOverflow  msg
      where
        msg = "in SDP.ByteList.STUnlist.(!>)"
    
    {-# INLINE overwrite #-}
    overwrite STUBEmpty ascs = isNull ascs ? return STUBEmpty $ fromAssocs (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    overwrite es@(STUblist n marr# marr) ascs = writes >> overwrite marr others' >> return es
      where
        (curr, others) = partition (inRange (0, n - 1) . fst) ascs
        writes  = ST $ foldr (fill marr#) (done n marr# marr) curr
        others' = [ (i - n, e) | (i, e) <- others ]
    
    p *? marr = fsts . filter (p . snd) <$> getAssocs marr

--------------------------------------------------------------------------------

{-# INLINE toChunk #-}
toChunk :: (Unboxed e) => e -> Int -> [e] -> ST s (STUblist s e)
toChunk e n@(I# n#) es = ST $ \ s1# -> case newUnboxed e n# s1# of
  (# s2#, marr# #) ->
    let go x r = \ i# s3# -> case writeByteArray# marr# i# x s3# of
          s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
    in case ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# ) of
      s5# -> (# s5#, STUblist n marr# STUBEmpty #)

{-# INLINE (.++) #-}
(.++) :: ST s (STUblist s e) -> ST s (STUblist s e) -> ST s (STUblist s e)
xs' .++ ys' = liftA2 cat xs' ys'
  where
    cat (STUblist n1 marr1# marr1) y = STUblist n1 marr1# (cat marr1 y)
    cat STUBEmpty y = y

{-# INLINE done #-}
done :: Int -> MutableByteArray# s -> STUblist s e -> STRep s (STUblist s e)
done n marr# rest = \ s1# -> (# s1#, STUblist n marr# rest #)

{-# INLINE fill #-}
fill :: (Unboxed e) => MutableByteArray# s -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) nxt = \ s1# -> case writeByteArray# marr# i# e s1# of s2# -> nxt s2#

unreachEx     :: String -> a
unreachEx msg =  throw . UnreachableException $ "in SDP.STUnlist." ++ msg

lim :: Int
lim =  1024


