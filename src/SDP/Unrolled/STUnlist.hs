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
  module SDP.SortM,
  
  STUnlist (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.SortM

import GHC.Base
  (
    MutableArray#, Int (..),
    
    newArray#, writeArray#, readArray#,
    
    isTrue#, sameMutableArray#, (+#), (-#), (==#)
  )

import GHC.ST ( ST (..), STRep )

import SDP.SortM.Tim
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
    
    getIndices es = return $ case es of {STUnlist n _ _ -> [0 .. n - 1]; _ -> []}
    getIndexOf es = \ i -> return $ case es of {STUnlist n _ _ -> i >= 0 && i < n; _ -> False}

instance LinearM (ST s) (STUnlist s e) e
  where
    {-# INLINE newLinear #-}
    newLinear es = liftA2 (foldr cat) rest' chs'
      where
        rest' = toChunk err (length rest) rest
        chs'  = forM chs $ toChunk err lim
        
        cat = \ (STUnlist n mubl# STUNEmpty) acc -> STUnlist n mubl# acc
        err = unreachEx "fromFoldableM"
        (chs :< rest) = chunks lim es
    
    getLeft  es = case es of {STUnlist n _ _ -> mapM (es >!) [0 .. n - 1]; _ -> return []}
    getRight es = case es of {STUnlist n _ _ -> mapM (es >!) [n - 1, n - 2 .. 0]; _ -> return []}
    reversed es = liftA2 zip (getIndices es) (getRight es) >>= overwrite es
    
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

{- IndexedM, IFoldM and SortM instances. -}

instance IndexedM (ST s) (STUnlist s e) Int e
  where
    {-# INLINE fromAssocs' #-}
    fromAssocs' bnds@(l, _) defvalue ascs = do
      arr <- size bnds `filled` defvalue
      overwrite arr [ (i - l, e) | (i, e) <- ascs ]
    
    (!#>) = (>!)
    
    {-# INLINE (>!) #-}
    (STUnlist n marr# marr) >! i@(I# i#) = i >= n ? marr >! (i - n) $ ST (readArray# marr# i#)
    _ >! _ = error "SDP.Unrolled.STUnlist.(>!) tried to find element in empty STUnlist"
    
    STUNEmpty           !> _ = throw $ EmptyRange "in SDP.Unrolled.STUnlist.(!>)"
    es@(STUnlist n _ _) !> i
      | n < 1 = throw $ EmptyRange     msg
      | i < 0 = throw $ IndexUnderflow msg
      | i < n = es >! i
      | True  = throw $ IndexOverflow  msg
      where
        msg = "in SDP.Unrolled.STUnlist.(!>)"
    
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
        
    
    fromIndexed' es = do
        copy <- filled n (unreachEx "fromIndexed'")
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        return copy
      where
        n = sizeOf es
    
    fromIndexedM es = do
      n    <- getSizeOf es
      copy <- filled n (unreachEx "fromIndexedM")
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy

instance IFoldM (ST s) (STUnlist s e) Int e
  where
    {-# INLINE ifoldrM #-}
    ifoldrM f base = \ ubl -> case ubl of
        STUNEmpty           -> return base
        (STUnlist c _ ubls) ->
          let go b i = c == i ? b $ bindM2 (ubl !#> i) (go b $ i + 1) (f i)
          in  ifoldrM f base ubls `go` 0
    
    {-# INLINE ifoldlM #-}
    ifoldlM f base = \ ubl -> case ubl of
      STUNEmpty           -> return base
      (STUnlist c _ ubls) ->
        let go b i = -1 == i ? b $ bindM2 (go b $ i - 1) (ubl !#> i) (f i)
        in  return base `go` (c - 1) >>= ifoldlM f `flip` ubls
    
    {-# INLINE i_foldrM #-}
    i_foldrM f base = \ ubl -> case ubl of
      STUNEmpty           -> return base
      (STUnlist c _ ubls) ->
        let go b i = c == i ? b $ bindM2 (ubl !#> i) (go b $ i + 1) f
        in  go (i_foldrM f base ubls) 0
    
    {-# INLINE i_foldlM #-}
    i_foldlM f base = \ ubl -> case ubl of
      STUNEmpty           -> return base
      (STUnlist c _ ubls) ->
        let go b i = -1 == i ? b $ bindM2 (go b $ i - 1) (ubl !#> i) f
        in  return base `go` (c - 1) >>= i_foldlM f `flip` ubls

instance SortM (ST s) (STUnlist s e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

{-# INLINE toChunk #-}
toChunk :: e -> Int -> [e] -> ST s (STUnlist s e)
toChunk e n@(I# n#) = \ es -> ST $ \ s1# -> case newArray# n# e s1# of
  (# s2#, mubl# #) ->
    let go x r = \ i# s3# -> case writeArray# mubl# i# x s3# of
          s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
    in case ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# ) of
      s5# -> (# s5#, STUnlist n mubl# STUNEmpty #)

{-# INLINE done #-}
done :: Int -> MutableArray# s e -> STUnlist s e -> STRep s (STUnlist s e)
done n marr# rest = \ s1# -> (# s1#, STUnlist n marr# rest #)

{-# INLINE fill #-}
fill :: MutableArray# s e -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) nxt = \ s1# -> case writeArray# marr# i# e s1# of s2# -> nxt s2#

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.STUnlist." ++ msg

lim :: Int
lim =  1024


