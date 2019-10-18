{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.Unrolled.STUnlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.Unrolled.STUnlist@ provides service type 'STUnlist' - mutable lazy
    boxed unrolled linked list for @Unlist@.
-}
module SDP.Unrolled.STUnlist
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STUnlist
  STUnlist (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.SortM

import GHC.Base ( Int (..) )
import GHC.ST   ( ST  (..) )

import SDP.SortM.Tim

import SDP.Internal.SArray
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | This STUnlist is mutable version of Unlist.
data STUnlist s e = STUNEmpty | STUnlist !(STArray# s e) (STUnlist s e)

type role STUnlist nominal representational

--------------------------------------------------------------------------------

{- Eq instance. -}

instance (Eq e) => Eq (STUnlist s e)
  where
    STUNEmpty == STUNEmpty = True
    (STUnlist marr1# marr1) == (STUnlist marr2# marr2) = marr1# == marr2# && marr1 == marr2
    _ == _ = False

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance BorderedM (ST s) (STUnlist s e) Int e
  where
    getLower _  = return 0
    getUpper es = do n <- getSizeOf es; return (n - 1)
    
    getSizeOf (STUnlist marr# marr) = liftA2 (+) (getSizeOf marr#) (getSizeOf marr)
    getSizeOf _ = return 0
    
    getIndices es = do n <- getSizeOf es; return [0 .. n - 1]
    getIndexOf es = \ i -> i < 0 ? return False $ do n <- getSizeOf es; return (i < n)

instance LinearM (ST s) (STUnlist s e) e
  where
    {-# INLINE newLinear #-}
    newLinear es = liftA2 (foldr STUnlist) rest' chs'
      where
        rest' = (`STUnlist` STUNEmpty) <$> newLinearN (length rest) rest
        chs'  = forM chs (newLinearN lim)
        (chs :< rest) = chunks lim es
    
    getLeft  (STUnlist marr# marr) = liftA2 (++) (getLeft marr#) (getLeft marr)
    getLeft  _ = return []
    
    getRight (STUnlist marr marr#) = liftA2 (flip (++)) (getRight marr#) (getRight marr)
    getRight _ = return []
    
    reversed es = go es STUNEmpty
      where
        go (STUnlist marr# marr) acc = go marr . (`STUnlist` acc) =<< reversed marr#
        go _ acc = return acc
    
    -- Note: STUnlist.filled is not so efficient as Unlist.replicate.
    filled n e
      | n <= 0 = return STUNEmpty
      |  True  = liftA2 STUnlist chunk rest
      where
        rest  = filled (n  -  lim) e
        chunk = filled (min n lim) e

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance IndexedM (ST s) (STUnlist s e) Int e
  where
    {-# INLINE fromAssocs' #-}
    fromAssocs' bnds@(l, _) defvalue ascs = do
      arr <- size bnds `filled` defvalue
      overwrite arr [ (i - l, e) | (i, e) <- ascs, inRange bnds i ]
    
    (!#>) (STUnlist marr# marr) i = getSizeOf marr# >>=
      \ n -> i < n ? marr# !#> i $ marr !#> (i - n)
    (!#>) _ _ = throw $ IndexOverflow "in SDP.Unlist.STUnlist.(>!)"
    
    {-# INLINE (>!) #-}
    (>!) es i = i < 0 ? err $ es !#> i
      where
        err = throw $ IndexUnderflow "in SDP.Unlist.STUnlist.(>!)"
    
    es !> i = getBounds es >>= \ bnds -> case inBounds bnds i of
        ER -> throw $ EmptyRange     msg
        UR -> throw $ IndexUnderflow msg
        IN -> es >! i
        OR -> throw $ IndexOverflow  msg
      where
        msg = "in SDP.Unrolled.STUnlist.(!>): " ++ show i
    
    writeM_ (STUnlist marr# marr) i e = getSizeOf marr# >>=
      \ n -> i < n ? writeM_ marr# i e $ writeM_ marr (i - n) e
    writeM_ _ _ _ = return ()
    
    writeM es i e = i < 0 ? return () $ writeM_ es i e
    
    {-# INLINE overwrite #-}
    overwrite STUNEmpty ascs = isNull ascs ? return STUNEmpty $ fromAssocs (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    overwrite es@(STUnlist marr# marr) ascs = getSizeOf es >>= \ n -> do
      let (curr, others) = partition (\ (i, _) -> i < n) ascs
      
      marr'  <- overwrite marr [ (i - n, e) | (i, e) <- others ]
      marr'# <- overwrite marr# curr
      return (STUnlist marr'# marr')
    
    fromIndexed' es = sizeOf es == 0 ? return STUNEmpty $ (`STUnlist` STUNEmpty) <$> fromIndexed' es
    fromIndexedM es = getSizeOf es >>= \ n -> n == 0 ? return STUNEmpty $ (`STUnlist` STUNEmpty) <$> fromIndexedM es

instance IFoldM (ST s) (STUnlist s e) Int e
  where
    {-# INLINE ifoldrM #-}
    ifoldrM f base (STUnlist marr# marr) = ifoldrM f base marr >>=
      \ base' -> ifoldrM f base' marr#
    ifoldrM _ base _ = return base
    
    {-# INLINE ifoldlM #-}
    ifoldlM f base (STUnlist marr# marr) = ifoldlM f base marr# >>=
      \ base' -> ifoldlM f base' marr
    ifoldlM _ base _ = return base
    
    {-# INLINE i_foldrM #-}
    i_foldrM f base (STUnlist marr# marr) = i_foldrM f base marr >>=
      \ base' -> i_foldrM f base' marr#
    i_foldrM _ base _ = return base
    
    {-# INLINE i_foldlM #-}
    i_foldlM f base (STUnlist marr# marr) = i_foldlM f base marr# >>=
      \ base' -> i_foldlM f base' marr
    i_foldlM _ base _ = return base

instance SortM (ST s) (STUnlist s e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

lim :: Int
lim =  1024


