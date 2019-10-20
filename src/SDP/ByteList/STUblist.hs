{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

{- |
    Module      :  SDP.ByteList.STUblist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.ByteList.STUblist@ provides 'STUblist' - mutable unboxed strict
    unrolled linked list.
-}
module SDP.ByteList.STUblist
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STUblist
  STUblist (..), fromPseudoMutableBytes#
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM

import GHC.Base ( Int (..) )
import GHC.ST   ( ST  (..) )

import SDP.SortM.Tim

import SDP.Internal.SBytes
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | This STUblist is mutable version of Ublist.
data STUblist s e = STUBEmpty | STUblist !(STBytes# s e) (STUblist s e) deriving ( Eq )

type role STUblist nominal representational

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance (Unboxed e) => BorderedM (ST s) (STUblist s e) Int e
  where
    getLower _  = return 0
    getUpper es = do n <- getSizeOf es; return (n - 1)
    
    getSizeOf (STUblist marr# marr) = liftA2 (+) (getSizeOf marr#) (getSizeOf marr)
    getSizeOf _ = return 0
    
    getIndices es = do n <- getSizeOf es; return [0 .. n - 1]
    getIndexOf es = \ i -> i < 0 ? return False $ do n <- getSizeOf es; return (i < n)

instance (Unboxed e) => LinearM (ST s) (STUblist s e) e
  where
    {-# INLINE newLinear #-}
    newLinear es = liftA2 (foldr STUblist) rest' chs'
      where
        rest' = (`STUblist` STUBEmpty) <$> newLinearN (length rest) rest
        chs'  = forM chs (newLinearN lim)
        (chs :< rest) = chunks lim es
    
    getLeft  (STUblist marr# marr) = liftA2 (++) (getLeft marr#) (getLeft marr)
    getLeft  _ = return []
    
    getRight (STUblist marr marr#) = liftA2 (flip (++)) (getRight marr#) (getRight marr)
    getRight _ = return []
    
    reversed es = go es STUBEmpty
      where
        go (STUblist marr# marr) acc = go marr . (`STUblist` acc) =<< reversed marr#
        go _ acc = return acc
    
    -- Note: STUnlist.filled is not so efficient as Unlist.replicate.
    filled n e = n <= 0 ? return STUBEmpty $ liftA2 STUblist chunk rest
      where
        rest  = filled (n  -  lim) e
        chunk = filled (min n lim) e

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Unboxed e) => IndexedM (ST s) (STUblist s e) Int e
  where
    {-# INLINE fromAssocs #-}
    fromAssocs bnds@(l, _) ascs = do
        ubl <- filled' $ size bnds
        overwrite ubl [ (i - l, e) | (i, e) <- ascs ]
      where
        filled' :: (Unboxed e) => Int -> ST s (STUblist s e)
        filled' n = n <= 0 ? return STUBEmpty $ liftA2 STUblist chunk rest
          where
            chunk = filled_ (min n lim)
            rest  = filled' (n - lim)
    
    {-# INLINE fromAssocs' #-}
    fromAssocs' bnds@(l, _) defvalue ascs = do
      arr <- size bnds `filled` defvalue
      overwrite arr [ (i - l, e) | (i, e) <- ascs, inRange bnds i ]
    
    (!#>) (STUblist marr# marr) i = getSizeOf marr# >>=
      \ n -> i < n ? marr# !#> i $ marr !#> (i - n)
    (!#>) _ _ = throw $ IndexOverflow "in SDP.ByteList.STUblist.(>!)"
    
    {-# INLINE (>!) #-}
    (>!) es i = i < 0 ? err $ es !#> i
      where
        err = throw $ IndexUnderflow "in SDP.ByteList.STUblist.(>!)"
    
    es !> i = getBounds es >>= \ bnds -> case inBounds bnds i of
        ER -> throw $ EmptyRange     msg
        UR -> throw $ IndexUnderflow msg
        IN -> es >! i
        OR -> throw $ IndexOverflow  msg
      where
        msg = "in SDP.ByteList.STUblist.(!>): " ++ show i
    
    writeM_ STUBEmpty _ _ = return ()
    writeM_ (STUblist marr# marr) i e = getSizeOf marr# >>=
      \ n -> i < n ? writeM_ marr# i e $ writeM_ marr (i - n) e
    
    writeM es i e = i < 0 ? return () $ writeM_ es i e
    
    {-# INLINE overwrite #-}
    overwrite STUBEmpty ascs = isNull ascs ? return STUBEmpty $ fromAssocs (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    overwrite es@(STUblist marr# marr) ascs = getSizeOf es >>= \ n -> do
      let (curr, others) = partition (\ (i, _) -> i < n) ascs
      
      marr'  <- overwrite marr [ (i - n, e) | (i, e) <- others ]
      marr'# <- overwrite marr# curr
      return (STUblist marr'# marr')
    
    fromIndexed' es = sizeOf es == 0 ? return STUBEmpty $ do
      es' <- fromIndexed' es
      return (STUblist es' STUBEmpty)
    fromIndexedM es = do
      n <- getSizeOf es
      n == 0 ? return STUBEmpty $ do es' <- fromIndexedM es; return (STUblist es' STUBEmpty)

instance (Unboxed e) => IFoldM (ST s) (STUblist s e) Int e
  where
    {-# INLINE ifoldrM #-}
    ifoldrM f base (STUblist marr# marr) = ifoldrM f base marr >>=
      \ base' -> ifoldrM f base' marr#
    ifoldrM _ base _ = return base
    
    {-# INLINE ifoldlM #-}
    ifoldlM f base (STUblist marr# marr) = ifoldlM f base marr# >>=
      \ base' -> ifoldlM f base' marr
    ifoldlM _ base _ = return base
    
    {-# INLINE i_foldrM #-}
    i_foldrM f base (STUblist marr# marr) = i_foldrM f base marr >>=
      \ base' -> i_foldrM f base' marr#
    i_foldrM _ base _ = return base
    
    {-# INLINE i_foldlM #-}
    i_foldlM f base (STUblist marr# marr) = i_foldlM f base marr# >>=
      \ base' -> i_foldlM f base' marr
    i_foldlM _ base _ = return base

instance (Unboxed e) => SortM (ST s) (STUblist s e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

lim :: Int
lim =  1024



