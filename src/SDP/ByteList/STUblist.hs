{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

{- |
    Module      :  SDP.ByteList.STUblist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.ByteList.STUblist@ provides 'STUblist' - mutable unboxed strict
    unrolled linked list.
-}
module SDP.ByteList.STUblist
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STUblist
  STUblist (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Unboxed

import SDP.SortM.Tim
import SDP.SortM

import GHC.Base ( Int (..) )
import GHC.ST   ( ST  (..) )

import SDP.Internal.Commons
import SDP.Internal.SBytes

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
    
    getSizeOf (STUblist mbytes# mbytes) = liftA2 (+) (getSizeOf mbytes#) (getSizeOf mbytes)
    getSizeOf _ = return 0
    
    getIndices es = do n <- getSizeOf es; return [0 .. n - 1]
    getIndexOf es = \ i -> i < 0 ? return False $ do n <- getSizeOf es; return (i < n)

instance (Unboxed e) => LinearM (ST s) (STUblist s e) e
  where
    newLinear es = liftA2 (foldr STUblist) rest' chs'
      where
        rest' = (`STUblist` STUBEmpty) <$> newLinearN (length rest) rest
        chs'  = forM chs (newLinearN lim)
        (chs :< rest) = chunks lim es
    
    getLeft  (STUblist mbytes# mbytes) = liftA2 (++) (getLeft mbytes#) (getLeft mbytes)
    getLeft  _ = return []
    
    getRight (STUblist mbytes mbytes#) = liftA2 (flip (++)) (getRight mbytes#) (getRight mbytes)
    getRight _ = return []
    
    reversed es = go es STUBEmpty
      where
        go (STUblist mbytes# mbytes) acc = go mbytes . (`STUblist` acc) =<< reversed mbytes#
        go _ acc = return acc
    
    filled n e = n <= 0 ? return STUBEmpty $ liftA2 STUblist chunk rest
      where
        rest  = filled (n  -  lim) e
        chunk = filled (min n lim) e

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Unboxed e) => IndexedM (ST s) (STUblist s e) Int e
  where
    fromAssocs bnds@(l, _) ascs = do
        ubl <- filled' $ size bnds
        overwrite ubl [ (i - l, e) | (i, e) <- ascs ]
      where
        filled' :: (Unboxed e) => Int -> ST s (STUblist s e)
        filled' n = n <= 0 ? return STUBEmpty $ liftA2 STUblist chunk rest
          where
            chunk = filled_ (min n lim)
            rest  = filled' (n - lim)
    
    fromAssocs' bnds@(l, _) defvalue ascs = do
      arr <- size bnds `filled` defvalue
      overwrite arr [ (i - l, e) | (i, e) <- ascs, inRange bnds i ]
    
    (!#>) (STUblist mbytes# mbytes) i = getSizeOf mbytes# >>=
      \ n -> i < n ? mbytes# !#> i $ mbytes !#> (i - n)
    (!#>) _ _ = throw $ IndexOverflow "in SDP.ByteList.STUblist.(>!)"
    
    {-# INLINE (>!) #-}
    (>!) es i = i < 0 ? err $ es !#> i
      where
        err = throw $ IndexUnderflow "in SDP.ByteList.STUblist.(>!)"
    
    {-# INLINE writeM_ #-}
    writeM_ STUBEmpty _ _ = return ()
    writeM_ (STUblist mbytes# mbytes) i e = do
      n <- getSizeOf mbytes#
      i < n ? writeM_ mbytes# i e $ writeM_ mbytes (i - n) e
    
    {-# INLINE writeM #-}
    writeM es i e = i < 0 ? return () $ writeM_ es i e
    
    overwrite STUBEmpty ascs = isNull ascs ? return STUBEmpty $ fromAssocs (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    overwrite es@(STUblist mbytes# mbytes) ascs = getSizeOf es >>= \ n -> do
      let (curr, others) = partition (\ (i, _) -> i < n) ascs
      
      mbytes'  <- overwrite mbytes [ (i - n, e) | (i, e) <- others ]
      mbytes'# <- overwrite mbytes# curr
      return (STUblist mbytes'# mbytes')
    
    fromIndexed' es = sizeOf es == 0 ? return STUBEmpty $ do
      es' <- fromIndexed' es
      return (STUblist es' STUBEmpty)
    fromIndexedM es = do
      n <- getSizeOf es
      n == 0 ? return STUBEmpty $ do es' <- fromIndexedM es; return (STUblist es' STUBEmpty)

instance (Unboxed e) => IFoldM (ST s) (STUblist s e) Int e
  where
    ifoldrM _ base STUBEmpty = return base
    ifoldrM f base (STUblist mbytes# mbytes) = do
      base' <- ifoldrM f base mbytes
      ifoldrM f base' mbytes#
    
    ifoldlM _ base STUBEmpty = return base
    ifoldlM f base (STUblist mbytes# mbytes) = do
      base' <- ifoldlM f base mbytes#
      ifoldlM f base' mbytes
    
    i_foldrM _ base STUBEmpty = return base
    i_foldrM f base (STUblist mbytes# mbytes) = do
      base' <- i_foldrM f base mbytes
      i_foldrM f base' mbytes#
    
    i_foldlM _ base STUBEmpty = return base
    i_foldlM f base (STUblist mbytes# mbytes) = do
      base' <- i_foldlM f base mbytes#
      i_foldlM f base' mbytes

instance (Unboxed e) => SortM (ST s) (STUblist s e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

lim :: Int
lim =  1024



