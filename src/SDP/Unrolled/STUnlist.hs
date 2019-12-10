{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

{- |
    Module      :  SDP.Unrolled.STUnlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Unrolled.STUnlist@ provides 'STUnlist' - mutable boxed lazy unrolled
    linked list.
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
data STUnlist s e = STUNEmpty | STUnlist !(STArray# s e) (STUnlist s e) deriving ( Eq )

type role STUnlist nominal representational

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
    newLinear es = liftA2 (foldr STUnlist) rest' chs'
      where
        rest' = (`STUnlist` STUNEmpty) <$> newLinear rest
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
    fromAssocs' bnds@(l, _) defvalue ascs = do
      arr <- size bnds `filled` defvalue
      overwrite arr [ (i - l, e) | (i, e) <- ascs, inRange bnds i ]
    
    (!#>) (STUnlist marr# marr) i = do
      n <- getSizeOf marr#
      i < n ? marr# !#> i $ marr !#> (i - n)
    (!#>) _ _ = throw $ IndexOverflow "in SDP.Unrolled.STUnlist.(>!)"
    
    {-# INLINE (>!) #-}
    (>!) es i = i < 0 ? err $ es !#> i
      where
        err = throw $ IndexUnderflow "in SDP.Unrolled.STUnlist.(>!)"
    
    writeM_ (STUnlist marr# marr) i e = do
      n <- getSizeOf marr#
      i < n ? writeM_ marr# i e $ writeM_ marr (i - n) e
    writeM_ _ _ _ = return ()
    
    {-# INLINE writeM #-}
    writeM es i e = i < 0 ? return () $ writeM_ es i e
    
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
    ifoldrM _ base STUNEmpty = return base
    ifoldrM f base (STUnlist marr# marr) = do
      base' <- ifoldrM f base marr
      ifoldrM f base' marr#
    
    ifoldlM _ base STUNEmpty = return base
    ifoldlM f base (STUnlist marr# marr) = do
      base' <- ifoldlM f base marr#
      ifoldlM f base' marr
    
    i_foldrM _ base STUNEmpty = return base
    i_foldrM f base (STUnlist marr# marr) = do
      base' <- i_foldrM f base marr
      i_foldrM f base' marr#
    
    i_foldlM _ base STUNEmpty = return base
    i_foldlM f base (STUnlist marr# marr) = do
      base' <- i_foldlM f base marr#
      i_foldlM f base' marr

instance SortM (ST s) (STUnlist s e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

lim :: Int
lim =  1024

