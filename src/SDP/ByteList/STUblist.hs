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
    
    getSizeOf (STUblist mbytes# mbytes) = liftA2 (+) (getSizeOf mbytes#) (getSizeOf mbytes)
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
    
    (!#>) (STUblist mbytes# mbytes) i = getSizeOf mbytes# >>=
      \ n -> i < n ? mbytes# !#> i $ mbytes !#> (i - n)
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
    writeM_ (STUblist mbytes# mbytes) i e = getSizeOf mbytes# >>=
      \ n -> i < n ? writeM_ mbytes# i e $ writeM_ mbytes (i - n) e
    
    writeM es i e = i < 0 ? return () $ writeM_ es i e
    
    {-# INLINE overwrite #-}
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
    {-# INLINE ifoldrM #-}
    ifoldrM f base (STUblist mbytes# mbytes) = ifoldrM f base mbytes >>=
      \ base' -> ifoldrM f base' mbytes#
    ifoldrM _ base _ = return base
    
    {-# INLINE ifoldlM #-}
    ifoldlM f base (STUblist mbytes# mbytes) = ifoldlM f base mbytes# >>=
      \ base' -> ifoldlM f base' mbytes
    ifoldlM _ base _ = return base
    
    {-# INLINE i_foldrM #-}
    i_foldrM f base (STUblist mbytes# mbytes) = i_foldrM f base mbytes >>=
      \ base' -> i_foldrM f base' mbytes#
    i_foldrM _ base _ = return base
    
    {-# INLINE i_foldlM #-}
    i_foldlM f base (STUblist mbytes# mbytes) = i_foldlM f base mbytes# >>=
      \ base' -> i_foldlM f base' mbytes
    i_foldlM _ base _ = return base

instance (Unboxed e) => SortM (ST s) (STUblist s e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

lim :: Int
lim =  1024




