{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

{- |
    Module      :  SDP.ByteList.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.ByteList.ST@ provides 'STByteList' - mutable unboxed strict unrolled
    linked list.
-}

module SDP.ByteList.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STByteList
  STByteList (..), STBytes#, fromPseudoMutableBytes#
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM

import GHC.Base ( Int (..) )
import GHC.ST   ( ST  (..) )

import SDP.ByteList.STUblist
import SDP.SortM.Tim
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | STByteList is mutable version of ByteList.
data STByteList s i e = STByteList !i !i (STUblist s e)

type role STByteList nominal nominal representational

--------------------------------------------------------------------------------

{- Eq instance. -}

instance (Index i, Unboxed e) => Eq (STByteList s i e)
  where
    (STByteList l1 u1 xs) == (STByteList l2 u2 ys) =
      isEmpty (l1, u1) && isEmpty (l2, u2) || xs == ys

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance (Index i, Unboxed e) => BorderedM (ST s) (STByteList s i e) i e
  where
    getLower  (STByteList l _ _) = return l
    getUpper  (STByteList _ u _) = return u
    getBounds (STByteList l u _) = return (l, u)
    getSizeOf (STByteList l u _) = return $ size (l, u)

instance (Index i, Unboxed e) => LinearM (ST s) (STByteList s i e) e
  where
    newLinear es = STByteList l u <$> newLinear es
      where
        (l, u) = defaultBounds $ sizeOf es
    
    filled n e = let (l, u) = defaultBounds n in STByteList l u <$> filled n e
    
    getLeft  (STByteList _ _ es) = getLeft es
    copied   (STByteList l u es) = STByteList l u <$> copied  es
    copied'  (STByteList l u es) = \ l' u' -> STByteList l u <$> copied' es l' u'
    reversed (STByteList l u es) = STByteList l u <$> reversed es

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Index i, Unboxed e) => IndexedM (ST s) (STByteList s i e) i e
  where
    fromAssocs (l, u) ascs = STByteList l u <$> fromAssocs bnds ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size  (l, u) - 1)
    
    {-# INLINE fromAssocs' #-}
    fromAssocs' (l, u) defvalue ascs = STByteList l u <$> fromAssocs' bnds defvalue ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    (STByteList _ _ ubl#) !#> i = ubl# !#> i
    
    (STByteList l u ubl#) >! i = ubl# >! offset  (l, u) i
    (STByteList l u ubl#) !> i = case inBounds (l, u) i of
        ER -> throw $ EmptyRange     msg
        UR -> throw $ IndexUnderflow msg
        IN -> ubl# >! offset (l, u) i
        OR -> throw $ IndexOverflow  msg
      where
        msg = "in SDP.ByteList.ST.(!>)"
    
    writeM_ (STByteList _ _ ubl#) = writeM ubl#
    writeM  (STByteList l u ubl#) = writeM ubl# . offset (l, u)
    
    overwrite es [] = return es
    overwrite (STByteList l u ubl#) ascs = if isEmpty (l, u)
        then fromAssocs (l', u') ascs
        else STByteList l u <$> overwrite ubl# ies
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        l'  = fst $ minimumBy cmpfst ascs
        u'  = fst $ maximumBy cmpfst ascs
    
    fromIndexed' es = STByteList l u <$> fromIndexed' es
      where
        (l, u) = defaultBounds $ sizeOf es
    
    fromIndexedM es = do
      es' <- fromIndexedM es
      n   <- getSizeOf es'
      return $ let (l, u) = defaultBounds n in STByteList l u es'

instance (Index i, Unboxed e) => IFoldM (ST s) (STByteList s i e) i e
  where
    {-# INLINE ifoldrM #-}
    ifoldrM f e (STByteList l u ubl#) = ifoldrM (f . index (l, u)) e ubl#
    
    {-# INLINE ifoldlM #-}
    ifoldlM f e (STByteList l u ubl#) = ifoldlM (f . index (l, u)) e ubl#
    
    {-# INLINE i_foldrM #-}
    i_foldrM f e (STByteList _ _ ubl#) = i_foldrM f e ubl#
    
    {-# INLINE i_foldlM #-}
    i_foldlM f e (STByteList _ _ ubl#) = i_foldlM f e ubl#

instance (Index i, Unboxed e) => SortM (ST s) (STByteList s i e) e
  where
    sortMBy = timSortBy



