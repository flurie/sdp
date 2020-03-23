{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

{- |
    Module      :  SDP.ByteList.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.ByteList.ST@ provides 'STByteList' - mutable unboxed strict unrolled
    linked list.
-}

module SDP.ByteList.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STByteList
  STByteList (..)
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

default ()

--------------------------------------------------------------------------------

-- | STByteList is mutable version of ByteList.
data STByteList s i e = STByteList !i !i (STUblist s e)

type role STByteList nominal nominal representational

--------------------------------------------------------------------------------

{- Eq instance. -}

instance (Index i) => Eq (STByteList s i e)
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
    prepend e = withBounds <=< prepend e . unpack
    append es = withBounds <=< append (unpack es)
    
    newLinear = withBounds <=< newLinear
    filled  n = withBounds <=< filled n
    
    getLeft   = getLeft  . unpack
    getRight  = getRight . unpack
    
    copied   (STByteList l u es) = STByteList l u <$> copied  es
    copied'  (STByteList l u es) = (STByteList l u <$>) ... copied' es
    reversed (STByteList l u es) = STByteList l u <$> reversed es

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Index i, Unboxed e) => IndexedM (ST s) (STByteList s i e) i e
  where
    fromAssocs (l, u) ascs = STByteList l u <$> fromAssocs bnds ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size  (l, u) - 1)
    
    fromAssocs' (l, u) defvalue ascs = STByteList l u <$> fromAssocs' bnds defvalue ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    {-# INLINE (!#>) #-}
    (!#>) es = (unpack es !#>)
    
    {-# INLINE (>!) #-}
    (>!) (STByteList l u es) = (es !#>) . offset (l, u)
    
    {-# INLINE writeM_ #-}
    writeM_ = writeM . unpack
    
    {-# INLINE writeM #-}
    writeM  (STByteList l u es) = writeM es . offset (l, u)
    
    overwrite es [] = return es
    overwrite (STByteList l u es) ascs = if isEmpty (l, u)
        then fromAssocs (l', u') ascs
        else STByteList l u <$> overwrite es ies
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        l'  = fst $ minimumBy cmpfst ascs
        u'  = fst $ maximumBy cmpfst ascs
    
    fromIndexed' = withBounds <=< fromIndexed'
    fromIndexedM = withBounds <=< fromIndexedM

instance (Index i, Unboxed e) => IFoldM (ST s) (STByteList s i e) i e
  where
    ifoldrM f e (STByteList l u es) = ifoldrM (f . index (l, u)) e es
    ifoldlM f e (STByteList l u es) = ifoldlM (f . index (l, u)) e es
    
    i_foldrM f e = i_foldrM f e . unpack
    i_foldlM f e = i_foldlM f e . unpack

instance (Index i, Unboxed e) => SortM (ST s) (STByteList s i e) e
  where
    sortMBy = timSortBy

--------------------------------------------------------------------------------

withBounds :: (Index i, Unboxed e) => STUblist s e -> ST s (STByteList s i e)
withBounds es = do
  n <- getSizeOf es
  let (l, u) = defaultBounds n
  return (STByteList l u es)

unpack :: STByteList s i e -> STUblist s e
unpack =  \ (STByteList _ _ es) -> es

