{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

{- |
    Module      :  SDP.Array.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Array.ST@ provides 'STArray' - mutable lazy boxed array type.
-}
module SDP.Array.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STArray
  STArray (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM

import GHC.Base ( Int (..) )
import GHC.ST   ( ST  (..) )

import SDP.SortM
import SDP.SortM.Tim

import SDP.Internal.SArray

default ()

--------------------------------------------------------------------------------

-- | STArray is mutable version of Array.
data STArray s i e = STArray !i !i (STArray# s e)

type role STArray nominal nominal representational

--------------------------------------------------------------------------------

instance (Index i) => Eq (STArray s i e)
  where
    (STArray l1 u1 arr1#) == (STArray l2 u2 arr2#) =
      isEmpty (l1, u1) == isEmpty (l2, u2) || arr1# == arr2#

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance (Index i) => BorderedM (ST s) (STArray s i e) i e
  where
    getLower   (STArray l _ _) = return l
    getUpper   (STArray _ u _) = return u
    getBounds  (STArray l u _) = return (l, u)
    getIndices (STArray l u _) = return $ range (l, u)
    getIndexOf (STArray l u _) = return . inRange (l, u)
    
    getSizeOf  (STArray _ _ marr#) = getSizeOf marr#

instance (Index i) => LinearM (ST s) (STArray s i e) e
  where
    newLinear = fromFoldableM
    
    newLinearN  n es = newLinearN  n es >>= withBounds
    fromFoldableM es = fromFoldableM es >>= withBounds
    
    getLeft  (STArray _ _ marr#) = getLeft  marr#
    getRight (STArray _ _ marr#) = getRight marr#
    reversed (STArray l u marr#) = STArray l u <$> reversed marr#
    
    copied  (STArray l u marr#) = STArray l u <$> copied marr#
    copied' (STArray _ _ marr#) = \ o c -> copied' marr# o c >>= withBounds
    
    filled n e = filled n e >>= withBounds

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Index i) => IndexedM (ST s) (STArray s i e) i e
  where
    fromAssocs' bs@(l, u) defvalue ascs = STArray l u <$> marr#
      where
        marr# = defaultBounds (size bs) `fromAssocs'` defvalue $ ies
        ies   = [ (offset bs i, e) | (i, e) <- ascs, inRange bs i ]
    
    {-# INLINE (!#>) #-}
    (!#>) (STArray _ _ marr#) = (marr# !#>)
    
    {-# INLINE (>!) #-}
    (>!) (STArray l u marr#) = (marr# !#>) . offset (l, u)
    
    {-# LANGUAGE writeM_ #-}
    writeM_ (STArray _ _ marr#) = writeM_ marr#
    
    {-# INLINE writeM #-}
    writeM  (STArray l u marr#) = writeM_ marr# . offset (l, u)
    
    overwrite es@(STArray l u _) ascs = mapM_ (uncurry $ writeM_ es) ies >> return es
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
    
    fromIndexed' es = fromIndexed' es >>= withBounds
    fromIndexedM es = fromIndexedM es >>= withBounds

instance (Index i) => IFoldM (ST s) (STArray s i e) i e
  where
    ifoldrM f base (STArray l u marr#) = ifoldrM (f . index (l, u)) base marr#
    ifoldlM f base (STArray l u marr#) = ifoldlM (f . index (l, u)) base marr#
    
    i_foldrM f base (STArray _ _ marr#) = i_foldrM f base marr#
    i_foldlM f base (STArray _ _ marr#) = i_foldlM f base marr#

instance (Index i) => SortM (ST s) (STArray s i e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

{-# INLINE withBounds #-}
withBounds :: (Index i) => STArray# s e -> ST s (STArray s i e)
withBounds marr# = do
  (l, u) <- defaultBounds <$> getSizeOf marr#
  return (STArray l u marr#)

