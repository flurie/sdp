{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

{- |
    Module      :  SDP.Bytes.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Bytes.ST@ provides 'STBytes' - mutable lazy boxed array type.
-}
module SDP.Bytes.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STBytes
  STBytes (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Unboxed

import GHC.Base ( Int (..) )
import GHC.ST   ( ST  (..) )

import SDP.SortM
import SDP.SortM.Tim

import SDP.Internal.SBytes

default ()

--------------------------------------------------------------------------------

-- | STBytes is mutable version of Bytes.
data STBytes s i e = STBytes !i !i (STBytes# s e)

type role STBytes nominal nominal representational

--------------------------------------------------------------------------------

instance (Index i) => Eq (STBytes s i e)
  where
    (STBytes l1 u1 arr1#) == (STBytes l2 u2 arr2#) =
      isEmpty (l1, u1) == isEmpty (l2, u2) || arr1# == arr2#

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance (Index i, Unboxed e) => BorderedM (ST s) (STBytes s i e) i e
  where
    getLower   (STBytes l _ _) = return l
    getUpper   (STBytes _ u _) = return u
    getBounds  (STBytes l u _) = return (l, u)
    getIndices (STBytes l u _) = return $ range (l, u)
    getIndexOf (STBytes l u _) = return . inRange (l, u)
    
    getSizeOf  (STBytes _ _ mbytes#) = getSizeOf mbytes#

instance (Index i, Unboxed e) => LinearM (ST s) (STBytes s i e) e
  where
    prepend e (STBytes _ _ mbytes#) = withBounds =<< prepend e mbytes#
    append  (STBytes _ _ mbytes#) e = withBounds =<< append  mbytes# e
    
    newLinear  = fromFoldableM
    filled n e = filled n e >>= withBounds
    
    newLinearN  n es = newLinearN  n es >>= withBounds
    fromFoldableM es = fromFoldableM es >>= withBounds
    
    getLeft  (STBytes _ _ mbytes#) = getLeft  mbytes#
    getRight (STBytes _ _ mbytes#) = getRight mbytes#
    reversed (STBytes l u mbytes#) = STBytes l u <$> reversed mbytes#
    
    copied  (STBytes l u mbytes#) = STBytes l u <$> copied mbytes#
    copied' (STBytes _ _ mbytes#) = \ o c -> copied' mbytes# o c >>= withBounds

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Index i, Unboxed e) => IndexedM (ST s) (STBytes s i e) i e
  where
    fromAssocs bs@(l, u) ascs = STBytes l u <$> mbytes#
      where
        mbytes# = defaultBounds (size bs) `fromAssocs` ies
        ies     = [ (offset bs i, e) | (i, e) <- ascs, inRange bs i ]
    
    fromAssocs' bs@(l, u) defvalue ascs = STBytes l u <$> mbytes#
      where
        mbytes# = defaultBounds (size bs) `fromAssocs'` defvalue $ ies
        ies     = [ (offset bs i, e) | (i, e) <- ascs, inRange bs i ]
    
    {-# INLINE (!#>) #-}
    (!#>) (STBytes _ _ mbytes#) = (mbytes# !#>)
    
    {-# INLINE (>!) #-}
    (>!) (STBytes l u mbytes#) = (mbytes# !#>) . offset (l, u)
    
    {-# INLINE writeM_ #-}
    writeM_ (STBytes _ _ mbytes#) = writeM_ mbytes#
    
    {-# INLINE writeM #-}
    writeM (STBytes l u mbytes#) = \ i -> writeM mbytes# $ offset (l, u) i
    
    overwrite es@(STBytes l u _) ascs = mapM_ (uncurry $ writeM_ es) ies >> return es
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
    
    fromIndexed' es = fromIndexed' es >>= withBounds
    fromIndexedM es = fromIndexedM es >>= withBounds

instance (Index i, Unboxed e) => IFoldM (ST s) (STBytes s i e) i e
  where
    ifoldrM f base = \ (STBytes l u mbytes#) -> ifoldrM (f . index (l, u)) base mbytes#
    ifoldlM f base = \ (STBytes l u mbytes#) -> ifoldlM (f . index (l, u)) base mbytes#
    
    i_foldrM f base = \ (STBytes _ _ mbytes#) -> i_foldrM f base mbytes#
    i_foldlM f base = \ (STBytes _ _ mbytes#) -> i_foldlM f base mbytes#

instance (Index i, Unboxed e) => SortM (ST s) (STBytes s i e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

{-# INLINE withBounds #-}
withBounds :: (Index i, Unboxed e) => STBytes# s e -> ST s (STBytes s i e)
withBounds marr# = do
  (l, u) <- defaultBounds <$> getSizeOf marr#
  return (STBytes l u marr#)



