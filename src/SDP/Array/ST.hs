{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

{- |
    Module      :  SDP.Array.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.Array.ST@ provides 'STArray' - mutable lazy boxed array type.
-}
module SDP.Array.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STArray
  STArray (..), STArray#, fromPseudoMutableArray#
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
import SDP.Simple

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
    {-# INLINE getLower #-}
    getLower   (STArray l _ _) = return l
    
    {-# INLINE getUpper #-}
    getUpper   (STArray _ u _) = return u
    
    {-# INLINE getBounds #-}
    getBounds  (STArray l u _) = return (l, u)
    
    {-# INLINE getSizeOf #-}
    getSizeOf  (STArray _ _ marr#) = getSizeOf marr#
    
    {-# INLINE getIndices #-}
    getIndices (STArray l u _) = return $ range (l, u)
    
    {-# INLINE getIndexOf #-}
    getIndexOf (STArray l u _) = return . inRange (l, u)

instance (Index i) => LinearM (ST s) (STArray s i e) e
  where
    newLinear = fromFoldableM
    
    {-# INLINE newLinearN #-}
    newLinearN n es = newLinearN n es >>= withBounds
    
    {-# INLINE fromFoldableM #-}
    fromFoldableM es = fromFoldableM es >>= withBounds
    
    getLeft  (STArray _ _ marr#) = getLeft  marr#
    getRight (STArray _ _ marr#) = getRight marr#
    
    {-# INLINE copied #-}
    copied (STArray l u marr#) = STArray l u <$> copied marr#
    
    {-# INLINE copied' #-}
    copied' (STArray _ _ marr#) o c = copied' marr# o c >>= withBounds
    
    {-# INLINE reversed #-}
    reversed (STArray l u marr#) = STArray l u <$> reversed marr#
    
    {-# INLINE filled #-}
    filled n e = filled n e >>= withBounds

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Index i) => IndexedM (ST s) (STArray s i e) i e
  where
    {-# INLINE fromAssocs' #-}
    fromAssocs' bs@(l, u) defvalue ascs = STArray l u <$> marr#
      where
        marr# = defaultBounds (size bs) `fromAssocs'` defvalue $ ies
        ies   = [ (offset bs i, e) | (i, e) <- ascs, inRange bs i ]
    
    {-# INLINE (!#>) #-}
    (!#>) (STArray _ _ marr#) = (marr# !#>)
    
    {-# INLINE (>!) #-}
    (>!) (STArray l u marr#) = (marr# !#>) . offset (l, u)
    
    {-# INLINE (!>) #-}
    (!>) (STArray l u marr#) = \ i -> case inBounds (l, u) i of
        ER -> throw $ EmptyRange     msg
        UR -> throw $ IndexUnderflow msg
        IN -> marr# !#> offset (l, u) i
        OR -> throw $ IndexOverflow  msg
      where
        msg = "in SDP.Array.ST.(!>)"
    
    writeM_ (STArray _ _ marr#) = writeM_ marr#
    
    {-# INLINE writeM #-}
    writeM (STArray l u marr#) = \ i -> writeM marr# $ offset (l, u) i
    
    {-# INLINE overwrite #-}
    overwrite es@(STArray l u _) ascs = mapM_ (uncurry $ writeM_ es) ies >> return es
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
    
    {-# INLINE fromIndexed' #-}
    fromIndexed' es = fromIndexed' es >>= withBounds
    
    {-# INLINE fromIndexedM #-}
    fromIndexedM es = fromIndexedM es >>= withBounds

instance (Index i) => IFoldM (ST s) (STArray s i e) i e
  where
    {-# INLINE ifoldrM #-}
    ifoldrM  f base (STArray l u marr#) = ifoldrM (f . index (l, u)) base marr#
    
    {-# INLINE ifoldlM #-}
    ifoldlM  f base (STArray l u marr#) = ifoldlM (f . index (l, u)) base marr#
    
    {-# INLINE i_foldrM #-}
    i_foldrM f base (STArray _ _ marr#) = i_foldrM f base marr#
    
    {-# INLINE i_foldlM #-}
    i_foldlM f base (STArray _ _ marr#) = i_foldlM f base marr#

instance (Index i) => SortM (ST s) (STArray s i e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

{-# INLINE withBounds #-}
withBounds :: (Index i) => STArray# s e -> ST s (STArray s i e)
withBounds marr# = (\ n -> let (l, u) = defaultBounds n in STArray l u marr#) <$> getSizeOf marr#




