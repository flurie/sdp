{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

{- |
    Module      :  SDP.Bytes.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.Bytes.ST@ provides 'STBytes' - mutable lazy boxed array type.
-}
module SDP.Bytes.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STBytes
  STBytes (..), fromPseudoMutableBytes#
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
import SDP.Simple

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
    {-# INLINE getLower #-}
    getLower   (STBytes l _ _) = return l
    
    {-# INLINE getUpper #-}
    getUpper   (STBytes _ u _) = return u
    
    {-# INLINE getBounds #-}
    getBounds  (STBytes l u _) = return (l, u)
    
    {-# INLINE getSizeOf #-}
    getSizeOf  (STBytes _ _ marr#) = getSizeOf marr#
    
    {-# INLINE getIndices #-}
    getIndices (STBytes l u _) = return $ range (l, u)
    
    {-# INLINE getIndexOf #-}
    getIndexOf (STBytes l u _) = return . inRange (l, u)

instance (Index i, Unboxed e) => LinearM (ST s) (STBytes s i e) e
  where
    newLinear = fromFoldableM
    
    {-# INLINE newLinearN #-}
    newLinearN n es = newLinearN n es >>= withBounds
    
    {-# INLINE fromFoldableM #-}
    fromFoldableM es = fromFoldableM es >>= withBounds
    
    getLeft  (STBytes _ _ marr#) = getLeft  marr#
    getRight (STBytes _ _ marr#) = getRight marr#
    
    {-# INLINE copied #-}
    copied (STBytes l u marr#) = STBytes l u <$> copied marr#
    
    {-# INLINE copied' #-}
    copied' (STBytes _ _ marr#) o c = copied' marr# o c >>= withBounds
    
    {-# INLINE reversed #-}
    reversed (STBytes l u marr#) = STBytes l u <$> reversed marr#
    
    {-# INLINE filled #-}
    filled n e = filled n e >>= withBounds

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Index i, Unboxed e) => IndexedM (ST s) (STBytes s i e) i e
  where
    {-# INLINE fromAssocs' #-}
    fromAssocs' bs@(l, u) defvalue ascs = STBytes l u <$> marr#
      where
        marr# = defaultBounds (size bs) `fromAssocs'` defvalue $ ies
        ies   = [ (offset bs i, e) | (i, e) <- ascs, inRange bs i ]
    
    {-# INLINE (!#>) #-}
    (!#>) (STBytes _ _ marr#) = (marr# !#>)
    
    {-# INLINE (>!) #-}
    (>!) (STBytes l u marr#) = (marr# !#>) . offset (l, u)
    
    {-# INLINE (!>) #-}
    (!>) (STBytes l u marr#) = \ i -> case inBounds (l, u) i of
        ER -> throw $ EmptyRange     msg
        UR -> throw $ IndexUnderflow msg
        IN -> marr# !#> offset (l, u) i
        OR -> throw $ IndexOverflow  msg
      where
        msg = "in SDP.Array.ST.(!>)"
    
    writeM_ (STBytes _ _ marr#) = writeM_ marr#
    
    {-# INLINE writeM #-}
    writeM (STBytes l u marr#) = \ i -> writeM marr# $ offset (l, u) i
    
    {-# INLINE overwrite #-}
    overwrite es@(STBytes l u _) ascs = mapM_ (uncurry $ writeM_ es) ies >> return es
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
    
    {-# INLINE fromIndexed' #-}
    fromIndexed' es = fromIndexed' es >>= withBounds
    
    {-# INLINE fromIndexedM #-}
    fromIndexedM es = fromIndexedM es >>= withBounds

instance (Index i, Unboxed e) => IFoldM (ST s) (STBytes s i e) i e
  where
    {-# INLINE ifoldrM #-}
    ifoldrM  f = \ base (STBytes l u marr#) ->
      ifoldrM (\ i -> f $ index (l, u) i) base marr#
    
    {-# INLINE ifoldlM #-}
    ifoldlM  f = \ base (STBytes l u marr#) ->
      ifoldlM (\ i -> f $ index (l, u) i) base marr#
    
    {-# INLINE i_foldrM #-}
    i_foldrM f = \ base (STBytes _ _ marr#) -> i_foldrM f base marr#
    
    {-# INLINE i_foldlM #-}
    i_foldlM f = \ base (STBytes _ _ marr#) -> i_foldlM f base marr#

instance (Index i, Unboxed e) => SortM (ST s) (STBytes s i e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

{-# INLINE withBounds #-}
withBounds :: (Index i, Unboxed e) => STBytes# s e -> ST s (STBytes s i e)
withBounds marr# = (\ n -> let (l, u) = defaultBounds n in STBytes l u marr#) <$> getSizeOf marr#

