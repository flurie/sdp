{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, TypeFamilies, RoleAnnotations #-}

{- |
    Module      :  SDP.Bytes
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    SDP.Bytes provides 'Bytes' - immutable strict unboxed array type.
-}
module SDP.Bytes
(
  -- * Exports
  module SDP.Unboxed,
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Set,
  
  -- * Bytes
  Bytes (..), fromPseudoBytes#
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Set

import Text.Read
import Text.Read.Lex ( expect )

import GHC.Base ( Int  (..) )
import GHC.Show (  appPrec  )

import GHC.ST   ( runST, ST (..) )

import qualified GHC.Exts as E
import Data.String ( IsString (..) )

import SDP.Bytes.ST

import SDP.Internal.SBytes
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | Bytes - unboxed array.
data Bytes i e = Bytes !i !i !(SBytes# e) deriving ( Eq )

type role Bytes nominal representational

--------------------------------------------------------------------------------

{- Ord instance. -}

instance (Index i, Unboxed e, Ord e) => Ord (Bytes i e)
  where
    compare (Bytes l1 u1 arr1#) (Bytes l2 u2 arr2#) =
      (arr1# <=> arr2#) <> (size (l1, u1) <=> size (l2, u2))

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance (Index i, Unboxed e) => Semigroup (Bytes i e) where (<>) = (++)
instance (Index i, Unboxed e) => Monoid    (Bytes i e) where mempty = Z
instance (Index i, Unboxed e) => Default   (Bytes i e)
  where
    def = let (l, u) = defaultBounds 0 in Bytes l u def

instance (Index i, Unboxed e, Arbitrary e) => Arbitrary (Bytes i e)
  where
    arbitrary = fromList <$> arbitrary

instance Estimate (Bytes i e)
  where
    (Bytes _ _ arr1#) <==> (Bytes _ _ arr2#) = arr1# <==> arr2#
    (Bytes _ _ arr1#) .>.  (Bytes _ _ arr2#) = arr1# .>.  arr2#
    (Bytes _ _ arr1#) .<.  (Bytes _ _ arr2#) = arr1# .<.  arr2#
    (Bytes _ _ arr1#) .<=. (Bytes _ _ arr2#) = arr1# .<=. arr2#
    (Bytes _ _ arr1#) .>=. (Bytes _ _ arr2#) = arr1# .>=. arr2#
    
    (Bytes _ _ arr1#) <.=> n2 = arr1# <.=> n2
    (Bytes _ _ arr1#)  .>  n2 = arr1#  .>  n2
    (Bytes _ _ arr1#)  .<  n2 = arr1#  .<  n2
    (Bytes _ _ arr1#) .>=  n2 = arr1# .>=  n2
    (Bytes _ _ arr1#) .<=  n2 = arr1# .<=  n2

--------------------------------------------------------------------------------

instance (Index i, Unboxed e) => E.IsList (Bytes i e)
  where
    type Item (Bytes i e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = listL

instance (Index i) => IsString (Bytes i Char) where fromString = fromList

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Index i, Unboxed e, Show i, Show e) => Show (Bytes i e)
  where
    showsPrec p arr@(Bytes l u _) = showParen (p > appPrec) $ showString "bytes "
                                                            . shows (l, u)
                                                            . showChar ' '
                                                            . shows (assocs arr)

instance (Index i, Unboxed e, Read i, Read e) => Read (Bytes i e)
  where
    readList = readListDefault
    readPrec = parens $ do
      prec appPrec (lift . expect $ Ident "bytes")
      liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i, Unboxed e) => Linear (Bytes i e) e
  where
    isNull (Bytes _ _ arr#) = isNull arr#
    
    {-# INLINE lzero #-}
    lzero = withBounds Z
    
    toHead e (Bytes _ _ arr#) = withBounds $ e :> arr#
    
    head es = isNull es ? pfailEx "(:>)" $ es !^ 0
    
    -- | O(1) 'tail', O(1) memory.
    tail Z = pfailEx "(:>)"
    tail (Bytes _ _ arr#) = withBounds $ tail arr#
    
    toLast (Bytes _ _ arr#) e = withBounds $ arr# :< e
    
    last es = isNull es ? pfailEx "(:<)" $ es !^ (sizeOf es - 1)
    
    -- | O(1) 'init', O(1) memory.
    init Z = pfailEx "(:<)"
    init (Bytes _ _ arr#) = withBounds $ init arr#
    
    fromList = fromFoldable
    
    {-# INLINE fromListN #-}
    fromListN n = withBounds . fromListN n
    
    {-# INLINE fromFoldable #-}
    fromFoldable = withBounds . fromFoldable
    
    {-# INLINE single #-}
    single = withBounds . single
    
    -- | O(n + m) '++', O(n + m) memory.
    (Bytes _ _ xs) ++ (Bytes _ _ ys) = withBounds $ xs ++ ys
    
    {-# INLINE replicate #-}
    -- | O(n) 'replicate', O(n) memory.
    replicate n = withBounds . replicate n
    
    listL (Bytes _ _ arr#) = listL arr#
    listR (Bytes _ _ arr#) = listR arr#
    
    {-# INLINE concatMap #-}
    concatMap f = fromList . foldr (\ a l -> i_foldr (:) l $ f a) []
    
    {-# INLINE concat #-}
    concat = fromList . foldr (\ a l -> i_foldr (:) l a) []
    
    partitions f es = fromList <$> partitions f (listL es)

instance (Index i, Unboxed e) => Split (Bytes i e) e
  where
    {-# INLINE take #-}
    -- | O(1) 'take', O(1) memory.
    take n (Bytes _ _ arr#) = withBounds $ take n arr#
    
    {-# INLINE drop #-}
    -- | O(1) 'drop', O(1) memory.
    drop n (Bytes _ _ arr#) = withBounds $ drop n arr#
    
    -- | O(m) 'splits', O(m) memory (m - sizes list length).
    splits ns (Bytes _ _ arr#) = withBounds <$> splits ns arr#
    
    -- | O(m) 'chuncks', O(m) memory (m - sizes list length).
    chunks ns (Bytes _ _ arr#) = withBounds <$> chunks ns arr#
    
    -- | O(m) 'parts', O(m) memory (m - sizes list length).
    parts  ns (Bytes _ _ arr#) = withBounds <$> parts  ns arr#
    
    isPrefixOf (Bytes l1 u1 arr1#) (Bytes l2 u2 arr2#) =
      size (l1, u1) <= size (l2, u2) && arr1# `isPrefixOf` arr2#
    
    isSuffixOf (Bytes l1 u1 arr1#) (Bytes l2 u2 arr2#) =
      size (l1, u1) <= size (l2, u2) && arr1# `isSuffixOf` arr2#
    
    {-# INLINE prefix #-}
    prefix p = i_foldr (\ e c -> p e ? c + 1 $ 0) 0
    
    {-# INLINE suffix #-}
    suffix p = i_foldl (\ c e -> p e ? c + 1 $ 0) 0

instance (Index i, Unboxed e) => Bordered (Bytes i e) i e
  where
    {-# INLINE indexIn #-}
    indexIn  (Bytes l u _) = inRange (l, u)
    
    {-# INLINE offsetOf #-}
    offsetOf (Bytes l u _) = offset (l, u)
    
    {-# INLINE indexOf #-}
    indexOf  (Bytes l u _) = index (l, u)
    
    {-# INLINE sizeOf #-}
    sizeOf (Bytes _ _ arr#) = sizeOf arr#
    
    {-# INLINE bounds #-}
    bounds (Bytes l u _) = (l, u)
    
    {-# INLINE lower #-}
    lower  (Bytes l _ _) = l
    
    {-# INLINE upper #-}
    upper  (Bytes _ u _) = u

--------------------------------------------------------------------------------

{- Indexed, IFold, Set and Sort instances. -}

instance (Index i, Unboxed e) => Indexed (Bytes i e) i e
  where
    {-# INLINE assoc #-}
    assoc bnds@(l, u) ascs = Bytes l u $ assoc bnds' ies
      where
        ies   = [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
        bnds' = defaultBounds $ size bnds
    
    {-# INLINE assoc' #-}
    assoc' bnds@(l, u) defvalue ascs = Bytes l u $ assoc' bnds' defvalue ies
      where
        ies   = [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
        bnds' = defaultBounds $ size bnds
    
    fromIndexed = withBounds . fromIndexed
    
    {-# INLINE (//) #-}
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    arr // ascs = runST $ thaw arr >>= (`overwrite` ascs) >>= done
    
    {-# INLINE (!^) #-}
    (!^) (Bytes _ _ arr#) = (arr# !^)
    
    {-# INLINE (!) #-}
    (!) (Bytes l u arr#) = (arr# !^) . offset (l, u)
    
    (*$) p = ifoldr (\ i e is -> p e ? (i : is) $ is) []

instance (Index i, Unboxed e) => IFold (Bytes i e) i e
  where
    {-# INLINE ifoldr #-}
    ifoldr f = \ base (Bytes l u arr#) -> ifoldr (\ i -> f $ index (l, u) i) base arr#
    
    {-# INLINE ifoldl #-}
    ifoldl f = \ base (Bytes l u arr#) -> ifoldl (\ i -> f $ index (l, u) i) base arr#
    
    {-# INLINE i_foldr #-}
    i_foldr f = \ base (Bytes _ _ arr#) -> i_foldr f base arr#
    
    {-# INLINE i_foldl #-}
    i_foldl f = \ base (Bytes _ _ arr#) -> i_foldl f base arr#

instance (Index i, Unboxed e) => Set (Bytes i e) e
  where
    setWith f (Bytes _ _ arr#) = withBounds $ setWith f arr#
    
    insertWith f e (Bytes _ _ arr#) = withBounds $ insertWith f e arr#
    deleteWith f e (Bytes _ _ arr#) = withBounds $ deleteWith f e arr#
    
    {-# INLINE intersectionWith #-}
    intersectionWith f (Bytes _ _ arr1#) (Bytes _ _ arr2#) =
      withBounds $ intersectionWith f arr1# arr2#
    
    {-# INLINE unionWith #-}
    unionWith f (Bytes _ _ arr1#) (Bytes _ _ arr2#) =
      withBounds $ unionWith f arr1# arr2#
    
    {-# INLINE differenceWith #-}
    differenceWith f (Bytes _ _ arr1#) (Bytes _ _ arr2#) =
      withBounds $ differenceWith f arr1# arr2#
    
    {-# INLINE symdiffWith #-}
    symdiffWith f (Bytes _ _ arr1#) (Bytes _ _ arr2#) =
      withBounds $ differenceWith f arr1# arr2#
    
    isContainedIn f e (Bytes _ _   es) = isContainedIn f e   es
    lookupLTWith  f o (Bytes _ _ arr#) = lookupLTWith  f o arr#
    lookupGTWith  f o (Bytes _ _ arr#) = lookupGTWith  f o arr#
    lookupLEWith  f o (Bytes _ _ arr#) = lookupLEWith  f o arr#
    lookupGEWith  f o (Bytes _ _ arr#) = lookupGEWith  f o arr#
    
    isSubsetWith f (Bytes _ _ xs) (Bytes _ _ ys) = isSubsetWith f xs ys

instance (Index i, Unboxed e) => Sort (Bytes i e) e
  where
    sortBy cmp (Bytes l u arr#) = Bytes l u (sortBy cmp arr#)

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance (Index i, Unboxed e) => Thaw (ST s) (Bytes i e) (STBytes s i e)
  where
    thaw       (Bytes l u arr#) = STBytes l u <$> thaw arr#
    unsafeThaw (Bytes l u arr#) = STBytes l u <$> unsafeThaw arr#

instance (Index i, Unboxed e) => Freeze (ST s) (STBytes s i e) (Bytes i e)
  where
    freeze       (STBytes l u marr#) = Bytes l u <$> freeze marr#
    unsafeFreeze (STBytes l u marr#) = Bytes l u <$> unsafeFreeze marr#

--------------------------------------------------------------------------------

{-# INLINE withBounds #-}
withBounds :: (Index i, Unboxed e) => SBytes# e -> Bytes i e
withBounds arr# = let (l, u) = defaultBounds (sizeOf arr#) in Bytes l u arr#

{-# INLINE done #-}
done :: (Unboxed e) => STBytes s i e -> ST s (Bytes i e)
done (STBytes l u marr#) = Bytes l u <$> unsafeFreeze marr#

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.Bytes." ++ msg



