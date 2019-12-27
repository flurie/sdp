{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, TypeFamilies, RoleAnnotations, DeriveGeneric #-}

{- |
    Module      :  SDP.Bytes
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    SDP.Bytes provides 'Bytes' - immutable strict unboxed array type.
-}
module SDP.Bytes
(
  -- * Exports
  module SDP.Unboxed,
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Bytes
  Bytes (..)
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import GHC.Generics ( Generic (..) )

import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Scan
import SDP.Set

import GHC.ST   ( ST  (..), runST )
import GHC.Base ( Int (..) )

import qualified GHC.Exts as E
import Data.String ( IsString (..) )

import SDP.Bytes.ST

import SDP.Internal.SBytes
import SDP.Internal.Read
import SDP.Internal.Show
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | Bytes - unboxed array.
data Bytes i e = Bytes !i !i !(SBytes# e) deriving ( Generic )

type role Bytes nominal representational

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Index i, Unboxed e) => Eq (Bytes i e)
  where
    Z == Z = True
    (Bytes l1 u1 arr1#) == (Bytes l2 u2 arr2#) =
      l1 == l2 && u1 == u2 && arr1# == arr2#

instance (Index i, Unboxed e, Ord e) => Ord (Bytes i e)
  where
    compare (Bytes l1 u1 bytes1#) (Bytes l2 u2 bytes2#) =
      (bytes1# <=> bytes2#) <> (size (l1, u1) <=> size (l2, u2))

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
    (Bytes _ _ bytes1#) <==> (Bytes _ _ bytes2#) = bytes1# <==> bytes2#
    (Bytes _ _ bytes1#) .>.  (Bytes _ _ bytes2#) = bytes1# .>.  bytes2#
    (Bytes _ _ bytes1#) .<.  (Bytes _ _ bytes2#) = bytes1# .<.  bytes2#
    (Bytes _ _ bytes1#) .<=. (Bytes _ _ bytes2#) = bytes1# .<=. bytes2#
    (Bytes _ _ bytes1#) .>=. (Bytes _ _ bytes2#) = bytes1# .>=. bytes2#
    
    (Bytes _ _ bytes1#) <.=> n2 = bytes1# <.=> n2
    (Bytes _ _ bytes1#)  .>  n2 = bytes1#  .>  n2
    (Bytes _ _ bytes1#)  .<  n2 = bytes1#  .<  n2
    (Bytes _ _ bytes1#) .>=  n2 = bytes1# .>=  n2
    (Bytes _ _ bytes1#) .<=  n2 = bytes1# .<=  n2

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
    showsPrec = assocsPrec "bytes "

instance (Index i, Unboxed e, Read i, Read e) => Read (Bytes i e)
  where
    readList = readListDefault
    readPrec = linearIndexedPrec "bytes"

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i, Unboxed e) => Linear (Bytes i e) e
  where
    isNull (Bytes _ _ bytes#) = isNull bytes#
    
    lzero = withBounds Z
    
    toHead e (Bytes _ _ bytes#) = withBounds $ e :> bytes#
    
    head es = isNull es ? pfailEx "(:>)" $ es !^ 0
    
    -- | O(1) 'tail', O(1) memory.
    tail Z = pfailEx "(:>)"
    tail (Bytes _ _ bytes#) = withBounds $ tail bytes#
    
    toLast (Bytes _ _ bytes#) e = withBounds $ bytes# :< e
    
    last es = isNull es ? pfailEx "(:<)" $ es !^ (sizeOf es - 1)
    
    -- | O(1) 'init', O(1) memory.
    init Z = pfailEx "(:<)"
    init (Bytes _ _ bytes#) = withBounds $ init bytes#
    
    fromList = fromFoldable
    
    fromListN  n = withBounds . fromListN  n
    fromFoldable = withBounds . fromFoldable
    
    single = withBounds . single
    
    -- | O(n + m) '++', O(n + m) memory.
    (Bytes _ _ xs) ++ (Bytes _ _ ys) = withBounds $ xs ++ ys
    
    -- | O(n) 'replicate', O(n) memory.
    replicate n = withBounds . replicate n
    
    listL (Bytes _ _ bytes#) = listL bytes#
    listR (Bytes _ _ bytes#) = listR bytes#
    
    concatMap f = fromList . foldr (\ a l -> i_foldr (:) l $ f a) []
    
    concat = fromList . foldr (\ a l -> i_foldr (:) l a) []
    
    partitions f es = fromList <$> partitions f (listL es)

instance (Index i, Unboxed e) => Split (Bytes i e) e
  where
    -- | O(1) 'take', O(1) memory.
    take n (Bytes _ _ bytes#) = withBounds $ take n bytes#
    
    -- | O(1) 'drop', O(1) memory.
    drop n (Bytes _ _ bytes#) = withBounds $ drop n bytes#
    
    -- | O(m) 'splits', O(m) memory (m - sizes list length).
    splits ns (Bytes _ _ bytes#) = withBounds <$> splits ns bytes#
    
    -- | O(m) 'chuncks', O(m) memory (m - sizes list length).
    chunks ns (Bytes _ _ bytes#) = withBounds <$> chunks ns bytes#
    
    -- | O(m) 'parts', O(m) memory (m - sizes list length).
    parts  ns (Bytes _ _ bytes#) = withBounds <$> parts  ns bytes#
    
    isPrefixOf (Bytes l1 u1 bytes1#) (Bytes l2 u2 bytes2#) =
      size (l1, u1) <= size (l2, u2) && bytes1# `isPrefixOf` bytes2#
    
    isSuffixOf (Bytes l1 u1 bytes1#) (Bytes l2 u2 bytes2#) =
      size (l1, u1) <= size (l2, u2) && bytes1# `isSuffixOf` bytes2#
    
    prefix p = i_foldr (\ e c -> p e ? c + 1 $ 0) 0
    suffix p = i_foldl (\ c e -> p e ? c + 1 $ 0) 0

instance (Index i, Unboxed e) => Bordered (Bytes i e) i e
  where
    offsetOf (Bytes l u _) = offset (l, u)
    
    indexIn  (Bytes l u _) = inRange (l, u)
    indexOf  (Bytes l u _) = index (l, u)
    bounds   (Bytes l u _) = (l, u)
    lower    (Bytes l _ _) = l
    upper    (Bytes _ u _) = u
    
    sizeOf (Bytes _ _ bytes#) = sizeOf bytes#

--------------------------------------------------------------------------------

{- Set, Scan and Sort instances. -}

instance (Index i, Unboxed e) => Set (Bytes i e) e
  where
    setWith f (Bytes _ _ bytes#) = withBounds $ setWith f bytes#
    
    insertWith f e (Bytes _ _ bytes#) = withBounds $ insertWith f e bytes#
    deleteWith f e (Bytes _ _ bytes#) = withBounds $ deleteWith f e bytes#
    
    {-# INLINE intersectionWith #-}
    intersectionWith f (Bytes _ _ bytes1#) (Bytes _ _ bytes2#) =
      withBounds $ intersectionWith f bytes1# bytes2#
    
    {-# INLINE unionWith #-}
    unionWith f (Bytes _ _ bytes1#) (Bytes _ _ bytes2#) =
      withBounds $ unionWith f bytes1# bytes2#
    
    {-# INLINE differenceWith #-}
    differenceWith f (Bytes _ _ bytes1#) (Bytes _ _ bytes2#) =
      withBounds $ differenceWith f bytes1# bytes2#
    
    {-# INLINE symdiffWith #-}
    symdiffWith f (Bytes _ _ bytes1#) (Bytes _ _ bytes2#) =
      withBounds $ differenceWith f bytes1# bytes2#
    
    isContainedIn f e (Bytes _ _     es) = isContainedIn f e     es
    lookupLTWith  f o (Bytes _ _ bytes#) = lookupLTWith  f o bytes#
    lookupGTWith  f o (Bytes _ _ bytes#) = lookupGTWith  f o bytes#
    lookupLEWith  f o (Bytes _ _ bytes#) = lookupLEWith  f o bytes#
    lookupGEWith  f o (Bytes _ _ bytes#) = lookupGEWith  f o bytes#
    
    isSubsetWith f (Bytes _ _ xs) (Bytes _ _ ys) = isSubsetWith f xs ys

instance (Index i, Unboxed e) => Scan (Bytes i e) e

instance (Index i, Unboxed e) => Sort (Bytes i e) e
  where
    sortBy cmp (Bytes l u bytes#) = Bytes l u (sortBy cmp bytes#)

--------------------------------------------------------------------------------

{- Indexed and IFold instances. -}

instance (Index i, Unboxed e) => Indexed (Bytes i e) i e
  where
    assoc bnds@(l, u) ascs = Bytes l u $ assoc bnds' ies
      where
        ies   = [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
        bnds' = defaultBounds $ size bnds
    
    assoc' bnds@(l, u) defvalue ascs = Bytes l u $ assoc' bnds' defvalue ies
      where
        ies   = [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
        bnds' = defaultBounds $ size bnds
    
    fromIndexed = withBounds . fromIndexed
    
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    bytes // ascs = runST $ thaw bytes >>= (`overwrite` ascs) >>= done
    
    {-# INLINE (!^) #-}
    (!^) (Bytes _ _ bytes#) = (bytes# !^)
    
    {-# INLINE (.!) #-}
    (.!) (Bytes l u bytes#) = (bytes# !^) . offset (l, u)
    
    (*$) p = ifoldr (\ i e is -> p e ? (i : is) $ is) []

instance (Index i, Unboxed e) => IFold (Bytes i e) i e
  where
    ifoldr f base = \ (Bytes l u bytes#) -> ifoldr (f . index (l, u)) base bytes#
    ifoldl f base = \ (Bytes l u bytes#) -> ifoldl (f . index (l, u)) base bytes#
    
    i_foldr f base = \ (Bytes _ _ bytes#) -> i_foldr f base bytes#
    i_foldl f base = \ (Bytes _ _ bytes#) -> i_foldl f base bytes#

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance (Index i, Unboxed e) => Thaw (ST s) (Bytes i e) (STBytes s i e)
  where
    thaw       (Bytes l u bytes#) = STBytes l u <$> thaw bytes#
    unsafeThaw (Bytes l u bytes#) = STBytes l u <$> unsafeThaw bytes#

instance (Index i, Unboxed e) => Freeze (ST s) (STBytes s i e) (Bytes i e)
  where
    freeze       (STBytes l u mbytes#) = Bytes l u <$> freeze mbytes#
    unsafeFreeze (STBytes l u mbytes#) = Bytes l u <$> unsafeFreeze mbytes#

--------------------------------------------------------------------------------

{-# INLINE withBounds #-}
withBounds :: (Index i, Unboxed e) => SBytes# e -> Bytes i e
withBounds bytes# = let (l, u) = defaultBounds (sizeOf bytes#) in Bytes l u bytes#

{-# INLINE done #-}
done :: (Unboxed e) => STBytes s i e -> ST s (Bytes i e)
done (STBytes l u mbytes#) = Bytes l u <$> unsafeFreeze mbytes#

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.Bytes." ++ msg



