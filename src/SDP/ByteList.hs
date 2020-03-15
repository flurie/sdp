{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, TypeFamilies, RoleAnnotations, DeriveGeneric #-}

{- |
    Module      :  SDP.ByteList
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.ByteList@ provides 'ByteList' - strict unboxed unrolled linked list.
-}
module SDP.ByteList
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Unboxed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * ByteList
  ByteList (..),
  
  -- * Ublist
  Ublist
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Scan
import SDP.Set

import Test.QuickCheck

import GHC.Generics ( Generic (..) )

import GHC.Base ( Int (..) )
import GHC.ST   ( ST  (..) )

import qualified GHC.Exts as E

import Data.String ( IsString (..) )

import Data.Bifunctor

import SDP.ByteList.Ublist
import SDP.ByteList.ST

import SDP.Internal.Commons
import SDP.Internal.Read
import SDP.Internal.Show

default ()

--------------------------------------------------------------------------------

-- | ByteList is bordered strict unboxed unrolled linked list.
data ByteList i e = ByteList !i !i !(Ublist e) deriving ( Generic )

type role ByteList nominal representational

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Eq e, Unboxed e, Index i) => Eq (ByteList i e)
  where
    xs == ys = on (==) unpack xs ys

instance (Ord e, Unboxed e, Index i) => Ord (ByteList i e)
  where
    compare = comparing unpack

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Index i, Show i, Unboxed e, Show e) => Show (ByteList i e)
  where
    showsPrec = assocsPrec "bytelist "

instance (Index i, Read i, Unboxed e, Read e) => Read (ByteList i e)
  where
    readList = readListDefault
    readPrec = linearIndexedPrec "bytelist"

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance (Index i, Unboxed e) => Semigroup (ByteList i e) where (<>) = (++)
instance (Index i, Unboxed e) => Monoid    (ByteList i e) where mempty = def

instance (Index i) => Default (ByteList i e) where def = withSize 0 def

instance (Index i, Unboxed e, Arbitrary e) => Arbitrary (ByteList i e)
  where
    arbitrary = fromList <$> arbitrary

instance (Index i) => Estimate (ByteList i e)
  where
    (ByteList l1 u1 _) <==> (ByteList l2 u2 _) = size (l1, u1) <=> size (l2, u2)
    (ByteList l1 u1 _) .>.  (ByteList l2 u2 _) = size (l1, u1)  >  size (l2, u2)
    (ByteList l1 u1 _) .<.  (ByteList l2 u2 _) = size (l1, u1)  <  size (l2, u2)
    (ByteList l1 u1 _) .<=. (ByteList l2 u2 _) = size (l1, u1) <=  size (l2, u2)
    (ByteList l1 u1 _) .>=. (ByteList l2 u2 _) = size (l1, u1) >=  size (l2, u2)
    
    (ByteList l1 u1 _) <.=> n2 = size (l1, u1) <=> n2
    (ByteList l1 u1 _)  .>  n2 = size (l1, u1)  >  n2
    (ByteList l1 u1 _)  .<  n2 = size (l1, u1)  <  n2
    (ByteList l1 u1 _) .>=  n2 = size (l1, u1) >=  n2
    (ByteList l1 u1 _) .<=  n2 = size (l1, u1) <=  n2

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i, Unboxed e) => Linear (ByteList i e) e
  where
    isNull (ByteList l u es) = isEmpty (l, u) || isNull es
    
    lzero = def
    
    toHead e es = withSize (sizeOf es + 1) (e :> unpack es)
    toLast es e = withSize (sizeOf es + 1) (unpack es :< e)
    
    head es@(ByteList _ _ xs) = isNull es ? pfailEx "(:>)" $ head xs
    last es@(ByteList _ _ xs) = isNull es ? pfailEx "(:<)" $ last xs
    
    tail Z = pfailEx "(:>)"
    tail (ByteList l u es) = ByteList (next (l, u) l) u (tail es)
    
    init Z = pfailEx "(:<)"
    init (ByteList l u es) = ByteList l (prev (l, u) u) (init es)
    
    uncons Z = pfailEx "(:>)"
    uncons (ByteList l u es) = (x, es .<= 1 ? Z $ ByteList (next (l, u) l) u xs)
      where
        (x, xs) = uncons es
    
    unsnoc Z = pfailEx "(:<)"
    unsnoc (ByteList l u es) = (es .<= 1 ? Z $ ByteList l (prev (l, u) u) xs, x)
      where
        (xs, x) = unsnoc es
    
    fromList = withBounds . fromList
    
    replicate n = withSize (max 0 n) . replicate n
    
    concat es = foldr' ((+) . sizeOf) 0 es `withSize` foldMap unpack es
    
    intersperse _  Z = Z
    intersperse e es = (2 * sizeOf es - 1) `withSize` intersperse e (unpack es)
    
    Z  ++ ys = ys
    xs ++  Z = xs
    xs ++ ys = on (+) sizeOf xs ys `withSize` on (++) unpack xs ys
    
    listL = listL . unpack
    listR = listR . unpack
    
    partitions ps = fmap fromList . partitions ps . listL
    
    select   f = select f . unpack
    extract  f = second withBounds . extract  f . unpack
    selects fs = second withBounds . selects fs . unpack

instance (Index i, Unboxed e) => Split (ByteList i e) e
  where
    take n xs@(ByteList l _ es)
      |  n <= 0  = Z
      | n >=. xs = xs
      |   True   = ByteList l (indexOf xs (n - 1)) (take n es)
    
    drop n xs@(ByteList _ u es)
      |  n <= 0  = xs
      | n >=. xs = Z
      |   True   = ByteList (indexOf xs n) u (drop n es)
    
    keep n xs@(ByteList _ u es)
      |  n <= 0  = Z
      | n >=. xs = xs
      |   True   = ByteList (indexOf xs (n - 1)) u (keep n es)
    
    sans n xs@(ByteList l _ es)
      |  n <= 0  = xs
      | n >=. xs = Z
      |   True   = ByteList l (indexOf xs (sizeOf xs - n - 1)) (sans n es)
    
    isPrefixOf = on isPrefixOf unpack
    isSuffixOf = on isSuffixOf unpack
    isInfixOf  = on isInfixOf  unpack
    
    prefix p = prefix p . unpack
    suffix p = suffix p . unpack

instance (Index i, Unboxed e) => Bordered (ByteList i e) i e
  where
    indexIn  (ByteList l u _) = inRange (l, u)
    offsetOf (ByteList l u _) = offset (l, u)
    indexOf  (ByteList l u _) = index (l, u)
    sizeOf   (ByteList l u _) = size (l, u)
    bounds   (ByteList l u _) = (l, u)
    lower    (ByteList l _ _) = l
    upper    (ByteList _ u _) = u

--------------------------------------------------------------------------------

{- Set, Scan and Sort instances. -}

instance (Index i, Unboxed e) => Set (ByteList i e) e
  where
    setWith f = withBounds . setWith f . unpack
    
    intersectionWith f = withBounds ... on (intersectionWith f) unpack
    unionWith        f = withBounds ... on (unionWith        f) unpack
    differenceWith   f = withBounds ... on (differenceWith   f) unpack
    symdiffWith      f = withBounds ... on (symdiffWith      f) unpack
    
    insertWith f e = withBounds . insertWith f e . unpack
    deleteWith f e = withBounds . deleteWith f e . unpack
    
    isSubsetWith f = on (isSubsetWith f) unpack
    
    isContainedIn f e = isContainedIn f e . unpack
    lookupLTWith  f e = lookupLTWith  f e . unpack
    lookupGTWith  f e = lookupGTWith  f e . unpack
    lookupLEWith  f e = lookupLEWith  f e . unpack
    lookupGEWith  f e = lookupGEWith  f e . unpack

instance (Index i, Unboxed e) => Scan (ByteList i e) e

instance (Index i, Unboxed e) => Sort (ByteList i e) e
  where
    sortBy cmp = withBounds . sortBy cmp . unpack

--------------------------------------------------------------------------------

{- Indexed and IFold instances. -}

instance (Index i, Unboxed e) => Indexed (ByteList i e) i e
  where
    assoc (l, u) ascs = ByteList l u (assoc bnds ies)
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    assoc' (l, u) defvalue ascs = ByteList l u $ assoc' bnds defvalue ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    Z  // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    es // ascs = withBounds $ unpack es // [ (offsetOf es i, e) | (i, e) <- ascs ]
    
    fromIndexed = withBounds . fromIndexed
    
    {-# INLINE (!^) #-}
    (!^) es = (unpack es !^)
    
    {-# INLINE (.!) #-}
    (.!) xs = (unpack xs !^) . offsetOf xs
    
    p .$ es = indexOf es <$> p .$ unpack es
    p *$ es = indexOf es <$> p *$ unpack es

instance (Index i, Unboxed e) => IFold (ByteList i e) i e
  where
    ifoldr  f base es = ifoldr (f . indexOf es) base (unpack es)
    ifoldl  f base es = ifoldl (f . indexOf es) base (unpack es)
    
    i_foldr f base = i_foldr f base . unpack
    i_foldl f base = i_foldl f base . unpack

--------------------------------------------------------------------------------

instance (Index i, Unboxed e) => E.IsList (ByteList i e)
  where
    type Item (ByteList i e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = listL

instance (Index i) => IsString (ByteList i Char) where fromString = fromList

--------------------------------------------------------------------------------

instance (Index i, Unboxed e) => Thaw (ST s) (ByteList i e) (STByteList s i e)
  where
    thaw       (ByteList l u es) = STByteList l u <$> thaw es
    unsafeThaw (ByteList l u es) = STByteList l u <$> unsafeThaw es

instance (Index i, Unboxed e) => Freeze (ST s) (STByteList s i e) (ByteList i e)
  where
    freeze       (STByteList l u es) = ByteList l u <$> freeze es
    unsafeFreeze (STByteList l u es) = ByteList l u <$> unsafeFreeze es

--------------------------------------------------------------------------------

{-# INLINE unpack #-}
unpack :: ByteList i e -> Ublist e
unpack =  \ (ByteList _ _ es) -> es

{-# INLINE withSize #-}
withSize :: (Index i) => Int -> Ublist e -> ByteList i e
withSize =  \ n es -> let (l, u) = defaultBounds n in ByteList l u es

{-# INLINE withBounds #-}
withBounds :: (Index i, Unboxed e) => Ublist e -> ByteList i e
withBounds =  \ es -> let (l, u) = defaultBounds (sizeOf es) in ByteList l u es

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.ByteList."

