{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, TypeFamilies, RoleAnnotations, DeriveGeneric #-}

{- |
    Module      :  SDP.Array
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Array@ provides 'Array' - immutable lazy boxed array type.
-}
module SDP.Array
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Array
  Array (..)
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Prim.SArray
import SDP.Array.ST

import SDP.Indexed
import SDP.Sort
import SDP.Scan
import SDP.Set

import qualified GHC.Exts as E

import SDP.Internal.Commons
import Text.Show.SDP
import Text.Read.SDP

import GHC.Generics

import Test.QuickCheck

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | Array - boxed array.
data Array i e = Array !i !i !(SArray# e) deriving ( Generic )

type role Array nominal representational

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Index i, Eq e) => Eq (Array i e) where (==) = eq1

instance (Index i) => Eq1 (Array i)
  where
    liftEq _ Z   Z = True
    liftEq f xs ys = liftEq f (unpack xs) (unpack ys)

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Index i, Ord e) => Ord (Array i e) where compare = compare1

instance (Index i) => Ord1 (Array i)
  where
    liftCompare cmp xs ys = liftCompare cmp (unpack xs) (unpack ys)

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance (Index i) => Semigroup (Array i e) where (<>) = (++)
instance (Index i) => Monoid    (Array i e) where mempty = Z
instance (Index i) => Default   (Array i e) where def = Z

instance (Index i, Arbitrary e) => Arbitrary (Array i e)
  where
    arbitrary = fromList <$> arbitrary

instance Estimate (Array i e)
  where
    (<==>) = on (<==>) unpack
    (.<=.) = on (.<=.) unpack
    (.>=.) = on (.>=.) unpack
    (.>.)  = on (.>.)  unpack
    (.<.)  = on (.<.)  unpack
    
    (<.=>) = (<.=>) . unpack
    (.>)   = (.>)   . unpack
    (.<)   = (.<)   . unpack
    (.>=)  = (.>=)  . unpack
    (.<=)  = (.<=)  . unpack

--------------------------------------------------------------------------------

instance (Index i) => E.IsList (Array i e)
  where
    type Item (Array i e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = toList

instance (Index i) => IsString (Array i Char) where fromString = fromList

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance {-# OVERLAPPABLE #-} (Index i, Show i, Show e) => Show (Array i e)
  where
    showsPrec = assocsPrec "array "

instance (Index i, Show i) => Show (Array i Char)
  where
    showsPrec = shows ... const listL

instance (Index i, Read i, Read e) => Read (Array i e)
  where
    readList = readListDefault
    readPrec = indexedPrec' "array"

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances. -}

instance (Index i) => Functor (Array i)
  where
    fmap f (Array l u arr#) = Array l u (f <$> arr#)

instance (Index i) => Zip (Array i)
  where
    zipWith f as bs              = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i)
        sz      = minimum [sizeOf as, sizeOf bs]
    
    zipWith3 f as bs cs          = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i)
        sz      = minimum [sizeOf as, sizeOf bs, sizeOf cs]
    
    zipWith4 f as bs cs ds       = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i) (ds !^ i)
        sz      = minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds]
    
    zipWith5 f as bs cs ds es    = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i) (ds !^ i) (es !^ i)
        sz      = minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds, sizeOf es]
    
    zipWith6 f as bs cs ds es fs = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i) (ds !^ i) (es !^ i) (fs !^ i)
        sz      = minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds, sizeOf es, sizeOf fs]

instance (Index i) => Applicative (Array i)
  where
    pure = single
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable and Traversable instances. -}

instance (Index i) => Foldable (Array i)
  where
    foldr  f base = foldr  f base . unpack
    foldl  f base = foldl  f base . unpack
    foldr' f base = foldr' f base . unpack
    foldl' f base = foldl' f base . unpack
    
    foldr1 f = foldr1 f . unpack
    foldl1 f = foldl1 f . unpack
    
    length = length . unpack
    toList = toList . unpack
    null   = null   . unpack

instance (Index i) => Traversable (Array i)
  where
    traverse f = fmap fromList . foldr (liftA2 (:) . f) (pure [])

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i) => Linear (Array i e) e
  where
    isNull = null
    
    lzero  = withBounds Z
    single = withBounds . single
    
    toHead e es = withBounds (e :> unpack es)
    toLast es e = withBounds (unpack es :< e)
    
    head es = isNull es ? pfailEx "(:>)" $ es !^ 0
    last es = isNull es ? pfailEx "(:<)" $ es !^ (sizeOf es - 1)
    tail es = isNull es ? pfailEx "(:>)" $ (withBounds . tail $ unpack es)
    init es = isNull es ? pfailEx "(:<)" $ (withBounds . init $ unpack es)
    
    fromList  = fromFoldable
    fromListN = withBounds ... fromListN
    replicate = withBounds ... replicate
    
    fromFoldable = withBounds . fromFoldable
    
    (++) = withBounds ... on (++) unpack
    
    listL = listL . unpack
    listR = listR . unpack
    
    concatMap f = fromList . foldr (flip $ (. f) . foldr (:)) []
    concat      = fromList . foldr (flip $ foldr (:)) []
    
    partitions f = fmap fromList . partitions f . listL
    
    select   f = select f . unpack
    extract  f = second withBounds . extract  f . unpack
    selects fs = second withBounds . selects fs . unpack

instance (Index i) => Split (Array i e) e
  where
    take n = withBounds . take n . unpack
    drop n = withBounds . drop n . unpack
    keep n = withBounds . keep n . unpack
    sans n = withBounds . sans n . unpack
    
    splits ns = fmap withBounds . splits ns . unpack
    chunks ns = fmap withBounds . chunks ns . unpack
    parts  ns = fmap withBounds . parts  ns . unpack
    
    isPrefixOf xs ys = xs .<=. ys && on isPrefixOf unpack xs ys
    isSuffixOf xs ys = xs .<=. ys && on isSuffixOf unpack xs ys
    
    prefix p = prefix p . unpack
    suffix p = suffix p . unpack

instance (Index i) => Bordered (Array i e) i e
  where
    offsetOf (Array l u _) = offset  (l, u)
    indexIn  (Array l u _) = inRange (l, u)
    indexOf  (Array l u _) = index (l, u)
    bounds   (Array l u _) = (l, u)
    lower    (Array l _ _) = l
    upper    (Array _ u _) = u
    
    sizeOf = sizeOf . unpack

--------------------------------------------------------------------------------

{- Set, Scan and Sort instances. -}

instance (Index i) => Set (Array i e) e
  where
    setWith f = withBounds . setWith f . unpack
    
    insertWith f e = withBounds . insertWith f e . unpack
    deleteWith f e = withBounds . deleteWith f e . unpack
    
    intersectionWith f = withBounds ... on (intersectionWith f) unpack
    unionWith        f = withBounds ... on (unionWith        f) unpack
    differenceWith   f = withBounds ... on (differenceWith   f) unpack
    symdiffWith      f = withBounds ... on (symdiffWith      f) unpack
    
    isContainedIn f e = isContainedIn f e . unpack
    lookupLTWith  f o = lookupLTWith  f o . unpack
    lookupGTWith  f o = lookupGTWith  f o . unpack
    lookupLEWith  f o = lookupLEWith  f o . unpack
    lookupGEWith  f o = lookupGEWith  f o . unpack

instance (Index i) => Scan (Array i e) e

instance (Index i) => Sort (Array i e) e
  where
    sortBy cmp (Array l u arr#) = Array l u (sortBy cmp arr#)

--------------------------------------------------------------------------------

{- Indexed and IFold instances. -}

instance (Index i) => Indexed (Array i e) i e
  where
    assoc' bnds@(l, u) defvalue ascs = Array l u (assoc' bnds' defvalue ies)
      where
        ies   = [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
        bnds' = defaultBounds $ size bnds
    
    fromIndexed = withBounds . fromIndexed
    
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    arr // ascs = runST $ do
        marr <- fromFoldableM arr
        overwrite (reshape' (bounds arr) marr) ascs >>= done
      where
        reshape' :: (i, i) -> STArray s i e -> STArray s i e
        reshape' (l, u) (STArray _ _ arr#) = STArray l u arr#
    
    {-# INLINE (!^) #-}
    (!^) (Array _ _ arr#) = (arr# !^)
    
    {-# INLINE (.!) #-}
    (.!) (Array l u arr#) = (arr# !^) . offset (l, u)
    
    (*$) p = ifoldr (\ i e is -> p e ? (i : is) $ is) []

instance (Index i) => IFold (Array i e) i e
  where
    ifoldr f base = \ es -> ifoldr (f . indexOf es) base (unpack es)
    ifoldl f base = \ es -> ifoldl (f . indexOf es) base (unpack es)
    
    i_foldr = foldr
    i_foldl = foldl

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance (Index i) => Thaw (ST s) (Array i e) (STArray s i e)
  where
    thaw       (Array l u arr#) = STArray l u <$> thaw arr#
    unsafeThaw (Array l u arr#) = STArray l u <$> unsafeThaw arr#

instance (Index i) => Freeze (ST s) (STArray s i e) (Array i e)
  where
    freeze       (STArray l u marr#) = Array l u <$> freeze marr#
    unsafeFreeze (STArray l u marr#) = Array l u <$> unsafeFreeze marr#

--------------------------------------------------------------------------------

{-# INLINE unpack #-}
unpack :: Array i e -> SArray# e
unpack =  \ (Array _ _ es) -> es

{-# INLINE withBounds #-}
withBounds :: (Index i) => SArray# e -> Array i e
withBounds arr# = Array l u arr# where (l, u) = defaultBounds (sizeOf arr#)

{-# INLINE done #-}
done :: STArray s i e -> ST s (Array i e)
done (STArray l u marr#) = Array l u <$> unsafeFreeze marr#

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.Array."





