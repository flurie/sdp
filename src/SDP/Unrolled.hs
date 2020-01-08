{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, TypeFamilies, RoleAnnotations, DeriveGeneric #-}

{- |
    Module      :  SDP.Unrolled
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Unrolled@ provides 'Unrolled' - lazy boxed unrolled linked list.
-}
module SDP.Unrolled
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Unrolled
  Unrolled (..),
  
  -- * Unlist
  Unlist
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed
import SDP.Sort
import SDP.Scan
import SDP.Set

import Test.QuickCheck

import GHC.Generics ( Generic (..) )

import GHC.Base ( Int (..) )
import GHC.ST   ( ST  (..) )

import qualified GHC.Exts as E
import Data.String ( IsString (..) )

import SDP.Unrolled.STUnlist
import SDP.Unrolled.Unlist
import SDP.Unrolled.ST

import SDP.Internal.Commons
import SDP.Internal.Read
import SDP.Internal.Show

default ()

--------------------------------------------------------------------------------

-- | Unrolled is bordered unrolled linked list.
data Unrolled i e = Unrolled !i !i !(Unlist e) deriving ( Generic )

type role Unrolled nominal representational

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Eq e, Index i) => Eq (Unrolled i e) where (==) = eq1

instance (Index i) => Eq1 (Unrolled i)
  where
    liftEq eq xs ys = liftEq eq (unpack xs) (unpack ys)

--------------------------------------------------------------------------------

{- Ord and Ord1 innstances. -}

instance (Ord e, Index i) => Ord (Unrolled i e) where compare = compare1

instance (Index i) => Ord1 (Unrolled i)
  where
    liftCompare cmp xs ys = liftCompare cmp (unpack xs) (unpack ys)

--------------------------------------------------------------------------------

{- Show and Read instances -}

instance (Index i, Show i, Show e) => Show (Unrolled i e)
  where
    showsPrec = assocsPrec "unrolled "

instance (Index i, Read i, Read e) => Read (Unrolled i e)
  where
    readList = readListDefault
    readPrec = linearIndexedPrec "unrolled"

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance (Index i) => Semigroup (Unrolled i e) where (<>) = (++)
instance (Index i) => Monoid    (Unrolled i e) where mempty = def
instance (Index i) => Default   (Unrolled i e) where def = withSize 0 def

instance (Index i, Arbitrary e) => Arbitrary (Unrolled i e)
  where
    arbitrary = fromList <$> arbitrary

instance (Index i) => Estimate (Unrolled i e)
  where
    xs <==> ys = sizeOf xs <=> sizeOf ys
    xs .>.  ys = sizeOf xs  >  sizeOf ys
    xs .<.  ys = sizeOf xs  <  sizeOf ys
    xs .<=. ys = sizeOf xs <=  sizeOf ys
    xs .>=. ys = sizeOf xs >=  sizeOf ys
    
    xs <.=> n2 = sizeOf xs <=> n2
    xs  .>  n2 = sizeOf xs  >  n2
    xs  .<  n2 = sizeOf xs  <  n2
    xs .>=  n2 = sizeOf xs >=  n2
    xs .<=  n2 = sizeOf xs <=  n2

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances -}

instance (Index i) => Functor (Unrolled i)
  where
    fmap f (Unrolled l u es) = Unrolled l u (f <$> es)

instance (Index i) => Zip (Unrolled i)
  where
    zipWith  f as bs             = fromList $ zipWith  f (toList as) (toList bs)
    zipWith3 f as bs cs          = fromList $ zipWith3 f (toList as) (toList bs) (toList cs)
    zipWith4 f as bs cs ds       = fromList $ zipWith4 f (toList as) (toList bs) (toList cs) (toList ds)
    zipWith5 f as bs cs ds es    = fromList $ zipWith5 f (toList as) (toList bs) (toList cs) (toList ds) (toList es)
    zipWith6 f as bs cs ds es fs = fromList $ zipWith6 f (toList as) (toList bs) (toList cs) (toList ds) (toList es) (toList fs)

instance (Index i) => Applicative (Unrolled i)
  where
    pure = single
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable and Traversable instances -}

instance (Index i) => Foldable (Unrolled i)
  where
    foldr  f base = foldr  f base . unpack
    foldl  f base = foldl  f base . unpack
    foldr' f base = foldr' f base . unpack
    foldl' f base = foldl' f base . unpack
    
    foldr1 f = foldr1 f . unpack
    foldl1 f = foldl1 f . unpack
    
    elem e = elem e . unpack
    toList = toList . unpack
    null   = isEmpty . bounds
    length = sizeOf

instance (Index i) => Traversable (Unrolled i)
  where
    traverse f (Unrolled l u es) = Unrolled l u <$> traverse f es

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i) => Linear (Unrolled i e) e
  where
    lzero = def
    
    isNull (Unrolled l u es) = isEmpty (l, u) || isNull es
    
    toHead e es = withSize (sizeOf es + 1) (e :> unpack es)
    
    uncons Z = pfailEx "(:>)"
    uncons (Unrolled l u es) = (x, sizeOf es < 2 ? Z $ Unrolled l1 u xs)
      where
        (x, xs) = uncons es
        l1 = next (l, u) l
    
    head Z  = pfailEx "(:>)"
    head es = head (unpack es)
    
    tail Z = pfailEx "(:>)"
    tail (Unrolled l u es) = Unrolled l' u $ tail es where l' = next (l, u) l
    
    toLast es e = withSize (sizeOf es + 1) (unpack es :< e)
    
    unsnoc Z = pfailEx "(:<)"
    unsnoc (Unrolled l u es) = (sizeOf es < 2 ? Z $ Unrolled l u1 xs, x)
      where
        (xs, x) = unsnoc es
        u1 = prev (l, u) u
    
    last Z  = pfailEx "(:<)"
    last es = last (unpack es)
    
    init Z = pfailEx "(:<)"
    init (Unrolled l u es) = Unrolled l u' $ init es where u' = prev (l, u) u
    
    fromList = withBounds . fromList
    
    replicate n = withSize n . replicate n
    
    Z  ++ ys = ys
    xs ++  Z = xs
    xs ++ ys = withSize (on (+) sizeOf xs ys) $ on (++) unpack xs ys
    
    concat es = withSize n $ foldr ((++) . unpack) Z es
      where
        n = foldr' ((+) . sizeOf) 0 es
    
    intersperse _  Z = Z
    intersperse e es = withSize (2 * sizeOf es - 1) $ intersperse e (unpack es)
    
    listL = listL . unpack
    listR = listR . unpack
    
    partitions ps = fmap fromList . partitions ps . toList

instance (Index i) => Split (Unrolled i e) e
  where
    take n xs@(Unrolled l _ es)
      |   n < 1  = Z
      | n >=. xs = xs
      |   True   = Unrolled l u' (take n es)
        where
          u' = indexOf xs (n - 1)
    
    drop n xs@(Unrolled _ u es)
      |   n < 1  = xs
      | n >=. xs = Z
      |   True   = Unrolled l' u (drop n es)
        where
          l' = indexOf xs n
    
    isPrefixOf = on isPrefixOf unpack
    isSuffixOf = on isSuffixOf unpack
    isInfixOf  = on isInfixOf  unpack
    
    prefix p = prefix p . unpack
    suffix p = suffix p . unpack

instance (Index i) => Bordered (Unrolled i e) i e
  where
    indexIn (Unrolled l u _) = inRange (l, u)
    indexOf (Unrolled l u _) = index (l, u)
    sizeOf  (Unrolled l u _) = size (l, u)
    bounds  (Unrolled l u _) = (l, u)
    lower   (Unrolled l _ _) = l
    upper   (Unrolled _ u _) = u
    
    offsetOf (Unrolled l u _) = offset (l, u)

--------------------------------------------------------------------------------

{- Set, Scan and Sort instances. -}

instance (Index i) => Set (Unrolled i e) e
  where
    setWith f = withBounds . setWith f . unpack
    
    intersectionWith f xs ys = withBounds $ on (intersectionWith f) unpack xs ys
    unionWith        f xs ys = withBounds $ on (unionWith        f) unpack xs ys
    
    differenceWith f xs ys = withBounds $ on (differenceWith f) unpack xs ys
    symdiffWith    f xs ys = withBounds $ on (symdiffWith    f) unpack xs ys
    
    insertWith f e = withBounds . insertWith f e . unpack
    deleteWith f e = withBounds . deleteWith f e . unpack
    
    isContainedIn f e = isContainedIn f e . unpack
    lookupLTWith  f e = lookupLTWith  f e . unpack
    lookupGTWith  f e = lookupGTWith  f e . unpack
    lookupLEWith  f e = lookupLEWith  f e . unpack
    lookupGEWith  f e = lookupGEWith  f e . unpack

instance (Index i) => Scan (Unrolled i e) e

instance (Index i) => Sort (Unrolled i e) e
  where
    sortBy cmp (Unrolled l u es) = Unrolled l u (sortBy cmp es)

--------------------------------------------------------------------------------

{- Indexed and IFold instances. -}

instance (Index i) => Indexed (Unrolled i e) i e
  where
    assoc' (l, u) defvalue ascs = Unrolled l u (assoc' bnds defvalue ies)
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    (Unrolled l u es) // ascs = withBounds (es // ies)
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs ]
    
    fromIndexed es = let (l, u) = defaultBounds $ sizeOf es in Unrolled l u $ fromIndexed es
    
    {-# INLINE (!^) #-}
    (!^) = (!^) . unpack
    
    {-# INLINE (.!) #-}
    (.!) es = (unpack es !^) . offsetOf es
    
    p .$ es = indexOf es <$> p .$ unpack es
    p *$ es = indexOf es <$> p *$ unpack es

instance (Index i) => IFold (Unrolled i e) i e
  where
    ifoldr f base = \ es -> ifoldr (f . indexOf es) base (unpack es)
    ifoldl f base = \ es -> ifoldl (f . indexOf es) base (unpack es)
    
    i_foldr = foldr
    i_foldl = foldl

--------------------------------------------------------------------------------

instance (Index i) => E.IsList (Unrolled i e)
  where
    type Item (Unrolled i e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = toList

instance (Index i) => IsString (Unrolled i Char) where fromString = fromList

--------------------------------------------------------------------------------

instance (Index i) => Thaw (ST s) (Unrolled i e) (STUnrolled s i e)
  where
    thaw       (Unrolled l u es) = STUnrolled l u <$> thaw es
    unsafeThaw (Unrolled l u es) = STUnrolled l u <$> unsafeThaw es

instance (Index i) => Freeze (ST s) (STUnrolled s i e) (Unrolled i e)
  where
    freeze       (STUnrolled l u es) = Unrolled l u <$> freeze es
    unsafeFreeze (STUnrolled l u es) = Unrolled l u <$> unsafeFreeze es

--------------------------------------------------------------------------------

{-# INLINE unpack #-}
unpack :: Unrolled i e -> Unlist e
unpack =  \ (Unrolled _ _ es) -> es

{-# INLINE withBounds #-}
withBounds :: (Index i) => Unlist e -> Unrolled i e
withBounds es = let (l, u) = defaultBounds (sizeOf es) in Unrolled l u es

{-# INLINE withSize #-}
withSize :: (Index i) => Int -> Unlist e -> Unrolled i e
withSize =  uncurry Unrolled . defaultBounds

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.Unrolled." ++ msg





