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
import SDP.Prim.SBytes
import SDP.Bytes.ST

import SDP.Indexed
import SDP.Unboxed
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

-- | Bytes - unboxed array.
data Bytes i e = Bytes !i !i !(SBytes# e) deriving ( Generic )

type role Bytes nominal representational

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Index i, Unboxed e) => Eq (Bytes i e)
  where
    (==) = on (==) unpack

instance (Index i, Unboxed e, Ord e) => Ord (Bytes i e)
  where
    compare = comparing unpack

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance (Index i, Unboxed e) => Semigroup (Bytes i e) where (<>) = (++)
instance (Index i, Unboxed e) => Monoid    (Bytes i e) where mempty = Z
instance (Index i, Unboxed e) => Default   (Bytes i e)
  where
    def = Bytes l u def where (l, u) = defaultBounds 0

instance (Index i, Unboxed e, Arbitrary e) => Arbitrary (Bytes i e)
  where
    arbitrary = fromList <$> arbitrary

instance Estimate (Bytes i e)
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

instance (Index i, Unboxed e) => E.IsList (Bytes i e)
  where
    type Item (Bytes i e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = listL

instance (Index i) => IsString (Bytes i Char) where fromString = fromList

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance {-# OVERLAPPABLE #-} (Index i, Unboxed e, Show i, Show e) => Show (Bytes i e)
  where
    showsPrec = assocsPrec "bytes "

instance (Index i, Show i) => Show (Bytes i Char)
  where
    showsPrec = shows ... const listL

instance (Index i, Unboxed e, Read i, Read e) => Read (Bytes i e)
  where
    readList = readListDefault
    readPrec = indexedPrec' "bytes"

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i, Unboxed e) => Linear (Bytes i e) e
  where
    isNull = isNull . unpack
    
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
    
    concatMap f = fromList . foldr (flip $ (. f) . i_foldr (:)) []
    concat      = fromList . foldr (flip $ i_foldr (:)) []
    
    partitions f = fmap fromList . partitions f . listL
    
    select   f = select f . unpack
    extract  f = second withBounds . extract  f . unpack
    selects fs = second withBounds . selects fs . unpack

instance (Index i, Unboxed e) => Split (Bytes i e) e
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

instance (Index i, Unboxed e) => Bordered (Bytes i e) i e
  where
    offsetOf (Bytes l u _) = offset  (l, u)
    indexIn  (Bytes l u _) = inRange (l, u)
    indexOf  (Bytes l u _) = index (l, u)
    bounds   (Bytes l u _) = (l, u)
    lower    (Bytes l _ _) = l
    upper    (Bytes _ u _) = u
    
    sizeOf = sizeOf . unpack

--------------------------------------------------------------------------------

{- Set, Scan and Sort instances. -}

instance (Index i, Unboxed e) => Set (Bytes i e) e
  where
    setWith f (Bytes _ _ bytes#) = withBounds $ setWith f bytes#
    
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
    
    isSubsetWith f = on (isSubsetWith f) unpack

instance (Index i, Unboxed e) => Scan (Bytes i e) e

instance (Index i, Unboxed e) => Sort (Bytes i e) e
  where
    sortBy cmp (Bytes l u es) = Bytes l u (sortBy cmp es)

--------------------------------------------------------------------------------

{- Indexed and IFold instances. -}

instance (Index i, Unboxed e) => Indexed (Bytes i e) i e
  where
    assoc bnds@(l, u) ascs = Bytes l u (assoc bnds' ies)
      where
        ies   = [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
        bnds' = defaultBounds (size bnds)
    
    assoc' bnds@(l, u) defvalue ascs = Bytes l u (assoc' bnds' defvalue ies)
      where
        ies   = [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
        bnds' = defaultBounds (size bnds)
    
    fromIndexed = withBounds . fromIndexed
    
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    bytes // ascs = runST $ do
        mbytes <- thaw bytes
        overwrite (reshape' (bounds bytes) mbytes) ascs >>= done
      where
        reshape' :: (i, i) -> STBytes s i e -> STBytes s i e
        reshape' (l, u) (STBytes _ _ bytes#) = STBytes l u bytes#
    
    {-# INLINE (!^) #-}
    (!^) es = (unpack es !^)
    
    {-# INLINE (.!) #-}
    (.!) es = (unpack es !^) . offsetOf es
    
    (*$) p = ifoldr (\ i e is -> p e ? (i : is) $ is) []

instance (Index i, Unboxed e) => IFold (Bytes i e) i e
  where
    ifoldr f base = \ es -> ifoldr (f . indexOf es) base (unpack es)
    ifoldl f base = \ es -> ifoldl (f . indexOf es) base (unpack es)
    
    i_foldr f base = i_foldr f base . unpack
    i_foldl f base = i_foldl f base . unpack

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance (Index i, Unboxed e) => Thaw (ST s) (Bytes i e) (STBytes s i e)
  where
    thaw       (Bytes l u es) = STBytes l u <$> thaw es
    unsafeThaw (Bytes l u es) = STBytes l u <$> unsafeThaw es

instance (Index i, Unboxed e) => Freeze (ST s) (STBytes s i e) (Bytes i e)
  where
    freeze       (STBytes l u es') = Bytes l u <$> freeze es'
    unsafeFreeze (STBytes l u es') = Bytes l u <$> unsafeFreeze es'

--------------------------------------------------------------------------------

{-# INLINE withBounds #-}
withBounds :: (Index i, Unboxed e) => SBytes# e -> Bytes i e
withBounds es = let (l, u) = defaultBounds (sizeOf es) in Bytes l u es

{-# INLINE unpack #-}
unpack :: Bytes i e -> SBytes# e
unpack =  \ (Bytes _ _ bytes#) -> bytes#

{-# INLINE done #-}
done :: (Unboxed e) => STBytes s i e -> ST s (Bytes i e)
done (STBytes l u es) = Bytes l u <$> unsafeFreeze es

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.Bytes."

