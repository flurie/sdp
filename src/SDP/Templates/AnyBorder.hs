{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, Trustworthy #-}

{- |
    Module      :  SDP.Templates.AnyBorder
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Plate.AnyBorder@ provides 'AnyBorder' - template of generalized by
    index type structure, based on int-indexed primitive.
-}
module SDP.Templates.AnyBorder
(
  -- * Export
  module SDP.IndexedM,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Border template
  AnyBorder (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.SortM
import SDP.Sort
import SDP.Scan
import SDP.Set

import qualified GHC.Exts as E

import Test.QuickCheck

import SDP.Internal

default ()

--------------------------------------------------------------------------------

-- | AnyBorder is template, that appends arbitrary bounds to any structure.
data AnyBorder rep i e = AnyBorder !i !i !(rep e)

--------------------------------------------------------------------------------

{- Eq ad Eq1 instances. -}

instance (Index i, Eq (rep e)) => Eq (AnyBorder rep i e)
  where
    xs == ys = unpack xs == unpack ys

instance (Index i, Eq1 rep) => Eq1 (AnyBorder rep i)
  where
    liftEq f xs ys = liftEq f (unpack xs) (unpack ys)

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Index i, Ord (rep e)) => Ord (AnyBorder rep i e)
  where
    compare xs ys = unpack xs <=> unpack ys

instance (Index i, Ord1 rep) => Ord1 (AnyBorder rep i)
  where
    liftCompare f xs ys = liftCompare f (unpack xs) (unpack ys)

--------------------------------------------------------------------------------

{- Overloaded Lists and String support. -}

instance (Index i, IsString (rep Char), Bordered1 rep Int Char) => IsString (AnyBorder rep i Char)
  where
    fromString = withBounds . fromString

instance (Index i, E.IsList (rep e), Bordered1 rep Int e) => E.IsList (AnyBorder rep i e)
  where
    type Item (AnyBorder rep i e) = E.Item (rep e)
    
    fromListN = withBounds ... E.fromListN
    fromList  = withBounds . E.fromList
    toList    = E.toList . unpack

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance (Linear1 (AnyBorder rep i) e) => Semigroup (AnyBorder rep i e) where (<>) = (++)
instance (Linear1 (AnyBorder rep i) e) => Monoid    (AnyBorder rep i e) where mempty = Z
instance (Linear1 (AnyBorder rep i) e) => Default   (AnyBorder rep i e) where def = Z

instance (Linear1 (AnyBorder rep i) e, Arbitrary e) => Arbitrary (AnyBorder rep i e)
  where
    arbitrary = fromList <$> arbitrary

instance (Index i) => Estimate (AnyBorder rep i e)
  where
    (<==>) = on (<=>) sizeOf
    (.<=.) = on (<=)  sizeOf
    (.>=.) = on (>=)  sizeOf
    (.>.)  = on (>)   sizeOf
    (.<.)  = on (<)   sizeOf
    
    (<.=>) = (<=>) . sizeOf
    (.>)   = (>)   . sizeOf
    (.<)   = (<)   . sizeOf
    (.>=)  = (>=)  . sizeOf
    (.<=)  = (<=)  . sizeOf

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances. -}

instance (Index i, Functor rep) => Functor (AnyBorder rep i)
  where
    fmap f (AnyBorder l u rep) = AnyBorder l u (f <$> rep)

instance (Index i, Zip rep) => Zip (AnyBorder rep i)
  where
    zipWith f as bs =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs]
      in  AnyBorder l u $ zipWith f (unpack as) (unpack bs)
    
    zipWith3 f as bs cs =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs, sizeOf cs]
      in  AnyBorder l u $ zipWith3 f (unpack as) (unpack bs) (unpack cs)
    
    zipWith4 f as bs cs ds =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds]
      in  AnyBorder l u $ zipWith4 f (unpack as) (unpack bs) (unpack cs) (unpack ds)
    
    zipWith5 f as bs cs ds es =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds, sizeOf es]
      in  AnyBorder l u $ zipWith5 f (unpack as) (unpack bs) (unpack cs) (unpack ds) (unpack es)
    
    zipWith6 f as bs cs ds es fs =
      let (l, u) = defaultBounds $ minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds, sizeOf es, sizeOf fs]
      in  AnyBorder l u $ zipWith6 f (unpack as) (unpack bs) (unpack cs) (unpack ds) (unpack es) (unpack fs)

instance (Index i, Applicative rep) => Applicative (AnyBorder rep i)
  where
    pure = uncurry AnyBorder (defaultBounds 1) . pure
    
    (AnyBorder lf uf fs) <*> (AnyBorder le ue es) =
      let (l, u) = defaultBounds (size (lf, uf) * size (le, ue))
      in  AnyBorder l u (fs <*> es)

--------------------------------------------------------------------------------

{- Foldable and Traversable instances. -}

instance (Index i, Foldable rep) => Foldable (AnyBorder rep i)
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

instance (Index i, Traversable rep) => Traversable (AnyBorder rep i)
  where
    traverse f (AnyBorder l u es) = AnyBorder l u <$> traverse f es

--------------------------------------------------------------------------------

{- Bordered, Linear and Split instances. -}

instance (Index i) => Bordered (AnyBorder rep i e) i
  where
    sizeOf (AnyBorder l u _) = size (l, u)
    bounds (AnyBorder l u _) = (l, u)
    lower  (AnyBorder l _ _) = l
    upper  (AnyBorder _ u _) = u
    
    indexIn (AnyBorder l u _) = inRange (l, u)
    indices (AnyBorder l u _) = range   (l, u)
    
    indexOf  (AnyBorder l u _) = index  (l, u)
    offsetOf (AnyBorder l u _) = offset (l, u)

instance (Index i, Linear1 rep e, Bordered1 rep Int e) => Linear (AnyBorder rep i e) e
  where
    isNull (AnyBorder l u rep) = isEmpty (l, u) || isNull rep
    
    lzero  = withBounds Z
    single = withBounds . single
    
    toHead e es = withBounds (e :> unpack es)
    toLast es e = withBounds (unpack es :< e)
    
    head = head . unpack
    last = last . unpack
    tail = withBounds . tail . unpack
    init = withBounds . init . unpack
    
    fromList  = fromFoldable
    fromListN = withBounds ... fromListN
    replicate = withBounds ... replicate
    iterate n = withBounds ... iterate n
    
    fromFoldable = withBounds . fromFoldable
    
    (++) = withBounds ... on (++) unpack
    
    listL = listL . unpack
    listR = listR . unpack
    
    concatMap f = withBounds . concatMap (unpack . f)
    concat      = withBounds . concatMap unpack
    
    partitions  f = fmap fromList . partitions f . listL
    intersperse e = withBounds . intersperse e . unpack
    
    filter f = withBounds . filter f . unpack
    
    reverse (AnyBorder l u rep) = AnyBorder l u (reverse rep)
    
    select   f = select f . unpack
    extract  f = second withBounds . extract  f . unpack
    selects fs = second withBounds . selects fs . unpack
    
    nubBy f = withBounds . nubBy f . unpack
    nub     = withBounds .   nub   . unpack

instance (Index i, Split1 rep e, Bordered1 rep Int e) => Split (AnyBorder rep i e) e
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

--------------------------------------------------------------------------------

{- BorderedM, LinearM and SplitM instances. -}

instance (Index i, BorderedM1 m rep Int e) => BorderedM m (AnyBorder rep i e) i
  where
    getIndexOf (AnyBorder l u _) = return . inRange (l, u)
    getIndices (AnyBorder l u _) = return $ range (l, u)
    getSizeOf  (AnyBorder l u _) = return $ size (l, u)
    getBounds  (AnyBorder l u _) = return (l, u)
    getLower   (AnyBorder l _ _) = return l
    getUpper   (AnyBorder _ u _) = return u

instance (Index i, LinearM1 m rep e, BorderedM1 m rep Int e) => LinearM m (AnyBorder rep i e) e
  where
    newNull = uncurry AnyBorder (defaultBounds 0) <$> newNull
    
    nowNull (AnyBorder l u es) = isEmpty (l, u) ? return True $ nowNull es
    
    getHead = getHead . unpack
    getLast = getLast . unpack
    
    prepend e = withBounds' <=< prepend e . unpack
    append es = withBounds' <=< append (unpack es)
    
    newLinear = withBounds' <=< newLinear
    filled  n = withBounds' <=< filled n
    
    getLeft   = getLeft  . unpack
    getRight  = getRight . unpack
    
    copied   (AnyBorder l u es) = AnyBorder l u <$> copied  es
    copied'  (AnyBorder l u es) = (AnyBorder l u <$>) ... copied' es
    reversed (AnyBorder l u es) = AnyBorder l u <$> reversed es
    
    copyTo src os trg ot = copyTo (unpack src) os (unpack trg) ot

instance (Index i, BorderedM1 m rep Int e, SplitM1 m rep e) => SplitM m (AnyBorder rep i e) e
  where
    takeM n es@(AnyBorder l u rep)
        | n <= 0 = newNull
        | n >= c = return es
        |  True  = AnyBorder l (index (l, u) n) <$> takeM n rep
      where
        c = size (l, u)
    
    dropM n es@(AnyBorder l u rep)
        | n >= c = newNull
        | n <= 0 = return es
        |  True  = AnyBorder (index (l, u) n) u <$> dropM n rep
      where
        c = size (l, u)
    
    keepM n es@(AnyBorder l u rep)
        | n <= 0 = newNull
        | n >= c = return es
        |  True  = AnyBorder (index (l, u) (c - n)) u <$> keepM n rep
      where
        c = size (l, u)
    
    sansM n es@(AnyBorder l u rep)
        | n >= c = newNull
        | n <= 0 = return es
        |  True  = AnyBorder (index (l, u) (c - n)) u <$> sansM n rep
      where
        c = size (l, u)
    
    splitM n es@(AnyBorder l u rep)
        | n <= 0 = do e' <- newNull; return (e', es)
        | n >= c = do e' <- newNull; return (es, e')
        |  True  = bimap (AnyBorder l i) (AnyBorder i u) <$> splitM n rep
      where
        i = index (l, u) n
        c = size  (l, u)
    
    divideM n es@(AnyBorder l u rep)
        | n <= 0 = do e' <- newNull; return (es, e')
        | n >= c = do e' <- newNull; return (e', es)
        |  True  = bimap (AnyBorder l i) (AnyBorder i u) <$> divideM n rep
      where
        i = index (l, u) (c - n)
        c = size  (l, u)
    
    prefixM p = prefixM p . unpack
    suffixM p = suffixM p . unpack
    mprefix p = mprefix p . unpack
    msuffix p = msuffix p . unpack

--------------------------------------------------------------------------------

{- Set, Scan and Sort instances. -}

instance (Index i, Set1 rep e, Bordered1 rep Int e) => Set (AnyBorder rep i e) e
  where
    isSubsetWith f = isSubsetWith f `on` unpack
    
    setWith f = withBounds . setWith f . unpack
    
    subsets = map withBounds . subsets . unpack
    
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

instance (Linear1 (AnyBorder rep i) e) => Scan (AnyBorder rep i e) e

instance (Index i, Sort (rep e) e) => Sort (AnyBorder rep i e) e
  where
    sortBy cmp (AnyBorder l u rep) = AnyBorder l u (sortBy cmp rep)

--------------------------------------------------------------------------------

{- Indexed and IFold instances. -}

instance (Index i, Indexed1 rep Int e) => Indexed (AnyBorder rep i e) i e
  where
    assoc bnds@(l, u) ascs = AnyBorder l u (assoc bnds' ies)
      where
        ies   = [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
        bnds' = defaultBounds $ size bnds
    
    assoc' bnds@(l, u) defvalue ascs = AnyBorder l u (assoc' bnds' defvalue ies)
      where
        ies   = [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
        bnds' = defaultBounds $ size bnds
    
    fromIndexed = withBounds . fromIndexed
    
    {-# INLINE (!^) #-}
    (!^) = (!^) . unpack
    
    {-# INLINE (.!) #-}
    (.!) (AnyBorder l u rep) = (rep !^) . offset (l, u)
    
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    (AnyBorder l u rep) // ascs = AnyBorder l u (rep // ies)
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
    
    p .$ (AnyBorder l u rep) = index (l, u) <$> p .$ rep
    p *$ (AnyBorder l u rep) = index (l, u) <$> p *$ rep

instance (Index i, Bordered1 rep Int e, IFold1 rep Int e) => IFold (AnyBorder rep i e) i e
  where
    ifoldr f base = \ es -> ifoldr (f . indexOf es) base (unpack es)
    ifoldl f base = \ es -> ifoldl (f . indexOf es) base (unpack es)
    
    i_foldr f base = i_foldr f base . unpack
    i_foldl f base = i_foldl f base . unpack

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Index i, IndexedM1 m rep Int e) => IndexedM m (AnyBorder rep i e) i e
  where
    fromAssocs (l, u) ascs = AnyBorder l u <$> fromAssocs bnds ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size  (l, u) - 1)
    
    fromAssocs' (l, u) defvalue ascs = AnyBorder l u <$> fromAssocs' bnds defvalue ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    {-# INLINE (!#>) #-}
    (!#>) = (!#>) . unpack
    
    {-# INLINE (>!) #-}
    (>!) (AnyBorder l u es) = (es !#>) . offset (l, u)
    
    {-# INLINE writeM_ #-}
    writeM_ = writeM . unpack
    
    {-# INLINE writeM #-}
    writeM  (AnyBorder l u es) = writeM es . offset (l, u)
    
    overwrite es [] = return es
    overwrite (AnyBorder l u es) ascs = if isEmpty (l, u)
        then fromAssocs (l', u') ascs
        else AnyBorder l u <$> overwrite es ies
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        l'  = fst $ minimumBy cmpfst ascs
        u'  = fst $ maximumBy cmpfst ascs
    
    fromIndexed' = withBounds' <=< fromIndexed'
    fromIndexedM = withBounds' <=< fromIndexedM

instance (Index i, IFoldM1 m rep Int e) => IFoldM m (AnyBorder rep i e) i e
  where
    ifoldrM f e (AnyBorder l u es) = ifoldrM (f . index (l, u)) e es
    ifoldlM f e (AnyBorder l u es) = ifoldlM (f . index (l, u)) e es
    
    i_foldrM f e = i_foldrM f e . unpack
    i_foldlM f e = i_foldlM f e . unpack

instance (Index i, SortM1 m rep e) => SortM m (AnyBorder rep i e) e
  where
    sortMBy f = sortMBy f . unpack

--------------------------------------------------------------------------------

{- Freeze and Thaw instances. -}

instance (Index i, Freeze m (rep e) imm) => Freeze m (AnyBorder rep i e) imm
  where
    unsafeFreeze = unsafeFreeze . unpack
    freeze       = freeze . unpack

instance (Index i, Thaw m (rep e) mut) => Thaw m (AnyBorder rep i e) mut
  where
    unsafeThaw = unsafeThaw . unpack
    thaw       = thaw . unpack

instance (Index i, Freeze m mut (rep e), Bordered1 rep Int e) => Freeze m mut (AnyBorder rep i e)
  where
    unsafeFreeze = fmap withBounds . unsafeFreeze
    freeze       = fmap withBounds . freeze

instance (Index i, Thaw m imm (rep e), Bordered1 rep Int e) => Thaw m imm (AnyBorder rep i e)
  where
    unsafeThaw = fmap withBounds . unsafeThaw
    thaw       = fmap withBounds . thaw

--------------------------------------------------------------------------------

{-# INLINE unpack #-}
unpack :: AnyBorder rep i e -> rep e
unpack =  \ (AnyBorder _ _ es) -> es

{-# INLINE withBounds #-}
withBounds :: (Index i, Bordered1 rep Int e) => rep e -> AnyBorder rep i e
withBounds rep = uncurry AnyBorder (defaultBounds $ sizeOf rep) rep

{-# INLINE withBounds' #-}
withBounds' :: (Index i, BorderedM1 m rep Int e) => rep e -> m (AnyBorder rep i e)
withBounds' rep = (\ n -> uncurry AnyBorder (defaultBounds n) rep) <$> getSizeOf rep





