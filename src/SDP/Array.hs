{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, TypeFamilies, RoleAnnotations #-}
{-# LANGUAGE DeriveGeneric #-}

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
  Array (..), SArray#, fromPseudoArray#
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

import GHC.Show ( appPrec )
import GHC.ST   ( ST (..), runST )

import Text.Read
import Text.Read.Lex ( expect )

import qualified GHC.Exts as E
import Data.String ( IsString (..) )

import SDP.Array.ST

import SDP.Internal.SArray
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | Array - standard type of array.
data Array i e = Array !i !i !(SArray# e) deriving ( Generic )

type role Array nominal representational

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Index i, Eq e) => Eq (Array i e) where (==) = eq1

instance (Index i) => Eq1 (Array i)
  where
    liftEq eq (Array l1 u1 arr1#) (Array l2 u2 arr2#) =
      l1 == l2 && u1 == u2 && liftEq eq arr1# arr2#

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Index i, Ord e) => Ord (Array i e) where compare = compare1

instance (Index i) => Ord1 (Array i)
  where
    liftCompare cmp (Array l1 u1 arr1#) (Array l2 u2 arr2#) =
      liftCompare cmp arr1# arr2# <> (size (l1, u1) <=> size (l2, u2))

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
    (Array _ _ arr1#) <==> (Array _ _ arr2#) = arr1# <==> arr2#
    (Array _ _ arr1#) .>.  (Array _ _ arr2#) = arr1# .>.  arr2#
    (Array _ _ arr1#) .<.  (Array _ _ arr2#) = arr1# .<.  arr2#
    (Array _ _ arr1#) .<=. (Array _ _ arr2#) = arr1# .<=. arr2#
    (Array _ _ arr1#) .>=. (Array _ _ arr2#) = arr1# .>=. arr2#
    
    (Array _ _ arr1#) <.=> n2 = arr1# <.=> n2
    (Array _ _ arr1#)  .>  n2 = arr1#  .>  n2
    (Array _ _ arr1#)  .<  n2 = arr1#  .<  n2
    (Array _ _ arr1#) .>=  n2 = arr1# .>=  n2
    (Array _ _ arr1#) .<=  n2 = arr1# .<=  n2

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

instance (Index i, Show i, Show e) => Show (Array i e)
  where
    showsPrec p arr@(Array l u _) = showParen (p > appPrec) $ showString "array "
                                                            . shows (l, u)
                                                            . showChar ' '
                                                            . shows (assocs arr)

instance (Index i, Read i, Read e) => Read (Array i e)
  where
    readList = readListDefault
    readPrec = parens $ do
      prec appPrec (lift . expect $ Ident "array")
      liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances. -}

instance (Index i) => Functor (Array i)
  where
    fmap f (Array l u arr#) = Array l u $ f <$> arr#

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

{- Foldable, Scan and Traversable instances. -}

instance (Index i) => Foldable (Array i)
  where
    foldr  f = \ base (Array _ _ arr#) -> foldr  f base arr#
    foldl  f = \ base (Array _ _ arr#) -> foldl  f base arr#
    foldr' f = \ base (Array _ _ arr#) -> foldr' f base arr#
    foldl' f = \ base (Array _ _ arr#) -> foldl' f base arr#
    
    foldr1 f = \ (Array _ _ arr#) -> foldr1 f arr#
    foldl1 f = \ (Array _ _ arr#) -> foldl1 f arr#
    
    length = \ (Array _ _ arr#) -> length arr#
    toList = \ (Array _ _ arr#) -> toList arr#
    null   = \ (Array _ _ arr#) -> null   arr#

instance (Index i) => Scan (Array i)
  where
    scanl  f = \ w (Array _ _ arr#) -> withBounds $ scanl  f w arr#
    scanr  f = \ w (Array _ _ arr#) -> withBounds $ scanr  f w arr#
    scanl' f = \ w (Array _ _ arr#) -> withBounds $ scanl' f w arr#
    
    scanl1 f = \ (Array _ _ arr#) -> withBounds $ scanl1 f arr#
    scanr1 f = \ (Array _ _ arr#) -> withBounds $ scanr1 f arr#

instance (Index i) => Traversable (Array i)
  where
    traverse f = fmap fromList . foldr (\ x ys -> liftA2 (:) (f x) ys) (pure [])

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i) => Linear (Array i e) e
  where
    isNull = null
    
    {-# INLINE lzero #-}
    lzero = withBounds Z
    
    toHead e (Array _ _ arr#) = withBounds $ e :> arr#
    
    head es = isNull es ? pfailEx "(:>)" $ es !^ 0
    
    -- | O(1) 'tail', O(1) memory.
    tail Z = pfailEx "(:>)"
    tail (Array _ _ arr#) = withBounds $ tail arr#
    
    toLast (Array _ _ arr#) e = withBounds $ arr# :< e
    
    last es = isNull es ? pfailEx "(:<)" $ es !^ (sizeOf es - 1)
    
    -- | O(1) 'init', O(1) memory.
    init Z = pfailEx "(:<)"
    init (Array _ _ arr#) = withBounds $ init arr#
    
    fromList = fromFoldable
    
    {-# INLINE fromListN #-}
    fromListN n = withBounds . fromListN n
    
    {-# INLINE fromFoldable #-}
    fromFoldable = withBounds . fromFoldable
    
    {-# INLINE single #-}
    single = withBounds . single
    
    -- | O(n + m) '++', O(n + m) memory.
    (Array _ _ xs) ++ (Array _ _ ys) = withBounds $ xs ++ ys
    
    {-# INLINE replicate #-}
    -- | O(n) 'replicate', O(n) memory.
    replicate n = withBounds . replicate n
    
    listL = toList
    
    listR = \ (Array _ _ arr#) -> listR arr#
    
    {-# INLINE concatMap #-}
    concatMap f = fromList . foldr (\ a l -> foldr (:) l $ f a) []
    
    {-# INLINE concat #-}
    concat = fromList . foldr (\ a l -> foldr (:) l a) []
    
    partitions f es = fromList <$> partitions f (listL es)

instance (Index i) => Split (Array i e) e
  where
    {-# INLINE take #-}
    -- | O(1) 'take', O(1) memory.
    take n (Array _ _ arr#) = withBounds $ take n arr#
    
    {-# INLINE drop #-}
    -- | O(1) 'drop', O(1) memory.
    drop n (Array _ _ arr#) = withBounds $ drop n arr#
    
    -- | O(m) 'splits', O(m) memory (m - sizes list length).
    splits ns (Array _ _ arr#) = withBounds <$> splits ns arr#
    
    -- | O(m) 'chuncks', O(m) memory (m - sizes list length).
    chunks ns (Array _ _ arr#) = withBounds <$> chunks ns arr#
    
    -- | O(m) 'parts', O(m) memory (m - sizes list length).
    parts  ns (Array _ _ arr#) = withBounds <$> parts  ns arr#
    
    isPrefixOf (Array l1 u1 arr1#) (Array l2 u2 arr2#) =
      size (l1, u1) <= size (l2, u2) && arr1# `isPrefixOf` arr2#
    
    isSuffixOf (Array l1 u1 arr1#) (Array l2 u2 arr2#) =
      size (l1, u1) <= size (l2, u2) && arr1# `isSuffixOf` arr2#

instance (Index i) => Bordered (Array i e) i e
  where
    {-# INLINE indexIn #-}
    indexIn (Array l u _) = inRange (l, u)
    
    {-# INLINE indexOf #-}
    indexOf (Array l u _) = index (l, u)
    
    {-# INLINE offsetOf #-}
    offsetOf (Array l u _) = offset (l, u)
    
    {-# INLINE sizeOf #-}
    sizeOf  (Array _ _ arr#) = sizeOf arr#
    
    {-# INLINE bounds #-}
    bounds  (Array l u _) = (l, u)
    
    {-# INLINE lower #-}
    lower   (Array l _ _) = l
    
    {-# INLINE upper #-}
    upper   (Array _ u _) = u

--------------------------------------------------------------------------------

{- Indexed, IFold, Set and Sort instances. -}

instance (Index i) => Indexed (Array i e) i e
  where
    {-# INLINE assoc' #-}
    assoc' bnds@(l, u) defvalue ascs = Array l u $ assoc' bnds' defvalue ies
      where
        ies   = [ (offset bnds i, e) | (i, e) <- ascs, inRange bnds i ]
        bnds' = defaultBounds $ size bnds
    
    fromIndexed = withBounds . fromIndexed
    
    {-# INLINE (//) #-}
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    arr // ascs = runST $ fromFoldableM arr >>= (`overwrite` ascs) >>= done
    
    {-# INLINE (!^) #-}
    (!^) (Array _ _ arr#) = (arr# !^)
    
    {-# INLINE (!) #-}
    (!) (Array l u arr#) = (arr# !^) . offset (l, u)
    
    (*$) p = ifoldr (\ i e is -> p e ? (i : is) $ is) []

instance (Index i) => IFold (Array i e) i e
  where
    {-# INLINE ifoldr #-}
    ifoldr f = \ base (Array l u arr#) -> ifoldr (\ i -> f $ index (l, u) i) base arr#
    
    {-# INLINE ifoldl #-}
    ifoldl f = \ base (Array l u arr#) -> ifoldl (\ i -> f $ index (l, u) i) base arr#
    
    i_foldr = foldr
    i_foldl = foldl

instance (Index i) => Set (Array i e) e
  where
    setWith f (Array _ _ arr#) = withBounds $ setWith f arr#
    
    insertWith f e (Array _ _ arr#) = withBounds $ insertWith f e arr#
    deleteWith f e (Array _ _ arr#) = withBounds $ deleteWith f e arr#
    
    {-# INLINE intersectionWith #-}
    intersectionWith f (Array _ _ arr1#) (Array _ _ arr2#) =
      withBounds $ intersectionWith f arr1# arr2#
    
    {-# INLINE unionWith #-}
    unionWith f (Array _ _ arr1#) (Array _ _ arr2#) =
      withBounds $ unionWith f arr1# arr2#
    
    {-# INLINE differenceWith #-}
    differenceWith f (Array _ _ arr1#) (Array _ _ arr2#) =
      withBounds $ differenceWith f arr1# arr2#
    
    {-# INLINE symdiffWith #-}
    symdiffWith f (Array _ _ arr1#) (Array _ _ arr2#) =
      withBounds $ differenceWith f arr1# arr2#
    
    isContainedIn f e (Array _ _   es) = isContainedIn f e   es
    lookupLTWith  f o (Array _ _ arr#) = lookupLTWith  f o arr#
    lookupGTWith  f o (Array _ _ arr#) = lookupGTWith  f o arr#
    lookupLEWith  f o (Array _ _ arr#) = lookupLEWith  f o arr#
    lookupGEWith  f o (Array _ _ arr#) = lookupGEWith  f o arr#

instance (Index i) => Sort (Array i e) e
  where
    sortBy cmp (Array l u arr#) = Array l u (sortBy cmp arr#)

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

{-# INLINE withBounds #-}
withBounds :: (Index i) => SArray# e -> Array i e
withBounds arr# = let (l, u) = defaultBounds (sizeOf arr#) in Array l u arr#

{-# INLINE done #-}
done :: STArray s i e -> ST s (Array i e)
done (STArray l u marr#) = Array l u <$> unsafeFreeze marr#

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.Array." ++ msg




