{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations, DeriveGeneric #-}

{- |
    Module      :  SDP.Unrolled.Unlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Unrolled.Unlist@ provides 'Unlist' - lazy boxed unrolled linked list.
-}
module SDP.Unrolled.Unlist
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Unlist
  Unlist (..)
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

import GHC.ST   ( ST  (..), runST )
import GHC.Base ( Int (..) )

import Data.String ( IsString (..) )

import Data.Bifunctor

import SDP.Unrolled.STUnlist
import SDP.SortM.Tim

import SDP.Internal.Commons
import SDP.Internal.SArray
import SDP.Internal.Read
import SDP.Internal.Show

default ()

--------------------------------------------------------------------------------

-- | 'Unlist' is unrolled linked list of boxed values.
newtype Unlist e = Unlist [SArray# e] deriving ( Generic )

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Eq e) => Eq (Unlist e)
  where
    Z == Z = True
    xs@(Unlist (x : xs')) == ys@(Unlist (y : ys')) = if n1 > n2
        then take n2 x == y && drop n2 xs == Unlist ys'
        else take n1 y == x && drop n1 ys == Unlist xs'
      where
        n1 = sizeOf x
        n2 = sizeOf y
    _ == _ = False

instance Eq1 Unlist
  where
    liftEq _ Z Z = True
    liftEq f xs@(Unlist (x : xs')) ys@(Unlist (y : ys')) = if n1 > n2
        then liftEq f (take n2 x) y && liftEq f (drop n2 xs) (Unlist ys')
        else liftEq f x (take n1 y) && liftEq f (Unlist xs') (drop n1 ys)
      where
        n1 = sizeOf x
        n2 = sizeOf y
    liftEq _ _ _ = False

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Ord e) => Ord (Unlist e)
  where
    compare Z Z = EQ
    compare Z _ = LT
    compare _ Z = GT
    compare xs@(Unlist ~(x : xs')) ys@(Unlist ~(y : ys')) = if n1 > n2
        then (take n2 x <=> y) <> (drop n2 xs <=> Unlist ys')
        else (x <=> take n1 y) <> (Unlist xs' <=> drop n1 ys)
      where
        n1 = sizeOf x
        n2 = sizeOf y

instance Ord1 Unlist
  where
    liftCompare _ Z Z = EQ
    liftCompare _ Z _ = LT
    liftCompare _ _ Z = GT
    liftCompare f xs@(Unlist ~(x : xs')) ys@(Unlist ~(y : ys')) = if n1 > n2
        then liftCompare f (take n2 x) y <> liftCompare f (drop n2 xs) (Unlist ys')
        else liftCompare f x (take n1 y) <> liftCompare f (Unlist xs') (drop n1 ys)
      where
        n1 = sizeOf x
        n2 = sizeOf y

--------------------------------------------------------------------------------

{- Show instance. -}

instance {-# OVERLAPPABLE #-} (Show e) => Show (Unlist e)
  where
    showsPrec = assocsPrec "unlist "

instance Show (Unlist Char)
  where
    showsPrec = shows ... const listL

instance (Read e) => Read (Unlist e)
  where
    readPrec = linearIndexedPrec "ublist"
    readList = readListDefault

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance Semigroup (Unlist e) where (<>) = (++)
instance Monoid    (Unlist e) where mempty = def
instance Default   (Unlist e) where def = Unlist []

instance (Arbitrary e) => Arbitrary (Unlist e)
  where
    arbitrary = fromList <$> arbitrary

instance Estimate (Unlist e)
  where
    (<==>) = go 0
      where
        go o Z   Z = o <=> 0
        go o Z  ys = o <=.> ys
        go o xs  Z = xs <.=> (-o)
        go o (Unlist ~(x : xs)) (Unlist ~(y : ys)) =
          go (o + sizeOf x - sizeOf y) (Unlist xs) (Unlist ys)
    
    (Unlist (x : xs)) <.=> n = x .> n ? GT $ Unlist xs <.=> (n - sizeOf x)
    _ <.=> n = 0 <=> n

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances -}

instance Functor Unlist where fmap f (Unlist es) = Unlist (fmap f <$> es)

instance Zip Unlist
  where
    zipWith  f as bs             = fromList $ zipWith  f (toList as) (toList bs)
    zipWith3 f as bs cs          = fromList $ zipWith3 f (toList as) (toList bs) (toList cs)
    zipWith4 f as bs cs ds       = fromList $ zipWith4 f (toList as) (toList bs) (toList cs) (toList ds)
    zipWith5 f as bs cs ds es    = fromList $ zipWith5 f (toList as) (toList bs) (toList cs) (toList ds) (toList es)
    zipWith6 f as bs cs ds es fs = fromList $ zipWith6 f (toList as) (toList bs) (toList cs) (toList ds) (toList es) (toList fs)

instance Applicative Unlist
  where
    pure = single
    fs <*> es = concatMap (<$> es) fs

--------------------------------------------------------------------------------

{- Foldable and Traversable instances. -}

instance Foldable Unlist
  where
    foldr = i_foldr
    foldl = i_foldl
    
    elem e (Unlist es) = elem e `any` es
    
    length = sizeOf
    toList = listL

instance Traversable Unlist
  where
    traverse f (Unlist es) = Unlist <$> traverse (traverse f) es

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance Linear (Unlist e) e
  where
    isNull (Unlist es) = all isNull es
    
    lzero = def
    
    toHead e (Unlist es@(x : xs)) = Unlist $ x .< lim ? (e :> x) : xs $ single e : es
    toHead e _ = single e
    
    uncons = uncons' . unpack'
      where
        uncons' ((x :> xs) : xss) = (x, Unlist (xs : xss))
        uncons' _ = pfailEx "(:>)"
    
    toLast (Unlist (xs :< x)) e = isNull x ? Unlist xs :< e $ Unlist (xs :< (x :< e))
    toLast _ e = single e
    
    unsnoc = unsnoc' . unpack'
      where
        unsnoc' (xss :< (xs :< x)) = (Unlist (xss :< xs), x)
        unsnoc' _ = pfailEx "(:<)"
    
    single e = Unlist [single e]
    fromList = Unlist . fmap fromList . chunks lim
    
    listL = foldr (flip $ i_foldr (:)) [] . unpack'
    
    Unlist xs ++ Unlist ys = Unlist (xs ++ ys)
    
    -- | Deduplicated Unlist: O(1), O(1) memory (limited by a constant on top).
    replicate n e = Unlist $ replicate count chunk :< rest
      where
        (count, rst) = n `divMod` lim
        
        chunk = replicate lim e
        rest  = replicate rst e
    
    reverse = fromList . i_foldl (flip (:)) []
    
    partition p = uncurry ((,) `on` fromList) . partition p . listL
    
    select  f (Unlist es) = concatMap (select f) es
    extract f (Unlist es) = bimap concat Unlist . unzip $ extract f <$> es
    
    selects fs = second fromList . selects fs . listL

instance Split (Unlist e) e
  where
    take n es | n < 1 = Z | es .<= n = es | True = Unlist $ take' n (unpack' es)
      where
        take' c (x : xs) = case c <=.> x of
          GT -> x : take' (c - sizeOf x) xs
          LT -> [take c x]
          EQ -> [x]
        take' _ _ = Z
    
    drop n es | n < 1 = es | es .<= n = Z | True = Unlist $ drop' n (unpack' es)
      where
        drop' c (x : xs) = case c <=.> x of
          GT -> drop' (c - sizeOf x) xs
          LT -> drop c x : xs
          EQ -> xs
        drop' _ _ = Z
    
    isPrefixOf xs ys = xs == take (sizeOf xs) ys
    isSuffixOf xs ys = xs == keep (sizeOf xs) ys
    isInfixOf = on isInfixOf listL
    
    prefix p = i_foldr (\ e c -> p e ? c + 1 $ 0) 0
    suffix p = i_foldl (\ c e -> p e ? c + 1 $ 0) 0

instance Bordered (Unlist e) Int e
  where
    sizeOf (Unlist es) = foldr' ((+) . sizeOf) 0 es
    
    indexIn es = \ i -> i >= 0 && i <. es
    
    -- | Quick unchecked offset.
    offsetOf = const id
    
    -- | Quick unchecked index.
    indexOf  = const id
    
    lower   _ = 0
    upper  es = sizeOf es - 1
    bounds es = (0, sizeOf es - 1)

--------------------------------------------------------------------------------

{- Set, Scan and Sort instances. -}

instance Set (Unlist e) e
  where
    setWith f = nubSorted f . sortBy f
    
    insertWith f' e' = Unlist . go f' e' . unpack'
      where
        go f e (x : xs) = isContainedIn f e x ? insertWith f e x : xs $ x : go f e xs
        go _ e _ = [single e]
    
    deleteWith f' e' = Unlist . go f' e' . unpack'
      where
        go f e (x : xs) = isContainedIn f e x ? deleteWith f e x : xs $ x : go f e xs
        go _ _ _ = []
    
    intersectionWith f = fromList ... on (intersectionWith f) listL
    unionWith        f = fromList ... on (unionWith        f) listL
    differenceWith   f = fromList ... on (differenceWith   f) listL
    symdiffWith      f = fromList ... on (symdiffWith      f) listL
    
    lookupLTWith f o = foldr ((<|>) . lookupLTWith f o) Nothing . unpack'
    lookupLEWith f o = foldr ((<|>) . lookupLEWith f o) Nothing . unpack'
    lookupGTWith f o = foldr ((<|>) . lookupGTWith f o) Nothing . unpack'
    lookupGEWith f o = foldr ((<|>) . lookupGEWith f o) Nothing . unpack'
    
    isContainedIn = binaryContain
    
    isSubsetWith f xs ys = i_foldr (\ e b -> isContainedIn f e ys && b) True xs

instance Scan (Unlist e) e

instance Sort (Unlist e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

{- Indexed and IFold instances. -}

instance Indexed (Unlist e) Int e
  where
    assoc bnds ascs = runST $ fromAssocs bnds ascs >>= done
    
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    Z // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    es // ascs = isNull ascs ? es $ runST $ thaw es >>= (`overwrite` ascs) >>= done
    
    fromIndexed es = runST $ fromIndexed' es >>= done
    
    (Unlist (x : xs)) !^ i = i <. x ? x !^ i $ Unlist xs !^ (i - sizeOf x)
    _ !^ _ = error "in SDP.Unrolled.Unlist.(!^)"
    
    (.!) = (!^)
    
    (.$) p (Unlist (x : xs)) = p .$ x <|> (+ sizeOf x) <$> p .$ Unlist xs
    (.$) _ _ = Nothing
    
    (*$) p (Unlist (x : xs)) = p *$ x ++ fmap (+ sizeOf x) (p *$ Unlist xs)
    (*$) _ _ = []

instance IFold (Unlist e) Int e
  where
    ifoldr f' base' = go 0 f' base' . unpack'
      where
        go :: Int -> (Int -> e -> r -> r) -> r -> [SArray# e] -> r
        go o f base (x : xs) = ifoldr (f . (o +)) (go (o + sizeOf x) f base xs) x
        go _ _ base _ = base
    
    ifoldl f' base' = go 0 f' base' . unpack'
      where
        go :: Int -> (Int -> r -> e -> r) -> r -> [SArray# e] -> r
        go o f base (x : xs) = go (o + sizeOf x) f (ifoldl (f . (o +)) base x) xs
        go _ _ base _ = base
    
    i_foldr f base = foldr (flip $ i_foldr f) base . unpack'
    i_foldl f base = foldl (i_foldl f) base . unpack'

--------------------------------------------------------------------------------

instance IsString (Unlist Char) where fromString = fromList

--------------------------------------------------------------------------------

instance Thaw   (ST s) (Unlist e) (STUnlist s e)
  where
    thaw (Unlist es) = STUnlist <$> mapM thaw es
    
    unsafeThaw (Unlist es) = STUnlist <$> mapM unsafeThaw es

instance Freeze (ST s) (STUnlist s e) (Unlist e)
  where
    freeze (STUnlist es) = Unlist <$> mapM freeze es
    
    unsafeFreeze (STUnlist es) = Unlist <$> mapM unsafeFreeze es

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: STUnlist s e -> ST s (Unlist e)
done =  freeze

unpack' :: Unlist e -> [SArray# e]
unpack' =  \ (Unlist es) -> except isNull es

{-# INLINE nubSorted #-}
nubSorted :: Compare e -> Unlist e -> Unlist e
nubSorted f (xs :< x) = fromList $ i_foldr (\ e ls@(l : _) -> e `f` l == EQ ? ls $ e : ls) [x] xs
nubSorted _ _ = Z

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.Unrolled.Unlist."

lim :: Int
lim =  1024

