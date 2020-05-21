{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, DeriveGeneric #-}

{- |
    Module      :  SDP.ByteList.Ublist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.ByteList.Ublist@ provides 'Ublist' - strict boxed unrolled linked list.
-}
module SDP.ByteList.Ublist
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Unboxed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Ublist
  Ublist (..)
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Prim.SBytes
import SDP.ByteList.STUblist

import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Scan
import SDP.Set

import SDP.SortM.Tim

import SDP.Internal
import Text.Show.SDP
import Text.Read.SDP

import GHC.Generics

import Test.QuickCheck

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | 'Ublist' is unrolled linked list of unboxed values.
newtype Ublist e = Ublist [SBytes# e] deriving ( Generic )

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Unboxed e) => Eq (Ublist e)
  where
    Z == Z = True
    xs@(Ublist (x : xs')) == ys@(Ublist (y : ys')) = if n1 > n2
        then take n2 x == y && drop n2 xs == Ublist ys'
        else take n1 y == x && drop n1 ys == Ublist xs'
      where
        n1 = sizeOf x
        n2 = sizeOf y
    _ == _ = False

instance (Unboxed e, Ord e) => Ord (Ublist e)
  where
    compare Z Z = EQ
    compare Z _ = LT
    compare _ Z = GT
    compare xs@(Ublist ~(x : xs')) ys@(Ublist ~(y : ys')) = if n1 > n2
        then (take n2 x <=> y) <> (drop n2 xs <=> Ublist ys')
        else (x <=> take n1 y) <> (Ublist xs' <=> drop n1 ys)
      where
        n1 = sizeOf x
        n2 = sizeOf y

--------------------------------------------------------------------------------

{- Show instance. -}

instance {-# OVERLAPPABLE #-} (Show e, Unboxed e) => Show (Ublist e)
  where
    showsPrec = assocsPrec "ublist "

instance Show (Ublist Char)
  where
    showsPrec = shows ... const listL

instance (Read e, Unboxed e) => Read (Ublist e)
  where
    readPrec = indexedPrec' "ublist"
    readList = readListDefault

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance (Unboxed e) => Semigroup (Ublist e) where  (<>)  = (++)
instance (Unboxed e) => Monoid    (Ublist e) where mempty = def

instance Default (Ublist e) where def = Ublist []

instance (Arbitrary e, Unboxed e) => Arbitrary (Ublist e)
  where
    arbitrary = fromList <$> arbitrary

instance (Unboxed e) => Estimate (Ublist e)
  where
    (<==>) = go 0
      where
        go o Z   Z = o <=> 0
        go o Z  ys = o <=.> ys
        go o xs  Z = xs <.=> (-o)
        go o (Ublist ~(x : xs)) (Ublist ~(y : ys)) =
          go (o + sizeOf x - sizeOf y) (Ublist xs) (Ublist ys)
    
    (Ublist (x : xs)) <.=> n = x .> n ? GT $ Ublist xs <.=> (n - sizeOf x)
    _ <.=> n = 0 <=> n

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Unboxed e) => Linear (Ublist e) e
  where
    isNull (Ublist es) = all isNull es
    
    lzero = def
    
    toHead e (Ublist es@(x : xs)) = Ublist $ x .< lim ? (e :> x) : xs $ single e : es
    toHead e _ = single e
    
    uncons = uncons' . unpack'
      where
        uncons' ((x :> xs) : xss) = (x, Ublist (xs : xss))
        uncons' _ = pfailEx "(:>)"
    
    toLast (Ublist (xs :< x)) e = isNull x ? Ublist xs :< e $ Ublist (xs :< (x :< e))
    toLast _ e = single e
    
    unsnoc = unsnoc' . unpack'
      where
        unsnoc' (xss :< (xs :< x)) = (Ublist (xss :< xs), x)
        unsnoc' _ = pfailEx "(:<)"
    
    single e = Ublist [single e]
    fromList = Ublist . fmap fromList . chunks lim
    
    listL = foldr (flip $ i_foldr (:)) [] . unpack'
    
    Ublist xs ++ Ublist ys = Ublist (xs ++ ys)
    
    -- | Deduplicated Unlist: O(1), O(1) memory (limited by a constant on top).
    replicate n e = Ublist $ replicate count chunk :< rest
      where
        (count, rst) = n `divMod` lim
        
        chunk = replicate lim e
        rest  = replicate rst e
    
    reverse = fromList . i_foldl (flip (:)) []
    
    partition p = uncurry ((,) `on` fromList) . partition p . listL
    
    select  f (Ublist es) = concatMap (select f) es
    extract f (Ublist es) = bimap concat Ublist . unzip $ extract f <$> es
    
    selects fs = second fromList . selects fs . listL

instance (Unboxed e) => Split (Ublist e) e
  where
    take n es | n < 1 = Z | es .<= n = es | True = Ublist $ take' n (unpack' es)
      where
        take' c (x : xs) = case c <=.> x of
          GT -> x : take' (c - sizeOf x) xs
          LT -> [take c x]
          EQ -> [x]
        take' _ _ = Z
    
    drop n es | n < 1 = es | es .<= n = Z | True = Ublist $ drop' n (unpack' es)
      where
        drop' c (x : xs) = case c <=.> x of
          GT -> drop' (c - sizeOf x) xs
          LT -> drop c x : xs
          EQ -> xs
        drop' _ _ = Z
    
    isPrefixOf xs ys = xs == take (sizeOf xs) ys
    isSuffixOf xs ys = xs == keep (sizeOf xs) ys
    isInfixOf = on isInfixOf listL
    
    prefix p = foldr f 0 . unpack'
      where
        f = \ x acc -> let n = prefix p x in n ==. x ? n + acc $ n
    
    suffix p = foldl f 0 . unpack'
      where
        f = \ acc x -> let n = suffix p x in n ==. x ? n + acc $ n

instance (Unboxed e) => Bordered (Ublist e) Int
  where
    sizeOf (Ublist es) = foldr' ((+) . sizeOf) 0 es
    
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

instance (Unboxed e) => Set (Ublist e) e
  where
    setWith f = nubSorted f . sortBy f
    
    insertWith f' e' = Ublist . go f' e' . unpack'
      where
        go f e (x : xs) = isContainedIn f e x ? insertWith f e x : xs $ x : go f e xs
        go _ e _ = [single e]
    
    deleteWith f' e' = Ublist . go f' e' . unpack'
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

instance (Unboxed e) => Scan (Ublist e) e

instance (Unboxed e) => Sort (Ublist e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

{- Indexed and IFold instances. -}

instance (Unboxed e) => Indexed (Ublist e) Int e
  where
    assoc bnds ascs = runST $ fromAssocs bnds ascs >>= done
    
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    Z // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    es // ascs = isNull ascs ? es $ runST $ thaw es >>= (`overwrite` ascs) >>= done
    
    fromIndexed es = runST $ fromIndexed' es >>= done
    
    (Ublist (x : xs)) !^ i = i <. x ? x !^ i $ Ublist xs !^ (i - sizeOf x)
    _ !^ _ = error "in SDP.ByteList.Ublist.(!^)"
    
    (.!) = (!^)
    
    (.$) p (Ublist (x : xs)) = p .$ x <|> (+ sizeOf x) <$> p .$ Ublist xs
    (.$) _ _ = Nothing
    
    (*$) p (Ublist (x : xs)) = p *$ x ++ fmap (+ sizeOf x) (p *$ Ublist xs)
    (*$) _ _ = []

instance (Unboxed e) => IFold (Ublist e) Int e
  where
    ifoldr f' base' = go 0 f' base' . unpack'
      where
        go :: (Unboxed e) => Int -> (Int -> e -> r -> r) -> r -> [SBytes# e] -> r
        go o f base (x : xs) = ifoldr (f . (o +)) (go (o + sizeOf x) f base xs) x
        go _ _ base _ = base
    
    ifoldl f' base' = go 0 f' base' . unpack'
      where
        go :: (Unboxed e) => Int -> (Int -> r -> e -> r) -> r -> [SBytes# e] -> r
        go o f base (x : xs) = go (o + sizeOf x) f (ifoldl (f . (o +)) base x) xs
        go _ _ base _ = base
    
    i_foldr f base = foldr (flip $ i_foldr f) base . unpack'
    i_foldl f base = foldl (i_foldl f) base . unpack'

--------------------------------------------------------------------------------

instance IsString (Ublist Char) where fromString = fromList

--------------------------------------------------------------------------------

instance (Unboxed e) => Thaw   (ST s) (Ublist e) (STUblist s e)
  where
    thaw (Ublist es) = STUblist <$> mapM thaw es
    
    unsafeThaw (Ublist es) = STUblist <$> mapM unsafeThaw es

instance (Unboxed e) => Freeze (ST s) (STUblist s e) (Ublist e)
  where
    freeze (STUblist es) = Ublist <$> mapM freeze es
    
    unsafeFreeze (STUblist es) = Ublist <$> mapM unsafeFreeze es

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: (Unboxed e) => STUblist s e -> ST s (Ublist e)
done =  freeze

unpack' :: (Unboxed e) => Ublist e -> [SBytes# e]
unpack' =  \ (Ublist es) -> except isNull es

{-# INLINE nubSorted #-}
nubSorted :: (Unboxed e) => Compare e -> Ublist e -> Ublist e
nubSorted f (xs :< x) = fromList $ i_foldr (\ e ls@(l : _) -> e `f` l == EQ ? ls $ e : ls) [x] xs
nubSorted _ _ = Z

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.ByteList.Ublist."

lim :: Int
lim =  1024


