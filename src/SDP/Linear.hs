{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, GADTs, DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE Trustworthy #-}

{- |
    Module      :  SDP.Linear
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
  
  Linear is a module that provides several convenient interfaces for working
  with various linear data structures.
-}
module SDP.Linear
(
  module SDP.Index,
  module SDP.Zip,
  
  Bordered (..),
  Linear (..),
  Split (..),
  
  pattern (:>), pattern (:<), pattern Z,
  
  intercalate, tails, inits, nub,
  
  stripPrefix, stripSuffix
)

where

import Prelude ()
import SDP.SafePrelude

import qualified Data.List as L

import SDP.Index
import SDP.Zip

import GHC.Types

import SDP.Simple

default ()

--------------------------------------------------------------------------------

infixr 5 :>, ++
infixl 5 :<

--------------------------------------------------------------------------------

{- |
    Class of linear data structures that can be created from list.
    Linear similar to IsList, but also provides overloaded pattern synonyms:
    Z, (head :> tail) and (init :< last).
    
    Linear doesn't require Foldable because it's a bit stricter than needed.
    
    This class tries to balance between efficiency, code observability and
    consistency, but some functions in Linear may seem redundant and/or not very
    relevant.
-}

class Linear l e | l -> e
  where
    {-# MINIMAL isNull, (listL|listR), (fromList|fromListN), (head,tail|uncons), (init,last|unsnoc) #-}
    
    {- Service functions. -}
    
    -- | isNull - synonym for null, that doesn't requires Foldable
    isNull :: l -> Bool
    
    -- | Empty line. Service constant, synonym for Z.
    lzero :: l
    lzero =  fromList []
    
    -- | Separates head and tail. Service function, synonym for (x :> xs).
    uncons      :: l -> (e, l)
    uncons xs   =  (head xs, tail xs)
    
    -- | Adds element to head of line. Service function, synonym for (x :> xs).
    toHead      :: e -> l -> l
    toHead e es =  single e ++ es
    
    head    :: l -> e
    head es =  fst (uncons es)
    
    tail    :: l -> l
    tail es =  snd (uncons es)
    
    -- | Separates init and last. Service function, synonym for (xs :< x).
    unsnoc      :: l -> (l, e)
    unsnoc xs   =  (init xs, last xs)
    
    -- | Adds element to end of line. Service function, synonym for (xs :< x).
    toLast      :: l -> e -> l
    toLast es e =  es ++ single e
    
    init    :: l -> l
    init es =  fst (unsnoc es)
    
    last    :: l -> e
    last es =  snd (unsnoc es)
    
    {- Construction. -}
    
    -- | Singleton.
    single      :: e -> l
    single x    =  fromList [x]
    
    -- | Generalization of (Some.Module.++).
    (++)        :: l -> l -> l
    xs ++ ys    =  fromList $ (listL xs) ++ (listL ys)
    
    -- | Line of n equal elements.
    replicate   :: Int -> e -> l
    replicate n =  fromListN n . replicate n
    
    -- | Creates line from list elements.
    fromList    :: [e] -> l
    fromList es =  fromListN (length es) es
    
    -- | May create finite line from infinite list. Doesn't evaluate list twice times.
    fromListN   :: Int -> [e] -> l
    fromListN n =  fromList . take n
    
    -- | Same as listL . reverse.
    listR    :: l -> [e]
    listR es =  listL (reverse es)
    
    -- | Same as toList, but formally doesn't requires Foldable.
    listL    :: l -> [e]
    listL es =  reverse (listR es)
    
    {- Generalized folds. -}
    
    {- |
      Very powerful generalisation of fromList, but not comfortable to use -
      oftentimes needed type signatures.
    -}
    fromFoldable     :: (Foldable f) => f e -> l
    fromFoldable es  =  fromList $ toList es
    
    -- | Generalization of Some.Module.concat.
    concat           :: (Foldable f) => f l -> l
    concat ess       =  foldr (++) Z ess
    
    -- | Generalization of Data.List.concatMap.
    concatMap        :: (Foldable f) => (a -> l) -> f a -> l
    concatMap f ess  =  foldr' (\ x y -> f x ++ y) Z ess
    
    -- | Generalization of Data.List.intersperse. May be moved.
    intersperse      :: e -> l -> l
    intersperse e es =  fromList . intersperse e $ listL es
    
    {- Filtering functions. -}
    
    -- | Generalization of Some.Module.filter.
    filter         :: (e -> Bool) -> l -> l
    filter p es    =  fromList . filter p $ listL es
    
    -- | Generalization of Some.Module.partition.
    partition      :: (e -> Bool) -> l -> (l, l)
    partition p es =  (filter p es, filter (not . p) es)
    
    -- | Generalization of partition, that select sublines by predicates.
    partitions       :: (Foldable f) => f (e -> Bool) -> l -> [l]
    partitions ps' es = partitions' (toList ps') es
      where
        partitions'    []    xs = [xs]
        partitions' (p : ps) xs = let (y, ys) = partition p xs in y : partitions' ps ys
    
    {- Special functions -}
    
    -- Compares lines as [multi]sets. May be rewrited and moved.
    isSubseqOf :: (Eq e) => l -> l -> Bool
    isSubseqOf xs@(x :> rest) (y :> ys) = x == y && rest `isSubseqOf` ys || xs `isSubseqOf` ys
    isSubseqOf Z _ =  True
    isSubseqOf _ _ =  False
    
    -- | Generalization of Some.Module.reverse.
    reverse    :: l -> l
    reverse es =  fromList $ listR es
    
    -- | Generalization of Data.List.subsequences. May be moved.
    subsequences     :: l -> [l]
    subsequences xxs =  Z : ss xxs
      where
        ss es = case es of {(x :> xs) -> single x : foldr (\ ys r -> ys : (x :> ys) : r) [] (ss xs); _ -> Z}
    
    -- | Generalization of Data.List.nubBy.
    nubBy      :: (e -> e -> Bool) -> l -> l
    nubBy f es =  fromList . nubBy f $ listL es

--------------------------------------------------------------------------------

{- |
  Class of bordered data structures.
  This is a enough general class: type must be Foldable, but not necessarily
  Linear or Indexed.
-}

class (Index i) => Bordered (b) i e | b -> i, b -> e
  where
    {-# MINIMAL (bounds|(lower, upper)) #-}
    
    {-# INLINE bounds #-}
    bounds       :: b -> (i, i)
    bounds  es   =  (lower es, upper es)
    
    {-# INLINE indices #-}
    indices      :: b -> [i]
    indices es   =  range (bounds es)
    
    {-# INLINE lower #-}
    lower        :: b -> i
    lower   es   =  fst (bounds es)
    
    {-# INLINE upper #-}
    upper        :: b -> i
    upper   es   =  snd (bounds es)
    
    assocs       :: b -> [(i, e)]
    
    {-# INLINE sizeOf #-}
    sizeOf       :: b -> Int
    sizeOf  es   =  size (bounds es)
    
    {-# INLINE indexOf #-}
    indexOf      :: b -> i -> Bool
    indexOf es i = bounds es `inRange` i
    
    default assocs :: (Linear b e) => b -> [(i, e)]
    assocs es      =  zip (indices es) (listL es)

--------------------------------------------------------------------------------

{- |
  Split - class of splittable data structures. Also allows simple comparing
  functions.
-}

class (Linear s e) => Split s e | s -> e
  where
    {-# MINIMAL (take,drop|split) #-}
    
    {- Simple splitters. -}
    
    take       :: Int -> s -> s
    take n es  =  fst $ split n es
    
    drop       :: Int -> s -> s
    drop n es  =  snd $ split n es
    
    -- | split is same as Some.Module.splitAt
    split      :: Int -> s -> (s, s)
    split n es =  (take n es, drop n es)
    
    {- |
      splits is generalization of split, that breaks a line into sublines of a
      given sizes.
    -}
    splits         :: (Foldable f) => f Int -> s -> [s]
    splits ints es =  splits' (toList ints) es
      where
        splits'    []    xs = [xs]
        splits' (i : is) xs = let (y, ys) = split i xs in y : splits is ys
    
    {- |
      parts is generalization of split, that breaks a sublines into sublines
      starting at a given indices.
    -}
    parts          :: (Foldable f) => f Int -> s -> [s]
    parts ints es  =  parts' 0 (toList ints) es
      where
        parts' _ [] xs = [xs]
        parts' c (i : is) xs = let (y, ys) = split (i - c) xs in y : parts' i is ys
    
    {- Subsequence checkers. -}
    
    -- | isPrefixOf checks whether the first line is the beginning of the second
    isPrefixOf :: (Eq e) => s -> s -> Bool
    isPrefixOf (x :> xs) (y :> ys) = (x == y) && (xs `isPrefixOf` ys)
    isPrefixOf Z _ = True
    isPrefixOf _ _ = False
    
    -- | isSuffixOf checks whether the first line is the ending of the second
    isSuffixOf :: (Eq e) => s -> s -> Bool
    isSuffixOf (xs :< x) (ys :< y) = (x == y) && (xs `isSuffixOf` ys)
    isSuffixOf Z _ = True
    isSuffixOf _ _ = False
    
    -- | isInfixOf checks whether the first line is the substring of the second
    isInfixOf  :: (Eq e) => s -> s -> Bool
    isInfixOf  Z _   = True
    isInfixOf  _ Z   = False
    isInfixOf  xs ys = xs `isPrefixOf` ys || xs `isInfixOf` (tail ys)
    
    {- Largest sequences. -}
    
    -- | prefix gives length of init, satisfying preducate.
    prefix :: (e -> Bool) -> s -> Int
    
    -- | suffix gives length of tail, satisfying predicate.
    suffix :: (e -> Bool) -> s -> Int
    
    {- "Clever" splitters. -}
    
    -- | Takes the longest init.
    takeWhile :: (e -> Bool) -> s -> s
    takeWhile p es = take (p `prefix` es) es
    
    -- | Drops the longest init.
    dropWhile :: (e -> Bool) -> s -> s
    dropWhile p es = drop (p `prefix` es) es
    
    -- | Takes the longest tail.
    takeEnd   :: (e -> Bool) -> s -> s
    
    -- | Drops the longest tail.
    dropEnd   :: (e -> Bool) -> s -> s
    
    -- | Left-side span, generalization of Data.List.span
    spanl       :: (e -> Bool) -> s -> (s, s)
    spanl  p es =  (takeWhile p es, dropWhile p es)
    
    -- | Left-side break, generalization of Data.List.break
    breakl      :: (e -> Bool) -> s -> (s, s)
    breakl p es =  (takeWhile (not . p) es, dropWhile (not . p) es)
    
    -- | Right-side span. And Now for Something Completely Different.
    spanr       :: (e -> Bool) -> s -> (s, s)
    spanr p es  =  (takeEnd p es, dropEnd p es)
    
    -- | Right-side break. See above.
    breakr      :: (e -> Bool) -> s -> (s, s)
    breakr p es = (takeEnd (not . p) es, dropEnd (not . p) es)
    
    default takeEnd :: (Bordered s i e) => (e -> Bool) -> s -> s
    takeEnd p es    =  drop (sizeOf es - suffix p es) es
    
    default dropEnd :: (Bordered s i e) => (e -> Bool) -> s -> s
    dropEnd p es    =  take (sizeOf es - suffix p es) es
    
    default prefix  :: (Foldable t, t e ~~ s) => (e -> Bool) -> s -> Int
    prefix p        =  foldr (\ e c -> p e ? c + 1 $ 0) 0

    default suffix  :: (Foldable t, t e ~~ s) => (e -> Bool) -> s -> Int
    suffix p        =  foldl (\ c e -> p e ? c + 1 $ 0) 0

--------------------------------------------------------------------------------

-- | Pattern Z is generalization of []. Same as isNull and lzero.
pattern Z :: (Linear l e) => l
pattern Z <- (isNull -> True)                           where  Z   = lzero

-- | Pattern (head :> tail) is generalization of (head : tail).
-- Same as uncons and toHead.
pattern  (:>)   :: (Linear l e) => e -> l -> l
pattern x :> xs <- ((isNull ?: uncons) -> Just (x, xs)) where (:>) = toHead

-- | Pattern (init :< last) is right-size version of (:>)
-- Same as unsnoc and toLast.
pattern   (:<)  :: (Linear l e) => l -> e -> l
pattern xs :< x <- ((isNull ?: unsnoc) -> Just (xs, x)) where (:<) = toLast

--------------------------------------------------------------------------------

{-# COMPLETE Z,  (:)  #-}
{-# COMPLETE [], (:>) #-}
{-# COMPLETE [], (:<) #-}

instance Linear [e] e
  where
    isNull = null
    
    fromList    = id
    fromListN n = take n
    lzero       = [ ]
    single x    = [x]
    
    (++)        = (L.++)
    toHead      = (:)
    toLast xs x = foldr' (:) [x] xs
    
    uncons   [ ]      = throw $ PatternMatchFail "in SDP.Linear.(:>)"
    uncons (e : es)   = (e, es)
    
    unsnoc    [ ]     = throw $ PatternMatchFail "in SDP.Linear.(:<)"
    unsnoc    [e]     = ([], e)
    unsnoc  (e : es)  = (e : es', e') where (es', e') = unsnoc es
    
    reverse      = L.reverse
    replicate    = L.replicate
    intersperse  = L.intersperse
    
    filter       = L.filter
    partition    = L.partition
    concatMap    = L.concatMap
    fromFoldable = toList
    
    isSubseqOf   = L.isSubsequenceOf
    listL        = toList
    listR        = L.reverse
    nubBy        = L.nubBy

instance Bordered [e] Int e
  where
    assocs  es = zip  [0 .. ] es
    sizeOf  es = length es
    
    lower   _  = 0
    upper   es = length es - 1

instance Split [e] e
  where
    take  = L.take
    drop  = L.drop
    split = L.splitAt
    
    isPrefixOf = L.isPrefixOf
    isInfixOf  = L.isInfixOf
    isSuffixOf = L.isSuffixOf
    
    spanl  = L.span
    breakl = L.break

--------------------------------------------------------------------------------

-- | stripPrefix sub line... strips prefix sub of line
stripPrefix :: (Split s e, Bordered s i e, Eq e) => s -> s -> s
stripPrefix sub line = sub `isPrefixOf` line ? drop n line $ line
  where
    n = sizeOf sub

-- | stripSuffix sub line... strips suffix sub of line
stripSuffix :: (Split s e, Bordered s i e, Eq e) => s -> s -> s
stripSuffix sub line = sub `isSuffixOf` line ? take n line $ line
  where
    n = sizeOf line - sizeOf sub

-- | intercalate is generalization of Data.List.intercalate
intercalate   :: (Foldable f, Linear (f l) l, Linear l e) => l -> f l -> l
intercalate l =  concat . intersperse l

-- | nub is generalization of Data.List.nub and synonym for nubBy (==).
nub   :: (Linear l e, Eq e) => l -> l
nub   =  nubBy (==)

-- | tails is generalization of Data.List.tails.
tails :: (Linear l e) => l -> [l]
tails Z  = Z
tails es = es : tails (tail es)

-- | tails is generalization of Data.List.inits.
inits :: (Linear l e) => l -> [l]
inits Z  = Z
inits es = es : inits (init es)

