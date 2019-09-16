{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DefaultSignatures #-}
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
  
  -- * Bordered class
  -- $borderedDoc
  Bordered (..),
  -- * Linear class
  -- $linearDoc
  Linear   (..),
  -- * Split class
  -- $splitDoc
  Split    (..),
  
  -- * Patterns
  -- $patternDoc
  pattern (:>), pattern (:<), pattern Z,
  
  -- * Other functions
  -- $linearStuff
  stripPrefix, stripSuffix,
  
  intercalate, tails, inits, sorted, ascending
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

infixr 5 :>, ++
infixl 5 :<

--------------------------------------------------------------------------------

{- $borderedDoc
  Bordered is a service class for immutable structures.
  
  Please note that in this class only 'assocs' can throw exceptions
  ('IndexException' is recommended). Other functions must be defined everywhere,
  including, on possible incorrect data.
  
  Also note that sizeOf shouldn't return negative values (if size field is
  incorrect and recovery is not possible, the structure should be considered
  empty to avoid even more serious errors).
-}

-- | Class of bordered data structures.
class (Index i) => Bordered (b) i e | b -> i, b -> e
  where
    {-# MINIMAL (bounds|(lower, upper)) #-}
    
    {-# INLINE bounds #-}
    {- |
      bounds is a function that returns the exact upper and lower bounds of a
      structure. If the structure doesn't have explicitly defined boundaries
      (list, for example), use the default bounds: unsafeBounds (sizeOf es).
    -}
    bounds    :: b -> (i, i)
    bounds es =  (lower es, upper es)
    
    {-# INLINE lower #-}
    -- | lower bound of structure
    lower :: b -> i
    lower =  fst . bounds
    
    {-# INLINE upper #-}
    -- | upper bound of structure
    upper :: b -> i
    upper =  snd . bounds
    
    {-# INLINE assocs #-}
    -- | list of associations (index, element).
    default assocs :: (Linear b e) => b -> [(i, e)]
    assocs    :: b -> [(i, e)]
    assocs es =  indices es `zip` listL es
    
    {-# INLINE sizeOf #-}
    -- | actual size of structure.
    sizeOf  :: b -> Int
    sizeOf  =  size . bounds
    
    {-# INLINE indexIn #-}
    -- | checks if an index falls within the boundaries of the structure.
    indexIn :: b -> i -> Bool
    indexIn = inRange . bounds
    
    {-# INLINE indices #-}
    -- | index range list.
    indices :: b -> [i]
    indices =  range . bounds
    
    {-# INLINE indexOf #-}
    -- | index by offset in structure.
    indexOf :: b -> Int -> i
    indexOf es = index (bounds es)
    
    {-# INLINE offsetOf #-}
    -- | index offset in structure bounds.
    offsetOf :: b -> i -> Int
    offsetOf es = offset (bounds es)

--------------------------------------------------------------------------------

{- $linearDoc
  Linear is a class for linear data structures that contains functions for:
  
  * simple separation: 'head', 'tail', 'init', 'last', 'uncons', 'unsnoc'
    
  * concatenation: 'toHead', 'toLast', '(++)', 'concat', 'concatMap'
    
  * creation: empty ('lzero'), singleton ('single'), finite ('fromListN',
    'replicate') and, if the structure allows, potentially infinite ('fromList',
    'fromFoldable') data structures
    
  * left- and right-side view ('listL' and 'listR'), 'reverse'
    
  * filtering by condition: 'filter', 'partition', 'partitions'
    
  * deleting repeats: 'nub', 'nubBy'
  
  Linear doesn't require 'Foldable' because it's a bit stricter than needed.
-}

{- |
  Class for data structures that can be created from list and retain its basic
  properties.
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
    
    head :: l -> e
    head =  fst . uncons
    
    tail :: l -> l
    tail =  snd . uncons
    
    -- | Separates init and last. Service function, synonym for (xs :< x).
    unsnoc      :: l -> (l, e)
    unsnoc xs   =  (init xs, last xs)
    
    -- | Adds element to end of line. Service function, synonym for (xs :< x).
    toLast      :: l -> e -> l
    toLast es e =  es ++ single e
    
    init :: l -> l
    init =  fst . unsnoc
    
    last :: l -> e
    last =  snd . unsnoc
    
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
    listR :: l -> [e]
    listR =  listL . reverse
    
    -- | Same as toList, but formally doesn't require 'Foldable'.
    listL :: l -> [e]
    listL =  reverse . listR
    
    {- Generalized folds. -}
    
    {- |
      Very powerful generalisation of 'fromList', but not comfortable to use -
      oftentimes needed type signatures.
    -}
    fromFoldable :: (Foldable f) => f e -> l
    fromFoldable =  fromList . toList
    
    -- | Generalization of Some.Module.concat.
    concat :: (Foldable f) => f l -> l
    concat =  foldr (++) Z
    
    -- | Generalization of Data.List.concatMap.
    concatMap   :: (Foldable f) => (a -> l) -> f a -> l
    concatMap f =  foldr' (\ x y -> f x ++ y) Z
    
    -- | Generalization of Data.List.intersperse. May be moved.
    intersperse   :: e -> l -> l
    intersperse e =  fromList . intersperse e . listL
    
    {- Filtering functions. -}
    
    -- | Generalization of Some.Module.filter.
    filter   :: (e -> Bool) -> l -> l
    filter p =  fromList . filter p . listL
    
    -- | Generalization of Some.Module.partition.
    partition      :: (e -> Bool) -> l -> (l, l)
    partition p es =  (filter p es, filter (not . p) es)
    
    -- | Generalization of partition, that select sublines by predicates.
    partitions       :: (Foldable f) => f (e -> Bool) -> l -> [l]
    partitions ps' = partitions' (toList ps')
      where
        partitions'    []    xs = [xs]
        partitions' (p : ps) xs = let (y, ys) = partition p xs in y : partitions' ps ys
    
    {- Special functions -}
    
    -- Compares lines as sorted multisets. May be rewrited and moved.
    isSubseqOf :: (Eq e) => l -> l -> Bool
    isSubseqOf xs@(x :> xs') (y :> ys) = x == y && xs' `isSubseqOf` ys || xs `isSubseqOf` ys
    isSubseqOf xs _ = isNull xs
    
    -- | Generalization of Some.Module.reverse.
    reverse :: l -> l
    reverse =  fromList . listR
    
    -- | Generalization of Data.List.subsequences.
    subsequences     :: l -> [l]
    subsequences xxs =  Z : ss xxs
      where
        ss es = case es of {(x :> xs) -> single x : foldr (\ ys r -> ys : (x :> ys) : r) [] (ss xs); _ -> Z}
    
    -- | same as nubBy (==)
    nub :: (Eq e) => l -> l
    nub = nubBy (==)
    
    -- | Generalization of Data.List.nubBy.
    nubBy :: (e -> e -> Bool) -> l -> l
    nubBy f = fromList . nubBy f . listL

--------------------------------------------------------------------------------

{- $splitDoc
  Split is class for structures that may be splitted on parts:
    
  * by length: 'take', 'drop' and 'split' (also known as splitAt)
    
  * by condition: 'takeWhile', 'dropWhile', 'spanl', 'breakl' (left to right)
    and 'takeEnd', 'dropEnd', 'spanr', 'breakr' (right to left).
  
  Also Split contain some comparators:
    
  * by content: 'isPrefixOf', 'isInfixOf' and 'isSuffixOf'
    
  * by condition: 'prefix' and 'suffix'.
-}

-- | Split - class of splittable data structures.
class (Linear s e) => Split s e | s -> e
  where
    {-# MINIMAL (take,drop|split) #-}
    
    {- Simple splitters. -}
    
    take   :: Int -> s -> s
    take n =  fst . split n
    
    drop   :: Int -> s -> s
    drop n =  snd . split n
    
    -- | split is same as Some.Module.splitAt
    split      :: Int -> s -> (s, s)
    split n es =  (take n es, drop n es)
    
    {-# INLINE splits #-}
    -- | splits splits structures into sublines by given lengths.
    splits      :: (Foldable f) => f Int -> s -> [s]
    splits ints =  splits' (toList ints)
      where
        splits'    []    xs = [xs]
        splits' (i : is) xs = let (y, ys) = split i xs in y : splits is ys
    
    {-# INLINE parts #-}
    -- | parts splits structures into parts by given initial indices.
    parts      :: (Foldable f) => f Int -> s -> [s]
    parts ints =  parts' 0 (toList ints)
      where
        parts' _ [] xs = [xs]
        parts' c (i : is) xs = let (y, ys) = split (i - c) xs in y : parts' i is ys
    
    {-# INLINE chunks #-}
    -- | chunks splits structures into chunks of a given size (and the rest).
    chunks :: Int -> s -> [s]
    chunks n = \ es -> case split n es of {(x, Z) -> [x]; (x, xs) -> x : chunks n xs}
    
    {- Subsequence checkers. -}
    
    -- | isPrefixOf checks whether the first line is the beginning of the second
    isPrefixOf :: (Eq e) => s -> s -> Bool
    isPrefixOf (x :> xs) (y :> ys) = x == y && xs `isPrefixOf` ys
    isPrefixOf xs _ = isNull xs
    
    -- | isSuffixOf checks whether the first line is the ending of the second
    isSuffixOf :: (Eq e) => s -> s -> Bool
    isSuffixOf (xs :< x) (ys :< y) = x == y && xs `isSuffixOf` ys
    isSuffixOf xs _ = isNull xs
    
    -- | isInfixOf checks whether the first line is the substring of the second
    isInfixOf  :: (Eq e) => s -> s -> Bool
    isInfixOf xs ys@(_ :> ys') = xs `isPrefixOf` ys || xs `isInfixOf` ys'
    isInfixOf _ _ = False
    
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

{- $patternDoc
  SDP.Linear also provides three overloaded patterns: 'Z', (':>') and (':<').
-}

-- | Pattern Z is generalization of []. Same as isNull and lzero.
pattern Z :: (Linear l e) => l
pattern Z <- (isNull -> True)                           where  Z   = lzero

-- | Pattern (:>) is left-size view of linear. Same as uncons and toHead.
pattern  (:>)   :: (Linear l e) => e -> l -> l
pattern x :> xs <- ((isNull ?: uncons) -> Just (x, xs)) where (:>) = toHead

-- | Pattern (:<) is right-size view of linear. Same as unsnoc and toLast.
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
    fromListN   = take
    lzero       = [ ]
    single x    = [x]
    
    (++)        = (L.++)
    toHead      = (:)
    toLast xs x = foldr' (:) [x] xs
    
    uncons   [ ]     = throw $ PatternMatchFail "in SDP.Linear.(:>)"
    uncons (e : es)  = (e, es)
    
    unsnoc    [ ]    = throw $ PatternMatchFail "in SDP.Linear.(:<)"
    unsnoc    [e]    = ([], e)
    unsnoc  (e : es) = let (es', e') = unsnoc es in (e : es', e')
    
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

{- $linearStuff
  And also SDP.Linear provides some common functions, not included to classes.
-}

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

-- | tails is generalization of Data.List.tails.
tails :: (Linear l e) => l -> [l]
tails Z  = Z
tails es = es : tails (tail es)

-- | tails is generalization of Data.List.inits.
inits :: (Linear l e) => l -> [l]
inits Z  = Z
inits es = es : inits (init es)

-- | sorted is a function that checks for sorting.
sorted :: (Linear l e, Ord e) => l -> Bool
sorted Z  = True
sorted es = and $ zipWith (<=) es' (tail es') where es' = listL es

{- |
  ascending is a function that checks if sequences of elements given in pairs
  (start, length) are sorted in ascending order.
-}
ascending :: (Split s e, Ord e) => s -> [(Int, Int)] -> Bool
ascending es ss = sorted `all` splits (snds ss) es





