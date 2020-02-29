{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, ConstraintKinds #-}
{-# LANGUAGE Trustworthy #-}

{- |
    Module      :  SDP.Linear
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
  
  @SDP.Linear@ is a module that provides several convenient interfaces for
  working with various linear data structures.
-}
module SDP.Linear
(
  module SDP.Index,
  module SDP.Zip,
  
  -- * Bordered class
  Bordered (..), Bordered1, Bordered2,
  
  -- * Linear class
  -- $linearDoc
  Linear (..), Linear1,
  
  -- * Split class
  -- $splitDoc
  Split (..), Split1,
  
  -- * Patterns
  -- $patternDoc
  pattern (:>), pattern (:<), pattern Z,
  
  -- * Other functions
  stripPrefix, stripSuffix, intercalate, tails, inits, sorted, ascending
)
where

import Prelude ()
import SDP.SafePrelude

import qualified Data.List as L

import SDP.Index
import SDP.Zip

import GHC.Types

import SDP.Internal.Commons

default ()

infixr 5 :>, ++
infixl 5 :<

--------------------------------------------------------------------------------

-- | Class of bordered data structures.
class (Index i) => Bordered (b) i e | b -> i, b -> e
  where
    {-# MINIMAL (bounds|(lower, upper)) #-}
    
    {-# INLINE bounds #-}
    {- |
      bounds is a function that returns the exact upper and lower bounds of a
      structure. If the structure doesn't have explicitly defined boundaries
      (list, for example), use the @'defaultBounds' ('sizeOf' es)@.
    -}
    bounds :: b -> (i, i)
    bounds es = (lower es, upper es)
    
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
    assocs :: b -> [(i, e)]
    assocs es = indices es `zip` listL es
    
    {-# INLINE sizeOf #-}
    -- | actual size of structure.
    sizeOf :: b -> Int
    sizeOf =  size . bounds
    
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
    indexOf =  index . bounds
    
    {-# INLINE offsetOf #-}
    -- | index offset in structure bounds.
    offsetOf :: b -> i -> Int
    offsetOf =  offset . bounds

--------------------------------------------------------------------------------

{- $linearDoc
  Linear is a class for linear data structures that contains functions for:
  * simple separation: 'head', 'tail', 'init', 'last', 'uncons', 'unsnoc'
  * append and concatenation: 'toHead', 'toLast', ('++'), 'concat', 'concatMap'
  * creation: empty ('lzero'), singleton ('single'), finite ('fromListN',
    'replicate') and, if the structure allows, potentially infinite ('fromList',
    'fromFoldable') data structures
  * left- and right-side view ('listL' and 'listR'), 'reverse'
  * filtering by condition: 'filter', 'partition', 'partitions'
  * deleting repeats: 'nub', 'nubBy'
  
  Linear doesn't require 'Foldable' because it's a bit stricter than needed.
-}

-- | Class of list-like data structures.
class Linear l e | l -> e
  where
    {-# MINIMAL isNull, (listL|listR), (fromList|fromListN), (head,tail|uncons), (init,last|unsnoc) #-}
    
    -- | isNull - synonym for 'null', check function for 'Z'.
    isNull :: l -> Bool
    
    -- | Empty line. Service constant, value for 'Z'.
    lzero :: l
    lzero =  fromList []
    
    -- | Separates 'head' and 'tail'. Service function, used in (':>').
    uncons :: l -> (e, l)
    uncons xs = (head xs, tail xs)
    
    -- | Adds element to line as 'head'. Service function, used in (':>').
    toHead :: e -> l -> l
    toHead =  (++) . single
    
    head :: l -> e
    head =  fst . uncons
    
    tail :: l -> l
    tail =  snd . uncons
    
    -- | Separates 'init' and 'last'. Service function, used in (':<').
    unsnoc :: l -> (l, e)
    unsnoc xs = (init xs, last xs)
    
    -- | Adds element to line as 'last'. Service function, used in (':<').
    toLast :: l -> e -> l
    toLast es =  (es ++) . single
    
    init :: l -> l
    init =  fst . unsnoc
    
    last :: l -> e
    last =  snd . unsnoc
    
    -- | Singleton.
    single :: e -> l
    single =  fromList . pure
    
    -- | Generalization of (++).
    (++) :: l -> l -> l
    xs ++ ys = fromList $ on (++) listL xs ys
    
    -- | Line of n equal elements.
    replicate :: Int -> e -> l
    replicate n = fromListN n . replicate n
    
    -- | Creates line from list elements.
    fromList :: [e] -> l
    fromList es = fromListN (length es) es
    
    -- | May create finite line from infinite list.
    fromListN :: Int -> [e] -> l
    fromListN n = fromList . take n
    
    -- | Same as @listL . reverse@.
    listR :: l -> [e]
    listR =  listL . reverse
    
    -- | Same as 'toList', but formally doesn't require 'Foldable'.
    listL :: l -> [e]
    listL =  reverse . listR
    
    -- | Generalisation of 'fromList'.
    fromFoldable :: (Foldable f) => f e -> l
    fromFoldable =  fromList . toList
    
    -- | Generalization of concat.
    concat :: (Foldable f) => f l -> l
    concat =  foldr (++) Z
    
    -- | Generalization of concatMap.
    concatMap :: (Foldable f) => (a -> l) -> f a -> l
    concatMap f = foldr' ((++) . f) Z
    
    -- | Generalization of intersperse.
    intersperse :: e -> l -> l
    intersperse e = fromList . intersperse e . listL
    
    -- | Generalization of filter.
    filter :: (e -> Bool) -> l -> l
    filter p = fromList . filter p . listL
    
    -- | Generalization of partition.
    partition :: (e -> Bool) -> l -> (l, l)
    partition p es = (filter p es, filter (not . p) es)
    
    -- | Generalization of partition, that select sublines by predicates.
    partitions :: (Foldable f) => f (e -> Bool) -> l -> [l]
    partitions ps es = reverse $ foldl f [es] ps
      where
        f = \ (x : xs) -> (\ (y, ys) -> (ys : y : xs)) . (`partition` x)
    
    -- Compares lines as sorted multisets.
    isSubseqOf :: (Eq e) => l -> l -> Bool
    isSubseqOf xs@(x :> xs') (y :> ys) = x == y && xs' `isSubseqOf` ys || xs `isSubseqOf` ys
    isSubseqOf xs _ = isNull xs
    
    -- | Generalization of reverse.
    reverse :: l -> l
    reverse =  fromList . listR
    
    -- | Generalization of subsequences.
    subsequences :: l -> [l]
    subsequences =  (Z :) . go
      where
        go es = case es of {(x :> xs) -> single x : foldr (\ ys r -> ys : (x :> ys) : r) [] (go xs); _ -> Z}
    
    -- | Same as nubBy ('=='), but may be faster (for bytestring, for example).
    nub :: (Eq e) => l -> l
    nub =  nubBy (==)
    
    -- | Generalization of nubBy.
    nubBy :: (e -> e -> Bool) -> l -> l
    nubBy f = fromList . nubBy f . listL

--------------------------------------------------------------------------------

{- $splitDoc
  Split is class of structures that may be:
  * truncated by length 'take', 'drop', 'keep', 'sans'
  * splitted by 'split', 'divide', 'splits', 'divides', 'parts', 'chunks'
  * truncated by 'each', 'takeWhile', 'dropWhile', 'spanl', 'breakl' (left to
  right), 'takeEnd', 'dropEnd', 'spanr', 'breakr' (right to left)
  
  Also Split contain comparators:
  * by content: 'isPrefixOf', 'isInfixOf' and 'isSuffixOf'
  * by condition: 'prefix' and 'suffix'.
-}

-- | Split - class of splittable data structures.
class (Linear s e) => Split s e | s -> e
  where
    {-# MINIMAL (take|sans), (drop|keep) #-}
    
    -- | @take n es@ takes first @n@ elements of @es@.
    take :: Int -> s -> s
    default take :: (Bordered s i e) => Int -> s -> s
    take n es = sans (sizeOf es - n) es
    
    -- | @drop n es@ drops first @n@ elements of @es@.
    drop :: Int -> s -> s
    default drop :: (Bordered s i e) => Int -> s -> s
    drop n es = keep (sizeOf es - n) es
    
    -- | @keep n es@ takes last @n@ elements of @es@.
    keep :: Int -> s -> s
    default keep :: (Bordered s i e) => Int -> s -> s
    keep n es = drop (sizeOf es - n) es
    
    -- | @sans n es@ drops last @n@ elements of @es@.
    sans :: Int -> s -> s
    default sans :: (Bordered s i e) => Int -> s -> s
    sans n es = take (sizeOf es - n) es
    
    -- | @split n es@ is same to @(take n es, drop n es)@.
    split :: Int -> s -> (s, s)
    split n es = (take n es, drop n es)
    
    -- | Splits line into sequences of given sizes (left to right).
    splits :: (Foldable f) => f Int -> s -> [s]
    splits ns es = reverse $ foldl (\ (r : ds) n -> let (d, r') = split n r in r' : d : ds) [es] ns
    
    -- | @divide n es@ is same to @(sans n es, keep n es)@.
    divide :: Int -> s -> (s, s)
    divide n es = (sans n es, keep n es)
    
    -- | Splits line into sequences of given sizes (left to right).
    divides :: (Foldable f) => f Int -> s -> [s]
    divides ns es = foldr (\ n (r : ds) -> let (r', d) = divide n r in r' : d : ds) [es] ns
    
    -- | Splits structures into parts by given initial indices.
    parts :: (Foldable f) => f Int -> s -> [s]
    parts =  splits . go . toList where go is = zipWith (-) is (0 : is)
    
    -- | Splits structures into chunks of a given size (and the rest).
    chunks :: Int -> s -> [s]
    chunks n es = case split n es of {(x, Z) -> [x]; (x, xs) -> x : chunks n xs}
    
    -- | Returns each nth element of structure. If @n < 0@, returns 'Z'.
    each :: Int -> s -> s
    each n = case n <=> 1 of {LT -> const Z; EQ -> id; GT -> fromList . each n . listL}
    
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
    isInfixOf _   Z = False
    isInfixOf xs ys = xs `isPrefixOf` ys || xs `isInfixOf` tail ys
    
    -- | prefix gives length of init, satisfying preducate.
    prefix :: (e -> Bool) -> s -> Int
    default prefix :: (Foldable t, t e ~~ s) => (e -> Bool) -> s -> Int
    prefix p = foldr (\ e c -> p e ? c + 1 $ 0) 0
    
    -- | suffix gives length of tail, satisfying predicate.
    suffix :: (e -> Bool) -> s -> Int
    default suffix :: (Foldable t, t e ~~ s) => (e -> Bool) -> s -> Int
    suffix p = foldl (\ c e -> p e ? c + 1 $ 0) 0
    
    -- | Takes the longest init by predicate.
    takeWhile :: (e -> Bool) -> s -> s
    takeWhile p es = take (prefix p es) es
    
    -- | Drops the longest init by predicate.
    dropWhile :: (e -> Bool) -> s -> s
    dropWhile p es = drop (prefix p es) es
    
    -- | Takes the longest suffix by predicate.
    takeEnd :: (e -> Bool) -> s -> s
    takeEnd p es = keep (suffix p es) es
    
    -- | Drops the longest prefix by predicate.
    dropEnd :: (e -> Bool) -> s -> s
    dropEnd p es = sans (suffix p es) es
    
    -- | Left-side span.
    spanl :: (e -> Bool) -> s -> (s, s)
    spanl p es = (takeWhile p es, dropWhile p es)
    
    -- | Left-side break.
    breakl :: (e -> Bool) -> s -> (s, s)
    breakl p es = (takeWhile (not . p) es, dropWhile (not . p) es)
    
    -- | Right-side span.
    spanr :: (e -> Bool) -> s -> (s, s)
    spanr p es = (takeEnd p es, dropEnd p es)
    
    -- | Right-side break.
    breakr :: (e -> Bool) -> s -> (s, s)
    breakr p es = (takeEnd (not . p) es, dropEnd (not . p) es)

--------------------------------------------------------------------------------

{- $patternDoc
  SDP.Linear also provides three overloaded patterns: 'Z', (':>') and (':<').
-}

-- | Pattern Z is overloaded empty (or incorrect) line. Same as 'isNull' and 'lzero'.
pattern Z :: (Linear l e) => l
pattern Z <- (isNull -> True)                           where  Z   = lzero

-- | Pattern (:>) is left-size view of line. Same as 'uncons' and 'toHead'.
pattern  (:>)   :: (Linear l e) => e -> l -> l
pattern x :> xs <- ((isNull ?: uncons) -> Just (x, xs)) where (:>) = toHead

-- | Pattern (:<) is right-size view of line. Same as 'unsnoc' and 'toLast'.
pattern   (:<)  :: (Linear l e) => l -> e -> l
pattern xs :< x <- ((isNull ?: unsnoc) -> Just (xs, x)) where (:<) = toLast

--------------------------------------------------------------------------------

-- | Rank (* -> *) 'Linear' structure.
type Linear1 l e = Linear (l e) e

-- | Rank (* -> *) 'Split' structure.
type Split1 s e = Split (s e) e

-- | Rank (* -> *) 'Bordered' structure.
type Bordered1 l i e = Bordered (l e) i e

-- | Rank (* -> * -> *) 'Bordered' structure.
type Bordered2 l i e = Bordered (l i e) i e

--------------------------------------------------------------------------------

{-# COMPLETE Z,  (:)  #-}
{-# COMPLETE [], (:>) #-}
{-# COMPLETE [], (:<) #-}

instance Linear [e] e
  where
    lzero  = []
    toHead = (:)
    isNull = null
    single = pure
    (++)   = (L.++)
    
    fromList     = id
    fromListN    = take
    fromFoldable = toList
    
    toLast = flip (foldr' (:) . pure)
    listR  = L.reverse
    listL  = toList
    nubBy  = L.nubBy
    
    uncons    []    = throw $ PatternMatchFail "in SDP.Linear.(:>)"
    uncons (e : es) = (e, es)
    
    unsnoc   [ ]    = throw $ PatternMatchFail "in SDP.Linear.(:<)"
    unsnoc   [e]    = ([], e)
    unsnoc (e : es) = let (es', e') = unsnoc es in (e : es', e')
    
    filter      = L.filter
    reverse     = L.reverse
    replicate   = L.replicate
    partition   = L.partition
    concatMap   = L.concatMap
    intersperse = L.intersperse
    isSubseqOf  = L.isSubsequenceOf

instance Bordered [e] Int e
  where
    assocs = zip [0 .. ]
    sizeOf = length
    
    lower _  = 0
    upper es = length es - 1

instance Split [e] e
  where
    take  = L.take
    drop  = L.drop
    split = L.splitAt
    
    each n = case n <=> 1 of {LT -> const []; EQ -> id; GT -> go n}
      where
        go _    []    = []
        go i (x : xs) = i == 1 ? x : go n xs $ go (i - 1) xs
    
    isPrefixOf = L.isPrefixOf
    isSuffixOf = L.isSuffixOf
    isInfixOf  = L.isInfixOf
    
    spanl  = L.span
    breakl = L.break

--------------------------------------------------------------------------------

-- | stripPrefix sub line... strips prefix sub of line
stripPrefix :: (Split s e, Bordered s i e, Eq e) => s -> s -> s
stripPrefix sub line = sub `isPrefixOf` line ? drop (sizeOf sub) line $ line

-- | stripSuffix sub line... strips suffix sub of line
stripSuffix :: (Split s e, Bordered s i e, Eq e) => s -> s -> s
stripSuffix sub line = sub `isSuffixOf` line ? take (sizeOf line - sizeOf sub) line $ line

-- | intercalate is generalization of intercalate
intercalate   :: (Foldable f, Linear (f l) l, Linear l e) => l -> f l -> l
intercalate l =  concat . intersperse l

-- | tails is generalization of tails.
tails :: (Linear l e) => l -> [l]
tails Z  = []
tails es = es : tails (tail es)

-- | tails is generalization of inits.
inits :: (Linear l e) => l -> [l]
inits Z  = Z
inits es = es : inits (init es)

-- | sorted is a function that checks for sorting.
sorted :: (Linear l e, Ord e) => l -> Bool
sorted Z  = True
sorted es = and $ zipWith (<=) xs tail' where xs@(_ : tail') = listL es

-- | @ascending line seqs@ checks if the @(start, count) <- seqs@ are sorted.
ascending :: (Split s e, Ord e) => s -> [Int] -> Bool
ascending es = all sorted . (`splits` es)
