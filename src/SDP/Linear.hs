{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, BangPatterns, DefaultSignatures #-}
{-# LANGUAGE TypeFamilies, TypeOperators, ConstraintKinds, Trustworthy #-}

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
  
  -- * Related functions
  intercalate, tails, inits, sorted, ascending,
  
  stripPrefix, stripSuffix, stripPrefix', stripSuffix'
)
where

import Prelude ()
import SDP.SafePrelude

import qualified Data.List as L

import SDP.Index
import SDP.Zip

import GHC.Types

import SDP.Internal

default ()

infix 8 `filter`, `except`

infixr 5 :>, ++
infixl 5 :<

--------------------------------------------------------------------------------

-- | Class of bordered data structures.
class (Index i, Estimate b) => Bordered b i | b -> i
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
    
    {-# INLINE sizeOf #-}
    -- | actual size of structure.
    sizeOf :: b -> Int
    sizeOf =  size . bounds
    
    {-# INLINE indexIn #-}
    -- | checks if an index falls within the boundaries of the structure.
    indexIn :: b -> i -> Bool
    indexIn =  inRange . bounds
    
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

instance (Index i) => Bordered (i, i) i
  where
    bounds = id
    lower  = fst
    upper  = snd
    
    indexIn = inRange
    indices = range
    
    offsetOf = offset
    indexOf  = index
    sizeOf   = size

instance Bordered [e] Int
  where
    sizeOf = length
    
    lower _  = 0
    upper es = length es - 1

--------------------------------------------------------------------------------

{- $linearDoc
  Linear is a class for linear (list-like) data structures which supports
  
  * creation: 'single', 'replicate', 'fromFoldable', 'fromList', 'fromListN'
  * deconstruction: 'head', 'tail', 'init', 'last', 'uncons', 'unsnoc'
  * construction, concatenation: 'toHead', 'toLast', '++', 'concat', 'concatMap'
  * left- and right-side view: 'listL', 'listR'
  * filtering, separation and selection: 'filter', 'except', 'partition',
  'partitions', 'select', 'select'', 'extract', 'extract'', 'selects' and
  'selects''
  
  select and extract are needed to combine filtering and mapping, simplifying
  lambdas and case-expressions in complex cases.
  
  > select' (p ?+ f) == fmap f . filter p
  > select' (p ?- f) == fmap f . except p
  
  > fmap (\ (OneOfCons x y z) -> x + y * z) . filter (\ es -> case es of {(OneOfCons _ _ _) -> True; _ -> False})
  
  is just
  
  > select (\ es -> case es of {(OneOfCons x y z) -> Just (x + y * z); _ -> Nothing})
  
  The code is greatly simplified if there are more than one such constructor or
  any additional conditions.
-}

{-# RULES
  "select/Just"  select  Just = listL;
  "select'/Just" select' Just = id;
#-}

-- | Class of list-like data structures.
class Linear l e | l -> e
  where
    {-# MINIMAL isNull, (listL|listR), (fromList|fromFoldable), (head,tail|uncons), (init,last|unsnoc) #-}
    
    -- | Synonym for 'null', check function for 'Z'.
    isNull :: l -> Bool
    
    -- | Empty line, value for 'Z'.
    lzero :: l
    lzero =  fromList []
    
    -- | Separates line to 'head' and 'tail', deconstructor for ':>' pattern.
    uncons :: l -> (e, l)
    uncons xs = (head xs, tail xs)
    
    -- | Adds element to line as 'head', constructor for ':>' pattern.
    toHead :: e -> l -> l
    toHead =  (++) . single
    
    -- | Returns first element of line. May fail.
    head :: l -> e
    head =  fst . uncons
    
    -- | Returns line, except 'head'. May fail.
    tail :: l -> l
    tail =  snd . uncons
    
    -- | Separates line to 'init' and 'last', deconstructor for ':<' pattern.
    unsnoc :: l -> (l, e)
    unsnoc xs = (init xs, last xs)
    
    -- | Adds element to line as 'last', constructor for ':<' pattern.
    toLast :: l -> e -> l
    toLast es =  (es ++) . single
    
    -- | Returns line, except 'last'. May fail.
    init :: l -> l
    init =  fst . unsnoc
    
    -- | Returns last element of line. May fail.
    last :: l -> e
    last =  snd . unsnoc
    
    -- | Singleton.
    single :: e -> l
    single =  fromList . pure
    
    -- | Concatenation of two lines.
    (++) :: l -> l -> l
    (++) =  fromList ... on (++) listL
    
    -- | @replicate n e@ returns a line of @n@ repetitions of the element @e@.
    replicate :: Int -> e -> l
    replicate n = fromListN n . replicate n
    
    -- | Creates line from list.
    fromList :: [e] -> l
    fromList =  fromFoldable
    
    -- | Create finite line from (possibly infinite) list.
    fromListN :: Int -> [e] -> l
    fromListN =  fromList ... take
    
    -- | Right to left view of line.
    listR :: l -> [e]
    listR =  listL . reverse
    
    -- | Left to right view of line, same to 'toList'.
    listL :: l -> [e]
    listL =  reverse . listR
    
    -- | Generalized 'fromList'.
    fromFoldable :: (Foldable f) => f e -> l
    fromFoldable =  fromList . toList
    
    -- | Generalized concat.
    concat :: (Foldable f) => f l -> l
    concat =  foldr (++) Z
    
    -- | Generalized concatMap.
    concatMap :: (Foldable f) => (a -> l) -> f a -> l
    concatMap f = concat . foldr ((:) . f) []
    
    -- | Generalized intersperse.
    intersperse :: e -> l -> l
    intersperse e = fromList . intersperse e . listL
    
    -- | Generalized filter.
    filter :: (e -> Bool) -> l -> l
    filter p = fromList . filter p . listL
    
    -- | Inverted filter.
    except :: (e -> Bool) -> l -> l
    except p = filter (not . p)
    
    -- | Generalization of partition.
    partition :: (e -> Bool) -> l -> (l, l)
    partition p es = (filter p es, except p es)
    
    -- | Generalization of partition, that select sublines by predicates.
    partitions :: (Foldable f) => f (e -> Bool) -> l -> [l]
    partitions ps es = reverse $ foldl f [es] ps
      where
        f = \ (x : xs) -> (\ (y, ys) -> (ys : y : xs)) . (`partition` x)
    
    -- | @select f es@ is selective map of @es@ elements to new list.
    select :: (e -> Maybe a) -> l -> [a]
    select f = catMaybes . map f . listL
    
    -- | @select' f es@ is selective map of @es@ elements to new line.
    select' :: (t e ~ l, Linear1 t a) => (e -> Maybe a) -> l -> t a
    select' =  fromList ... select
    
    {- |
      @extract f es@ returns a selective map of @es@ elements to new list and
      the remaining elements of the line.
    -}
    extract :: (e -> Maybe a) -> l -> ([a], l)
    extract f = fmap fromList . foldr g ([], []) . listL
      where
        g = \ b -> second (b :) `maybe` (first . (:)) $ f b
    
    {- |
      @extract' f es@ returns a selective map of @es@ elements to new line and
      the remaining elements of the line.
    -}
    extract' :: (t e ~ l, Linear1 t a) => (e -> Maybe a) -> l -> (t a, l)
    extract' =  first fromList ... extract
    
    {- |
      @selects fs es@ sequentially applies the functions from @fs@ to the
      remainder of @es@, returns a list of selections and the remainder of the
      last selection.
    -}
    selects :: (Foldable f) => f (e -> Maybe a) -> l -> ([[a]], l)
    selects fs es = foldl g ([], es) fs
      where
        g = uncurry $ \ as -> first (: as) ... flip extract
    
    {- |
      @selects' fs es@ sequentially applies the functions from @fs@ to the
      remainder of @es@, returns a line of selections and the remainder of the
      last selection.
    -}
    selects' :: (Foldable f, t e ~ l, Linear1 t a) => f (e -> Maybe a) -> l -> ([t a], l)
    selects' =  first (map fromList) ... selects
    
    {- |
      The @isSubseqOf xs ys@ checks if all the elements of the @xs@ occur,
      in order, in the @ys@. The elements don't have to occur consecutively.
    -}
    isSubseqOf :: (Eq e) => l -> l -> Bool
    isSubseqOf =  isSubseqOf `on` listL
    
    -- | Generalized reverse.
    reverse :: l -> l
    reverse =  fromList . listR
    
    -- | Generalized subsequences.
    subsequences :: l -> [l]
    subsequences =  (Z :) . go
      where
        go es = case es of {(x :> xs) -> single x : foldr (\ ys r -> ys : (x :> ys) : r) [] (go xs); _ -> Z}
    
    {- |
      @iterate n f x@ returns an list of repeated applications of @f@ to @x@.
      
      Note that @iterate@ returns finite list, instead "Prelude" prototype.
    -}
    iterate :: Int -> (e -> e) -> e -> l
    iterate n = fromListN n ... iterate n
    
    -- | Same as @nubBy ('==')@.
    nub :: (Eq e) => l -> l
    nub =  nubBy (==)
    
    -- | Generalization of nubBy.
    nubBy :: Equal e -> l -> l
    nubBy f = fromList . nubBy f . listL

--------------------------------------------------------------------------------

{- $splitDoc
  Split is class of structures that may be splitted by
  
  * length: 'take', 'drop', 'split', 'splits', 'keep', 'sans', 'divide',
  'divides', 'parts', 'chunks'
  * predicate: 'takeWhile', 'dropWhile', 'spanl', 'breakl' (left to right),
  'takeEnd', 'dropEnd', 'spanr', 'breakr' (right to left)
  * selector: 'selectWhile', 'selectEnd', 'extractWhile', 'extractEnd',
  'selectWhile'', 'selectEnd'', 'extractWhile'', 'extractEnd''
  
  Also Split contain useful comparators: 'isPrefixOf', 'isInfixOf' and
  'isSuffixOf'.
-}

-- | Split - class of splittable data structures.
class (Linear s e) => Split s e | s -> e
  where
    {-# MINIMAL (take|sans), (drop|keep) #-}
    
    -- | @take n es@ takes first @n@ elements of @es@.
    take :: Int -> s -> s
    default take :: (Bordered s i) => Int -> s -> s
    take n es = sans (sizeOf es - n) es
    
    -- | @drop n es@ drops first @n@ elements of @es@.
    drop :: Int -> s -> s
    default drop :: (Bordered s i) => Int -> s -> s
    drop n es = keep (sizeOf es - n) es
    
    -- | @keep n es@ takes last @n@ elements of @es@.
    keep :: Int -> s -> s
    default keep :: (Bordered s i) => Int -> s -> s
    keep n es = drop (sizeOf es - n) es
    
    -- | @sans n es@ drops last @n@ elements of @es@.
    sans :: Int -> s -> s
    default sans :: (Bordered s i) => Int -> s -> s
    sans n es = take (sizeOf es - n) es
    
    {- |
      @save n es@ takes first @n@ elements of @es@ if @n > 0@ and last @-n@
      elements otherwise.
    -}
    save :: Int -> s -> s
    save n = n > 0 ? take n $ keep (-n)
    
    {- |
      @skip n es@ drops first @n@ elements of @es@ if @n > 0@ and last @-n@
      elements otherwise.
    -}
    skip :: Int -> s -> s
    skip n = n > 0 ? drop n $ sans (-n)
    
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
    
    {- |
      Splits structures into chunks of size @n@ and the rest.
      
      > chunks x [] = [] -- forall x
      > chunks 0 es = [] -- forall es
      
      > chunks 3 [1 .. 10] == [[1,2,3],[4,5,6],[7,8,9],[10]]
    -}
    chunks :: Int -> s -> [s]
    chunks _  Z = []
    chunks n es = n < 1 ? [] $ let (x, xs) = split n es in x : chunks n xs
    
    {- |
      Split line by first (left) separation element. If there is no such
      element, returns (es, Z).
    -}
    splitBy :: (e -> Bool) -> s -> (s, s)
    splitBy f = bimap fromList fromList . splitBy f . listL
    
    -- | Splits line by separation elements.
    splitsBy :: (e -> Bool) -> s -> [s]
    splitsBy e = map fromList . splitsBy e . listL
    
    -- | @splitsOn sub line@ splits @line@ by @sub@.
    default splitsOn :: (Eq e, Bordered s i) => s -> s -> [s]
    splitsOn :: (Eq e) => s -> s -> [s]
    splitsOn sub line = drop (sizeOf sub) <$> parts (infixes sub line) line
    
    {- |
      @replaceBy sub new line@ replace every non-overlapping occurrence of @sub@
      in @line@ with @new@.
    -}
    replaceBy :: (Eq e) => s -> s -> s -> s
    replaceBy sub new = intercalate new . splitsOn sub
    
    {- |
      Removes every non-overlapping occurrence of @sub@ with 'Z'.
      
      > removeAll = concat ... splitsOn
      > (`replaceBy` Z) = removeAll
    -}
    removeAll :: (Eq e) => s -> s -> s
    removeAll =  concat ... splitsOn
    
    {- |
      @combo f es@ returns the length of the @es@ subsequence (left to tight)
      whose elements are in order @f@.
      
      > combo (<) [] == 0
      > combo (<) [1] == 1
      > combo (<) [7, 4, 12] == 1
      > combo (<) [1, 7, 3, 12] == 2
    -}
    default combo :: (Bordered s i) => Equal e -> s -> Int
    combo :: (e -> e -> Bool) -> s -> Int
    combo f = combo f . listL
    
    -- | @complement n e es@ shrinks @es@ to @n@ elements, pad with @e@ if shorter.
    complement :: Int -> e -> s -> s
    complement n e = take n . (++ replicate n e)
    
    {- |
      @each n es@ returns each nth element of structure.
      If @n == 1@, returns @es@.
      If @n < 1@, returns 'Z'.
    -}
    each :: Int -> s -> s
    each n = fromList . each n . listL
    
    {- |
      @each' o n es@ returns each nth element of structure, beginning from o.
      
      > each' o n = each n . drop o
      
      > each' 0 2 [1 .. 20] == [2, 4 .. 20]
      > each' 1 2 [1 .. 20] == [3, 5 .. 19]
    -}
    each' :: Int -> Int -> s -> s
    each' o n = each n . drop o
    
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
    
    {- |
      @infixes inf es@ returns a list of @inf@ positions in @es@, without
      intersections.
      
      > "" `infixes` es = []
      > "abba" `infixes` "baababba" == [4]
      > "abab" `infixes` "baababab" == [2]
      > "aaaa" `infixes` "aaaaaaaa" == [0, 4]
    -}
    infixes :: (Eq e) => s -> s -> [Int]
    infixes =  on infixes listL
    
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
    spanr p es = (dropEnd p es, takeEnd p es)
    
    -- | Right-side break.
    breakr :: (e -> Bool) -> s -> (s, s)
    breakr p es = (dropEnd (not . p) es, takeEnd (not . p) es)
    
    {- |
      @selectWhile f es@ selects results of applying @f@ to @es@ (left to right)
      untill first fail.
    -}
    selectWhile :: (e -> Maybe a) -> s -> [a]
    selectWhile f = selectWhile f . listL
    
    {- |
      @selectEnd f es@ selects results of applying @f@ to @es@ (right to left)
      untill first fail.
    -}
    selectEnd :: (e -> Maybe a) -> s -> [a]
    selectEnd f = selectEnd f . listL
    
    {- |
      @extractWhile f es@ selects results of applying @f@ to @es@ (left to
      right) untill first fail. Returns selected results and rest of line.
    -}
    extractWhile :: (e -> Maybe a) -> s -> ([a], s)
    extractWhile f es = let as = selectWhile f es in (as, length as `drop` es)
    
    {- |
      @extractEnd f es@ selects results of applying @f@ to @es@ (right to left)
      untill first fail. Returns rest of line and selected results.
    -}
    extractEnd :: (e -> Maybe a) -> s -> (s, [a])
    extractEnd f es = let as = selectEnd f es in (length as `sans` es, as)
    
    -- | @selectWhile'@ is 'selectWhile' version for generalized structures.
    selectWhile' :: (t e ~ l, Split1 t a) => (e -> Maybe a) -> s -> t a
    selectWhile' =  fromList ... selectWhile
    
    -- | @selectEnd'@ is 'selectEnd' version for generalized structures.
    selectEnd' :: (t e ~ l, Split1 t a) => (e -> Maybe a) -> s -> t a
    selectEnd' =  fromList ... selectEnd
    
    -- | @extractWhile'@ is 'extractWhile' version for generalized structures.
    extractWhile' :: (t e ~ l, Split1 t a) => (e -> Maybe a) -> s -> (t a, s)
    extractWhile' =  first fromList ... extractWhile
    
    -- | @extractEnd'@ is 'extractEnd' version for generalized structures.
    extractEnd' :: (t e ~ l, Split1 t a) => (e -> Maybe a) -> s -> (s, t a)
    extractEnd' =  second fromList ... extractEnd

--------------------------------------------------------------------------------

{- $patternDoc
  SDP.Linear also provides three overloaded patterns: 'Z', (':>') and (':<').
-}

-- | Pattern Z is overloaded empty (or incorrect) line. Same as 'isNull' and 'lzero'.
pattern Z :: (Linear l e) => l
pattern Z <- (isNull -> True)                           where  Z   = lzero

-- | Pattern (:>) is left-size view of line. Same as 'uncons' and 'toHead'.
pattern  (:>)   :: (Linear l e) => e -> l -> l
pattern x :> xs <- ((isNull ?- uncons) -> Just (x, xs)) where (:>) = toHead

-- | Pattern (:<) is right-size view of line. Same as 'unsnoc' and 'toLast'.
pattern   (:<)  :: (Linear l e) => l -> e -> l
pattern xs :< x <- ((isNull ?- unsnoc) -> Just (xs, x)) where (:<) = toLast

--------------------------------------------------------------------------------

-- | Rank (* -> *) 'Linear' structure.
type Linear1 l e = Linear (l e) e

-- | Rank (* -> *) 'Split' structure.
type Split1 s e = Split (s e) e

-- | Rank (* -> *) 'Bordered' structure.
type Bordered1 l i e = Bordered (l e) i

-- | Rank (* -> * -> *) 'Bordered' structure.
type Bordered2 l i e = Bordered (l i e) i

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
    
    iterate n f e = n < 1 ? [] $ e : iterate (n - 1) f (f e)

instance Split [e] e
  where
    take  = L.take
    drop  = L.drop
    split = L.splitAt
    
    each n = case n <=> 1 of {LT -> const []; EQ -> id; GT -> go n}
      where
        go i (x : xs) = i == 1 ? x : go n xs $ go (i - 1) xs
        go _ _ = []
    
    infixes  Z  = const []
    infixes sub = go 0
      where
        go _ [] = []
        go i es = sub `isPrefixOf` es ? i : go (i + n) (drop n es) $ go (i + 1) (tail es)
        
        n = sizeOf sub
    
    combo _    []    = 0
    combo f (x : xs) = go 1 x xs
      where
        go !n e1 (e2 : es) = e1 `f` e2 ? go (n + 1) e2 es $ n
        go !n _ _ = n
    
    splitBy f es = let (as, bs) = breakl f es in isNull bs ? (es, []) $ (as, tail bs)
    
    splitsBy f es = dropWhile f <$> L.findIndices f es `parts` es
    
    isPrefixOf = L.isPrefixOf
    isSuffixOf = L.isSuffixOf
    isInfixOf  = L.isInfixOf
    
    spanl  = L.span
    breakl = L.break
    
    selectWhile _    []    = []
    selectWhile f (x : xs) = case f x of {(Just e) -> e : select f xs; _ -> []}
    
    selectEnd f = reverse . selectWhile f . reverse

--------------------------------------------------------------------------------

-- | @stripPrefix sub line@ strips prefix @sub@ of @line@ (if any).
stripPrefix :: (Split s e, Bordered s i, Eq e) => s -> s -> s
stripPrefix sub line = sub `isPrefixOf` line ? drop (sizeOf sub) line $ line

-- | @stripSuffix sub line@ strips suffix @sub@ of @line@ (if any).
stripSuffix :: (Split s e, Bordered s i, Eq e) => s -> s -> s
stripSuffix sub line = sub `isSuffixOf` line ? sans (sizeOf sub) line $ line

-- | @stripPrefix' sub line@ strips prefix @sub@ of @line@ or returns 'Nothing'.
stripPrefix' :: (Split s e, Bordered s i, Eq e) => s -> s -> Maybe s
stripPrefix' sub = isPrefixOf sub ?+ drop (sizeOf sub)

-- | @stripSuffix sub line@ strips suffix @sub@ of @line@ or returns 'Nothing'.
stripSuffix' :: (Split s e, Bordered s i, Eq e) => s -> s -> Maybe s
stripSuffix' sub = isSuffixOf sub ?+ sans (sizeOf sub)

-- | intercalate is generalization of intercalate
intercalate :: (Foldable f, Linear (f l) l, Linear l e) => l -> f l -> l
intercalate =  concat ... intersperse

-- | @tails es@ returns sequence of @es@ tails.
tails :: (Linear l e) => l -> [l]
tails Z  = [Z]
tails es = es : tails (tail es)

-- | tails is generalization of inits.
inits :: (Linear l e) => l -> [l]
inits Z  = [Z]
inits es = es : inits (init es)

-- | sorted is a function that checks for sorting.
sorted :: (Split s e, Bordered s i, Ord e) => s -> Bool
sorted es = combo (<=) es == sizeOf es

{- |
  @ascending es lens@ checks if the subsequences of @es@ of lengths @lens@ is
  sorted.
-}
ascending :: (Split s e, Bordered s i, Ord e) => s -> [Int] -> Bool
ascending =  all sorted ... flip splits




