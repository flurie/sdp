{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, BangPatterns, DefaultSignatures #-}
{-# LANGUAGE Trustworthy, CPP, TypeFamilies, ConstraintKinds #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Linear
    Copyright   :  (c) Andrey Mulik 2019-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Linear" is a module that provides several convenient interfaces for
    working with various linear data structures.
-}
module SDP.Linear
(
  -- * Exports
  module SDP.Nullable,
  module SDP.Index,
  module SDP.Sort,
  module SDP.Zip,
  
  -- * Bordered class
  Bordered (..), Bordered1, Bordered2,
  
#if __GLASGOW_HASKELL__ >= 806
  Bordered', Bordered'',
#endif
  
  -- * Linear class
  Linear (..), Linear1, Linear2, pattern (:>), pattern (:<),
  
#if __GLASGOW_HASKELL__ >= 806
  Linear', Linear'',
#endif
  
  -- * Related functions
  stripPrefix, stripSuffix, stripPrefix', stripSuffix',
  intercalate, tails, inits, ascending, save, skip, parts
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Forceable
import SDP.Nullable
import SDP.Index
import SDP.Sort
import SDP.Zip

import qualified Data.List as L

import Control.Exception.SDP

default ()

infix  8 `filter`, `except`
infixr 5 :>, ++
infixl 5 :<
infixl 9 !^

--------------------------------------------------------------------------------

{- |
  'Bordered' is class of data structures with constant bounds of 'Index' type.
  
  'Bordered' instances must follow some rules:
  
  @
    bounds es === (lower es, upper es)
    sizeOf es === product (sizesOf es)
    
    -- For 'Foldable' instances
    sizeOf  === length
    null xs === sizeOf xs == 0
    
    -- For 'Nullable' instances
    sizeOf  Z === 0
    isNull xs === sizeOf xs === 0
  @
-}
class (Index i, Estimate b) => Bordered b i | b -> i
  where
    {-# MINIMAL (bounds|(lower, upper)), rebound #-}
    
    {-# INLINE bounds #-}
    {- |
      Returns the exact 'lower' and 'upper' bounds of given structure. If the
      structure doesn't have explicitly defined boundaries (list, for example),
      use the @'defaultBounds' . 'sizeOf'@.
    -}
    bounds :: b -> (i, i)
    bounds es = (lower es, upper es)
    
    {-# INLINE lower #-}
    -- | Returns lower bound of structure
    lower :: b -> i
    lower =  fst . bounds
    
    {-# INLINE upper #-}
    -- | Returns upper bound of structure
    upper :: b -> i
    upper =  snd . bounds
    
    {-# INLINE sizeOf #-}
    -- | Returns actual size of structure.
    sizeOf :: b -> Int
    sizeOf =  size . bounds
    
    -- | Returns actual sizes of structure.
    sizesOf :: b -> [Int]
    sizesOf =  sizes . bounds
    
    {-# INLINE indexIn #-}
    -- | Checks if an index falls within the boundaries of the structure.
    indexIn :: b -> i -> Bool
    indexIn =  inRange . bounds
    
    {-# INLINE indices #-}
    -- | Returns index range list.
    indices :: b -> [i]
    indices =  range . bounds
    
    {-# INLINE indexOf #-}
    -- | Returns index by offset in structure.
    indexOf :: b -> Int -> i
    indexOf =  index . bounds
    
    {-# INLINE offsetOf #-}
    -- | Returns index offset in structure bounds.
    offsetOf :: b -> i -> Int
    offsetOf =  offset . bounds
    
    {- |
      @'rebound' es bnds@ returns (old) structure with bounds @bnds@, if
      possible.
      
      * 'rebound' doesn't change the existing structure and shouldn't create a
      new one
      * 'rebound' don't selects elements from its subrange and keeps the
      original element order, so @'bounds' es@ don't affect the 'rebound'
      result
      * if @'size' bnds > sizeOf es@, upper bound will be replaced by
      @'index' bnds ('sizeOf' es)@
      * if the specified 'bounds' cannot be set, 'rebound' uses the
      @'defaultBounds' ('size' bnds)@ value.
      
      @
        sizeOf (rebound es bnds) === min (sizeOf es) (size bnds)
        bounds (rebound es bnds) =/= bnds -- generally speaking
      @
      
      > rebound [1 .. 10] (5, 8) :: [Int]
      [1,2,3,4] -- bounds: (0, 9) => (0, 3)
      > rebound (fromList [1 .. 10]) (5, 8) :: SArray# Int
      [1,2,3,4] -- bounds: (0, 9) => (0, 3)
      > rebound (fromList [1 .. 10]) (5, 8) :: Array Int Int
      array (5,8) [(5,1),(6,2),(7,3),(8,4)] -- bounds: (0, 9) => (5, 8)
    -}
    rebound :: b -> (i, i) -> b

--------------------------------------------------------------------------------

{-# RULES
  "select/Just"  select  Just = listL;
  "select'/Just" select' Just = id;
#-}

{- |
  'Linear' is one of the main SDP classes, a class of linear data structures.
  
  A structure of type @l@ must be able to contain an arbitrary ordered dataset
  of type @e@, with arbitrary length. The ordering implies that each @e@ element
  in @l@ can be associated with a non-negative number.
  
  Structures that cannot store, for example, a zero number of elements (e.g.
  'Data.List.NonEmpty.NonEmpty'), or have some static size, aren't 'Linear'.
  
  Of course, the @l@ structure can also have a greater dimension, for example,
  a matrix (of scalar elements) can be 'Linear' if some order is defined for its
  elements. The standard SDP structures use the 'Index' class to define this
  order. You may also want to think of the table as a linear structure
  containing rows/columns, or use a different ordering method such as Gray code.
  
  'Linear' structures must follow some rules:
  
  @
    mempty  === lzero
    mappend === (<>) === (++)
    mconcat === fold === concat
    
    isNull  (single e)   === True
    isNull (toHead x xs) === True
    isNull (toLast xs x) === True
    
    reverse . reverse === id
    listL === reverse . listR === listR . reverse
    listR === reverse . listL === listL . reverse
    
    fromList === fromFoldable
    fromFoldable === foldr toHead Z
    
    -- For 'Foldable' instances:
    length (toHead x xs) === length xs + 1
    length (toLast xs x) === length xs + 1
    
    length (replicate n e) === n
    
    isNull === length == 0
    length === length . listL === length . listR
    toList === listL === reverse . listR === listR . reverse
    
    o_foldr  === foldr
    o_foldl  === foldl
    o_foldr1 === foldr1
    o_foldl1 === foldl1
    
    ofoldr f base xs === foldr (uncurry f) base (assocs xs)
    filter p = fromList . foldr (\ x xs -> p x ? x : xs $ xs) []
    select f = foldr (\ x es -> case f x of {Just e -> e : es; _ -> es}) []
  @
-}
class (Nullable l, Forceable l, Monoid l) => Linear l e | l -> e
  where
    {-# MINIMAL toHead, toLast, (take|sans), (drop|keep),
        (uncons'|(head,tail)|uncons), (unsnoc'|(init,last)|unsnoc) #-}
    
    {- Core: basis for other definitions. -}
    
    -- | Prepends element to line, constructor for ':>' pattern.
    toHead :: e -> l -> l
    toHead e es = single e ++ es
    
    -- | Appends element to line, constructor for ':<' pattern.
    toLast :: l -> e -> l
    toLast es e = es ++ single e
    
    -- | Same as @'isNull' '?-' 'uncons'@
    uncons' :: l -> Maybe (e, l)
    uncons' =  isNull ?- \ xs -> (head xs, tail xs)
    
    -- | Same as @'isNull' '?-' 'unsnoc'@
    unsnoc' :: l -> Maybe (l, e)
    unsnoc' =  isNull ?- \ xs -> (init xs, last xs)
    
    -- | Separates line to 'head' and 'tail', deconstructor for ':>' pattern.
    uncons :: l -> (e, l)
    uncons xs = case uncons' xs of {Just res -> res; _ -> emptyEx "(:>)"}
    
    -- | Separates line to 'init' and 'last', deconstructor for ':<' pattern.
    unsnoc :: l -> (l, e)
    unsnoc xs = case unsnoc' xs of {Just res -> res; _ -> emptyEx "(:<)"}
    
    -- | Returns first element of line, may fail.
    head :: l -> e
    head =  fst . uncons
    
    -- | Returns line except first, may fail.
    tail :: l -> l
    tail =  snd . uncons
    
    -- | Returns line except 'last' element, may fail.
    init :: l -> l
    init =  fst . unsnoc
    
    -- | Returns last element, may fail.
    last :: l -> e
    last =  snd . unsnoc
    
    -- | Left to right view of line, same to 'toList'.
    listL :: l -> [e]
    listL =  L.unfoldr uncons'
    
    -- | Right to left view of line.
    listR :: l -> [e]
    listR =  L.reverse . listL
    
    -- | 'ofoldr' is right fold with offset.
    ofoldr :: (Int -> e -> b -> b) -> b -> l -> b
    ofoldr f base = ofoldr f base . listL
    
    -- | 'ofoldl' is left fold with offset.
    ofoldl :: (Int -> b -> e -> b) -> b -> l -> b
    ofoldl f base = ofoldl f base . listL
    
    {- Perf: most sensitive for performance -}
    
    -- | Just singleton.
    single :: e -> l
    single =  (`toHead` Z)
    
    -- | Creates line from list.
    fromList :: [e] -> l
    fromList =  fromFoldable
    
    -- | Create finite line from (possibly infinite) list.
    fromListN :: Int -> [e] -> l
    fromListN =  fromList ... L.take
    
    -- | Generalized 'fromList'.
    fromFoldable :: (Foldable f) => f e -> l
    fromFoldable =  foldr toHead Z
    
    -- | @replicate n e@ returns a line of @n@ repetitions of the element @e@.
    replicate :: Int -> e -> l
    replicate n = fromListN n . replicate n
    
    -- | Concatenation of two lines.
    (++) :: l -> l -> l
    (++) =  (<>)
    
    {- |
      Returns the element of a sequence by offset, may be completely unsafe.
      This is an optimistic read function and shouldn't perform checks for
      efficiency reasons.
      
      If you need safety, use (!) or (!?). The generalization of this function
      by index type (.!).
      
      > es !^ i = listL es !! i
    -}
    (!^) :: l -> Int -> e
    (!^) =  (L.!!) . listL
    
    {- |
      @write es n e@ writes value @e@ in position @n@ (offset), returns new
      structure. If @n@ is out of range, returns equal structure (@es@ or copy).
    -}
    write :: l -> Int -> e -> l
    write es = fromList ... write (listL es)
    
    {- Custom -}
    
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
    filter p = fromList . o_foldr (\ x xs -> p x ? x : xs $ xs) []
    
    -- | Inverted filter.
    except :: (e -> Bool) -> l -> l
    except p = filter (not . p)
    
    -- | Generalization of partition.
    partition :: (e -> Bool) -> l -> (l, l)
    partition p es = (filter p es, except p es)
    
    -- | Generalization of partition, that select sublines by predicates.
    partitions :: (Foldable f) => f (e -> Bool) -> l -> [l]
    partitions ps es =
      let f = \ (x : xs) -> (\ (y, ys) -> (ys : y : xs)) . (`partition` x)
      in  L.reverse $ foldl f [es] ps
    
    -- | @select f es@ is selective map of @es@ elements to new list.
    select :: (e -> Maybe a) -> l -> [a]
    select f = o_foldr (\ x es -> case f x of {Just e -> e : es; _ -> es}) []
    
    -- | @select' f es@ is selective map of @es@ elements to new line.
    select' :: (t e ~ l, Linear1 t a) => (e -> Maybe a) -> l -> t a
    select' =  fromList ... select
    
    {- |
      @extract f es@ returns a selective map of @es@ elements to new list and
      the remaining elements of the line.
    -}
    extract :: (e -> Maybe a) -> l -> ([a], l)
    extract f =
      let g = \ b -> second (b :) `maybe` (first . (:)) $ f b
      in  fmap fromList . o_foldr' g ([], [])
    
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
    selects fs es =
      let g = \ as -> first (: as) ... flip extract
      in  foldl (uncurry g) ([], es) fs
    
    {- |
      @selects' fs es@ sequentially applies the functions from @fs@ to the
      remainder of @es@, returns a line of selections and the remainder of the
      last selection.
    -}
    selects' :: (Foldable f, t e ~ l, Linear1 t a) => f (e -> Maybe a) -> l -> ([t a], l)
    selects' =  first (map fromList) ... selects
    
    -- | Generalized reverse.
    reverse :: l -> l
    reverse =  fromList . listR
    
    {- |
      @iterate n f x@ returns sequence of @n@ applications of @f@ to @x@.
      
      Note that @iterate@ returns finite sequence, instead "Prelude" prototype.
    -}
    iterate :: Int -> (e -> e) -> e -> l
    iterate n = fromListN n ... iterate n
    
    -- | Same as @nubBy ('==')@.
    nub :: (Eq e) => l -> l
    nub =  nubBy (==)
    
    -- | Generalization of nubBy.
    nubBy :: Equal e -> l -> l
    nubBy f = fromList . nubBy f . listL
    
    {- Additional folds. -}
    
    -- | 'o_foldr' is just 'foldr' in 'Linear' context.
    o_foldr :: (e -> b -> b) -> b -> l -> b
    o_foldr =  ofoldr . const
    
    -- | 'o_foldl' is just 'foldl' in 'Linear' context.
    o_foldl :: (b -> e -> b) -> b -> l -> b
    o_foldl =  ofoldl . const
    
    -- | 'ofoldr'' is strict version of 'ofoldr'.
    ofoldr' :: (Int -> e -> b -> b) -> b -> l -> b
    ofoldr' f = ofoldr (\ !i e !b -> f i e b)
    
    -- | 'ofoldl'' is strict version of 'ofoldl'.
    ofoldl' :: (Int -> b -> e -> b) -> b -> l -> b
    ofoldl' f = ofoldl (\ !i !b e -> f i b e)
    
    -- | 'o_foldr'' is just 'foldr'' in 'Linear' context.
    o_foldr' :: (e -> b -> b) -> b -> l -> b
    o_foldr' =  ofoldr' . const
    
    -- | 'o_foldl'' is just 'foldl'' in 'Linear' context.
    o_foldl' :: (b -> e -> b) -> b -> l -> b
    o_foldl' =  ofoldl' . const
    
    -- | 'o_foldr1' is just 'Data.Foldable.foldr1' in 'Linear' context.
    o_foldr1 :: (e -> e -> e) -> l -> e
    o_foldr1 f = \ (es :< e) -> o_foldr f e es
    
    -- | 'o_foldl1' is just 'Data.Foldable.foldl1' in 'Linear' context.
    o_foldl1 :: (e -> e -> e) -> l -> e
    o_foldl1 f = \ (e :> es) -> o_foldl f e es
    
    -- | 'o_foldr1'' is just strict 'Data.Foldable.foldr1' in 'Linear' context.
    o_foldr1' :: (e -> e -> e) -> l -> e
    o_foldr1' f = \ (es :< e) -> o_foldr' f e es
    
    -- | 'o_foldl1'' is just 'Data.Foldable.foldl1'' in 'Linear' context.
    o_foldl1' :: (e -> e -> e) -> l -> e
    o_foldl1' f = \ (e :> es) -> o_foldl' f e es
    
    {- Don't touch. -}
    
    {- |
      The @isSubseqOf xs ys@ checks if all the elements of the @xs@ occur,
      in order, in the @ys@. The elements don't have to occur consecutively.
    -}
    isSubseqOf :: (Eq e) => l -> l -> Bool
    isSubseqOf =  isSubseqOf `on` listL
    
    -- | Generalized 'subsequences'.
    subsequences :: l -> [l]
    subsequences =  (Z :) . go
      where
        go (x :> xs) = single x : foldr (\ ys r -> ys : (x :> ys) : r) [] (go xs)
        go     _     = Z
    
    {- New functions. -}
    
    {- |
      @since 0.2.1
      
      @'before' es i e@ insert @e@ to @es@ before element with offset @i@. If
      @i@ goes beyond the lower or upper bounds, @e@ is prepended or appended to
      @es@ respectively.
      
      > before [0 .. 5] (-1) 7 == [7,0,1,2,3,4,5]
      > before [0 .. 5]   0  7 == [7,0,1,2,3,4,5]
      > before [0 .. 5]   3  7 == [0,1,2,7,3,4,5]
      > before [0 .. 5]   5  7 == [0,1,2,3,4,7,5]
      > before [0 .. 5]   6  7 == [0,1,2,3,4,5,7]
      > before [0 .. 5]  19  7 == [0,1,2,3,4,5,7]
    -}
    before :: l -> Int -> e -> l
    before es = fromList ... before (listL es)
    
    {- |
      @since 0.2.1
      
      @'after' es i e@ insert @e@ to @es@ after element with offset @i@.
      
      > after es i e == before es (i + 1) e
    -}
    after :: l -> Int -> e -> l
    after es i e = before es (i + 1) e
    
    {- |
      @since 0.2.1
      
      @'remove' es i@ delete element with offset @i@ from @es@.
      
      > remove (-1) [0 .. 5] == [0 .. 5]
      > remove   6  [0 .. 5] == [0 .. 5]
      > remove   0  [0 .. 5] == [1,2,3,4,5]
      > remove   3  [0 .. 5] == [0,1,2,4,5]
      > remove   5  [0 .. 5] == [0,1,2,3,4]
    -}
    remove :: Int -> l -> l
    remove n = fromList . remove n . listL
    
    {- |
      @since 0.3
      
      @unfoldr gen x@ creates new structure using producing function @gen@ and
      initial value @x@. @gen@ takes the element and returns 'Nothing' if it is
      done producing the linear structure or returns @Just (e, a)@, in which
      case, @e@ is a prepended to the structure and @a@ is used as the next
      element in a recursive call.
    -}
    unfoldr :: (b -> Maybe (e, b)) -> b -> l
    unfoldr =  fromList ... L.unfoldr
    
    {- From Split -}
    
    -- | @take n es@ takes first @n@ elements of @es@.
    take :: Int -> l -> l
    take n es = sans (sizeOf es - n) es
    default take :: (Bordered l i) => Int -> l -> l
    
    -- | @drop n es@ drops first @n@ elements of @es@.
    drop :: Int -> l -> l
    drop n es = keep (sizeOf es - n) es
    default drop :: (Bordered l i) => Int -> l -> l
    
    -- | @keep n es@ takes last @n@ elements of @es@.
    keep :: Int -> l -> l
    keep n es = drop (sizeOf es - n) es
    default keep :: (Bordered l i) => Int -> l -> l
    
    -- | @sans n es@ drops last @n@ elements of @es@.
    sans :: Int -> l -> l
    sans n es = take (sizeOf es - n) es
    default sans :: (Bordered l i) => Int -> l -> l
    
    -- | @split n es@ is same to @(take n es, drop n es)@.
    split :: Int -> l -> (l, l)
    split n es = (take n es, drop n es)
    
    -- | @divide n es@ is same to @(sans n es, keep n es)@.
    divide :: Int -> l -> (l, l)
    divide n es = (sans n es, keep n es)
    
    {- |
      Splits line into sequences of given sizes (left to right).
      
      > splits [5, 3, 12] ['a'..'z'] = ["abcde","fgh","ijklmnopqrst","uvwxyz"]
    -}
    splits :: (Foldable f) => f Int -> l -> [l]
    splits ns es =
      let f = \ (r : ds) n -> let (d, r') = split n r in r' : d : ds
      in  reverse $ foldl f [es] ns
    
    {- |
      Splits line into sequences of given sizes (right to left).
      
      > divides [5,3,12] ['a'..'z'] == ["abcdef","ghijk","lmn","opqrstuvwxyz"]
    -}
    divides :: (Foldable f) => f Int -> l -> [l]
    divides ns es =
      let f = \ n (r : ds) -> let (r', d) = divide n r in r' : d : ds
      in  foldr f [es] ns
    
    {- |
      Splits structures into chunks of size @n@ and the rest.
      
      > chunks x [] = [] -- forall x
      > chunks 0 es = [] -- forall es
      
      > chunks 3 [1 .. 10] == [[1,2,3],[4,5,6],[7,8,9],[10]]
    -}
    chunks :: Int -> l -> [l]
    chunks n es = isNull es || n < 1 ? [] $ let (x, xs) = split n es in x : chunks n xs
    
    {- |
      Split line by first (left) separation element. If there is no such
      element, @splitBy es = (es, Z)@.
      
      > splitBy (== '.') "foo" == ("foo","")
      > splitBy (== '.') "foo." == ("foo","")
      > splitBy (== '.') ".foo" == ("","foo")
      > splitBy (== '.') "foo.bar" == ("foo","bar")
      > splitBy (== '.') "foo.bar.baz" == ("foo","bar.baz")
    -}
    splitBy :: (e -> Bool) -> l -> (l, l)
    splitBy f = bimap fromList fromList . splitBy f . listL
    
    {- |
      Split line by last (right) separation element. If there is no such
      element, @divide es = (Z, es)@.
      
      > divideBy (== '.') "foo" == ("","foo")
      > divideBy (== '.') ".foo" == ("","foo")
      > divideBy (== '.') "foo." == ("foo","")
      > divideBy (== '.') "foo.bar" == ("foo","bar")
      > divideBy (== '.') "foo.bar.baz" == ("foo.bar","baz")
    -}
    divideBy :: (e -> Bool) -> l -> (l, l)
    divideBy f = bimap fromList fromList . divideBy f . listL
    
    -- | Splits line by separation elements.
    splitsBy :: (e -> Bool) -> l -> [l]
    splitsBy e = map fromList . splitsBy e . listL
    
    {- |
      @splitsOn sub line@ splits @line@ by @sub@.
      
      > splitsOn "fo" "foobar bazfoobar1" == ["","obar baz","obar1"]
    -}
    splitsOn :: (Eq e) => l -> l -> [l]
#if __GLASGOW_HASKELL__ >= 820
    default splitsOn :: (Eq e, Bordered l i) => l -> l -> [l]
    splitsOn sub line = drop (sizeOf sub) <$> parts (infixes sub line) line
    -- ghc-8.0.1 has bug in default signatures, so this can be used with it
#else
    {-
      Not tested, but should be significantly slower than the definitions below.
      If you plan to support ghc-8.0.1, override splitsOn in all your instances.
    -}
    splitsOn sub line = fromList <$> splitsOn (listL sub) (listL line)
#endif
    
    {- |
      @replaceBy sub new line@ replace every non-overlapping occurrence of @sub@
      in @line@ with @new@.
      
      > replaceBy "foo" "bar" "foobafoorbaz" == "barbabarrbaz"
    -}
    replaceBy :: (Eq e) => l -> l -> l -> l
    replaceBy sub new = intercalate new . splitsOn sub
    
    {- |
      Removes every non-overlapping occurrence of @sub@ with 'Z'.
      
      > removeAll = concat ... splitsOn
      > (`replaceBy` Z) = removeAll
    -}
    removeAll :: (Eq e) => l -> l -> l
    removeAll =  concat ... splitsOn
    
    {- |
      @combo f es@ returns the length of the @es@ subsequence (left to tight)
      whose elements are in order @f@.
      
      > combo (<) [] == 0
      > combo (<) [1] == 1
      > combo (<) [7, 4, 12] == 1
      > combo (<) [1, 7, 3, 12] == 2
    -}
    combo :: Equal e -> l -> Int
    combo f = combo f . listL
    
    {- |
      @justifyL n e es@ prepends @(n - sizeOf es)@ elements @e@ to @es@ from the
      left side if @(sizeOf es < n)@. Otherwise returns the first @n@ elements
      of @es@, like @'take' n es@ do.
    -}
    justifyL :: Int -> e -> l -> l
    justifyL n e = take n . (++ replicate n e)
    
    {- |
      @justifyR n e es@ appends @(n - sizeOf es)@ elements @e@ to @es@ from the
      right side if @(sizeOf es < n)@. Otherwise returns the first @n@ elements
      of @es@, like @'keep' n es@ do.
    -}
    justifyR :: Int -> e -> l -> l
    justifyR n e = keep n . (replicate n e ++)
    
    {- |
      @each n es@ returns each @n@-th element of structure.
      
      @
        each n [1 .. 5] = []
        each 1 [1 .. 5] = [1 .. 5]
        each 2 [1 .. 5] = [1, 3, 5]
      @
      
      If @n == 1@, returns @es@.
      If @n < 1@, returns 'Z'.
    -}
    each :: Int -> l -> l
    each n es = case n <=> 1 of
      GT -> fromList $ ofoldr (\ i x xs -> mod i n == 0 ? x : xs $ xs) [] es
      EQ -> es
      LT -> Z
    
    {- |
      @eachFrom o n es@ returns each nth element of structure, beginning from o.
      
      @
        eachFrom o n = each n . drop o
        eachFrom 0 2 [1 .. 20] == [2, 4 .. 20]
        eachFrom 1 2 [1 .. 20] == [3, 5 .. 19]
      @
    -}
    eachFrom :: Int -> Int -> l -> l
    eachFrom o n = each n . drop o
    
    -- | @sub `'isPrefixOf'` es@ checks if @sub@ is beginning of @es@.
    isPrefixOf :: (Eq e) => l -> l -> Bool
    isPrefixOf (x :> xs) (y :> ys) = x == y && xs `isPrefixOf` ys
    isPrefixOf xs               ys = isNull xs && isNull ys
    
    -- | @sub `'isSuffixOf'` es@ checks if @sub@ is ending of @es@.
    isSuffixOf :: (Eq e) => l -> l -> Bool
    isSuffixOf (xs :< x) (ys :< y) = x == y && xs `isSuffixOf` ys
    isSuffixOf xs               ys = isNull xs && isNull ys
    
    -- | isInfixOf checks whether the first line is the substring of the second
    isInfixOf  :: (Eq e) => l -> l -> Bool
    isInfixOf _   Z = False
    isInfixOf xs ys = xs `isPrefixOf` ys || xs `isInfixOf` tail ys
    
    -- | prefix gives length of init, satisfying preducate.
    prefix :: (e -> Bool) -> l -> Int
    prefix p = o_foldr' (\ e c -> p e ? succ c $ 0) 0
    
    -- | suffix gives length of tail, satisfying predicate.
    suffix :: (e -> Bool) -> l -> Int
    suffix p = o_foldl' (\ c e -> p e ? succ c $ 0) 0
    
    {- |
      @infixes inf es@ returns a list of @inf@ positions in @es@, without
      intersections.
      
      > "" `infixes` es = []
      > "abba" `infixes` "baababba" == [4]
      > "abab" `infixes` "baababab" == [2]
      > "aaaa" `infixes` "aaaaaaaa" == [0, 4]
    -}
    infixes :: (Eq e) => l -> l -> [Int]
    infixes =  on infixes listL
    
    -- | @dropSide f = dropWhile f . dropEnd f@.
    dropSide :: (e -> Bool) -> l -> l
    dropSide f = dropWhile f . dropEnd f
    
    -- | Takes the longest 'prefix' by predicate.
    takeWhile :: (e -> Bool) -> l -> l
    takeWhile p es = take (prefix p es) es
    
    -- | Drops the longest 'prefix' by predicate.
    dropWhile :: (e -> Bool) -> l -> l
    dropWhile p es = drop (prefix p es) es
    
    -- | Takes the longest 'suffix' by predicate.
    takeEnd :: (e -> Bool) -> l -> l
    takeEnd p es = keep (suffix p es) es
    
    -- | Drops the longest 'suffix' by predicate.
    dropEnd :: (e -> Bool) -> l -> l
    dropEnd p es = sans (suffix p es) es
    
    -- | Left-side span.
    spanl :: (e -> Bool) -> l -> (l, l)
    spanl p es = (takeWhile p es, dropWhile p es)
    
    -- | Left-side break.
    breakl :: (e -> Bool) -> l -> (l, l)
    breakl p es = (takeWhile (not . p) es, dropWhile (not . p) es)
    
    -- | Right-side span.
    spanr :: (e -> Bool) -> l -> (l, l)
    spanr p es = (dropEnd p es, takeEnd p es)
    
    -- | Right-side break.
    breakr :: (e -> Bool) -> l -> (l, l)
    breakr p es = (dropEnd (not . p) es, takeEnd (not . p) es)
    
    {- |
      @selectWhile f es@ selects results of applying @f@ to @es@ (left to right)
      untill first fail.
    -}
    selectWhile :: (e -> Maybe a) -> l -> [a]
    selectWhile f = selectWhile f . listL
    
    {- |
      @selectEnd f es@ selects results of applying @f@ to @es@ (right to left)
      untill first fail.
    -}
    selectEnd :: (e -> Maybe a) -> l -> [a]
    selectEnd f = selectEnd f . listL
    
    {- |
      @extractWhile f es@ selects results of applying @f@ to @es@ (left to
      right) untill first fail. Returns selected results and rest of line.
    -}
    extractWhile :: (e -> Maybe a) -> l -> ([a], l)
    extractWhile f es = let as = selectWhile f es in (as, length as `drop` es)
    
    {- |
      @extractEnd f es@ selects results of applying @f@ to @es@ (right to left)
      untill first fail. Returns rest of line and selected results.
    -}
    extractEnd :: (e -> Maybe a) -> l -> (l, [a])
    extractEnd f es = let as = selectEnd f es in (length as `sans` es, as)
    
    -- | @selectWhile'@ is 'selectWhile' version for generalized structures.
    selectWhile' :: (t e ~ l, Linear1 t a) => (e -> Maybe a) -> l -> t a
    selectWhile' =  fromList ... selectWhile
    
    -- | @selectEnd'@ is 'selectEnd' version for generalized structures.
    selectEnd' :: (t e ~ l, Linear1 t a) => (e -> Maybe a) -> l -> t a
    selectEnd' =  fromList ... selectEnd
    
    -- | @extractWhile'@ is 'extractWhile' version for generalized structures.
    extractWhile' :: (t e ~ l, Linear1 t a) => (e -> Maybe a) -> l -> (t a, l)
    extractWhile' =  first fromList ... extractWhile
    
    -- | @extractEnd'@ is 'extractEnd' version for generalized structures.
    extractEnd' :: (t e ~ l, Linear1 t a) => (e -> Maybe a) -> l -> (l, t a)
    extractEnd' =  second fromList ... extractEnd

--------------------------------------------------------------------------------

-- | Pattern @(':>')@ is left-size view of line. Same as 'uncons' and 'toHead'.
pattern  (:>)   :: (Linear l e) => e -> l -> l
pattern x :> xs <- (uncons' -> Just (x, xs)) where (:>) = toHead

-- | Pattern @(':<')@ is right-size view of line. Same as 'unsnoc' and 'toLast'.
pattern   (:<)  :: (Linear l e) => l -> e -> l
pattern xs :< x <- (unsnoc' -> Just (xs, x)) where (:<) = toLast

--------------------------------------------------------------------------------

-- | 'Linear' contraint for @(Type -> Type)@-kind types.
type Linear1 l e = Linear (l e) e

-- | 'Linear' contraint for @(Type -> Type -> Type)@-kind types.
type Linear2 l i e = Linear (l i e) e

-- | 'Bordered' contraint for @(Type -> Type)@-kind types.
type Bordered1 l i e = Bordered (l e) i

-- | 'Bordered' contraint for @(Type -> Type -> Type)@-kind types.
type Bordered2 l i e = Bordered (l i e) i

#if __GLASGOW_HASKELL__ >= 806

{- |
  'Linear' contraint for @(Type -> Type)@-kind types.
  
  Only for GHC >= 8.6.1
-}
type Linear' l = forall e . Linear (l e) e

{- |
  'Linear' contraint for @(Type -> Type -> Type)@-kind types.
  
  Only for GHC >= 8.6.1
-}
type Linear'' l = forall i e . Linear (l i e) e

{- |
  'Bordered' contraint for @(Type -> Type)@-kind types.
  
  Only for GHC >= 8.6.1
-}
type Bordered' l i = forall e . Bordered (l e) i

{- |
  'Bordered' contraint for @(Type -> Type -> Type)@-kind types.
  
  Only for GHC >= 8.6.1
-}
type Bordered'' l = forall i e . Bordered (l i e) i

#endif

--------------------------------------------------------------------------------

{-# COMPLETE [], (:>) #-}
{-# COMPLETE [], (:<) #-}

instance Bordered [e] Int
  where
    sizeOf = length
    lower  = const 0
    
    rebound es = \ bnds -> size bnds `take` es
    upper   es = length es - 1

instance Linear [e] e
  where
    toHead = (:)
    toLast = flip (foldr' (:) . pure)
    
    uncons' = isNull ?- uncons
    unsnoc' = isNull ?- unsnoc
    
    uncons    []    = throw $ PatternMatchFail "in SDP.Linear.(:>)"
    uncons (e : es) = (e, es)
    
    unsnoc   [ ]    = throw $ PatternMatchFail "in SDP.Linear.(:<)"
    unsnoc   [e]    = ([], e)
    unsnoc (e : es) = let (es', e') = unsnoc es in (e : es', e')
    
    head  = L.head
    tail  = L.tail
    init  = L.init
    last  = L.last
    listL = toList
    listR = L.reverse
    
    ofoldr f base =
      let go !i es = case es of {x : xs -> f i x $ go (i + 1) xs; _ -> base}
      in  go 0
    
    ofoldl f =
      let go !i base es = case es of {x : xs -> go (i + 1) (f i base x) xs; _ -> base}
      in  go 0
    
    single       = pure
    fromList     = id
    fromListN    = L.take
    fromFoldable = toList
    replicate    = L.replicate
    
    (++)   = (L.++)
    (!^)   = (L.!!)
    
    write es n e = n < 0 ? es $ go n es
      where
        go i (x : xs) = i == 0 ? e : xs $ x : go (i - 1) xs
        go _ _ = []
    
    nubBy       = L.nubBy
    filter      = L.filter
    concat      = L.concat
    reverse     = L.reverse
    unfoldr     = L.unfoldr
    concatMap   = L.concatMap
    partition   = L.partition
    intersperse = L.intersperse
    isSubseqOf  = L.isSubsequenceOf
    
    o_foldr' = foldr'
    o_foldl' = foldl'
    o_foldr  = foldr
    o_foldl  = foldl
    
    iterate n f e = n < 1 ? [] $ e : iterate (n - 1) f (f e)
    
    before es i e = go (max 0 i) es
      where
        go 0    xs    = e : xs
        go n (x : xs) = x : go (n - 1) xs
        go _    []    = [e]
    
    remove i es = i < 0 ? es $ go i es
      where
        go 0 (_ : xs) = xs
        go n (x : xs) = x : go (n - 1) xs
        go _    []    = []
    
    take  = L.take
    drop  = L.drop
    split = L.splitAt
    
    infixes  Z  = const []
    infixes sub = go 0
      where
        go _ [] = []
        go i es = sub `isPrefixOf` es ? i : go (i + n) (drop n es) $ go (i + 1) (tail es)
        
        n = sizeOf sub
    
    combo _ [ ] = 0
    combo _ [_] = 1
    combo f (e1 : e2 : es) = e1 `f` e2 ? go 2 e2 es $ 1
      where
        go !i p (x : xs) = p `f` x ? go (i + 1) x xs $ i
        go  i _    _     = i
    
    splitBy  f es = let (as, bs) = breakl f es in isNull bs ? (es, []) $ (as, tail bs)
    divideBy f es = let (as, bs) = breakr f es in isNull as ? ([], es) $ (init as, bs)
    splitsBy f es = dropWhile f <$> L.findIndices f es `parts` es
    
    isPrefixOf = L.isPrefixOf
    isSuffixOf = L.isSuffixOf
    isInfixOf  = L.isInfixOf
    
    breakl = L.break
    spanl  = L.span
    
    selectWhile _    []    = []
    selectWhile f (x : xs) = case f x of {(Just e) -> e : select f xs; _ -> []}
    
    selectEnd f = reverse . selectWhile f . reverse

--------------------------------------------------------------------------------

instance (Index i) => Bordered (i, i) i
  where
    bounds = id
    lower  = fst
    upper  = snd
    
    indices = range
    indexIn = inRange
    rebound = const id
    
    sizeOf   = size
    indexOf  = index
    offsetOf = offset

--------------------------------------------------------------------------------

{- |
  @save n es@ takes first @n@ elements of @es@ if @n > 0@ and last @-n@
  elements otherwise.
-}
save :: (Linear l e) => Int -> l -> l
save n = n > 0 ? take n $ keep (-n)

{- |
  @skip n es@ drops first @n@ elements of @es@ if @n > 0@ and last @-n@
  elements otherwise.
-}
skip :: (Linear l e) => Int -> l -> l
skip n = n > 0 ? drop n $ sans (-n)

{- |
  Splits structures into parts by given offsets.
  
  @
    parts [0,5,6,12,26] ['a'..'z'] = ["","abcde","f","ghijkl","mnopqrstuvwxyz",""]
    -- if previous offset is equal or greater, subline is empty and next
    -- begins from previous:
    parts [0, 5, 4, 12, 26] ['a' .. 'z'] = ["","abcde","","fghijklm","nopqrstuvwxyz",""]
  @
-}
parts :: (Linear l e, Foldable f) => f Int -> l -> [l]
parts =
  let go o is' = case is' of {i : is -> (i - o) : go i is; _ -> []}
  in  splits . go 0 . toList

-- | @stripPrefix sub line@ strips prefix @sub@ of @line@ (if any).
stripPrefix :: (Linear s e, Bordered s i, Eq e) => s -> s -> s
stripPrefix sub line = sub `isPrefixOf` line ? drop (sizeOf sub) line $ line

-- | @stripSuffix sub line@ strips suffix @sub@ of @line@ (if any).
stripSuffix :: (Linear s e, Bordered s i, Eq e) => s -> s -> s
stripSuffix sub line = sub `isSuffixOf` line ? sans (sizeOf sub) line $ line

-- | @stripPrefix' sub line@ strips prefix @sub@ of @line@ or returns 'Nothing'.
stripPrefix' :: (Linear s e, Bordered s i, Eq e) => s -> s -> Maybe s
stripPrefix' sub = isPrefixOf sub ?+ drop (sizeOf sub)

-- | @stripSuffix sub line@ strips suffix @sub@ of @line@ or returns 'Nothing'.
stripSuffix' :: (Linear s e, Bordered s i, Eq e) => s -> s -> Maybe s
stripSuffix' sub = isSuffixOf sub ?+ sans (sizeOf sub)

-- | intercalate is generalization of intercalate
intercalate :: (Foldable f, Linear1 f l, Linear l e) => l -> f l -> l
intercalate =  concat ... intersperse

-- | @tails es@ returns sequence of @es@ tails.
tails :: (Linear l e) => l -> [l]
tails Z  = [Z]
tails es = es : tails (tail es)

-- | tails is generalization of inits.
inits :: (Linear l e) => l -> [l]
inits Z  = [Z]
inits es = es : inits (init es)

{- |
  @ascending es lengths@ checks if the subsequences of @es@ of lengths @lengths@
  is sorted.
-}
ascending :: (Linear s e, Sort s e, Ord e) => s -> [Int] -> Bool
ascending =  all sorted ... flip splits

--------------------------------------------------------------------------------

emptyEx :: String -> a
emptyEx =  throw . PatternMatchFail . showString "in SDP.Linear."

