{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
-- {-# LANGUAGE CPP #-} -- for future use

{- |
    Module      :  SDP.Linear
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
  
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
  LineS (..),
  
  pattern (:>), pattern (:<), pattern  Z,
  
  tails, inits, nub
)

where

--------------------------------------------------------------------------------

import Prelude ()
import qualified Data.List as L hiding ( concat )

import SDP.SafePrelude
import SDP.Simple
import SDP.Index
import SDP.Zip

--------------------------------------------------------------------------------

infixr 5 ++, :>
infixl 5 :<

--------------------------------------------------------------------------------

{- |
    Class of linear data structures that can be created from list.
    Linear similar to IsList, but requires Foldable context and provides
    overloaded pattern synonyms: Z, (head :> tail) and (init :< last).
    
    This class tries to balance between efficiency, code observability and
    consistency, but some functions in Linear may seem redundant or not very
    relevant.
-}

class (Functor l, Foldable l) => Linear l
  where
    {-# MINIMAL (fromList|fromListN), (head, tail|uncons), (init, last|unsnoc) #-}
    
    {- Service functions. -}
    
    -- | Empty line. Service constant, synonym for Z.
    lzero :: l e
    lzero =  fromList []
    
    -- | Separates head and tail. Service function, synonym for (x :> xs).
    uncons      :: l e -> (e, l e)
    uncons xs   =  (head xs, tail xs)
    
    -- | Adds element to head of line. Service function, synonym for (x :> xs).
    toHead      :: e -> l e -> l e
    toHead e es =  single e ++ es
    
    head :: l e -> e
    head =  fst . uncons
    
    tail :: l e -> l e
    tail =  snd . uncons
    
    -- | Separates init and last. Service function, synonym for (xs :< x).
    unsnoc      :: l e -> (l e, e)
    unsnoc xs   =  (init xs, last xs)
    
    -- | Adds element to end of line. Service function, synonym for (xs :< x).
    toLast      :: l e ->   e -> l e
    toLast es e =  es ++ single e
    
    init :: l e -> l e
    init =  fst . unsnoc
    
    last :: l e -> e
    last =  snd . unsnoc
    
    {- Construction. -}
    
    -- | Singleton.
    single      :: e -> l e
    single x    =  fromList [x]
    
    -- | Line of n equal elements.
    replicate   :: Int -> e -> l e
    replicate n =  fromListN n . replicate n
    
    -- | Creates line from list elements.
    fromList  :: [e] -> l e
    fromList es = fromListN (length es) es
    
    -- | May create finite line from infinite list. Doesn't evaluate list twice times.
    fromListN   :: Int -> [e] -> l e
    fromListN n =  fromList . take n
    
    {- |
      Generalisation of fromList. fromFoldable is very powerful, but not
      comfortable to use, because needed type signatures.
    -}
    fromFoldable  :: (Foldable f) => f e -> l e
    fromFoldable  =  fromList . toList
    
    -- | Generalization of (Some.Library.++).
    (++)        :: l e -> l e -> l e
    xs ++ ys    =  fromList $ (toList xs) ++ (toList ys)
    
    -- | Generalization of Some.Library.concat.
    concat      :: (Foldable f) => f (l e) -> l e
    concat      =  foldr (++) lzero
    
    {- Filtering functions. -}
    
    -- | Generalization of Some.Library.filter.
    filter         :: (e -> Bool) -> l e -> l e
    filter p       =  fromList . filter p . toList
    
    -- | Generalization of Some.Library.partition.
    partition      :: (e -> Bool) -> l e -> (l e, l e)
    partition p es = (filter p es, filter (not . p) es)
    
    {- Special functions -}
    
    -- Compares lines as [multi]sets. May be rewrited and moved.
    isSubseqOf    :: (Eq e) => l e -> l e -> Bool
    isSubseqOf Z _ = True
    isSubseqOf _ Z = False
    isSubseqOf xs@(x :> rest) (y :> ys) = x == y && rest `isSubseqOf` ys || xs `isSubseqOf` ys
    
    -- | Generalization of Some.Library.reverse.
    reverse       :: l e -> l e
    reverse       =  fromList . reverse . toList
    
    -- | Generalization of Data.List.intersperse. May be moved.
    intersperse   :: e -> l e -> l e
    intersperse e =  fromList . intersperse e . toList
    
    -- | Generalization of Data.List.concatMap.
    concatMap     :: (Foldable f) => (e -> l r) -> f e -> l r
    concatMap   f =  foldr' (\ x y -> f x ++ y) Z
    
    -- | Generalization of Data.List.subsequences. May be moved.
    subsequences  :: (Linear l) => l e -> [l e]
    subsequences xxs =  Z : subsequences' xxs
      where
        subsequences' (x :> xs) = single x : foldr f [] (subsequences' xs)
          where
            f ys rest = ys : (x :> ys) : rest
        subsequences'     _     = Z
    
    {- |
      Same as toList . reverse (default) or reverse . toList.
      The listR looks rather inappropriate, but allows you to speed up processing
      a bit in some cases and not worry about the details of the implementation
      of specific structures when writing generalized functions.
    -}
    listR   :: l e -> [e]
    listR   =  reverse . toList
    
    -- | Generalization of Data.List.nubBy.
    nubBy   :: (e -> e -> Bool) -> l e -> l e
    nubBy f =  fromList . nubBy f . toList

--------------------------------------------------------------------------------

{- |
  Class of bordered data structures.
  This is a enough general class: type must be Foldable, but not necessarily
  Linear or Indexed.
-}

class (Foldable b, Index i) => Bordered (b) i | b -> i
  where
    {-# MINIMAL bounds|(lower, upper) #-}
    
    bounds    :: b e ->  (i, i)
    bounds es =  (lower es, upper es)
    
    assocs    :: b e -> [(i, e)]
    assocs xs =  zip (indices xs) (toList xs)
    
    indices   :: b e -> [i]
    indices   =  range . bounds
    
    lower     :: b e -> i
    lower     =  fst  . bounds
    
    upper     :: b e -> i
    upper     =  snd  . bounds

--------------------------------------------------------------------------------

{- |
  LineS is class of structures that can be coverted to stream.
  It's generalization of ShowS (aka String -> String) to any Linear structure
  and any type.
  This class is separated from Linear for reasons of code observability.
-}

class (Linear l) => LineS l
  where
    -- | fromFoldable for stream
    stream :: (Foldable f) => f e -> (l e -> l e)
    stream es xs = foldr (:>) xs es
    
    -- | This function has a purely syntactic meaning. More productive will be the use of (.).
    (<+>)  :: (l e -> l e) -> l e -> (l e -> l e)
    (<+>) xss ys = xss . openS ys
    
    -- | This function is used to create a stream from an already existing string (like shows for String).
    openS  :: l e -> (l e -> l e)
    openS  =  (++)
    
    -- | This function has a purely syntactic meaning. It closes the open stream.
    closeS    :: (l e -> l e) -> l e
    closeS xs =  xs Z

--------------------------------------------------------------------------------

{- |
  Split - class of splittable data structures. Also allows simple comparing
  functions.
-}

class (Linear s) => Split s
  where
    {- Simple splitters. -}
    
    take       :: Int -> s e -> s e
    take n es  =  fromList . take n $ toList es
    
    drop       :: Int -> s e -> s e
    drop n es  =  fromList . drop n $ toList es
    
    -- | split is same as Some.Library.splitAt
    split      :: Int -> s e -> (s e, s e)
    split n es =  (take n es, drop n es)
    
    -- | splits is generalization of split. Can be generalized yet.
    splits :: (Foldable f) => f Int -> s e -> [s e]
    splits ints es = splits' (toList ints) es
      where
        splits'    []    xs = [xs]
        splits' (i : is) xs = case split i xs of (y, ys) -> y : splits is ys
    
    {- Subsequence checkers. -}
    
    isPrefixOf :: (Eq e) => s e -> s e -> Bool
    isPrefixOf Z _ = True
    isPrefixOf (x :> xs) (y :> ys) = (x == y) && (xs `isPrefixOf` ys)
    isPrefixOf _ _ = False
    
    isSuffixOf :: (Eq e) => s e -> s e -> Bool
    isSuffixOf Z _ = True
    isSuffixOf (xs :< x) (ys :< y) = (x == y) && (xs `isSuffixOf` ys)
    isSuffixOf _ _ = False
    
    isInfixOf  :: (Eq e) => s e -> s e -> Bool
    isInfixOf Z _   = True
    isInfixOf _ Z   = False
    isInfixOf xs ys = xs `isPrefixOf` ys || xs `isInfixOf` (tail ys)
    
    {- Largest sequences. -}
    
    -- | prefix gives length of init, satisfying preducate.
    prefix :: (e -> Bool) -> s e -> Int
    prefix p = prefix p . toList
    
    -- | suffix gives length of tail, satisfying predicate.
    suffix :: (e -> Bool) -> s e -> Int
    suffix p = suffix p . toList
    
    {- "Clever" splitters. -}
    
    -- | Takes the longest init.
    takeWhile :: (e -> Bool) -> s e -> s e
    takeWhile p es = take (p `prefix` es) es
    
    -- | Drops the longest init.
    dropWhile :: (e -> Bool) -> s e -> s e
    dropWhile p es = drop (p `prefix` es) es
    
    -- | Takes the longest tail.
    takeEnd   :: (e -> Bool) -> s e -> s e
    takeEnd p es = drop (length es - suffix p es) es
    
    -- | Drops the longest tail.
    dropEnd   :: (e -> Bool) -> s e -> s e
    dropEnd p es = take (length es - suffix p es) es
    
    -- | Left-side span, generalization of Data.List.span
    spanl       :: (e -> Bool) -> s e -> (s e, s e)
    spanl  p es =  split (p `prefix` es) es
    
    -- | Left-side break, generalization of Data.List.break
    breakl      :: (e -> Bool) -> s e -> (s e, s e)
    breakl p es =  split ((not . p) `prefix` es) es
    
    -- | Right-side span. And Now for Something Completely Different.
    spanr       :: (e -> Bool) -> s e -> (s e, s e)
    spanr  p es =  split (length es - suffix p es) es
    
    -- | Right-side break. See above.
    breakr      :: (e -> Bool) -> s e -> (s e, s e)
    breakr p es =  split (length es - suffix (not . p) es) es

--------------------------------------------------------------------------------

-- | Pattern Z is generalization of []. Same as null and lzero.
pattern   Z   :: (Linear l) => l e
pattern Z <- (null -> True)                           where  Z   = lzero

-- | Pattern (head :> tail) is generalization of (head : tail).
-- Same as uncons and toHead.
pattern  (:>) :: (Linear l) =>   e -> l e -> l e
pattern x :> xs <- ((null ?: uncons) -> Just (x, xs)) where (:>) = toHead

-- | Pattern (init :< last) is right-size version of (:>)
-- Same as unsnoc and toLast.
pattern  (:<) :: (Linear l) => l e ->   e -> l e
pattern xs :< x <- ((null ?: unsnoc) -> Just (xs, x)) where (:<) = toLast

--------------------------------------------------------------------------------

instance Linear []
  where
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
    listR        = L.reverse
    nubBy        = L.nubBy

instance Bordered [] Int
  where
    bounds  es = (0,   length es - 1)
    assocs  es = zip  [0 .. ] es
    indices es = [0 .. length es - 1]
    lower   _  = 0
    upper   es = length es - 1

instance LineS []
  where
    openS   xs = \ ys -> null ys ? xs $ foldr (:) ys xs
    closeS ess = ess []

instance Split []
  where
    take = L.take
    drop = L.drop
    
    split = L.splitAt
    
    isPrefixOf = L.isPrefixOf
    isInfixOf  = L.isInfixOf
    isSuffixOf = L.isSuffixOf
    
    spanl  = L.span
    breakl = L.break
    
    takeWhile _ [] = []
    takeWhile p (e : es) = p e ? e : takeWhile p es $ []
    
    dropWhile _ [] = []
    dropWhile p es@(x : xs) = p x ? dropWhile p xs $ es
    
    takeEnd p list = t list list
      where
        t [] res = res
        t xs@(e : es) res = t es (p e ? res $ xs)
    
    dropEnd p = foldr (\ e es -> p e && null es ? [] $ (e : es)) []

--------------------------------------------------------------------------------

-- | nub is generalization of Data.List.nub and synonym for nubBy (==).
nub   :: (Linear l, Eq e) => l e -> l e
nub   =  nubBy (==)

-- | tails is generalization of Data.List.tails.
tails :: (Linear l) => l e -> [l e]
tails Z  = Z
tails es = es : tails (tail es)

-- | tails is generalization of Data.List.inits.
inits :: (Linear l) => l e -> [l e]
inits Z  = Z
inits es = es : inits (init es)
