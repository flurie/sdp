{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

-- For default definitions.
{-# LANGUAGE TypeOperators, GADTs, DefaultSignatures #-}

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
  LineS (..),
  
  pattern (:>), pattern (:<), pattern  Z,
  
  tails, inits, nub
)

where

--------------------------------------------------------------------------------

import Prelude ()
import qualified Data.List as L

import SDP.SafePrelude
import SDP.Simple
import SDP.Index
import SDP.Zip

import GHC.Types

--------------------------------------------------------------------------------

infixr 5 :>, ++
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

class Linear l e | l -> e
  where
    {-# MINIMAL (fromList|fromListN), (head,tail|uncons), (init,last|unsnoc) #-}
    
    {- Service functions. -}
    
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
    
    -- | Line of n equal elements.
    replicate   :: Int -> e -> l
    replicate n =  fromListN n . replicate n
    
    -- | Creates line from list elements.
    fromList    :: [e] -> l
    fromList es =  fromListN (length es) es
    
    -- | May create finite line from infinite list. Doesn't evaluate list twice times.
    fromListN   :: Int -> [e] -> l
    fromListN n =  fromList . take n
    
    {- |
      Generalisation of fromList. fromFoldable is very powerful, but not
      comfortable to use, because needed type signatures.
    -}
    fromFoldable  :: (Foldable f) => f e -> l
    fromFoldable  =  fromList . toList
    
    -- | Generalization of (Some.Library.++).
    (++)          :: l -> l -> l
    
    -- | Generalization of Some.Library.concat.
    concat        :: (Foldable f) => f l -> l
    concat        =  foldr (++) lzero
    
    {- Filtering functions. -}
    
    -- | Generalization of Some.Library.filter.
    filter         :: (e -> Bool) -> l -> l
    
    -- | Generalization of Some.Library.partition.
    partition      :: (e -> Bool) -> l -> (l, l)
    partition p es = (filter p es, filter (not . p) es)
    
    {- Special functions -}
    
    -- Compares lines as [multi]sets. May be rewrited and moved.
    isSubseqOf    :: (Eq e) => l -> l -> Bool
    
    -- | Generalization of Some.Library.reverse.
    reverse       :: l -> l
    
    -- | Generalization of Data.List.intersperse. May be moved.
    intersperse   :: e -> l -> l
    
    -- | Generalization of Data.List.concatMap.
    concatMap     :: (Foldable f) => (a -> l) -> f a -> l
    
    -- | Generalization of Data.List.subsequences. May be moved.
    subsequences  :: l -> [l]
    
    {- |
      Same as toList . reverse (default) or reverse . toList.
      The listR looks rather inappropriate, but allows you to speed up processing
      a bit in some cases and not worry about the details of the specific
      structures implementation when writing generalized functions.
    -}
    listR :: l -> [e]
    
    -- | Generalization of Data.List.nubBy.
    nubBy :: (e -> e -> Bool) -> l -> l
    
    {- Default definitions. -}
    
    default (++)         :: (t e ~~ l, Foldable t) => l -> l -> l
    xs ++ ys             =  fromList $ (toList xs) ++ (toList ys)
    
    default filter       :: (t e ~~ l, Foldable t) => (e -> Bool) -> l -> l
    filter p             =  fromList . filter p . toList
    
    default reverse      :: (t e ~~ l, Foldable t) => l -> l
    reverse              =  fromList . reverse . toList
    
    default intersperse  :: (t e ~~ l, Foldable t) => e -> l -> l
    intersperse e        =  fromList . intersperse e . toList
    
    default subsequences :: (t e ~~ l, Foldable t) => l -> [l]
    subsequences xxs   =  Z : ss xxs
      where
        ss es = case es of {(x :> xs) -> single x : foldr (\ ys rest -> ys : (x :> ys) : rest) [] (ss xs); _ -> Z}
    
    default isSubseqOf   :: (t e ~~ l, Foldable t, Eq e) => l -> l -> Bool
    isSubseqOf Z _       =  True
    isSubseqOf _ Z       =  False
    isSubseqOf xs@(x :> rest) (y :> ys) = x == y && rest `isSubseqOf` ys || xs `isSubseqOf` ys
    
    default concatMap    :: (t e ~~ l, Foldable t, Foldable f) => (a -> l) -> f a -> l
    concatMap        f   =  foldr' (\ x y -> f x ++ y) Z
    
    default listR :: (t e ~~ l, Foldable t) => l -> [e]
    listR         =  toList . reverse
    
    default nubBy :: (t e ~~ l, Foldable t) => (e -> e -> Bool) -> l -> l
    nubBy f       =  fromList . nubBy f . toList

--------------------------------------------------------------------------------

{- |
  Class of bordered data structures.
  This is a enough general class: type must be Foldable, but not necessarily
  Linear or Indexed.
-}

class (Index i) => Bordered (b) i e | b -> i, b -> e
  where
    {-# MINIMAL (bounds|(lower, upper)) #-}
    
    bounds    :: b -> (i, i)
    bounds es =  (lower es, upper es)
    
    indices   :: b -> [i]
    indices   =  range . bounds
    
    lower     :: b -> i
    lower     =  fst  . bounds
    
    upper     :: b -> i
    upper     =  snd  . bounds
    
    assocs    :: b -> [(i, e)]
    
    default assocs :: (t e ~~ b, Foldable t) => b -> [(i, e)]
    assocs es = zip (indices es) (toList es)

--------------------------------------------------------------------------------

{- |
  LineS is class of structures that can be coverted to stream.
  It's generalization of ShowS (aka String -> String) to any Linear structure
  and any type.
  This class is separated from Linear for reasons of code observability.
-}

class (Linear l e) => LineS l e | l -> e
  where
    -- | fromFoldable for stream
    stream :: (Foldable f) => f e -> (l -> l)
    
    default stream :: (t e ~~ l, Foldable t, Foldable f) => f e -> (l -> l)
    stream es xs = foldr (:>) xs es

--------------------------------------------------------------------------------

{- |
  Split - class of splittable data structures. Also allows simple comparing
  functions.
-}

class (Linear s e) => Split s e | s -> e
  where
    {-# MINIMAL (take,drop|split), prefix, suffix #-}
    
    {- Simple splitters. -}
    
    take       :: Int -> s -> s
    take n     =  fst . split n
    
    drop       :: Int -> s -> s
    drop n     =  snd . split n
    
    -- | split is same as Some.Library.splitAt
    split      :: Int -> s -> (s, s)
    split n es =  (take n es, drop n es)
    
    -- | splits is generalization of split. Can be generalized yet.
    splits :: (Foldable f) => f Int -> s -> [s]
    splits ints es = splits' (toList ints) es
      where
        splits'    []    xs = [xs]
        splits' (i : is) xs = case split i xs of (y, ys) -> y : splits is ys
    
    {- Subsequence checkers. -}
    
    isPrefixOf :: (Eq e) => s -> s -> Bool
    
    isSuffixOf :: (Eq e) => s -> s -> Bool
    
    isInfixOf  :: (Eq e) => s -> s -> Bool
    
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
    
    default takeEnd :: (t e ~~ s, Foldable t) => (e -> Bool) -> s -> s
    takeEnd p es = drop (length es - suffix p es) es
    
    default dropEnd :: (t e ~~ s, Foldable t) => (e -> Bool) -> s -> s
    dropEnd p es = take (length es - suffix p es) es
    
    default isPrefixOf :: (t e ~~ s, Foldable t, Eq e) => s -> s -> Bool
    isPrefixOf (x :> xs) (y :> ys) = (x == y) && (xs `isPrefixOf` ys)
    isPrefixOf Z _ = True
    isPrefixOf _ _ = False
    
    default isSuffixOf :: (t e ~~ s, Foldable t, Eq e) => s -> s -> Bool
    isSuffixOf Z _ = True
    isSuffixOf (xs :< x) (ys :< y) = (x == y) && (xs `isSuffixOf` ys)
    isSuffixOf _ _ = False
    
    default isInfixOf  :: (t e ~~ s, Foldable t, Eq e) => s -> s -> Bool
    isInfixOf Z _   = True
    isInfixOf _ Z   = False
    isInfixOf xs ys = xs `isPrefixOf` ys || xs `isInfixOf` (tail ys)

--------------------------------------------------------------------------------

-- | Pattern Z is generalization of []. Same as null and lzero.
pattern Z :: (Foldable f, Linear (f e) e) => f e
pattern Z <- (null -> True)                           where  Z   = lzero

-- | Pattern (head :> tail) is generalization of (head : tail).
-- Same as uncons and toHead.
pattern  (:>)   :: (Foldable f, Linear (f e) e) => e -> f e -> f e
pattern x :> xs <- ((null ?: uncons) -> Just (x, xs)) where (:>) = toHead

-- | Pattern (init :< last) is right-size version of (:>)
-- Same as unsnoc and toLast.
pattern   (:<)  :: (Foldable f, Linear (f e) e) => f e -> e -> f e
pattern xs :< x <- ((null ?: unsnoc) -> Just (xs, x)) where (:<) = toLast

--------------------------------------------------------------------------------

instance Linear [e] e
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

instance Bordered [e] Int e
  where
    indices es = [0 .. length es - 1]
    bounds  es = (0,   length es - 1)
    
    assocs  es = zip  [0 .. ] es
    
    lower   _  = 0
    upper   es = length es - 1

instance LineS [e] e

instance Split [e] e
  where
    take  = L.take
    drop  = L.drop
    split = L.splitAt
    
    prefix = undefined
    suffix = undefined
    
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
nub   :: (Linear l e, Eq e) => l -> l
nub   =  nubBy (==)

-- | tails is generalization of Data.List.tails.
tails :: (Foldable f, Linear (f e) e) => f e -> [f e]
tails Z  = Z
tails es = es : tails (tail es)

-- | tails is generalization of Data.List.inits.
inits :: (Foldable f, Linear (f e) e) => f e -> [f e]
inits Z  = Z
inits es = es : inits (init es)
