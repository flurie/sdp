{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, TypeFamilies, DefaultSignatures #-}
{-# LANGUAGE Trustworthy #-}

{- |
    Module      :  SDP.Set
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
  
    @SDP.Set@ provides 'Set' - class for basic set operations.
-}
module SDP.Set
(
  -- * Exports
  module SDP.Linear,
  
  -- * Set
  Set (..),
  
  -- * Related functions
  set, insert, delete, intersections, unions, differences, symdiffs, isSetElem,
  
  lookupLT, lookupGT, lookupLE, lookupGE,
  
  (\/), (/\), (\\), (\^/), (\?/), (/?\), (\+/)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Linear

import Data.Maybe ( isJust )
import Data.List  ( sortBy )

import GHC.Types

default ()

--------------------------------------------------------------------------------

{- |
  Set is a class of data structures, that can represent sets.
  
  SDP doesn't making the difference between sets and any arbitrary data, but the
  function in Set (except 'setWith') works correctly only on correct sets. SDP
  only ensures that any function in Set returns the correct set.
  
  Note that function of type @Compare e@ must follow 'Ord' rules. If you use the
  wrong comparator, the result will depend on the implementation of the
  function.
  
  Example: by default, 'lookupLEWith' and 'lookupGEWith' functions use
  'isContainedIn' to search for an equal element in the set - if such an element
  is found, then the result is the given element (since they are equal).
-}
class (Linear s o) => Set s o | s -> o
  where
    {-# MINIMAL setWith, intersectionWith, unionWith, differenceWith, lookupLTWith, lookupGTWith #-}
    
    {- Creation functions. -}
    
    -- | Creates ordered set from linear structure.
    setWith :: Compare o -> s -> s
    
    {- |
      Creates set from linear structure using additional function for
      choice/merge equal elements.
    -}
    default groupSetWith :: (Linear s o) => Compare o -> (o -> o -> o) -> s -> s
    groupSetWith :: Compare o -> (o -> o -> o) -> s -> s
    groupSetWith cmp mrg = fromList . groupSetWith cmp mrg . listL
    
    -- | Adding element to set.
    insertWith :: Compare o -> o -> s -> s
    insertWith f e es = unionWith f es $ single e
    
    -- | Deleting element from set.
    deleteWith :: Compare o -> o -> s -> s
    deleteWith f e es = differenceWith f es $ single e
    
    {- Basic operations on sets. -}
    
    -- | Intersection of two sets.
    intersectionWith :: Compare o -> s -> s -> s
    
    -- | Difference (relative complement, aka A / B) of two sets.
    differenceWith :: Compare o -> s -> s -> s
    
    -- | Symmetric difference of two sets.
    symdiffWith :: Compare o -> s -> s -> s
    symdiffWith f xs ys = differenceWith f (unionWith f xs ys) (intersectionWith f xs ys)
    
    -- | Union of two sets.
    unionWith :: Compare o -> s -> s -> s
    
    {- Generalization of basic set operations on foldable. -}
    
    -- | Fold by intersectionWith.
    intersectionsWith :: (Foldable f) => Compare o -> f s -> s
    intersectionsWith f = intersectionWith f `foldl` Z
    
    -- | Fold by differenceWith.
    differencesWith :: (Foldable f) => Compare o -> f s -> s
    differencesWith f = differenceWith f `foldl` Z
    
    -- | Fold by unionWith.
    unionsWith :: (Foldable f) => Compare o -> f s -> s
    unionsWith f = unionWith f `foldl` Z
    
    -- | Fold by symdiffWith.
    symdiffsWith :: (Foldable f) => Compare o -> f s -> s
    symdiffsWith f = symdiffWith f `foldl` Z
    
    {- Сomparsion operations. -}
    
    -- | Compares sets on intersection.
    isIntersectsWith :: Compare o -> s -> s -> Bool
    isIntersectsWith f xs ys = not . isNull $ intersectionWith f xs ys
    
    -- | Compares sets on disjoint.
    isDisjointWith :: Compare o -> s -> s -> Bool
    isDisjointWith f xs ys = isNull $ intersectionWith f xs ys
    
    -- | Same as 'elem', but can work faster. By default, uses 'find'.
    default isContainedIn :: (t o ~~ s, Foldable t) => Compare o -> o -> s -> Bool
    isContainedIn :: Compare o -> o -> s -> Bool
    isContainedIn f e = isJust . find (\ x -> f e x == EQ)
    
    -- | Сhecks whether a first set is a subset of second.
    default isSubsetWith :: (t o ~~ s, Foldable t) => Compare o -> s -> s -> Bool
    isSubsetWith :: Compare o -> s -> s -> Bool
    isSubsetWith f xs ys = all (\ e -> isContainedIn f e ys) xs
    
    -- | Generates a list of different subsets (including empty and equivalent).
    default subsets :: (Ord s, Ord o) => s -> [s]
    subsets :: (Ord o) => s -> [s]
    subsets = set . subsequences . set
    
    {- Lookups. -}
    
    -- | lookupLTWith trying to find lesser element in set.
    lookupLTWith :: Compare o -> o -> s -> Maybe o
    
    -- | lookupGTWith trying to find greater element in set.
    lookupGTWith :: Compare o -> o -> s -> Maybe o
    
    -- | lookupGEWith trying to find greater or equal element in set.
    lookupGEWith :: Compare o -> o -> s -> Maybe o
    lookupGEWith f e es = isContainedIn f e es ? Just e $ lookupGTWith f e es
    
    -- | lookupLEWith trying to find lesser or equal element in set.
    lookupLEWith :: Compare o -> o -> s -> Maybe o
    lookupLEWith f e es = isContainedIn f e es ? Just e $ lookupLTWith f e es

--------------------------------------------------------------------------------

{- Useful functions. -}

-- | The same as sort . nub for list.
{-# INLINE set #-}
set :: (Set s o, Ord o) => s -> s
set =  setWith compare

-- | Same as insert compare.
{-# INLINE insert #-}
insert :: (Set s o, Ord o) => o -> s -> s
insert =  insertWith compare

-- | Same as deleteWith compare.
{-# INLINE delete #-}
delete :: (Set s o, Ord o) => o -> s -> s
delete =  deleteWith compare

-- | Intersection of two sets.
{-# INLINE (/\) #-}
(/\) :: (Set s o, Ord o) => s -> s -> s
(/\) =  intersectionWith compare

-- | Union of two sets.
{-# INLINE (\/) #-}
(\/) :: (Set s o, Ord o) => s -> s -> s
(\/) =  unionWith compare

-- | Difference (relative complement, aka A / B) of two sets.
{-# INLINE (\\) #-}
(\\) :: (Set s o, Ord o) => s -> s -> s
(\\) =  differenceWith compare

-- | Symetric difference (disjunctive union).
{-# INLINE (\^/) #-}
(\^/) :: (Set s o, Ord o) => s -> s -> s
(\^/) =  symdiffWith compare

-- | Synonym for 'isDisjointWith'.
{-# INLINE (/?\) #-}
(/?\) :: (Set s o, Ord o) => s -> s -> Bool
(/?\) =  isDisjointWith compare

-- | Same as isIntersectsWith compare.
{-# INLINE (\?/) #-}
(\?/) :: (Set s o, Ord o) => s -> s -> Bool
(\?/) =  isIntersectsWith compare

-- | Same as isSubsetWith compare.
{-# INLINE (\+/) #-}
(\+/) :: (Set s o, Ord o) => s -> s -> Bool
(\+/) =  isSubsetWith compare

-- | Intersection of some sets.
{-# INLINE intersections #-}
intersections :: (Foldable f, Set s o, Ord o) => f s -> s
intersections =  intersectionsWith compare

-- | Union of some sets.
{-# INLINE unions #-}
unions  :: (Foldable f, Set s o, Ord o) => f s -> s
unions  =  unionsWith compare

-- | Diference of some sets.
{-# INLINE differences #-}
differences :: (Foldable f, Set s o, Ord o) => f s -> s
differences =  differencesWith compare

-- | Symmetric difference of some sets.
{-# INLINE symdiffs #-}
symdiffs :: (Foldable f, Set s o, Ord o) => f s -> s
symdiffs =  symdiffsWith compare

-- | isSetElem is 'elem', but faster.
{-# INLINE isSetElem #-}
isSetElem :: (Set s o, Ord o) => o -> s -> Bool
isSetElem =  isContainedIn compare

-- | lookupLT tries to search lesser element in set.
lookupLT :: (Set s o, Ord o) => o -> s -> Maybe o
lookupLT =  lookupLTWith compare

-- | lookupGT tries to search greater element in set.
lookupGT :: (Set s o, Ord o) => o -> s -> Maybe o
lookupGT =  lookupGTWith compare

-- | lookupLE tries to search lesser of equal element in set.
lookupLE :: (Set s o, Ord o) => o -> s -> Maybe o
lookupLE =  lookupLEWith compare

-- | lookupGE tries to search greater or equal element in set.
lookupGE :: (Set s o, Ord o) => o -> s -> Maybe o
lookupGE =  lookupGEWith compare

--------------------------------------------------------------------------------

instance Set [e] e
  where
    {-
      [internal]: O(n) at best, but O(n ^ 2) at worst, rewrite setWith.
      
      setWith -- minor improvement of insertion sort for partially ordered data.
      It selects ordered sequences of elements and merges them.  For an ordered
      list, it has complexity O(n) and O (n ^ 2) for an reversed.
    -}
    setWith _ [] = []
    setWith f xs = dumbMergeList ordered (setWith f rest)
      where
        (ordered, rest) = splitSeq xs
        
        splitSeq (e1 : e2 : es) = let (initseq, others) = splitSeq (e2 : es) in
          case f e1 e2 of
            LT -> (e1 : initseq, others)
            EQ -> (initseq, others)
            GT -> ([e1], e2 : es)
        splitSeq es = (es, [])
        
        dumbMergeList    []       bs    = bs
        dumbMergeList    as       []    = as
        dumbMergeList (a : as) (b : bs) = case f a b of
          LT -> a : dumbMergeList as (b : bs)
          GT -> b : dumbMergeList (a : as) bs
          EQ -> a : dumbMergeList as bs
    
    insertWith _ e [] = [e]
    insertWith f e (x : xs) = case f e x of
      LT -> e : x : xs
      EQ -> x : xs
      GT -> x : insertWith f e xs
    
    deleteWith _ _ [] = []
    deleteWith f e (x : xs) = case f e x of
      LT -> x : xs
      EQ -> xs
      GT -> x : deleteWith f e xs
    
    isContainedIn _ _ [] = False
    isContainedIn f e (x : xs) = case f e x of
      LT -> False
      EQ -> True
      GT -> isContainedIn f e xs
    
    intersectionWith _ [] _  = []
    intersectionWith _ _  [] = []
    intersectionWith f (x : xs) (y : ys) = case f x y of
      LT -> intersectionWith f xs (y : ys)
      EQ -> x : intersectionWith f xs ys
      GT -> intersectionWith f (x : xs) ys
    
    unionWith _ [] ys = ys
    unionWith _ xs [] = xs
    unionWith f (x : xs) (y : ys) = case f x y of
      LT -> x : unionWith f xs (y : ys)
      EQ -> x : unionWith f xs ys
      GT -> y : unionWith f (x : xs) ys
    
    differenceWith _ xs [] = xs
    differenceWith _ [] _  = []
    differenceWith f (x : xs) (y : ys) = case f x y of
      LT -> x : differenceWith f xs (y : ys)
      EQ -> differenceWith f xs ys
      GT -> differenceWith f (x : xs) ys
    
    symdiffWith _ xs [] = xs
    symdiffWith _ [] ys = ys
    symdiffWith f (x : xs) (y : ys) = case f x y of
      EQ -> symdiffWith f xs ys
      LT -> x : symdiffWith f xs (y : ys)
      GT -> y : symdiffWith f (x : xs) ys
    
    isIntersectsWith f (x : xs) (y : ys) = case f x y of
      LT -> isIntersectsWith f xs (y : ys)
      EQ -> True
      GT -> isIntersectsWith f (x : xs) ys
    isIntersectsWith _ _  _  = False
    
    isDisjointWith f (x : xs) (y : ys) = case f x y of
      LT -> isDisjointWith f xs (y : ys)
      EQ -> False
      GT -> isDisjointWith f (x : xs) ys
    isDisjointWith _ _  _  = True
    
    lookupLTWith _ _    []    = Nothing
    lookupLTWith f o (x : xs) = case o `f` x of {GT -> look x xs; _ -> Nothing}
      where
        look r [] = Just r
        look r (e : es) = case o `f` e of {GT -> look e es; _ -> Just r}
    
    lookupGTWith _ _    []    = Nothing
    lookupGTWith f o (x : xs) = case o `f` x of {LT -> Just x; _ -> look xs}
      where
        look    []    = Nothing
        look (e : es) = case o `f` e of {LT -> Just e; _ -> look es}
    
    lookupLEWith _ _    []    = Nothing
    lookupLEWith f o (x : xs) = case o `f` x of {LT -> Nothing; _ -> look x xs}
      where
        look r    []    = Just r
        look r (e : es) = case o `f` e of {LT -> Just r; _ -> look e es}
    
    lookupGEWith _ _    []    = Nothing
    lookupGEWith f o (x : xs) = case o `f` x of {GT -> look xs; _ -> Just x}
      where
        look    []    = Nothing
        look (e : es) = case o `f` e of {GT -> look es; _ -> Just e}
    
    groupSetWith cmp mrg = group . sortBy cmp
      where
        group (e1 : e2 : es) = case e1 `cmp` e2 of
          EQ -> group (e1 `mrg` e2 : es)
          _  -> e1 : group (e2 : es)
        group es = es




