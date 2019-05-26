{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

-- For default definitions.
{-# LANGUAGE TypeOperators, GADTs, DefaultSignatures #-}

{- |
    Module      :  SDP.Set
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
  
    Set is a class that allows you to create sets and perform simple operations
  on them.
-}

module SDP.Set
(
  module SDP.Linear,
  
  Set (..),
  
  set, insert, delete, intersections, unions, differences, symdiffs, isSetElem,
  
  (\/), (/\), (\\), (\^/), (\?/), (/?\), (\+/)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Linear

import GHC.Types

--------------------------------------------------------------------------------

{- |
    A class of data structures, that can represent sets.
  
    Some implementations may be inefficient and useful for internal definitions
  only.
  
    This library does not provide a distinction between sets and arbitrary data,
  but all functions relying on the definitions of this class must ensure that
  the properties of the result are preserved (if responsibility is not
  explicitly passed on to the programmer). Structures that are mainly used as
  sets will impose restrictions on the conversions. However, when working with
  lists and, for example, arrays, you must be careful.
  
    Unlike other classes in this library, Set provides fairly general functions
  because I don't want to be remembered with bad words every time while creating
  another newtype. For everyday use, the synonyms below are quite enough.
-}

class (Linear s o) => Set s o | s -> o
  where
    {-# MINIMAL intersectionWith, unionWith, differenceWith #-}
    
    {- Creation functions. -}
    
    -- | Creates ordered set from linear structure.
    setWith    :: (o -> o -> Ordering) -> s -> s
    
    -- | Adding element to set.
    insertWith :: (o -> o -> Ordering) -> o -> s -> s
    insertWith f e es = intersectionWith f es (single e)
    
    -- | Deleting element from set.
    deleteWith :: (o -> o -> Ordering) -> o -> s -> s
    deleteWith f e es = differenceWith   f es (single e)
    
    {- Basic operations on sets. -}
    
    -- | Intersection of two sets.
    intersectionWith  :: (o -> o -> Ordering) -> s -> s -> s
    
    -- | Difference (relative complement, aka A / B) of two sets.
    differenceWith    :: (o -> o -> Ordering) -> s -> s -> s
    
    -- | Symmetric difference of two sets.
    symdiffWith       :: (o -> o -> Ordering) -> s -> s -> s
    symdiffWith f xs ys = differenceWith f (unionWith f xs ys) (intersectionWith f xs ys)
    
    -- | Union of two sets.
    unionWith         :: (o -> o -> Ordering) -> s -> s -> s
    
    {- Generalization of basic set operations on foldable. -}
    
    -- | Generalization of intersection on Foldable.
    intersectionsWith   :: (Foldable f) => (o -> o -> Ordering) -> f s -> s
    
    -- | Generalization of difference on Foldable.
    differencesWith     :: (Foldable f) => (o -> o -> Ordering) -> f s -> s
    
    -- | Generalization of union on Foldable.
    unionsWith          :: (Foldable f) => (o -> o -> Ordering) -> f s -> s
    
    -- | Generalization of symdiff on Foldable.
    symdiffsWith        :: (Foldable f) => (o -> o -> Ordering) -> f s -> s
    
    {- Сomparison operations -}
    
    -- | Compares sets on intersection.
    isIntersectsWith  :: (o -> o -> Ordering) -> s -> s -> Bool
    
    -- | Compares sets on disjoint.
    isDisjointWith    :: (o -> o -> Ordering) -> s -> s -> Bool
    
    -- | Same as elem (Foldable), but can work faster. By default, uses find.
    isContainedIn     :: (o -> o -> Ordering) -> o -> s -> Bool
    
    -- | Сhecks whether a first set is a subset of second.
    isSubsetWith :: (o -> o -> Ordering) -> s -> s -> Bool
    
    -- | Generates a list of different subsets (including empty and equivalent).
    subsets  :: (Ord o) => s -> [s]
    
    {- Default definitions. -}
    
    default setWith :: (((t o) ~~ s), Foldable t) => (o -> o -> Ordering) -> s -> s
    setWith f es    =  foldl (flip $ insertWith f) Z es
    
    default subsets :: (Ord s, Ord o) => s -> [s]
    subsets         =  setWith compare . subsequences . set
    
    -- comparsion.
    
    default isSubsetWith  :: (((t o) ~~ s), Foldable t) => (o -> o -> Ordering) -> s -> s -> Bool
    isSubsetWith f xs ys  =  any (\ es -> isContainedIn f es ys) xs
    
    default isContainedIn :: (((t o) ~~ s), Foldable t) => (o -> o -> Ordering) -> o -> s -> Bool
    isContainedIn f e es  = case find finder es of {Nothing -> False; _ -> True}
      where
        finder x = case f e x of {EQ -> True; _ -> False}
    
    default isIntersectsWith  :: (((t o) ~~ s), Foldable t) => (o -> o -> Ordering) -> s -> s -> Bool
    isIntersectsWith f xs ys = not . null $ intersectionWith f xs ys
    
    default isDisjointWith    :: (((t o) ~~ s), Foldable t) => (o -> o -> Ordering) -> s -> s -> Bool
    isDisjointWith f xs ys = null $ intersectionWith f xs ys
    
    -- basic set operators.
    
    default intersectionsWith :: (((t o) ~~ s), Foldable f, Foldable t) => (o -> o -> Ordering) -> f s -> s
    intersectionsWith f       =  foldl (intersectionWith f) Z
    
    default differencesWith   :: (((t o) ~~ s), Foldable f, Foldable t) => (o -> o -> Ordering) -> f s -> s
    differencesWith   f       =  foldl (differenceWith f) Z
    
    default unionsWith        :: (((t o) ~~ s), Foldable f, Foldable t) => (o -> o -> Ordering) -> f s -> s
    unionsWith        f       =  foldl (unionWith f) Z
    
    default symdiffsWith      :: (((t o) ~~ s), Foldable f, Foldable t) => (o -> o -> Ordering) -> f s -> s
    symdiffsWith      f       =  foldl (symdiffWith f) Z

--------------------------------------------------------------------------------

{- Useful functions. -}

-- | The same as sort . nub for list.
set    :: (Set s o, Ord o) => s -> s
set    =  setWith compare

-- | Same as insert compare.
insert :: (Set s o, Ord o) => o -> s -> s
insert =  insertWith compare

-- | Same as deleteWith compare.
delete :: (Set s o, Ord o) => o -> s -> s
delete =  deleteWith compare

-- | Intersection of two sets.
(/\)  :: (Set s o, Ord o) => s -> s -> s
(/\)  =  intersectionWith compare

-- | Union of two sets.
(\/)  :: (Set s o, Ord o) => s -> s -> s
(\/)  =  unionWith compare

-- | Difference (relative complement, aka A / B) of two sets.
(\\)  :: (Set s o, Ord o) => s -> s -> s
(\\)  =  differenceWith compare

-- | Symetric difference (disjunctive union).
(\^/) :: (Set s o, Ord o) => s -> s -> s
(\^/) =  symdiffWith compare

-- | isDisjoint synonym. Mnemonic: is the intersection of sets (/\) empty?
(/?\) :: (Set s o, Ord o) => s -> s -> Bool
(/?\) =  isDisjointWith compare

-- | Same as isIntersectsWith compare.
(\?/) :: (Set s o, Ord o) => s -> s -> Bool
(\?/) =  isIntersectsWith compare

-- | Same as isSubsetWith compare.
(\+/) :: (Set s o, Ord o) => s -> s -> Bool
(\+/) =  isSubsetWith compare

-- | Intersection of some sets.
intersections :: (Foldable f, Set s o, Ord o) => f s -> s
intersections =  intersectionsWith compare

-- | Union of some sets.
unions        :: (Foldable f, Set s o, Ord o) => f s -> s
unions        =  unionsWith compare

-- | Diference of some sets.
differences   :: (Foldable f, Set s o, Ord o) => f s -> s
differences   =  differencesWith compare

-- | Symmetric difference of some sets.
symdiffs      :: (Foldable f, Set s o, Ord o) => f s -> s
symdiffs      =  symdiffsWith compare

-- | isSetElem o so = elem o so, but faster.
isSetElem     :: (Set s o, Ord o) => o -> s -> Bool
isSetElem     =  isContainedIn compare

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
        
        splitSeq (e1 : e2 : es) = case f e1 e2 of
            LT -> (e1 : initseq, others)
            EQ -> (initseq, others)
            GT -> ([e1], e2 : es)
          where
            (initseq, others) = splitSeq (e2 : es)
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

--------------------------------------------------------------------------------
