{-# LANGUAGE DefaultSignatures #-}

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
import Data.Functor.Classes

--------------------------------------------------------------------------------

{-
  A class of data structures that can represent sets.
  
  Some implementations may be inefficient and used for internal definitions only.
  
  This library  does not provide  a distinction between sets and arbitrary data,
  but  all functions  relying on the definitions of this class  must ensure that
  the  properties  of  the  result  are  preserved  (if  responsibility  is  not
  explicitly  passed on  to the programmer). Structures that  are mainly used as
  sets  will impose restrictions on the conversions. However,  when working with
  lists and, for example, arrays, you must be careful.
  
  Unlike other classes  in this library,  Set provides  fairly general functions
  because I don't want to be remembered with bad words every time while creating
  another newtype. For everyday use, the synonyms below are quite enough.
-}

class (Linear s) => Set s
  where
    {-# MINIMAL intersectionWith, unionWith, differenceWith #-}
    
    {- Creation functions. -}
    
    -- Creates ordered set from linear structure.
    setWith      :: (o -> o -> Ordering) -> s o -> s o
    setWith f es =  foldl (flip $ insertWith f) Z es
    
    -- Adding element to set.
    insertWith :: (o -> o -> Ordering) -> o -> s o -> s o
    insertWith f e es = intersectionWith f es (single e)
    
    -- Deleting element from set.
    deleteWith :: (o -> o -> Ordering) -> o -> s o -> s o
    deleteWith f e es = differenceWith   f es (single e)
    
    {- Basic operations on sets. -}
    
    -- Intersection of two sets.
    intersectionWith  :: (o -> o -> Ordering) -> s o -> s o -> s o
    
    -- Difference (relative complement, aka A / B) of two sets.
    differenceWith    :: (o -> o -> Ordering) -> s o -> s o -> s o
    
    -- Symmetric difference of two sets.
    symdiffWith       :: (o -> o -> Ordering) -> s o -> s o -> s o
    symdiffWith f xs ys = differenceWith f (unionWith f xs ys) (intersectionWith f xs ys)
    
    -- Union of two sets.
    unionWith         :: (o -> o -> Ordering) -> s o -> s o -> s o
    
    {- Generalization of basic set operations on foldable. -}
    
    intersectionsWith   :: (Foldable f) => (o -> o -> Ordering) -> f (s o) -> s o
    intersectionsWith f =  foldl (intersectionWith f) Z
    
    differencesWith     :: (Foldable f) => (o -> o -> Ordering) -> f (s o) -> s o
    differencesWith   f =  foldl (differenceWith f) Z
    
    unionsWith          :: (Foldable f) => (o -> o -> Ordering) -> f (s o) -> s o
    unionsWith        f =  foldl (unionWith f) Z
    
    symdiffsWith        :: (Foldable f) => (o -> o -> Ordering) -> f (s o) -> s o
    symdiffsWith      f =  foldl (symdiffWith f) Z
    
    {- Сomparison operations -}
    
    isIntersectsWith  :: (o -> o -> Ordering) -> s o -> s o -> Bool
    isIntersectsWith f xs ys = not . null $ intersectionWith f xs ys
    
    isDisjointWith    :: (o -> o -> Ordering) -> s o -> s o -> Bool
    isDisjointWith f xs ys = null $ intersectionWith f xs ys
    
    -- Is there element in the set? Same as elem, but can work faster.
    isContainedIn     :: (o -> o -> Ordering) -> o -> s o -> Bool
    isContainedIn f e es = case find finder es of {Nothing -> False; _ -> True}
      where
        finder x = case f e x of {EQ -> True; _ -> False}
    
    -- Сhecks whether a first set is a subset of second.
    isSubsetWith :: (o -> o -> Ordering) -> s o -> s o -> Bool
    isSubsetWith f xs ys = any (\ xs -> isContainedIn f xs ys) xs
    
    default subsets :: (Ord1 s, Ord o) => s o -> [s o]
    
    -- Generates a list of different subsets (including empty and equivalent).
    subsets  :: (Ord o) => s o -> [s o]
    subsets  =  setWith compare1 . subsequences . set

--------------------------------------------------------------------------------

{- Useful functions. -}

-- The same as sort . nub for list.
set    :: (Set s, Ord o) => s o -> s o
set    =  setWith compare

insert :: (Set s, Ord o) => o -> s o -> s o
insert =  insertWith compare

delete :: (Set s, Ord o) => o -> s o -> s o
delete =  deleteWith compare

-- Intersection of two sets.
(/\)  :: (Set s, Ord o) => s o -> s o -> s o
(/\)  =  intersectionWith compare

-- Union of two sets.
(\/)  :: (Set s, Ord o) => s o -> s o -> s o
(\/)  =  unionWith compare

-- Difference (relative complement, aka A / B) of two sets.
(\\)  :: (Set s, Ord o) => s o -> s o -> s o
(\\)  =  differenceWith compare

-- Symetric difference (disjunctive union)
(\^/) :: (Set s, Ord o) => s o -> s o -> s o
(\^/) =  symdiffWith compare

-- Intersection of some sets.
intersections :: (Set s, Foldable f, Ord o) => f (s o) -> s o
intersections =  intersectionsWith compare

-- Union of some sets.
unions        :: (Set s, Foldable f, Ord o) => f (s o) -> s o
unions        =  unionsWith compare

-- Diference of some sets.
differences   :: (Set s, Foldable f, Ord o) => f (s o) -> s o
differences   =  differencesWith compare

-- Symmetric difference of some sets.
symdiffs      :: (Set s, Foldable f, Ord o) => f (s o) -> s o
symdiffs      =  symdiffsWith compare

-- isSetElem o so = elem o so, but faster.
isSetElem :: (Set s, Ord o) => o -> s o -> Bool
isSetElem =  isContainedIn compare

-- isDisjoint synonym. Mnemonic: is the intersection of sets (/\) empty?
(/?\) :: (Set s, Ord o) => s o -> s o -> Bool
(/?\) =  isDisjointWith compare

-- isIntersects synonym.
(\?/) :: (Set s, Ord o) => s o -> s o -> Bool
(\?/) =  isIntersectsWith compare

(\+/) :: (Set s, Ord o) => s o -> s o -> Bool
(\+/) =  isSubsetWith compare

--------------------------------------------------------------------------------

instance Set []
  where
    {-
      [internal]: O(n) at best, but O(n ^ 2) at worst, rewrite setWith.
      
      setWith -- minor improvement of insertion sort for partially ordered data.
      It selects ordered sequences of elements and merges them.  For an ordered
      list, it has complexity O(n) and O (n ^ 2) for an reversed.
    -}
    setWith f [] = []
    setWith f xs = dumbMergeList f ordered (setWith f tail)
      where
        (ordered, tail) = splitSeq f xs
        
        splitSeq :: (o -> o -> Ordering) -> [o] -> ([o], [o])
        splitSeq f (e1 : e2 : es) = case f e1 e2 of
            LT -> (e1 : seq, tail)
            EQ -> (seq, tail)
            GT -> ([e1], e2 : es)
          where
            (seq, tail) = splitSeq f (e2 : es)
        splitSeq _ xs = (xs, [])
        
        dumbMergeList :: (o -> o -> Ordering) -> [o] -> [o] -> [o]
        dumbMergeList _    []       ys    = ys
        dumbMergeList _    xs       []    = xs
        dumbMergeList f (x : xs) (y : ys) = case f x y of
          LT -> x : dumbMergeList f xs (y : ys)
          GT -> y : dumbMergeList f (x : xs) ys
          EQ -> x : dumbMergeList f xs ys
    
    insertWith _ e [] = [e]
    insertWith f e (x : xs) = case f e x of
      LT -> e : x : xs
      EQ -> x : xs
      GT -> x : insertWith f e xs
    
    deleteWith f e [] = []
    deleteWith f e (x : xs) = case f e x of
      LT -> x : xs
      EQ -> xs
      GT -> x : deleteWith f e xs
    
    isContainedIn f e [] = False
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
