{-# LANGUAGE DefaultSignatures #-}

{- |
    Module      :  SDP.Set
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
  
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

--------------------------------------------------------------------------------

{- |
    A class of data structures, that can represent sets.
  
    Some implementations may be inefficient and used for internal definitions only.
  
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

class (Linear s) => Set s
  where
    {-# MINIMAL intersectionWith, unionWith, differenceWith #-}
    
    {- Creation functions. -}
    
    -- | Creates ordered set from linear structure.
    setWith      :: (o -> o -> Ordering) -> s o -> s o
    setWith f es =  foldl (flip $ insertWith f) Z es
    
    -- | Adding element to set.
    insertWith :: (o -> o -> Ordering) -> o -> s o -> s o
    insertWith f e es = intersectionWith f es (single e)
    
    -- | Deleting element from set.
    deleteWith :: (o -> o -> Ordering) -> o -> s o -> s o
    deleteWith f e es = differenceWith   f es (single e)
    
    {- Basic operations on sets. -}
    
    -- | Intersection of two sets.
    intersectionWith  :: (o -> o -> Ordering) -> s o -> s o -> s o
    
    -- | Difference (relative complement, aka A / B) of two sets.
    differenceWith    :: (o -> o -> Ordering) -> s o -> s o -> s o
    
    -- | Symmetric difference of two sets.
    symdiffWith       :: (o -> o -> Ordering) -> s o -> s o -> s o
    symdiffWith f xs ys = differenceWith f (unionWith f xs ys) (intersectionWith f xs ys)
    
    -- | Union of two sets.
    unionWith         :: (o -> o -> Ordering) -> s o -> s o -> s o
    
    {- Generalization of basic set operations on foldable. -}
    
    -- | Generalization of intersection on Foldable.
    intersectionsWith   :: (Foldable f) => (o -> o -> Ordering) -> f (s o) -> s o
    intersectionsWith f =  foldl (intersectionWith f) Z
    
    -- | Generalization of difference on Foldable.
    differencesWith     :: (Foldable f) => (o -> o -> Ordering) -> f (s o) -> s o
    differencesWith   f =  foldl (differenceWith f) Z
    
    -- | Generalization of union on Foldable.
    unionsWith          :: (Foldable f) => (o -> o -> Ordering) -> f (s o) -> s o
    unionsWith        f =  foldl (unionWith f) Z
    
    -- | Generalization of symdiff on Foldable.
    symdiffsWith        :: (Foldable f) => (o -> o -> Ordering) -> f (s o) -> s o
    symdiffsWith      f =  foldl (symdiffWith f) Z
    
    {- Сomparison operations -}
    
    -- | Compares sets on intersection.
    isIntersectsWith  :: (o -> o -> Ordering) -> s o -> s o -> Bool
    isIntersectsWith f xs ys = not . null $ intersectionWith f xs ys
    
    -- | Compares sets on disjoint.
    isDisjointWith    :: (o -> o -> Ordering) -> s o -> s o -> Bool
    isDisjointWith f xs ys = null $ intersectionWith f xs ys
    
    -- | Same as elem (Foldable), but can work faster. By default, uses find.
    isContainedIn     :: (o -> o -> Ordering) -> o -> s o -> Bool
    isContainedIn f e es = case find finder es of {Nothing -> False; _ -> True}
      where
        finder x = case f e x of {EQ -> True; _ -> False}
    
    -- | Сhecks whether a first set is a subset of second.
    isSubsetWith :: (o -> o -> Ordering) -> s o -> s o -> Bool
    isSubsetWith f xs ys = any (\ es -> isContainedIn f es ys) xs
    
    default subsets :: (Ord1 s, Ord o) => s o -> [s o]
    
    -- | Generates a list of different subsets (including empty and equivalent).
    subsets  :: (Ord o) => s o -> [s o]
    subsets  =  setWith compare1 . subsequences . set

--------------------------------------------------------------------------------

{- Useful functions. -}

-- | The same as sort . nub for list.
set    :: (Set s, Ord o) => s o -> s o
set    =  setWith compare

-- | Same as insert compare.
insert :: (Set s, Ord o) => o -> s o -> s o
insert =  insertWith compare

-- | Same as deleteWith compare.
delete :: (Set s, Ord o) => o -> s o -> s o
delete =  deleteWith compare

-- | Intersection of two sets.
(/\)  :: (Set s, Ord o) => s o -> s o -> s o
(/\)  =  intersectionWith compare

-- | Union of two sets.
(\/)  :: (Set s, Ord o) => s o -> s o -> s o
(\/)  =  unionWith compare

-- | Difference (relative complement, aka A / B) of two sets.
(\\)  :: (Set s, Ord o) => s o -> s o -> s o
(\\)  =  differenceWith compare

-- | Symetric difference (disjunctive union)
(\^/) :: (Set s, Ord o) => s o -> s o -> s o
(\^/) =  symdiffWith compare

-- | Intersection of some sets.
intersections :: (Set s, Foldable f, Ord o) => f (s o) -> s o
intersections =  intersectionsWith compare

-- | Union of some sets.
unions        :: (Set s, Foldable f, Ord o) => f (s o) -> s o
unions        =  unionsWith compare

-- | Diference of some sets.
differences   :: (Set s, Foldable f, Ord o) => f (s o) -> s o
differences   =  differencesWith compare

-- | Symmetric difference of some sets.
symdiffs      :: (Set s, Foldable f, Ord o) => f (s o) -> s o
symdiffs      =  symdiffsWith compare

-- | isSetElem o so = elem o so, but faster.
isSetElem :: (Set s, Ord o) => o -> s o -> Bool
isSetElem =  isContainedIn compare

-- | isDisjoint synonym. Mnemonic: is the intersection of sets (/\) empty?
(/?\) :: (Set s, Ord o) => s o -> s o -> Bool
(/?\) =  isDisjointWith compare

-- | Same as isIntersectsWith compare.
(\?/) :: (Set s, Ord o) => s o -> s o -> Bool
(\?/) =  isIntersectsWith compare

-- | Same as isSubsetWith compare.
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
