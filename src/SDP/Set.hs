{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, TypeFamilies, ConstraintKinds, DefaultSignatures #-}
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
  Set (..), Set1,
  
  -- * Related functions
  set, insert, delete, intersections, unions, differences, symdiffs, isSetElem,
  
  lookupLT, lookupGT, lookupLE, lookupGE,
  
  (\/), (/\), (\\), (\^/), (\?/), (/?\), (\+/)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Linear

import Data.List ( sortBy, groupBy )

import GHC.Types

import SDP.Internal

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
    {-# MINIMAL intersectionWith, unionWith, differenceWith, lookupLTWith, lookupGTWith #-}
    
    {- Creation functions. -}
    
    -- | Creates ordered set from linear structure.
    setWith :: Compare o -> s -> s
    setWith f = fromList . setWith f. listL
    
    {- |
      Creates set from linear structure using additional function for
      choice/merge equal elements.
    -}
    default groupSetWith :: (Linear s o) => Compare o -> (o -> o -> o) -> s -> s
    groupSetWith :: Compare o -> (o -> o -> o) -> s -> s
    groupSetWith cmp mrg = fromList . groupSetWith cmp mrg . listL
    
    -- | Adding element to set.
    insertWith :: Compare o -> o -> s -> s
    insertWith f = unionWith f . single
    
    -- | Deleting element from set.
    deleteWith :: Compare o -> o -> s -> s
    deleteWith f = flip (differenceWith f) . single
    
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
    intersectionsWith =  (`foldl` Z) . intersectionWith
    
    -- | Fold by differenceWith.
    differencesWith :: (Foldable f) => Compare o -> f s -> s
    differencesWith =  (`foldl` Z) . differenceWith
    
    -- | Fold by unionWith.
    unionsWith :: (Foldable f) => Compare o -> f s -> s
    unionsWith =  (`foldl` Z) . unionWith
    
    -- | Fold by symdiffWith.
    symdiffsWith :: (Foldable f) => Compare o -> f s -> s
    symdiffsWith =  (`foldl` Z) . symdiffWith
    
    {- Сomparsion operations. -}
    
    -- | Compares sets on intersection.
    isIntersectsWith :: Compare o -> s -> s -> Bool
    isIntersectsWith f = not ... isDisjointWith f
    
    -- | Compares sets on disjoint.
    isDisjointWith :: Compare o -> s -> s -> Bool
    isDisjointWith f = isNull ... intersectionWith f
    
    -- | Same as 'elem', but can work faster. By default, uses 'find'.
    default isContainedIn :: (t o ~~ s, Foldable t) => Compare o -> o -> s -> Bool
    isContainedIn :: Compare o -> o -> s -> Bool
    isContainedIn f e = isJust . find (\ x -> f e x == EQ)
    
    -- | Сhecks whether a first set is a subset of second.
    default isSubsetWith :: (t o ~~ s, Foldable t) => Compare o -> s -> s -> Bool
    isSubsetWith :: Compare o -> s -> s -> Bool
    isSubsetWith f xs ys = all (\ x -> isContainedIn f x ys) xs
    
    -- | Generates a list of different subsets (including empty and equivalent).
    subsets :: (Ord o) => s -> [s]
    subsets =  subsequences . set
    
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

-- | Rank (* -> *) 'Set'.
type Set1 s e = Set (s e) e

{- Useful functions. -}

-- | The same as @setWith compare@.
{-# INLINE set #-}
set :: (Set s o, Ord o) => s -> s
set =  setWith compare

-- | Same as @insert compare@.
{-# INLINE insert #-}
insert :: (Set s o, Ord o) => o -> s -> s
insert =  insertWith compare

-- | Same as @deleteWith compare@.
{-# INLINE delete #-}
delete :: (Set s o, Ord o) => o -> s -> s
delete =  deleteWith compare

-- | Same as @intersectionWith compare@.
{-# INLINE (/\) #-}
(/\) :: (Set s o, Ord o) => s -> s -> s
(/\) =  intersectionWith compare

-- | Same as @unionWith compare@.
{-# INLINE (\/) #-}
(\/) :: (Set s o, Ord o) => s -> s -> s
(\/) =  unionWith compare

-- | Same as @differenceWith compare@.
{-# INLINE (\\) #-}
(\\) :: (Set s o, Ord o) => s -> s -> s
(\\) =  differenceWith compare

-- | Same as @symdiffWith compare@.
{-# INLINE (\^/) #-}
(\^/) :: (Set s o, Ord o) => s -> s -> s
(\^/) =  symdiffWith compare

-- | Same as @isDisjointWith compare@.
{-# INLINE (/?\) #-}
(/?\) :: (Set s o, Ord o) => s -> s -> Bool
(/?\) =  isDisjointWith compare

-- | Same as i@sIntersectsWith compare@.
{-# INLINE (\?/) #-}
(\?/) :: (Set s o, Ord o) => s -> s -> Bool
(\?/) =  isIntersectsWith compare

-- | Same as @isSubsetWith compare@.
{-# INLINE (\+/) #-}
(\+/) :: (Set s o, Ord o) => s -> s -> Bool
(\+/) =  isSubsetWith compare

-- | Same as @intersectionsWith compare@.
{-# INLINE intersections #-}
intersections :: (Foldable f, Set s o, Ord o) => f s -> s
intersections =  intersectionsWith compare

-- | Same as @unionsWith compare@.
{-# INLINE unions #-}
unions :: (Foldable f, Set s o, Ord o) => f s -> s
unions =  unionsWith compare

-- | Same as @differencesWith compare@.
{-# INLINE differences #-}
differences :: (Foldable f, Set s o, Ord o) => f s -> s
differences =  differencesWith compare

-- | Same as @symdiffsWith compare@.
{-# INLINE symdiffs #-}
symdiffs :: (Foldable f, Set s o, Ord o) => f s -> s
symdiffs =  symdiffsWith compare

-- | Same as @isContainedIn compare@.
{-# INLINE isSetElem #-}
isSetElem :: (Set s o, Ord o) => o -> s -> Bool
isSetElem =  isContainedIn compare

-- | Same as @lookupLTWith compare@.
{-# INLINE lookupLT #-}
lookupLT :: (Set s o, Ord o) => o -> s -> Maybe o
lookupLT =  lookupLTWith compare

-- | Same as @lookupGTWith compare@.
{-# INLINE lookupGT #-}
lookupGT :: (Set s o, Ord o) => o -> s -> Maybe o
lookupGT =  lookupGTWith compare

-- | Same as @lookupLEWith compare@.
{-# INLINE lookupLE #-}
lookupLE :: (Set s o, Ord o) => o -> s -> Maybe o
lookupLE =  lookupLEWith compare

-- | Same as @lookupGEWith compare@.
{-# INLINE lookupGE #-}
lookupGE :: (Set s o, Ord o) => o -> s -> Maybe o
lookupGE =  lookupGEWith compare

--------------------------------------------------------------------------------

instance Set [e] e
  where
    setWith f = sortBy f . nubBy ((EQ ==) ... f)
    
    insertWith f e es@(x : xs) = case e `f` x of {GT -> x : insertWith f e xs; LT -> e : es; EQ -> es}
    insertWith _ e _ = [e]
    
    deleteWith f e es@(x : xs) = case e `f` x of {GT -> x : deleteWith f e xs; LT -> es; EQ -> xs}
    deleteWith _ _ _ = []
    
    isContainedIn f e (x : xs) = case e `f` x of {GT -> isContainedIn f e xs; LT -> False; EQ -> True}
    isContainedIn _ _ _ = False
    
    intersectionWith f xs'@(x : xs) ys'@(y : ys) = case x `f` y of
      LT -> intersectionWith f xs  ys'
      GT -> intersectionWith f xs' ys
      EQ -> x : intersectionWith f xs ys
    intersectionWith _ _ _ = []
    
    unionWith f xs'@(x : xs) ys'@(y : ys) = case x `f` y of
      LT -> x : unionWith f xs  ys'
      EQ -> x : unionWith f xs  ys
      GT -> y : unionWith f xs' ys
    unionWith _ xs ys = xs ++ ys
    
    differenceWith f xs'@(x : xs) ys'@(y : ys) = case f x y of
      LT -> x : differenceWith f xs ys'
      EQ -> differenceWith f xs  ys
      GT -> differenceWith f xs' ys
    differenceWith _ xs _ = xs
    
    symdiffWith f xs'@(x : xs) ys'@(y : ys) = case f x y of
      EQ -> symdiffWith f xs ys
      LT -> x : symdiffWith f xs  ys'
      GT -> y : symdiffWith f xs' ys
    symdiffWith _ xs ys = xs ++ ys
    
    isIntersectsWith f xs'@(x : xs) ys'@(y : ys) = case f x y of
      LT -> isIntersectsWith f xs  ys'
      GT -> isIntersectsWith f xs' ys
      EQ -> True
    isIntersectsWith _ _ _ = False
    
    isDisjointWith f xs'@(x : xs) ys'@(y : ys) = case f x y of
      LT -> isDisjointWith f xs  ys'
      GT -> isDisjointWith f xs' ys
      EQ -> False
    isDisjointWith _ _ _ = True
    
    lookupLTWith f o (x : xs) = case o `f` x of {GT -> look x xs; _ -> Nothing}
      where
        look r (e : es) = case o `f` e of {GT -> look e es; _ -> Just r}
        look r _ = Just r
    lookupLTWith _ _ _ = Nothing
    
    lookupGTWith f o (x : xs) = case o `f` x of {LT -> Just x; _ -> look xs}
      where
        look (e : es) = case o `f` e of {LT -> Just e; _ -> look es}
        look _ = Nothing
    lookupGTWith _ _ _ = Nothing
    
    lookupLEWith f o (x : xs) = case o `f` x of {LT -> Nothing; _ -> look x xs}
      where
        look r (e : es) = case o `f` e of {LT -> Just r; _ -> look e es}
        look r _ = Just r
    lookupLEWith _ _ _ = Nothing
    
    lookupGEWith f o (x : xs) = case o `f` x of {GT -> look xs; _ -> Just x}
      where
        look (e : es) = case o `f` e of {GT -> look es; _ -> Just e}
        look _ = Nothing
    lookupGEWith _ _ _ = Nothing
    
    groupSetWith cmp f = map (foldr1 f) . groupBy ((== EQ) ... cmp) . sortBy cmp



