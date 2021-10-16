{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe, CPP, ConstraintKinds #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Index
    Copyright   :  (c) Andrey Mulik 2019-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Sort" provides 'Sort' - class of sortable immutable structures.
-}
module SDP.Sort
(
  -- * Sort
  Sort (..), Sort1, Sort2,
  
#if __GLASGOW_HASKELL__ >= 806
  -- * Rank 2 quantified constraints
  -- | GHC 8.6.1+ only
  Sort', Sort''
#endif
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Zip

import qualified Data.List as L

default ()

--------------------------------------------------------------------------------

-- | 'Sort' is class of types that can be sorted.
class Sort s e | s -> e
  where
    {-# MINIMAL sortBy, sortedBy #-}
    
    {- |
      Checks if structure is already sorted. Should always return 'True' for
      structures with less than 2 elements.
    -}
    sortedBy :: (e -> e -> Bool) -> s -> Bool
    
    {- |
      Sort by comparing the results of a given function applied to each element.
      
      > sortedOn = sortedBy . on (<=)
    -}
    sortedOn :: (Ord o) => (e -> o) -> s -> Bool
    sortedOn =  sortedBy . on (<=)
    
    {- |
      Checks if the structure is 'sorted'.
      
      > sorted = sortedBy (<=)
    -}
    sorted :: (Ord e) => s -> Bool
    sorted =  sortedBy (<=)
    
    -- | 'sortBy' function is common sorting algorithm.
    sortBy :: Compare e -> s -> s
    
    {- |
      Sort by comparing the results of a given function applied to each element.
      
      > sortOn = sortBy . comparing
    -}
    sortOn :: (Ord o) => (e -> o) -> s -> s
    sortOn =  sortBy . comparing
    
    -- | 'sort' is just @'sortBy' 'compare'@.
    sort :: (Ord e) => s -> s
    sort =  sortBy compare

--------------------------------------------------------------------------------

instance Sort [a] a
  where
    sortedBy f (e : es) = all2 f (e : es) es
    sortedBy _    []    = True
    
    sortBy = L.sortBy

--------------------------------------------------------------------------------

-- | 'Sort' contraint for @(Type -> Type)@-kind types.
type Sort1 rep e = Sort (rep e)

-- | 'Sort' contraint for @(Type -> Type -> Type)@-kind types.
type Sort2 rep i e = Sort (rep i e)

#if __GLASGOW_HASKELL__ >= 806

-- | 'Sort' contraint for @(Type -> Type)@-kind types.
type Sort' rep = forall e . Sort (rep e)

-- | 'Sort' contraint for @(Type -> Type -> Type)@-kind types.
type Sort'' rep = forall i e . Sort (rep i e)

#endif



