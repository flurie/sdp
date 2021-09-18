{-# LANGUAGE Safe, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds, QuantifiedConstraints, RankNTypes #-}

{- |
    Module      :  SDP.SortM
    Copyright   :  (c) Andrey Mulik 2019-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.SortM" provides 'SortM' - class of sortable mutable structures.
-}
module SDP.SortM
(
  -- * SortM
  SortM (..), SortM1, SortM2, SortM', SortM''
)
where

import Prelude ()
import SDP.SafePrelude

default ()

--------------------------------------------------------------------------------

-- | 'SortM' is class of sortable mutable structures.
class SortM m s e | s -> m, s -> e
  where
    {-# MINIMAL sortedMBy, sortMBy #-}
    
    {- |
      Checks if structure is already sorted. Should always return 'True' for
      structures with less than 2 elements.
    -}
    sortedMBy :: (e -> e -> Bool) -> s -> m Bool

    {- |
      Sort by comparing the results of a given function applied to each element.
      
      > sortedMOn = sortedMBy . on (<=)
    -}
    sortedMOn :: (Ord o) => (e -> o) -> s -> m Bool
    sortedMOn =  sortedMBy . on (<=)
    
    {- |
      Checks if the structure is sorted.
      
      > sortedM = sortedMBy (<=)
    -}
    sortedM :: (Ord e) => s -> m Bool
    sortedM =  sortedMBy (<=)
    
    -- | 'sortMBy' is common sorting algorithm.
    sortMBy :: Compare e -> s -> m ()
    
    {- |
      Sort by comparing the results of a key function applied to each element.
      
      > sortMOn = sortMBy . comparing
    -}
    sortMOn :: (Ord o) => (e -> o) -> s -> m ()
    sortMOn =  sortMBy . comparing
    
    -- | 'sortM' is just @'sortMBy' 'compare'@.
    sortM :: (Ord e) => s -> m ()
    sortM =  sortMBy compare

--------------------------------------------------------------------------------

-- | 'SortM' contraint for @(Type -> Type)@-kind types.
type SortM1 m s e = SortM m (s e) e

-- | 'SortM' contraint for @(Type -> Type -> Type)@-kind types.
type SortM2 m s i e = SortM m (s i e)

-- | 'SortM' contraint for @(Type -> Type)@-kind types.
type SortM' m s = forall e . SortM m (s e) e

-- | 'SortM' contraint for @(Type -> Type -> Type)@-kind types.
type SortM'' m s = forall i e . SortM m (s i e)




