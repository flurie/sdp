{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ConstraintKinds #-}
{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.SortM
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.SortM" provides 'SortM' - class of sortable mutable structures.
-}
module SDP.SortM
  (
    -- * SortM
    SortM (..), SortM1, sortM, sortMOn, sortedM, sortedMOn
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
    
    -- | 'sortMBy' is common sorting algorithm.
    sortMBy :: Compare e -> s -> m ()

--------------------------------------------------------------------------------

-- | Kind @(* -> *)@ version of 'SortM'.
type SortM1 m s e = SortM m (s e) e

--------------------------------------------------------------------------------

-- | Checks if the structure is sorted.
sortedM :: (SortM m s e, Ord e) => s -> m Bool
sortedM =  sortedMBy (<=)

-- | Sort by comparing the results of a given function applied to each element.
sortedMOn :: (SortM m s e, Ord o) => (e -> o) -> s -> m Bool
sortedMOn =  sortedMBy . (on (<=))

-- | 'sortM' is just @'sortMBy' 'compare'@
sortM :: (SortM m s e, Ord e) => s -> m ()
sortM =  sortMBy compare

-- | Sort by comparing the results of a key function applied to each element.
sortMOn :: (SortM m s e, Ord o) => (e -> o) -> s -> m ()
sortMOn =  sortMBy . comparing



