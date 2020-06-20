{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ConstraintKinds #-}

{- |
    Module      :  SDP.SortM
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.SortM@ provides 'SortM' - class of sortable mutable structures.
-}

module SDP.SortM
  (
    -- * SortM
    SortM (..), SortM1,
    
    -- * Related functions
    sortM, sortMOn, mathsortM, mathsortMOn
  )
where

import Prelude ()
import SDP.SafePrelude

default ()

--------------------------------------------------------------------------------

-- | SortM is class of sortable mutable structures.
class SortM m s e | s -> m, s -> e
  where
    {-# MINIMAL sortMBy #-}
    
    -- | sortMBy is common sorting algorithm.
    sortMBy :: Compare e -> s -> m ()
    
    {- |
      mathsortMBy is sortMBy modiffication, that which is optimized for sorting
      data with a lot of repetitions.
    -}
    mathsortMBy :: Compare e -> s -> m ()
    mathsortMBy =  sortMBy

-- | Kind (* -> *) version of 'SortM'.
type SortM1 m s e = SortM m (s e) e

--------------------------------------------------------------------------------

-- | sortM is just synonym for @sortMBy compare@
sortM :: (SortM m s e, Ord e) => s -> m ()
sortM =  sortMBy compare

-- | Sort by comparing the results of a key function applied to each element.
sortMOn :: (SortM m s e, Ord o) => (e -> o) -> s -> m ()
sortMOn =  sortMBy . comparing

-- | mathsort is just synonym for @mathsortBy compare@
mathsortM :: (SortM m s e, Ord e) => s -> m ()
mathsortM =  mathsortMBy compare

-- | Math sort by comparing the results of a key function applied to each element.
mathsortMOn :: (SortM m s e, Ord o) => (e -> o) -> s -> m ()
mathsortMOn =  mathsortMBy . comparing

