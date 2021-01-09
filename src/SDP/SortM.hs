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
    SortM (..), SortM1, sortM, sortMOn, mathsortM, mathsortMOn
  )
where

import Prelude ()
import SDP.SafePrelude

default ()

--------------------------------------------------------------------------------

-- | 'SortM' is class of sortable mutable structures.
class SortM m s e | s -> m, s -> e
  where
    -- | 'sortMBy' is common sorting algorithm.
    sortMBy :: Compare e -> s -> m ()
    
    {- |
      'mathsortMBy' is sortMBy modiffication, that which is optimized for
      sorting data with a lot of repetitions.
    -}
    mathsortMBy :: Compare e -> s -> m ()
    mathsortMBy =  sortMBy

-- | Kind (* -> *) version of 'SortM'.
type SortM1 m s e = SortM m (s e) e

--------------------------------------------------------------------------------

-- | 'sortM' is just @'sortMBy' 'compare'@
sortM :: (SortM m s e, Ord e) => s -> m ()
sortM =  sortMBy compare

-- | Sort by comparing the results of a key function applied to each element.
sortMOn :: (SortM m s e, Ord o) => (e -> o) -> s -> m ()
sortMOn =  sortMBy . comparing

-- | 'mathsortM' is just @'mathsortMBy' 'compare'@
mathsortM :: (SortM m s e, Ord e) => s -> m ()
mathsortM =  mathsortMBy compare

-- | Math sort by comparing the results of a key function applied to each element.
mathsortMOn :: (SortM m s e, Ord o) => (e -> o) -> s -> m ()
mathsortMOn =  mathsortMBy . comparing

