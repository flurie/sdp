{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

{- |
    Module      :  SDP.SortM
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    Stability   :  stable
    
    SDP.SortM provides SortM - class of sortable immutable structures.
-}

module SDP.SortM ( SortM (..), sortM, sortMOn, mathsortM, mathsortMOn ) where

import Prelude ()
import SDP.SafePrelude

import SDP.Simple

default ()

--------------------------------------------------------------------------------

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
    mathsortMBy cmp es = sortMBy cmp es

-- | sortM is just synonym for sortMBy compare
sortM   :: (SortM m s e, Ord e) => s -> m ()
sortM es = sortMBy compare es

-- | Sort by comparing the results of a key function applied to each element.
sortMOn :: (SortM m s e, Ord o) => (e -> o) -> s -> m ()
sortMOn f es = sortMBy (compare `on` f) es

-- | mathsort is just synonym for mathsortBy compare
mathsortM   :: (SortM m s e, Ord e) => s -> m ()
mathsortM es = mathsortMBy compare es

-- | Math sort by comparing the results of a key function applied to each element.
mathsortMOn :: (SortM m s e, Ord o) => (e -> o) -> s -> m ()
mathsortMOn f es = mathsortMBy (compare `on` f) es


