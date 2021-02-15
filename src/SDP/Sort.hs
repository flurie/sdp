{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.Index
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Sort" provides 'Sort' - class of sortable immutable structures.
-}
module SDP.Sort
(
  -- * Sort
  Sort (..), sort, sortOn, sorted, sortedOn
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
    
    -- | 'sortBy' function is common sorting algorithm.
    sortBy :: Compare e -> s -> s

instance Sort [a] a
  where
    sortedBy f (e : es) = all2 f (e : es) es
    sortedBy _    []    = True
    
    sortBy = L.sortBy

--------------------------------------------------------------------------------

-- | Checks if the structure is 'sorted'.
sorted :: (Sort s e, Ord e) => s -> Bool
sorted =  sortedBy (<=)

-- | Sort by comparing the results of a given function applied to each element.
sortedOn :: (Sort s e, Ord o) => (e -> o) -> s -> Bool
sortedOn =  sortedBy . (on (<=))

-- | 'sort' is just @'sortBy' 'compare'@
sort :: (Sort s e, Ord e) => s -> s
sort =  sortBy compare

-- | Sort by comparing the results of a given function applied to each element.
sortOn :: (Sort s e, Ord o) => (e -> o) -> s -> s
sortOn =  sortBy . comparing



