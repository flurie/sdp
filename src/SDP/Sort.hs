{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{- |
    Module      :  SDP.Index
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.Sort@ provides 'Sort' - class of sortable immutable structures.
-}
module SDP.Sort
  (
    -- * Sort
    Sort (..),
    
    -- * Related functions
    sort, sortOn, mathsort, mathsortOn
  )
where

import Prelude ()
import SDP.SafePrelude

import qualified Data.List as L

default ()

--------------------------------------------------------------------------------

-- | Sort - is class of types that can be sorted.
class Sort s e | s -> e
  where
    {-# MINIMAL sortBy #-}
    
    -- | sortBy function is common sorting algorithm.
    sortBy :: Compare e -> s -> s
    
    {- |
      mathsortBy is sortBy modiffication, that which is optimized for sorting
      data with a lot of repetitions.
    -}
    mathsortBy :: Compare e -> s -> s
    mathsortBy =  sortBy

-- | sort is just synonym for @sortBy compare@
sort :: (Sort s e, Ord e) => s -> s
sort =  sortBy compare

-- | Sort by comparing the results of a key function applied to each element.
sortOn :: (Sort s e, Ord o) => (e -> o) -> s -> s
sortOn =  sortBy . comparing

-- | mathsort is just synonym for @mathsortBy compare@
mathsort :: (Sort s e, Ord e) => s -> s
mathsort =  mathsortBy compare

-- | Math sort by comparing the results of a key function applied to each element.
mathsortOn :: (Sort s e, Ord o) => (e -> o) -> s -> s
mathsortOn =  mathsortBy . comparing

--------------------------------------------------------------------------------

instance Sort [a] a where sortBy = L.sortBy

