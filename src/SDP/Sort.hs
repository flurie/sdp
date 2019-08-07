{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

{- |
    Module      :  SDP.Index
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    Stability   :  experimental
    
    This module provides Sort (class of sortable one-parametric types).
-}

module SDP.Sort ( Sort (..), sort, sortOn, mathsort, mathsortOn ) where

import Prelude ()
import SDP.SafePrelude

import qualified Data.List as L

default ()

--------------------------------------------------------------------------------

-- | Sort - is class of types that can be sorted.
class Sort s e | s -> e
  where
    -- | sortBy function is common sorting algorithm.
    sortBy :: (e -> e -> Ordering) -> s -> s
    
    {- |
      mathsortBy is a sortBy modification that counts duplicate elements, sorts
      a set of vectors and merges them.
    -}
    mathsortBy :: (e -> e -> Ordering) -> s -> s

-- | sort is just synonym for sortBy compare
sort   :: (Sort s e, Ord e) => s -> s
sort es = sortBy compare es

-- | Sort by comparing the results of a key function applied to each element.
sortOn :: (Sort s e, Ord o) => (e -> o) -> s -> s
sortOn f es = sortBy (\ x y -> f x `compare` f y) es

-- | mathsort is just synonym for mathsortBy compare
mathsort   :: (Sort s e, Ord e) => s -> s
mathsort es = mathsortBy compare es

-- | Math sort by comparing the results of a key function applied to each element.
mathsortOn :: (Sort s e, Ord o) => (e -> o) -> s -> s
mathsortOn f es = mathsortBy (\ x y -> f x `compare` f y) es

--------------------------------------------------------------------------------

instance Sort [a] a
  where
    sortBy     f es = L.sortBy f es
    
    {- |
      This version of mathsort doesn't create duplicates, just split list on
      subsequences of equals.
    -}
    mathsortBy f es = L.concat sorted
      where
        sorted = sortBy (\ x y -> L.head x `f` L.head y) $ split' es
        
        split' [] = []
        split' xs@(x : _) = y : split' ys
          where
            (y, ys) = L.partition (\ e -> x `f` e == EQ) xs

