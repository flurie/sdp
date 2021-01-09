{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.SortM.Insertion
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.SortM.Insertion" provides insertion sort - simple sorting algorithm.
-}
module SDP.SortM.Insertion
(
  -- * Insertion Sort
  insertionSort, insertionSortBy, insertionSortOn
)
where

import SDP.SortM.Tim

