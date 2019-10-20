{- |
    Module      :  SDP.SortM.Insertion
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (require SDP.IndexedM).
    
    SDP.SortM.Insertion provides insertion sort - simple sorting algorithm.
-}
module SDP.SortM.Insertion
(
  -- * Insertion Sort
  insertionSort, insertionSortBy, insertionSortOn
)
where

import Prelude ()

import SDP.SortM.Tim

