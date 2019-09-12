{- |
    Module      :  SDP.SortM.Insertion
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (imports SDP.IndexedM).
    
    SDP.SortM.Stuff provides some sorting algorithms.
-}
module SDP.SortM.Insertion
(
  insertionSort, insertionSortBy, insertionSortOn
)
where

import Prelude ()

import SDP.SortM.Tim


