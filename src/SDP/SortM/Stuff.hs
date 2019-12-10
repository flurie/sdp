{- |
    Module      :  SDP.SortM.Stuff
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (import SDP.IndexedM).
    
    SDP.SortM.Stuff provides some sorting algorithms.
-}
module SDP.SortM.Stuff
{-# DEPRECATED "use SDP.SortM.Tim or SDP.SortM.Insertion instead" #-}
(
  module SDP.SortM.Tim,
  
  sorted, ascending, sortedM
)
where

import Prelude ()

import SDP.LinearM ( sorted, ascending, sortedM )

import SDP.SortM.Tim


