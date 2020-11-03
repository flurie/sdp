{-# LANGUAGE Unsafe #-}

{- |
    Module      :  SDP.Unrolled.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Unrolled.ST" provides 'STUnrolled' - mutable boxed lazy unrolled linked
    list.
-}
module SDP.Unrolled.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STUnrolled
  STUnrolled
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.SortM

import SDP.Templates.AnyBorder
import SDP.Unrolled.STUnlist

default ()

--------------------------------------------------------------------------------

-- | 'STUnrolled' is mutable version 'SDP.Unrolled.Unrolled'.
type STUnrolled s = AnyBorder (STUnlist s)



