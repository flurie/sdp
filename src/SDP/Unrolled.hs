{-# LANGUAGE Unsafe #-}

{- |
    Module      :  SDP.Unrolled
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Unrolled" provides 'Unrolled' - lazy boxed unrolled linked list.
-}
module SDP.Unrolled
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Unrolled
  Unrolled,
  
  -- * Unlist
  Unlist
)
where

import SDP.Indexed
import SDP.Sort
import SDP.Scan
import SDP.Set

import SDP.Templates.AnyBorder
import SDP.Unrolled.Unlist

-- | 'Unrolled' is bordered unrolled linked list.
type Unrolled = AnyBorder Unlist



