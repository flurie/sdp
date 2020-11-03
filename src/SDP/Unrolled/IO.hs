{- |
    Module      :  SDP.Unrolled.IO
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Unrolled.IO" provides 'IOUnrolled' - mutable lazy boxed unrolled linked
    list type.
-}
module SDP.Unrolled.IO
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * IOUnrolled
  IOUnrolled
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.SortM

import SDP.Templates.AnyBorder
import SDP.Unrolled.IOUnlist

default ()

--------------------------------------------------------------------------------

-- | 'IOUnrolled' is mutable version of 'SDP.Unrolled.Unrolled'.
type IOUnrolled = AnyBorder IOUnlist





