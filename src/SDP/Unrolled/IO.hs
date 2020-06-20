{- |
    Module      :  SDP.Unrolled.IO
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.Unrolled.IO@ provides 'IOUnrolled' - mutable strict unboxed unrolled
    linked list type.
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

import SDP.Templates.AnyBorder
import SDP.Unrolled.IOUnlist

import SDP.IndexedM
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'IOUnrolled' is mutable version of Unrolled.
type IOUnrolled = AnyBorder IOUnlist




