{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Bytes.IO
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Bytes.IO" provides 'IOBytes' - mutable strict unboxed array type.
-}
module SDP.Bytes.IO
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.Unboxed,
  module SDP.SortM,
  
  -- * IOBytes
  IOBytes
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM

import SDP.Templates.AnyBorder
import SDP.Prim.SBytes

default ()

--------------------------------------------------------------------------------

-- | 'IOBytes' is mutable version of 'SDP.Bytes.Bytes'.
type IOBytes = AnyBorder IOBytes#


