{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.ByteList.IO
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.ByteList.IO" provides 'IOByteList' - mutable strict unboxed unrolled
    linked list type.
-}
module SDP.ByteList.IO
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.Unboxed,
  module SDP.SortM,
  
  -- * IOByteList
  IOByteList
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM

import SDP.Templates.AnyBorder
import SDP.ByteList.IOUblist

default ()

--------------------------------------------------------------------------------

-- | 'IOByteList' is mutable version of 'SDP.ByteList.ByteList'.
type IOByteList = AnyBorder IOUblist

