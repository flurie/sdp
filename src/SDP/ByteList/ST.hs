{-# LANGUAGE Unsafe #-}

{- |
    Module      :  SDP.ByteList.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.ByteList.ST" provides 'STByteList' - mutable unboxed strict unrolled
    linked list.
-}
module SDP.ByteList.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.Unboxed,
  module SDP.SortM,
  
  -- * STByteList
  STByteList
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM

import SDP.Templates.AnyBorder
import SDP.ByteList.STUblist

default ()

--------------------------------------------------------------------------------

-- | 'STByteList' is mutable version of 'SDP.ByteList.ByteList'.
type STByteList s = AnyBorder (STUblist s)

