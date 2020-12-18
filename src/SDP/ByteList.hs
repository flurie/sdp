{-# LANGUAGE Unsafe #-}

{- |
    Module      :  SDP.ByteList
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.ByteList" provides 'ByteList' - strict unboxed unrolled linked list.
-}
module SDP.ByteList
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Unboxed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * ByteList
  ByteList,
  
  -- * Ublist
  Ublist
)
where

import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Scan
import SDP.Set

import SDP.Templates.AnyBorder
import SDP.ByteList.Ublist

default ()

--------------------------------------------------------------------------------

-- | 'ByteList' is bordered strict unboxed unrolled linked list.
type ByteList = AnyBorder Ublist


