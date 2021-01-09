{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Bytes
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Bytes" provides 'Bytes' - immutable strict unboxed array type.
-}
module SDP.Bytes
(
  -- * Exports
  module SDP.Unboxed,
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Bytes
  Bytes
)
where

import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Scan
import SDP.Set

import SDP.Templates.AnyBorder
import SDP.Prim.SBytes

default ()

--------------------------------------------------------------------------------

-- | 'Bytes' - unboxed array.
type Bytes = AnyBorder SBytes#





