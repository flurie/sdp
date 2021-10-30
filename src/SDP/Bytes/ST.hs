{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Bytes.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Bytes.ST" provides 'STBytes' - mutable strict unboxed array type.
-}
module SDP.Bytes.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STBytes
  STBytes
)
where

import SDP.Templates.AnyBorder
import SDP.Prim.SBytes
import SDP.IndexedM
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'STBytes' is mutable version of 'SDP.Bytes.Bytes'.
type STBytes s = AnyBorder (STBytes# s)


