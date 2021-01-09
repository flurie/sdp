{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Bytes.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Bytes.ST" provides 'STBytes' - mutable lazy boxed array type.
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

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.SortM

import SDP.Templates.AnyBorder
import SDP.Prim.SBytes

default ()

--------------------------------------------------------------------------------

-- | 'STBytes' is mutable version of 'SDP.Bytes.Bytes'.
type STBytes s = AnyBorder (STBytes# s)




