{-# LANGUAGE Unsafe, MagicHash, MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.ByteList.STUblist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.ByteList.STUblist" provides 'STUblist' - mutable unboxed strict
    unrolled linked list.
-}
module SDP.ByteList.STUblist
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.Unboxed,
  module SDP.SortM,
  
  -- * STUblist
  STUblist
)
where

import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM

import SDP.Templates.AnyChunks
import SDP.Prim.SBytes

default ()

--------------------------------------------------------------------------------

-- | This 'STUblist' is mutable version of 'SDP.ByteList.Ublist.Ublist'.
type STUblist s = AnyChunks (STBytes# s)



