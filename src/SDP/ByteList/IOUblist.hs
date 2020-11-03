{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MagicHash, Unsafe #-}

{- |
    Module      :  SDP.ByteList.IOUblist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    "SDP.ByteList.IOUblist" provides 'IOUblist' - mutable strict unboxed
    unrolled linked list.
-}
module SDP.ByteList.IOUblist
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.Unboxed,
  module SDP.SortM,
  
  -- * IOUblist
  IOUblist
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM

import SDP.Templates.AnyChunks
import SDP.Prim.SBytes

import SDP.SortM.Tim

default ()

--------------------------------------------------------------------------------

-- | 'IOUblist' is mutable version of 'SDP.ByteList.Ublist.Ublist'.
type IOUblist = AnyChunks IOBytes#

instance (Unboxed e) => SortM IO (IOUblist e) e where sortMBy = timSortBy


