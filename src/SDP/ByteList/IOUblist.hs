{-# LANGUAGE Safe, MagicHash #-}

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
  
  -- * MonadIO and IO Ublists
  MIOUblist, IOUblist
)
where

import SDP.Templates.AnyChunks
import SDP.Prim.SBytes
import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'MIOUblist' is mutable version of 'SDP.ByteList.Ublist.Ublist'.
type MIOUblist io = AnyChunks (MIOBytes# io)

-- | 'IOUblist' is mutable version of 'SDP.ByteList.Ublist.Ublist'.
type IOUblist = AnyChunks IOBytes#

