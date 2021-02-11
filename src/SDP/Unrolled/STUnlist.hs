{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Unrolled.STUnlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Unrolled.STUnlist" provides 'STUnlist' - mutable boxed lazy unrolled
    linked list.
-}
module SDP.Unrolled.STUnlist
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STUnlist
  STUnlist
)
where

import SDP.Templates.AnyChunks
import SDP.Prim.SArray
import SDP.IndexedM
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'STUnlist' is mutable version of 'SDP.Unrolled.Unlist.Unlist'.
type STUnlist s = AnyChunks (STArray# s)

