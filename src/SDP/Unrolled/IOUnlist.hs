{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MagicHash, Unsafe #-}

{- |
    Module      :  SDP.Unrolled.IOUnlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Unrolled.IOUnlist" provides 'IOUnlist' - mutable lazy boxed unrolled
    linked list.
-}
module SDP.Unrolled.IOUnlist
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * IOUnlist
  IOUnlist
)
where

import SDP.IndexedM
import SDP.SortM

import SDP.Templates.AnyChunks
import SDP.Prim.SArray

default ()

--------------------------------------------------------------------------------

-- | 'IOUnlist' is mutable version of 'SDP.Unrolled.Unlist.Unlist'.
type IOUnlist = AnyChunks IOArray#





