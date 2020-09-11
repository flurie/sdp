{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MagicHash, Unsafe #-}

{- |
    Module      :  SDP.Unrolled.IOUnlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.Unrolled.IOUnlist@ provides 'IOUnlist' - mutable lazy boxed unrolled
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

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.SortM

import SDP.Templates.AnyChunks
import SDP.Prim.SArray

import SDP.SortM.Tim

default ()

--------------------------------------------------------------------------------

-- | IOUnlist is mutable version of Unlist.
type IOUnlist = AnyChunks IOArray#

instance SortM IO (IOUnlist e) e where sortMBy = timSortBy




