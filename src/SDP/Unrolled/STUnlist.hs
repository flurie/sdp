{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, BangPatterns #-}

{- |
    Module      :  SDP.Unrolled.STUnlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Unrolled.STUnlist@ provides 'STUnlist' - mutable boxed lazy unrolled
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

import Prelude ()
import SDP.SafePrelude

import SDP.Templates.AnyChunks
import SDP.Prim.SArray

import SDP.IndexedM
import SDP.SortM

import Control.Monad.ST
import SDP.SortM.Tim

default ()

--------------------------------------------------------------------------------

-- | This STUnlist is mutable version of Unlist.
type STUnlist s = AnyChunks (STArray# s)

instance SortM (ST s) (STUnlist s e) e where sortMBy = timSortBy

