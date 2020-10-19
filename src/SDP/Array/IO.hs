{-# LANGUAGE MagicHash #-}

{- |
    Module      :  SDP.Array.IO
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.Array.IO@ provides 'IOArray' - mutable lazy boxed array type.
-}
module SDP.Array.IO
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * IOArray
  IOArray
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.SortM

import SDP.Templates.AnyBorder
import SDP.Prim.SArray

default ()

--------------------------------------------------------------------------------

-- | 'IOArray' is mutable version of Array.
type IOArray = AnyBorder IOArray#




