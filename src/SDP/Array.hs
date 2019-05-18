{- |
    Module      :  SDP.Array
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    SDP.Array provides immutable lazy array type. This implementation of array
    no much different from Data.Array (array), but incopatible with it.
    The main difference is the Index class instead of Ix.
-}

module SDP.Array
(
  module SDP.Array.Immutable
)
where

{-
  This is reexport of SDP.Array.Immutable (internal module).
-}

import Prelude ()
import SDP.Array.Immutable hiding ( STUArray )

