{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Array.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Array.ST" provides 'STArray' - mutable lazy boxed array type.
-}
module SDP.Array.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STArray
  STArray
)
where

import SDP.Templates.AnyBorder
import SDP.Prim.SArray
import SDP.IndexedM
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'STArray' is mutable version of 'SDP.Array.Array'.
type STArray s = AnyBorder (STArray# s)


