{-# LANGUAGE Safe, MagicHash #-}

{- |
    Module      :  SDP.Array.IO
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Array.IO provides 'IOArray' - mutable lazy boxed array type.
-}
module SDP.Array.IO
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * MonadIO and IO Array
  MIOArray, IOArray
)
where

import SDP.Templates.AnyBorder
import SDP.Prim.SArray
import SDP.IndexedM
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'MIOArray' is mutable version of 'SDP.Array.Array'.
type MIOArray io = AnyBorder (MIOArray# io)

-- | 'IOArray' is mutable version of 'SDP.Array.Array'.
type IOArray = AnyBorder IOArray#




