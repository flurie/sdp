{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.Unrolled.IO
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Unrolled.IO" provides 'IOUnrolled' - mutable lazy boxed unrolled linked
    list type.
-}
module SDP.Unrolled.IO
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * MonadIO and IO Unrolled
  MIOUnrolled, IOUnrolled
)
where

import SDP.Templates.AnyBorder
import SDP.Unrolled.IOUnlist
import SDP.IndexedM
import SDP.SortM

default ()

--------------------------------------------------------------------------------

-- | 'MIOUnrolled' is mutable version of 'SDP.Unrolled.Unrolled'.
type MIOUnrolled io = AnyBorder (MIOUnlist io)

-- | 'IOUnrolled' is mutable version of 'SDP.Unrolled.Unrolled'.
type IOUnrolled = AnyBorder IOUnlist



