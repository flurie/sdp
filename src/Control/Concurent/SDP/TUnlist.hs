{-# LANGUAGE MagicHash #-}

{- |
    Module      :  Control.Concurent.SDP.TUnlist
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable
    
    "Control.Concurent.SDP.TUnlist" provides lazy boxed unrolled linked list of
    @stm@ 'TVar's.
-}
module Control.Concurent.SDP.TUnlist
(
  -- * TUnlist
  TUnlist, STM, TVar
)
where

import SDP.Templates.AnyChunks
import SDP.Prim.TArray

default ()

--------------------------------------------------------------------------------

-- | Lazy boxed unrolled linked list.
type TUnlist = AnyChunks TArray#


