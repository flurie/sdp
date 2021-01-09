{-# LANGUAGE Safe #-}

{- |
    Module      :  Control.Concurent.SDP.TUnrolled
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "Control.Concurent.SDP.TUnrolled" provides lazy boxed unrolled linked list
    of @stm@ 'TVar's.
-}
module Control.Concurent.SDP.TUnrolled
(
  -- * TUnrolled
  TUnrolled, STM, TVar
)
where

import Control.Concurent.SDP.TUnlist
import SDP.Templates.AnyBorder

default ()

--------------------------------------------------------------------------------

-- | Lazy boxed unrolled libked list.
type TUnrolled = AnyBorder TUnlist


