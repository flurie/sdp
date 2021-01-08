{-# LANGUAGE MagicHash #-}

{- |
    Module      :  Control.Concurent.SDP.TArray
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable
    
    "Control.Concurent.SDP.TArray" provides lazy boxed array of @stm@ 'TVar's.
-}
module Control.Concurent.SDP.TArray
(
  -- * TArray
  TArray, STM, TVar
)
where

import SDP.Templates.AnyBorder
import SDP.Prim.TArray

default ()

--------------------------------------------------------------------------------

-- | Lazy boxed array.
type TArray = AnyBorder TArray#



