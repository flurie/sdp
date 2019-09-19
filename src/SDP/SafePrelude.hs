{-# OPTIONS_HADDOCK ignore-exports #-}

{- |
    Module      :  SDP.SafePrelude
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
  
  Module of common definitions.
-}
module SDP.SafePrelude
(
  module Control.Applicative,
  module Control.Monad,
  module Data.Foldable,
  module SDP.Estimate,
  module Prelude,
  
  (?)
)
where

import Prelude hiding
  (
    -- defined in SDP.Zip and Data.List (originally in GHC.List)
    zip, zip3, zipWith, zipWith3,
    
    -- defined in SDP.Scan and Data.List (originally in GHC.List)
    scanl, scanr, scanl1, scanr1,
    
    -- defined in SDP.Linear and Data.List (originally in GHC.List)
    head, tail, init, last, take, drop, (!!), (++), reverse, filter,
    concat, concatMap, replicate, takeWhile, dropWhile
  )

import Control.Applicative
import Control.Monad

import Data.Foldable hiding ( concat, concatMap )
import SDP.Estimate

infixr 1 ? -- Lowest priority, compatible with infixr 0 $

default ()

--------------------------------------------------------------------------------

-- | Ternary operator.
-- > odd 1 ? "is True" $ "is False"
-- "is True"
{-# INLINE (?) #-}
(?) :: Bool -> a -> a -> a
(?) p t e = if p then t else e

