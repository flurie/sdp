{-
  NOTE: This is an internal module for typical functions and imports.
  Its contents may be changed or removed without reference to the changelog.
-}
module SDP.Internal.Commons
(
  module Control.Exception.SDP,
  
  module Data.Bifunctor,
  module Data.Function,
  module Data.Default,
  module Data.Coerce,
  module Data.String,
  module Data.Maybe,
  module Data.Char,
  module Data.Bool,
  module Data.Ord,
  module Data.Eq,
  
  Bounded (..), Enum (..),
  
  fst, snd, fsts, snds, both,
  
  (?>), bindM2, (>>=>)
)

where

import Prelude ()
import SDP.SafePrelude

import Control.Exception.SDP

import Data.Bifunctor
import Data.Function
import Data.Default
import Data.String
import Data.Coerce
import Data.Maybe
import Data.Char
import Data.Bool
import Data.Ord
import Data.Eq

default ()

--------------------------------------------------------------------------------

fsts :: (Functor f) => f (a, b) -> f a
fsts =  fmap fst

snds :: (Functor f) => f (a, b) -> f b
snds =  fmap snd

both :: (a -> b) -> (a, a) -> (b, b)
both =  uncurry . on (,)

--------------------------------------------------------------------------------

-- Monadic conditional toMaybe.
(?>) :: (Monad m) => (a -> m Bool) -> (a -> m b) -> a -> m (Maybe b)
p ?> f = \ a -> do b <- p a; if b then Just <$> f a else return Nothing

-- Composition of liftM2 and join.
bindM2 :: (Monad m) => m a -> m b -> (a -> b -> m c) -> m c
bindM2 ma mb kl2 = join $ liftM2 kl2 ma mb

(>>=>) :: (Monad m) => (a -> b -> m c) -> (c -> m d) -> a -> b -> m d
k1 >>=> k2 = (>=> k2) . k1

