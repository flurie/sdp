{-
  NOTE: This is an internal module for typical functions and imports.
  Its contents may be changed or removed without reference to the changelog.
-}
module SDP.Internal.Commons
(
  module Control.Exception.SDP,
  
  module Data.Function,
  module Data.Default,
  module Data.Maybe,
  module Data.Bool,
  module Data.Ord,
  module Data.Eq,
  
  Bounded (..), Enum (..),
  
  fst, snd,
  
  (?:), minMax,
  
  fsts, snds,
  
  (?>), bindM2, bindM3, bindM4
)

where

import Control.Exception.SDP
import Control.Monad

import Data.Function
import Data.Default
import Data.Maybe
import Data.Bool
import Data.Ord
import Data.Eq

default ()

--------------------------------------------------------------------------------

{- Simplest. -}

-- Conditional toMaybe.
(?:) :: (a -> Bool) -> (a -> b) -> a -> Maybe b
p ?: f = \ a -> if p a then Nothing else Just (f a)
-- >>> odd ?: (`div` 2) $ 1
-- Nothing
-- >>> odd ?: (`div` 2) $ 2
-- Just 1

minMax :: (Ord i) => i -> i -> (i, i)
minMax x y = if y > x then (x, y) else (y, x)

--------------------------------------------------------------------------------

{- Functors. -}

-- Gives all first elements.
fsts :: (Functor f) => f (a, b) -> f a
fsts =  fmap fst

-- Gives all second elements.
snds :: (Functor f) => f (a, b) -> f b
snds =  fmap snd

--------------------------------------------------------------------------------

-- Monadic conditional toMaybe.
(?>) :: (Monad m) => (a -> m Bool) -> (a -> m b) -> a -> m (Maybe b)
p ?> f = \ a -> p a >>= \ b -> if b then Just <$> f a else return Nothing

-- Composition of liftM2 and join.
bindM2 :: (Monad m) => m a -> m b -> (a -> b -> m c) -> m c
bindM2 ma mb kl2 = join $ liftM2 kl2 ma mb

-- Composition of liftM3 and (>>=).
bindM3 :: (Monad m) => m a -> m b -> m c -> (a -> b -> c -> m d) -> m d
bindM3 ma mb mc kl3 = join $ liftM3 kl3 ma mb mc

-- Composition of liftM4 and (>>=).
bindM4 :: (Monad m) => m a -> m b -> m c -> m d -> (a -> b -> c -> d -> m e) -> m e
bindM4 ma mb mc md kl4 = join $ liftM4 kl4 ma mb mc md

