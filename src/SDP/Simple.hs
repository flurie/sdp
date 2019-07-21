{-
  NOTE: This is an internal module for typical functions and imports.
  Its contents may be changed or removed without reference to the changelog.
-}

module SDP.Simple
(
  (?:), (?>), fsts, snds,
  cmpfst, cmpsnd, eqfst, eqsnd,
  
  module Control.Exception.SDP,
  
  module Data.Function,
  module Data.Default,
  module Data.Maybe,
  module Data.Bool,
  module Data.Ord,
  module Data.Eq,
  
  Bounded (..), Enum (..),
  minMax, fst, snd
)

where

import Control.Exception.SDP

import Data.Function
import Data.Default
import Data.Maybe
import Data.Bool
import Data.Ord 
import Data.Eq

default ()

-- conditional toMaybe
(?:) :: (a -> Bool) -> (a -> b) -> a -> Maybe b
p ?: f = \ a -> if p a then Nothing else Just (f a)
-- >>> odd ?: (`div` 2) $ 1
-- Nothing
-- >>> odd ?: (`div` 2) $ 2
-- Just 1

-- monadic conditional toMaybe
(?>) :: (Monad m) => (a -> m Bool) -> (a -> m b) -> a -> m (Maybe b)
p ?> f = \ a -> p a >>= \ b -> if b then Just <$> f a else return Nothing

-- gives all first elements
fsts :: (Functor f) => f (a, b) -> f a
fsts = fmap fst

-- gives all second elements
snds :: (Functor f) => f (a, b) -> f b
snds = fmap snd

-- compare tuples by first elements
cmpfst :: (Ord a) => (a, b) -> (a, b) -> Ordering
cmpfst = (compare `on` fst)

-- compare tuples by second elements
cmpsnd :: (Ord b) => (a, b) -> (a, b) -> Ordering
cmpsnd = (compare `on` snd)

-- compare tuples by first elements
eqfst :: (Eq a) => (a, b) -> (a, b) -> Bool
eqfst = on (==) fst

-- compare tuples by second elements
eqsnd :: (Eq b) => (a, b) -> (a, b) -> Bool
eqsnd = on (==) snd

minMax :: (Ord i) => i -> i -> (i, i)
minMax x y = if y > x then (x, y) else (y, x)
