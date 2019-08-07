{-
  NOTE: This is an internal module for typical functions and imports.
  Its contents may be changed or removed without reference to the changelog.
-}

module SDP.Simple
(
  module Control.Exception.SDP,
  
  module Data.Function,
  module Data.Default,
  module Data.Maybe,
  module Data.Bool,
  module Data.Ord,
  module Data.Eq,
  
  Bounded (..), Enum (..),
  
  Compare,
  
  fst, snd,
  
  (?:), minMax,
  
  fsts, snds,
  
  cmpfst, cmpsnd, eqfst, eqsnd,
  
  (?>), bindM2, bindM3, bindM4,
  
  minrunTS
)

where

import Control.Exception.SDP

import Data.Function
import Data.Default
import Data.Maybe
import Data.Bits
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
fsts = fmap fst

-- Gives all second elements.
snds :: (Functor f) => f (a, b) -> f b
snds = fmap snd

--------------------------------------------------------------------------------

{- Common comparators. -}

-- Compare tuples by first elements.
cmpfst :: (Ord a) => (a, b) -> (a, b) -> Ordering
cmpfst = (compare `on` fst)

-- Compare tuples by second elements.
cmpsnd :: (Ord b) => (a, b) -> (a, b) -> Ordering
cmpsnd = (compare `on` snd)

-- Compare tuples by first elements.
eqfst :: (Eq a) => (a, b) -> (a, b) -> Bool
eqfst = on (==) fst

-- Compare tuples by second elements.
eqsnd :: (Eq b) => (a, b) -> (a, b) -> Bool
eqsnd = on (==) snd

--------------------------------------------------------------------------------

-- Monadic conditional toMaybe.
(?>) :: (Monad m) => (a -> m Bool) -> (a -> m b) -> a -> m (Maybe b)
p ?> f = \ a -> p a >>= \ b -> if b then Just <$> f a else return Nothing

-- Composition of liftM2 and (>>=).
bindM2 :: (Monad m) => m a -> m b -> (a -> b -> m c) -> m c
bindM2 ma mb kl2 = do a <- ma; b <- mb; kl2 a b

-- Composition of liftM3 and (>>=).
bindM3 :: (Monad m) => m a -> m b -> m c -> (a -> b -> c -> m d) -> m d
bindM3 ma mb mc kl3 = do a <- ma; b <- mb; c <- mc; kl3 a b c

-- Composition of liftM4 and (>>=).
bindM4 :: (Monad m) => m a -> m b -> m c -> m d -> (a -> b -> c -> d -> m e) -> m e
bindM4 ma mb mc md kl4 = do a <- ma; b <- mb; c <- mc; d <- md; kl4 a b c d

--------------------------------------------------------------------------------

type Compare e = e -> e -> Ordering

--------------------------------------------------------------------------------

{- Sorting stuff. -}

-- nimrunTS returns Timsort chunk size.
minrunTS :: Int -> Int
minrunTS i = mr i 0
  where
    mr :: Int -> Int -> Int
    mr n r = if n >= 64 then mr (n `shiftR` 1) (n .&. 1) else n + r

