{-
  NOTE: This is an internal module for typical functions.
  Its contents may be changed or removed without reference to the changelog.
-}

module SDP.Simple
(
  (?), (?:), fsts, snds,
  cmpfst, cmpsnd, eqfst, eqsnd,
  
  module Control.Exception.SDP,
  
  module Data.Function,
  module Data.Default,
  module Data.Maybe,
  module Data.Bool,
  module Data.Ord,
  module Data.Eq,
  
  Bounded (..), Enum (..),
  onTup, onTup3, onTup4,
  sort', nub',
  fst, snd
)

where

import qualified Data.List as L

import Control.Exception.SDP

import Data.Function
import Data.Default
import Data.Maybe
import Data.Bool
import Data.Ord 
import Data.Eq

-- ternary operator
(?)   :: Bool -> a -> a -> a
(?) pred t e = if pred then t else e

-- conditional toMaybe
(?:)  :: (a -> Bool) -> (a -> b) -> a -> Maybe b
(?:) pred f = \ a -> if pred a then Nothing else Just (f a)
-- >>> odd ?: (`div` 2) $ 1
-- Nothing
-- >>> odd ?: (`div` 2) $ 2
-- Just 1

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

sort' :: (Ord i) => [(i, e)] -> [(i, e)]
sort' = L.sortBy cmpfst

nub' :: (Eq i)  => [(i, e)] -> [(i, e)]
nub'  = L.nubBy eqfst

minMax :: (Ord i) => i -> i -> (i, i)
minMax x y = (y > x) ? (x, y) $ (y, x)

onTup :: (a -> b) -> (a, a) -> (b, b)
onTup f (x, y) = (f x, f y)

onTup3 :: (a -> b) -> (a, a, a) -> (b, b, b)
onTup3 f (x, y, z) = (f x, f y, f z)

onTup4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
onTup4 f (x, y, z, u) = (f x, f y, f z, f u)
