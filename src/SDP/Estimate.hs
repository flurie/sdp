module SDP.Estimate
(
  (<=>), (<==>), (>.), (<.), (>=.), (<=.), emin, emax, eminimum, emaximum
)
where

import Prelude ()

import Data.Function
import Data.Foldable
import Data.Bool
import Data.Ord
import Data.Eq

import Data.Functor.Classes

--------------------------------------------------------------------------------

-- "spaceship operator" - infix version of compare.
(<=>) :: (Ord a) => a -> a -> Ordering
(<=>) = compare

class (Foldable e) => Estimate e
  where
    (<==>) :: e o -> e o -> Ordering
    (<==>) = (<=>) `on` length
    
    (>.), (<.), (<=.), (>=.) :: e o -> e o -> Bool
    
    xs >.  ys = case xs <==> ys of {GT ->  True; _ -> False}
    xs <.  ys = case xs <==> ys of {LT ->  True; _ -> False}
    xs <=. ys = case xs <==> ys of {GT -> False; _ ->  True}
    xs >=. ys = case xs <==> ys of {LT -> False; _ ->  True}

emin, emax :: (Estimate e) => e o -> e o -> e o

-- Estimated min.
emin xs ys = case xs <==> ys of {LT -> xs; _ -> ys}

-- Estimated max (not emacs).
emax xs ys = case xs <==> ys of {GT -> xs; _ -> ys}

eminimum, emaximum :: (Foldable f, Estimate e) => f (e o) -> e o

-- Estimated minimum.
eminimum = foldl1 emin

-- Estimated maximum.
emaximum = foldl1 emax

instance Estimate []
  where
    [] <==> [] = EQ
    [] <==> _  = LT
    _  <==> [] = GT
    (_ : xs) <==> (_ : ys) = xs <==> ys

