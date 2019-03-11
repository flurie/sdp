module SDP.Estimate
(
  (<=>), (<==>), (>.), (<.), (>=.), (<=.), emin, emax, eminimum, emaximum
)
where

import Prelude ( Ord (..), Foldable (..), Ordering (..), Bool (..) )
import Data.Functor.Classes

--------------------------------------------------------------------------------

-- "spaceship operator" - infix version of compare.
(<=>) :: (Ord a) => a -> a -> Ordering
(<=>) = compare

-- infix version of compare1
(<==>) :: (Ord1 e, Ord o) => e o -> e o -> Ordering
xs <==> ys = liftCompare compare xs ys

(>.), (<.), (<=.), (>=.) :: (Ord1 e, Ord o) => e o -> e o -> Bool

xs >.  ys = case xs <==> ys of {GT ->  True; _ -> False}
xs <.  ys = case xs <==> ys of {LT ->  True; _ -> False}
xs <=. ys = case xs <==> ys of {GT -> False; _ ->  True}
xs >=. ys = case xs <==> ys of {LT -> False; _ ->  True}

emin, emax :: (Ord1 e, Ord o) => e o -> e o -> e o

-- Estimated min.
emin xs ys = case xs <==> ys of {LT -> xs; _ -> ys}

-- Estimated max (not emacs).
emax xs ys = case xs <==> ys of {GT -> xs; _ -> ys}

eminimum, emaximum :: (Foldable f, Ord1 e, Ord o) => f (e o) -> e o

-- Estimated minimum.
eminimum = foldl1 emin

-- Estimated maximum.
emaximum = foldl1 emax
