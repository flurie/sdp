module SDP.Estimate ( Estimate (..), (<=>), emin, emax, eminimum, emaximum )
where

import Prelude ( Bool (..), Eq (..), Ord (..), Ordering (..), Num (..), Foldable (..), Int, ($), (||), not, undefined )

import Data.Functor.Classes ( Ord1 (..) )

--------------------------------------------------------------------------------

-- "spaceship operator" - infix version of compare.
(<=>) :: (Ord a) => a -> a -> Ordering
(<=>) = compare

class (Foldable e, Ord1 e) => Estimate e
  where
    (<==>) :: e o -> e o -> Ordering
    xs <==> ys = length xs <=> length ys
    
    (>.), (<.), (<=.), (>=.) :: e o -> e o -> Bool
    
    xs >.  ys = case xs <==> ys of {GT ->  True; _ -> False}
    xs <.  ys = case xs <==> ys of {LT ->  True; _ -> False}
    xs <=. ys = case xs <==> ys of {GT -> False; _ ->  True}
    xs >=. ys = case xs <==> ys of {LT -> False; _ ->  True}
    
    (.>.), (.<.), (.<=.), (.>=.), (.==.), (./=.) :: e o -> Int -> Bool
    
    xs .>.  n = length xs >  n
    xs .<.  n = length xs <  n
    xs .>=. n = length xs >= n
    xs .<=. n = length xs <= n
    
    xs .==. n = length xs == n
    xs ./=. n = length xs /= n

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
    
    []        .>. n = 0 >  n
    (x : xs)  .>. n = 1 >  n || xs .>.  (n - 1)
    
    []        .<. n = 0 <  n
    (x : xs)  .<. n = 1 >  n || xs .<.  (n - 1)
    
    []       .>=. n = 0 >= n
    (x : xs) .>=. n = 1 >= n || xs .>=. (n - 1)
    
    []       .<=. n = 0 <= n
    (x : xs) .<=. n = 1 <= n || xs .<=. (n - 1)

