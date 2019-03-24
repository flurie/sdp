{-# LANGUAGE ExistentialQuantification #-}

module SDP.Estimate
(
  module Data.Functor.Classes,
  
  Estimate (..), EList (..), (<=>)
)
where

import Prelude ( Bool (..), Eq (..), Ord (..), Ordering (..), Num (..), Foldable (..), Int, (&&), (||) )

import Data.Functor.Classes

--------------------------------------------------------------------------------

-- "spaceship operator" - infix version of compare.
(<=>) :: (Ord a) => a -> a -> Ordering
(<=>) = compare

{-
    This module is exported by SafePrelude.
    
    Estimate class  allows the lazy comparsion structures by  length (including,
  with different types of arguments).
    For  some  types  (lists, for example),  this  allows  you  to speed up  the
  comparison  or  make  it  finite (if  at least one  structure is finite).  For
  others (for example, arrays) it can be a convenient abbreviation.
-}

data EList e = forall o . EList (e o)

class (Foldable e, Ord1 e) => Estimate e
  where
    {-# MINIMAL (<==>) #-}
    
    (<==>) :: e a -> e b -> Ordering
    xs <==> ys = length xs <=> length ys
    
    {- Symmetric comparsion by length. -}
    
    (.>.), (.<.), (.<=.), (.>=.), (.==.), (./=.) :: e a -> e b -> Bool
    
    xs .>.  ys = case xs <==> ys of {GT ->  True; _ -> False}
    xs .<.  ys = case xs <==> ys of {LT ->  True; _ -> False}
    
    xs .<=. ys = case xs <==> ys of {GT -> False; _ ->  True}
    xs .>=. ys = case xs <==> ys of {LT -> False; _ ->  True}
    
    xs .==. ys = case xs <==> ys of {EQ ->  True; _ -> False}
    xs ./=. ys = case xs <==> ys of {EQ -> False; _ ->  True}
    
    {- Left-side comparsion with length. -}
    
    (>.), (<.), (<=.), (>=.), (==.), (/=.) :: e o -> Int -> Bool
    
    xs >.  n = length xs >  n
    xs <.  n = length xs <  n
    xs >=. n = length xs >= n
    xs <=. n = length xs <= n
    
    xs ==. n = length xs == n
    xs /=. n = length xs /= n
    
    emin, emax :: e a -> e b -> Int
    
    emin xs ys = if xs .>. ys then length ys else length xs
    emax xs ys = if xs .<. ys then length ys else length xs
    
    eminimum, emaximum :: (Foldable f) => f (EList e) -> Int
    
    eminimum = foldl (\ llen (EList es) -> if llen < 0 || es <. llen then length es else llen) (-1)
    emaximum = foldl (\ llen (EList es) -> if es >. llen then length es else llen) 0

instance Estimate []
  where
    [] <==> [] = EQ
    [] <==> _  = LT
    _  <==> [] = GT
    (_ : xs) <==> (_ : ys) = xs <==> ys
    
    []        >. n = 0 >  n
    (_ : xs)  >. n = 1 >  n || xs >.  (n - 1)
    
    []        <. n = 0 <  n
    (_ : xs)  <. n = 1 <  n && xs <.  (n - 1)
    
    []       >=. n = 0 >= n
    (_ : xs) >=. n = 1 >= n || xs >=. (n - 1)
    
    []       <=. n = 0 <= n
    (_ : xs) <=. n = 1 <= n && xs <=. (n - 1)
    
    []       ==. n = n == 0
    (_ : xs) ==. n = n >  0 && xs ==. (n - 1)
    
    []       /=. n = n /= 0
    (_ : xs) /=. n = n <  0 || xs /=. (n - 1)

