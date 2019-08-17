{-# LANGUAGE ExistentialQuantification #-}

{- |
    Module      :  SDP.Estimate
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions: ExistentialQuantification)
    Stability   :  stable
    
    This module is exported by "SDP.SafePrelude".
-}

module SDP.Estimate
(
  module Data.Functor.Classes,
  
  Estimate (..), EL (..), min_, max_
)
where

import Prelude
  (
    Eq (..), Ord (..), Num (..), Foldable (..),
    
    Bool (..), Ordering (..), Int,
    
    (&&), (||)
  )

import Data.Functor.Classes

default ()

--------------------------------------------------------------------------------

{- |
    Type EL allows you to consistently represent all kinds of structures
    that have an instance of Estimate.
-}

data EL e = forall o . EL (e o)

{- |
    Estimate class  allows the lazy comparsion structures by  length (including,
  with different types of arguments).
    For  some  types  (lists, for example),  this  allows  you  to speed up  the
  comparison  or  make  it  finite (if  at least one  structure is finite).  For
  others (for example, arrays) it can be a convenient abbreviation.
-}

class (Foldable e, Ord1 e) => Estimate e
  where
    {-# MINIMAL (<==>) #-}
    
    {- Symmetric comparsions. -}
    
    -- | Symmetric comparing on length by (<=>).
    (<==>) :: e a -> e b -> Ordering
    
    -- | Symmetric comparing on length by (>).
    (.>.)  :: e a -> e b -> Bool
    
    -- | Symmetric comparing on length by (<).
    (.<.)  :: e a -> e b -> Bool
    
    -- | Symmetric comparing on length by (<=).
    (.<=.) :: e a -> e b -> Bool
    
    -- | Symmetric comparing on length by (>=).
    (.>=.) :: e a -> e b -> Bool
    
    -- | Symmetric comparing on length by (==).
    (.==.) :: e a -> e b -> Bool
    
    -- | Symmetric comparing on length by (/=).
    (./=.) :: e a -> e b -> Bool
    
    {- Left-side comparsion with length. -}
    
    -- | Left-side comparsion with known length by (>).
    (>.)  :: e o -> Int -> Bool
    
    -- | Left-side comparsion with known length by (<).
    (<.)  :: e o -> Int -> Bool
    
    -- | Left-side comparsion with known length by (<=).
    (<=.) :: e o -> Int -> Bool
    
    -- | Left-side comparsion with known length by (>=).
    (>=.) :: e o -> Int -> Bool
    
    {- Additional functions. -}
    
    -- | Left-side comparsion with known length by (==).
    (==.) :: e o -> Int -> Bool
    
    -- | Left-side comparsion with known length by (/=).
    (/=.) :: e o -> Int -> Bool
    
    -- | Same as on min length.
    emin :: e a -> e b -> Int
    
    -- | Same as on max length.
    emax :: e a -> e b -> Int
    
    -- | Composition of length with minimum.
    eminimum :: (Foldable f) => f (EL e) -> Int
    
    -- | Composition of length with maximum.
    emaximum :: (Foldable f) => f (EL e) -> Int
    
    xs .>.  ys = case xs <==> ys of {GT ->  True; _ -> False}
    xs .<.  ys = case xs <==> ys of {LT ->  True; _ -> False}
    
    xs .<=. ys = case xs <==> ys of {GT -> False; _ ->  True}
    xs .>=. ys = case xs <==> ys of {LT -> False; _ ->  True}
    
    xs .==. ys = case xs <==> ys of {EQ ->  True; _ -> False}
    xs ./=. ys = case xs <==> ys of {EQ -> False; _ ->  True}
    
    xs  >.   n = length xs >  n
    xs  <.   n = length xs <  n
    xs  >=.  n = length xs >= n
    xs  <=.  n = length xs <= n
    
    xs  ==.  n = length xs == n
    xs  /=.  n = length xs /= n
    
    emin xs ys = if xs .>. ys then length ys else length xs
    emax xs ys = if xs .<. ys then length ys else length xs
    
    eminimum   = foldl (\ len' (EL es) -> if len' < 0 || es <. len' then length es else len') (-1)
    emaximum   = foldl (\ len' (EL es) -> if es >. len' then length es else len') 0

--------------------------------------------------------------------------------

-- | min_ is min which compares on length.
min_ :: (Estimate e) => e a -> e a -> e a
min_ xs ys = if xs .<=. ys then xs else ys

-- | max_ is max which compares on length.
max_ :: (Estimate e) => e a -> e a -> e a
max_ xs ys = if xs .>=. ys then xs else ys

--------------------------------------------------------------------------------

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

