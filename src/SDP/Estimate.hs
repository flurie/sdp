{- |
    Module      :  SDP.Estimate
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.Estimate@ provides 'Estimate' class, type synonyms and some common
    comparators. This module is exported by "SDP.SafePrelude".
-}
module SDP.Estimate
(
  -- * Exports
  module Data.Functor.Classes,
  
  -- * Estimate
  Estimate (..),
  
  -- * Right-size versions of Estimate functions
  (<=.>), (<.), (>.), (<=.), (>=.), (==.), (/=.)
)
where

import Data.Functor.Classes

import SDP.Comparing

infixl 4 <==>, .<., .>., .<=., .>=., .==., ./=.

infixl 4 <.=>, .<, .>, .<=, .>=, .==, ./=
infixl 4 <=.>, <., >., <=., >=., ==., /=.

default ()

--------------------------------------------------------------------------------

{- |
  Estimate class allows the lazy comparsion structures by length.
  
  For some types (for example, lists), this allows you to speed up the
  comparison or make it finite. For others (e.g., arrays), it may be convenient
  abbreviation.
-}
class Estimate e
  where
    {-# MINIMAL (<.=>), (<==>) #-}
    
    (<.=>) :: e -> Int -> Ordering
    (<==>) :: Compare e
    
    (.<),  (.>),  (.<=),  (.>=),  (.==),  (./=)  :: e -> Int -> Bool
    (.<.), (.>.), (.<=.), (.>=.), (.==.), (./=.) :: Equal e
    
    e .<  i = case e <.=> i of {LT -> True; _ -> False}
    e .>  i = case e <.=> i of {GT -> True; _ -> False}
    e .== i = case e <.=> i of {EQ -> True; _ -> False}
    
    e .<= i = case e <.=> i of {GT -> False; _ -> True}
    e .>= i = case e <.=> i of {LT -> False; _ -> True}
    e ./= i = case e <.=> i of {EQ -> False; _ -> True}
    
    e1 .<.  e2 = case e1 <==> e2 of {LT -> True; _ -> False}
    e1 .>.  e2 = case e1 <==> e2 of {GT -> True; _ -> False}
    e1 .==. e2 = case e1 <==> e2 of {EQ -> True; _ -> False}
    
    e1 .<=. e2 = case e1 <==> e2 of {GT -> False; _ -> True}
    e1 .>=. e2 = case e1 <==> e2 of {LT -> False; _ -> True}
    e1 ./=. e2 = case e1 <==> e2 of {EQ -> False; _ -> True}

--------------------------------------------------------------------------------

instance Estimate [a]
  where
    [] <==> [] = EQ
    [] <==>  _ = LT
    _  <==> [] = GT
    xs <==> ys = tail xs <==> tail ys
    
    [] <.=> n = 0 <=> n
    es <.=> n = if n < 0 then LT else go es n
      where
        go xs c | c == 0 = GT | null xs = 0 <=> c | True = tail xs `go` (c - 1)

--------------------------------------------------------------------------------

{- Right-side versions of Estimate functions. -}

-- | Right-side version of (<.=>).
(<=.>) :: (Estimate e) => Int -> e -> Ordering
i <=.> e = case e <.=> i of {LT -> GT; EQ -> EQ; GT -> LT}

-- | Right-side version of (.<).
(<.) :: (Estimate e) => Int -> e -> Bool
(<.) = flip (.>)

-- | Right-side version of (.>).
(>.) :: (Estimate e) => Int -> e -> Bool
(>.) = flip (.<)

-- | Right-side version of (.<=).
(<=.) :: (Estimate e) => Int -> e -> Bool
(<=.) = flip (.>=)

-- | Right-side version of (.>=).
(>=.) :: (Estimate e) => Int -> e -> Bool
(>=.) = flip (.<=)

-- | Right-side version of (.==).
(==.) :: (Estimate e) => Int -> e -> Bool
(==.) = flip (.==)

-- | Right-side version of (./=).
(/=.) :: (Estimate e) => Int -> e -> Bool
(/=.) = flip (./=)

