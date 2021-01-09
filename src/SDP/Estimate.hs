{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.Estimate
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Estimate" provides 'Estimate' class, type synonyms and some common
    comparators. This module is exported by "SDP.SafePrelude".
-}
module SDP.Estimate
(
  -- * Exports
  module Data.Functor.Classes,
  
  -- * Estimate
  Estimate (..), (<=.>), (<.), (>.), (<=.), (>=.), (==.), (/=.)
)
where

import Data.Functor.Classes

import SDP.Comparing

default ()

infixl 4 <==>, .<., .>., .<=., .>=., .==., ./=.

infixl 4 <.=>, .<, .>, .<=, .>=, .==, ./=
infixl 4 <=.>, <., >., <=., >=., ==., /=.

--------------------------------------------------------------------------------

{- |
  'Estimate' class provides the lazy comparsion structures by length.
  
  For some types (e.g., lists), this allows you to speed up the comparison or
  make it finite. For others (e.g., arrays), it may be convenient abbreviation.
-}
class Estimate e
  where
    {-# MINIMAL (<.=>), (<==>) #-}
    
    -- | Left-side structure length with number comparison.
    (<.=>) :: e -> Int -> Ordering
    
    -- | Two structures by length comparison.
    (<==>) :: Compare e
    
    -- | Left-side structure length with number comparison.
    (.==), (./=), (.<=), (.>=), (.<), (.>) :: e -> Int -> Bool
    
    -- | Two structures comparison by length.
    (.<.), (.>.), (.<=.), (.>=.), (.==.), (./=.) :: e -> e -> Bool
    
    e .<  i = case e <.=> i of {LT -> True; _ -> False}
    e .>  i = case e <.=> i of {GT -> True; _ -> False}
    e .<= i = case e <.=> i of {GT -> False; _ -> True}
    e .>= i = case e <.=> i of {LT -> False; _ -> True}
    e .== i = case e <.=> i of {EQ -> True; _ -> False}
    e ./= i = case e <.=> i of {EQ -> False; _ -> True}
    
    e1 .<.  e2 = case e1 <==> e2 of {LT -> True; _ -> False}
    e1 .>.  e2 = case e1 <==> e2 of {GT -> True; _ -> False}
    e1 .<=. e2 = case e1 <==> e2 of {GT -> False; _ -> True}
    e1 .>=. e2 = case e1 <==> e2 of {LT -> False; _ -> True}
    e1 .==. e2 = case e1 <==> e2 of {EQ -> True; _ -> False}
    e1 ./=. e2 = case e1 <==> e2 of {EQ -> False; _ -> True}

-- | Right-side number with structure length comparison.
(<=.>) :: (Estimate e) => Int -> e -> Ordering
i <=.> e = case e <.=> i of {LT -> GT; EQ -> EQ; GT -> LT}

-- | Right-side number with structure length comparison.
(==.), (/=.), (<=.), (>=.), (<.), (>.) :: (Estimate e) => Int -> e -> Bool

(==.) = flip (.==)
(/=.) = flip (./=)
(<=.) = flip (.>=)
(>=.) = flip (.<=)
(<.)  = flip (.>)
(>.)  = flip (.<)

--------------------------------------------------------------------------------

instance Estimate [a]
  where
    [] <==> [] = EQ
    [] <==>  _ = LT
    _  <==> [] = GT
    xs <==> ys = tail xs <==> tail ys
    
    [] <.=> n = 0 <=> n
    es <.=> n =
      let go xs c | c == 0 = GT | null xs = 0 <=> c | True = tail xs `go` (c - 1)
      in  if n < 0 then LT else go es n


