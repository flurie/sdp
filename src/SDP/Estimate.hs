{- |
    Module      :  SDP.Estimate
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.Estimate@ provides 'Estimate' class, typedefs and some common
    comparators. This module is exported by "SDP.SafePrelude".
-}
module SDP.Estimate
(
  -- * Exports
  module Data.Functor.Classes,
  
  -- * Estimate
  Estimate (..),
  
  -- * Type synonyms
  Equal, Compare,
  
  -- * common comparators
  (<=>), cmpfst, cmpsnd, eqfst, eqsnd,
  
  -- * right-size versions of Estimate functions
  (<=.>), (<.), (>.), (<=.), (>=.), (==.), (/=.)
)
where

import Prelude
  (
    Eq (..), Ord (..), Num (..),
    
    Bool (..), Ordering (..), Int,
    
    tail, fst, snd
  )

import Data.Functor.Classes
import Data.Function

infixl 4 <=>, <==>, .<., .>., .<=., .>=., .==., ./=.

infixl 4 <.=>, .<, .>, .<=, .>=, .==, ./=
infixl 4 <=.>, <., >., <=., >=., ==., /=.

default ()

--------------------------------------------------------------------------------

{- |
  Estimate class allows the lazy comparsion structures by length.
  
  For some types (for example, lists), this allows you to speed up the
  comparison or make it final (in most cases). For others (e.g., arrays), it may
  be convenient abbreviation.
-}
class Estimate e
  where
    {-# MINIMAL (<.=>), (<==>) #-}
    
    (<.=>) :: e -> Int -> Ordering
    (<==>) :: e ->  e  -> Ordering
    
    (.<),  (.>),  (.<=),  (.>=),  (.==),  (./=)  :: e -> Int -> Bool
    (.<.), (.>.), (.<=.), (.>=.), (.==.), (./=.) :: e ->  e  -> Bool
    
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
    []       <==>       [] = EQ
    _        <==>       [] = GT
    []       <==>        _ = LT
    (_ : xs) <==> (_ : ys) = xs <==> ys
    
    [] <.=> n = 0 <=> n
    es <.=> n = if n < 0 then LT else go es n
      where
        go _  0 = GT
        go [] c = 0 <=> c
        go xs c = tail xs <.=> (c - 1)

--------------------------------------------------------------------------------

{- Type synonyms. -}

-- | Equal is just synonym of (e -> e -> Bool)
type Equal   e = e -> e -> Bool

-- | Compare is just synonym of (e -> e -> Ordering)
type Compare e = e -> e -> Ordering

--------------------------------------------------------------------------------

{- Common comparators. -}

-- | "spaceship operator" - infix version of compare.
(<=>) :: (Ord o) => Compare o
(<=>) = compare

-- | Compare tuples by first elements.
cmpfst :: (Ord o) => Compare (o, s)
cmpfst = (compare `on` fst)

-- | Compare tuples by second elements.
cmpsnd :: (Ord o) => Compare (f, o)
cmpsnd = (compare `on` snd)

-- | Compare tuples by first elements.
eqfst :: (Eq e) => Equal (e, s)
eqfst = on (==) fst

-- | Compare tuples by second elements.
eqsnd :: (Eq e) => Equal (f, e)
eqsnd = on (==) snd

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

