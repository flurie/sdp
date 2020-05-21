{- |
    Module      :  Test.SDP.Estimate
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable modules)
  
  @Test.SDP.Estimate@ provides basic test suite for 'Estimate' instances.
-}
module Test.SDP.Estimate
  (
    -- * Test type synonym
    TestEstimate,
    
    -- * Default test
    estimateTest
  )
where

import Prelude ()
import SDP.SafePrelude hiding ( eq1 )

import SDP.Linear

default ()

--------------------------------------------------------------------------------

-- | TestEstimate is service type synonym for more comfortable quickCheck using.
type TestEstimate e = Int -> e -> e -> Bool

-- | estimateTest is basic test suite for 'Estimate' instances.
estimateTest :: (Estimate b, Bordered b i) => Int -> b -> b -> Bool
estimateTest n xs ys = and
  [
    -- by definition
    cmp == (sx <=>  sy),
    cmp == (sx <=.> ys),
    cmp == (xs <.=> sy),
    
    (xs <.=> n) == (sx <=> n),
    
    case xs <.=> n of
      LT -> xs .< n && n >. xs
      EQ -> xs .== n && not (xs ./= n)
      GT -> xs .> n && n <. xs
    ,
    
    case cmp of
      LT -> lt1 && ys .>. xs
      EQ -> eq1 && not ne1
      GT -> gt1 && ys .<. xs
    ,
    
    not (xs .>. xs), (xs .>=. xs),
    not (xs .<. xs), (xs .<=. xs),
    
    -- equality
    gt1 == (ys .<. xs),
    lt1 == (ys .>. xs),
    
    ge1 == (ys .<=. xs),
    le1 == (ys .>=. xs),
    
    ge1 || le1,
    gt1 || lt1 || eq1,
    
    -- inequality
    (gt1 || lt1) /= eq1,
    
    gt1 /= le1,
    lt1 /= ge1
  ]
  where
    gt1 = xs .>.  ys; lt1 = xs .<.  ys
    ge1 = xs .>=. ys; le1 = xs .<=. ys
    eq1 = xs .==. ys; ne1 = xs ./=. ys
    
    cmp = xs <==> ys; sx = sizeOf xs; sy = sizeOf ys

