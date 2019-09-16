module Test.SDP.Estimate
  (
    TestEstimate,
    
    estimateTest
  )
where

import Prelude ()
import SDP.SafePrelude

import SDP.Linear

default ()

--------------------------------------------------------------------------------

type TestEstimate e = Int -> e -> e -> Bool

estimateTest :: (Estimate b, Bordered b i e, Ord e) => Int -> b -> b -> Bool
estimateTest n xs ys = and
  [
    -- by definition
    (xs <==> ys) == (sizeOf xs <=> sizeOf ys),
    (xs <==> ys) == (sizeOf xs <=.> ys),
    (xs <==> ys) == (xs <.=> sizeOf ys),
    
    (xs <.=> n) == (sizeOf xs <=> n),
    
    case xs <.=> n of
      LT -> xs  .< n && n >=. xs
      EQ -> xs .== n && not (xs ./= n)
      GT -> xs  .> n && n <=. xs
    ,
    
    case xs <==> ys of
      LT -> xs  .<. ys && ys .>=. xs
      EQ -> xs .==. ys && not (xs ./=. ys)
      GT -> xs  .>. ys && ys .<=. xs
    ,
    
    not (xs .>. xs), (xs .>=. xs),
    not (xs .<. xs), (xs .<=. xs),
    
    -- equality
    (xs  .>. ys) == (ys .<.  xs),
    (xs  .<. ys) == (ys .>.  xs),
    
    (xs .>=. ys) == (ys .<=. xs),
    (xs .<=. ys) == (ys .>=. xs),
    
    (xs .>=. ys) || (xs .<=. ys),
    (xs .>.  ys) || (xs  .<. ys) || (xs .==. ys),
    
    -- inequality
    (xs .>. ys || xs .<. ys) /= (xs .==. ys),
    
    (xs .>. ys) /= (xs .<=. ys),
    (xs .<. ys) /= (xs .>=. ys)
  ]





