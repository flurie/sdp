module Test.SDP.Indexed
(
  module Test.QuickCheck,
  
  TestIndexed,
  
  testIndexed
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck
import SDP.Indexed

import Data.Maybe

default ()

--------------------------------------------------------------------------------

type TestIndexed l i e = i -> l e -> Bool

testIndexed :: (Indexed l i, Bordered l i, Eq e, Eq (l e)) => i -> l e -> Bool
testIndexed ix xs = and
  [
    null xs || inRange bnds i
    
    , assoc bnds (assocs xs) == xs
    
    -- just strict calculation that checks (.!) on all range
    , fmap (xs .!) rs == toList xs
    , null xs || (xs ! i == xs .! i)
    
    , inRange bnds ix || isNothing (xs !? ix)
    , null xs || isJust (xs !? i)
    
    , xs // (assocs xs) == xs
    , Z  // (assocs xs) == xs
    , xs // [] == xs
    
    -- if structure contain dublicates, (.$) may find earlier match.
    , null xs || i >= fromJust ((== xs ! i) .$ xs)
    , null xs || elem i ((== xs ! i) *$ xs)
  ]
  where
    i    = safeElem bnds ix
    rs   = indices xs
    bnds = bounds xs
