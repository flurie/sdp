{- |
    Module      :  Test.SDP.Eq
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
  
  @Test.SDP.Eq@ provides basic test suite for 'Eq' instances.
-}
module Test.SDP.Eq
  (
    -- * Test type synonym
    TestEq,
    
    -- * Default test
    eqTest
  )
where

import Prelude ()
import SDP.SafePrelude

default ()

--------------------------------------------------------------------------------

-- | TestEq is service type synonym for more comfortable quickCheck using.
type TestEq l = l -> l -> l -> Bool

-- | eqTest is basic test suite for 'Eq' instances.
eqTest :: (Eq l) => l -> l -> l -> Bool
eqTest xs ys zs = and
  [
    -- transitive
    (xs == ys && ys == zs) <= (xs == zs),
    
    -- symmetric
    (xs == ys) == (ys == xs),
    
    -- reflexive
    xs == xs
  ]



