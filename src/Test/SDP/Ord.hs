{- |
    Module      :  Test.SDP.Ord
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
  
  @Test.SDP.Ord@ provides basic test suite for 'Ord' instances.
-}
module Test.SDP.Ord
  (
    -- * Test type synonym
    TestOrd,
    
    -- * Default test
    ordTest
  )
where

import Prelude ()
import SDP.SafePrelude

default ()

--------------------------------------------------------------------------------

-- | TestEq is service type synonym for more comfortable quickCheck using.
type TestOrd l = l -> l -> l -> Bool

-- | eqTest is basic test suite for 'Eq' instances.
ordTest :: (Ord l) => l -> l -> l -> Bool
ordTest xs ys zs = and
  [
    -- antisymmetry
    (xs <= ys && ys <= xs) <= (xs == ys),
    
    -- transitivity
    (xs <= ys && ys <= zs) <= (xs <= zs),
    
    -- not ((xs <= ys) && (ys <= zs)) || (xs <= zs),
    
    -- totality
    (xs <= ys) /= (xs > ys)
  ]



