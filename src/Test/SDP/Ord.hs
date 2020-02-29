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
    ordTest,
    
    -- * Lexicographic test
    lexicographicOrdTest
  )
where

import Prelude ()
import SDP.SafePrelude

import SDP.Linear

default ()

--------------------------------------------------------------------------------

-- | TestOrd is service type synonym for more comfortable quickCheck using.
type TestOrd l = l -> l -> l -> Bool

-- | ordTest is basic test suite for 'Ord' instances.
ordTest :: (Ord l) => l -> l -> l -> Bool
ordTest xs ys zs = and
  [
    -- antisymmetry
    (xs <= ys && ys <= xs) <= (xs == ys),
    
    -- transitivity
    (xs <= ys && ys <= zs) <= (xs <= zs),
    
    -- totality
    (xs <= ys) /= (xs > ys)
  ]

-- | lexicographicOrdTest checks 'Linear' structures for lexicographic order.
lexicographicOrdTest :: (Linear l e, Ord l, Ord e) => l -> l -> Bool
lexicographicOrdTest xs ys = (xs <=> ys) == (listL xs <=> listL ys)




