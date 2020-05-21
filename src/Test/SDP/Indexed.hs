{- |
    Module      :  Test.SDP.Indexed
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable modules)
    
    @Test.SDP.Indexed@ provides simple set of test for 'Indexed' class.
-}
module Test.SDP.Indexed
(
  -- * Test type synonyms
  TestIndexed2,
  TestIndexed1,
  TestIndexed,
  
  -- * Default test
  indexedTest,
  
  -- * Particular tests
  basicIndexedTest,
  assocIndexedTest,
  readIndexedTest
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed

import Data.Maybe

default ()

--------------------------------------------------------------------------------

-- | TestIndexed  is service type synonym for more comfortable quickCheck using.
type TestIndexed  l i = i -> l -> Bool

-- | TestIndexed1 is service type synonym for more comfortable quickCheck using.
type TestIndexed1 l i e = i -> l e -> Bool

-- | TestIndexed2 is service type synonym for more comfortable quickCheck using.
type TestIndexed2 l i e = i -> l i e -> Bool

-- | 'basicIndexedTest' checks relations of 'isNull', 'safeElem' and 'inRange'.
basicIndexedTest :: (Bordered l i, Indexed l i e) => i -> l -> Bool
basicIndexedTest i es = isNull es || inRange bnds (safeElem bnds i)
  where
    bnds = bounds es

{- |
  'assocIndexedTest' checks relations of 'assoc', 'assocs', ('.$'), ('*$') and
  ('//').
-}
assocIndexedTest :: (Bordered l i, Indexed l i e, Eq e, Eq l) => i -> l -> Bool
assocIndexedTest i es = and
  [
    assoc (bounds es) (assocs es) == es,
    
    es // (assocs es) == es,
    Z  // (assocs es) == es,
    es //     []      == es,
    
    -- if structure contain dublicates, (.$) may find earlier match.
    isNull es || i' >= fromJust ((== es ! i') .$ es),
    isNull es || elem i' ((== es ! (safeElem bnds i)) *$ es)
  ]
  where
    i'   = safeElem bnds i
    bnds = bounds es

-- | 'readIndexedTest' checks relations of 'listL', ('.!'), ('!') and ('!?').
readIndexedTest :: (Bordered l i, Indexed l i e, Eq e) => i -> l -> Bool
readIndexedTest i es = and
    [
      -- just check (.!) on all range
      fmap (es .!) (indices es) == listL es,
      
      isNull es || (es ! i' == es .! i'),
      
      inRange bnds i || isNothing (es !? i),
      isNull es || isJust (es !? i')
    ]
  where
    i'   = safeElem bnds i
    bnds = bounds es

-- | 'indexedTest' is complex test, that includes all other tests.
indexedTest :: (Bordered l i, Indexed l i e, Eq e, Eq l) => i -> l -> Bool
indexedTest i es = and
  [
    basicIndexedTest i es,
    assocIndexedTest i es,
    readIndexedTest  i es
  ]



