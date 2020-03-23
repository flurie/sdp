module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.Unrolled

import Test.SDP

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    -- common tests
    testProperty "unrolled-eq             " eqProp,
    testProperty "unrolled-ord            " ordProp,
    testProperty "unrolled-lexicographic  " lgoProp,
    
    -- linear tests
    testProperty "unrolled-linear-basic   " basicLinearProp,
    testProperty "unrolled-linear-decons  " deconstructionLinearProp,
    testProperty "unrolled-linear-cons    " constructionLinearProp,
    testProperty "unrolled-linear-reverse " reverseProp,
    testProperty "unrolled-linear-concat  " concatProp,
    
    -- split test
    testProperty "unrolled-split          " splitProp,
    
    -- indexed tests
    testProperty "unrolled-indexed-basic  " basicIndexedProp,
    testProperty "unrolled-indexed-assoc  " assocIndexedProp,
    testProperty "unrolled-indexed-read   " readIndexedProp,
    
    -- sort test
    testProperty "unrolled-sort           " sortProp,
    
    -- set test
    testProperty "unrolled-set            " setProp,
    
    -- estimate test
    testProperty "unrolled-estimate       " estimateProp
  ]

--------------------------------------------------------------------------------

{- Eq property. -}

eqProp :: TestEq (Unrolled Int Int)
eqProp =  eqTest

--------------------------------------------------------------------------------

{- Ord property. -}

ordProp :: TestOrd (Unrolled Int Int)
ordProp =  ordTest

lgoProp :: Long (Unrolled Int Int) -> Long (Unrolled Int Int) -> Bool
lgoProp (Long xs) (Long ys) = lexicographicOrdTest xs ys

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp          :: Char -> Unrolled Int Char -> Bool
basicLinearProp          =  basicLinearTest

deconstructionLinearProp :: Unrolled Int Char -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp   :: Char -> Unrolled Int Char -> Bool
constructionLinearProp   =  constructionLinearTest

reverseProp              :: Unrolled Int Char -> Bool
reverseProp              =  reverseTest

replicateProp            :: TestLinear2 Unrolled Int Char
replicateProp            =  replicateTest

concatProp               :: Unrolled Int Char -> Bool
concatProp               =  concatTest

--------------------------------------------------------------------------------

{- Split property. -}

splitProp :: TestSplit2 Unrolled Int Char
splitProp =  splitTest

--------------------------------------------------------------------------------

{- Indexed property. -}

basicIndexedProp :: TestIndexed2 Unrolled Int Char
basicIndexedProp =  basicIndexedTest

assocIndexedProp :: TestIndexed2 Unrolled Int Char
assocIndexedProp =  assocIndexedTest

readIndexedProp  :: TestIndexed2 Unrolled Int Char
readIndexedProp  =  readIndexedTest

--------------------------------------------------------------------------------

{- Sort property. -}

sortProp :: Medium (Unrolled Int Char) -> Bool
sortProp =  sortTest

--------------------------------------------------------------------------------

{- Set property. -}

setProp :: TestSet1 (Unrolled Int) Int
setProp =  setTest

--------------------------------------------------------------------------------

{- Estimate property. -}

estimateProp :: TestEstimate (Unrolled Int Int)
estimateProp =  estimateTest





