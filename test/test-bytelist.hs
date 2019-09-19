module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.ByteList

import Test.SDP.Estimate
import Test.SDP.Indexed
import Test.SDP.Linear
import Test.SDP.Sort
import Test.SDP.Set

import Test.SDP.Eq
import Test.SDP.Ord

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    -- common tests
    testProperty "bytelist-eq             " eqProp,
    testProperty "bytelist-ord            " ordProp,
    
    -- linear tests
    testProperty "bytelist-linear-basic   " basicLinearProp,
    testProperty "bytelist-linear-decons  " deconstructionLinearProp,
    testProperty "bytelist-linear-cons    " constructionLinearProp,
    testProperty "bytelist-linear-reverse " reverseProp,
    testProperty "bytelist-linear-concat  " concatProp,
    
    -- split test
    testProperty "bytelist-split          " splitProp,
    
    -- indexed tests
    testProperty "bytelist-indexed-basic  " basicIndexedProp,
    testProperty "bytelist-indexed-assoc  " assocIndexedProp,
    testProperty "bytelist-indexed-read   " readIndexedProp,
    
    -- sort test
    testProperty "bytelist-sort           " sortProp,
    
    -- set test
    testProperty "bytelist-set            " setProp,
    
    -- estimate test
    testProperty "bytelist-estimate       " estimateProp
  ]

--------------------------------------------------------------------------------

{- Eq property. -}

eqProp :: TestEq (ByteList Int Int)
eqProp =  eqTest

--------------------------------------------------------------------------------

{- Ord property. -}

ordProp :: TestOrd (ByteList Int Int)
ordProp =  ordTest

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp          :: Char -> ByteList Int Char -> Bool
basicLinearProp          =  basicLinearTest

deconstructionLinearProp :: ByteList Int Char -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp   :: Char -> ByteList Int Char -> Bool
constructionLinearProp   =  constructionLinearTest

reverseProp              :: ByteList Int Char -> Bool
reverseProp              =  reverseTest

replicateProp            :: TestLinear2 ByteList Int Char
replicateProp            =  replicateTest

concatProp               :: ByteList Int Char -> Bool
concatProp               =  concatTest

--------------------------------------------------------------------------------

{- Split property. -}

splitProp :: TestSplit2 ByteList Int Char
splitProp =  splitTest

--------------------------------------------------------------------------------

{- Indexed property. -}

basicIndexedProp :: TestIndexed2 ByteList Int Char
basicIndexedProp =  basicIndexedTest

assocIndexedProp :: TestIndexed2 ByteList Int Char
assocIndexedProp =  assocIndexedTest

readIndexedProp  :: TestIndexed2 ByteList Int Char
readIndexedProp  =  readIndexedTest

--------------------------------------------------------------------------------

{- Sort property. -}

sortProp :: ByteList Int Char -> Bool
sortProp =  sortTest

--------------------------------------------------------------------------------

{- Set property. -}

setProp :: TestSet1 (ByteList Int) Char
setProp =  setTest

--------------------------------------------------------------------------------

{- Estimate property. -}

estimateProp :: TestEstimate (ByteList Int Int)
estimateProp =  estimateTest



