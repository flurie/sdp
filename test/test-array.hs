module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.Array

import Test.SDP.Estimate
import Test.SDP.Indexed
import Test.SDP.Linear
import Test.SDP.Sort
import Test.SDP.Set

import Test.SDP.Eq

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    -- common tests
    testProperty "array-eq             " eqProp,
    
    -- linear tests
    testProperty "array-linear-basic   " basicLinearProp,
    testProperty "array-linear-decons  " deconstructionLinearProp,
    testProperty "array-linear-cons    " constructionLinearProp,
    testProperty "array-linear-reverse " reverseProp,
    testProperty "array-linear-concat  " concatProp,
    
    -- split test
    testProperty "array-split          " splitProp,
    
    -- indexed tests
    testProperty "array-indexed-basic  " basicIndexedProp,
    testProperty "array-indexed-assoc  " assocIndexedProp,
    testProperty "array-indexed-read   " readIndexedProp,
    
    -- sort test
    testProperty "array-sort           " sortProp,
    
    -- set test
    testProperty "array-set            " setProp,
    
    -- estimate test
    testProperty "array-estimate       " estimateProp
  ]

--------------------------------------------------------------------------------

{- Eq property. -}

eqProp :: TestEq (Array Int Int)
eqProp =  eqTest

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp          :: Char -> Array Int Char -> Bool
basicLinearProp          =  basicLinearTest

deconstructionLinearProp :: Array Int Char -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp   :: Char -> Array Int Char -> Bool
constructionLinearProp   =  constructionLinearTest

reverseProp              :: Array Int Char -> Bool
reverseProp              =  reverseTest

replicateProp            :: TestLinear2 Array Int Char
replicateProp            =  replicateTest

concatProp               :: Array Int Char -> Bool
concatProp               =  concatTest

--------------------------------------------------------------------------------

{- Split property. -}

splitProp :: TestSplit2 Array Int Char
splitProp =  splitTest

--------------------------------------------------------------------------------

{- Indexed property. -}

basicIndexedProp :: TestIndexed2 Array Int Char
basicIndexedProp =  basicIndexedTest

assocIndexedProp :: TestIndexed2 Array Int Char
assocIndexedProp =  assocIndexedTest

readIndexedProp  :: TestIndexed2 Array Int Char
readIndexedProp  =  readIndexedTest

--------------------------------------------------------------------------------

{- Sort property. -}

sortProp :: Array Int Char -> Bool
sortProp =  sortTest

--------------------------------------------------------------------------------

{- Set property. -}

setProp :: TestSet1 (Array Int) Char
setProp =  setTest

--------------------------------------------------------------------------------

{- Estimate property. -}

estimateProp :: TestEstimate (Array Int Int)
estimateProp =  estimateTest



