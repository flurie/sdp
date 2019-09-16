module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.Bytes

import Test.SDP.Estimate
import Test.SDP.Indexed
import Test.SDP.Linear
import Test.SDP.Sort
import Test.SDP.Set

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    -- linear tests
    testProperty "bytes-linear-basic   " basicLinearProp,
    testProperty "bytes-linear-decons  " deconstructionLinearProp,
    testProperty "bytes-linear-cons    " constructionLinearProp,
    testProperty "bytes-linear-reverse " reverseProp,
    testProperty "bytes-linear-concat  " concatProp,
    
    -- split test
    testProperty "bytes-split          " splitProp,
    
    -- indexed tests
    testProperty "bytes-indexed-basic  " basicIndexedProp,
    testProperty "bytes-indexed-assoc  " assocIndexedProp,
    testProperty "bytes-indexed-read   " readIndexedProp,
    
    -- sort test
    testProperty "bytes-sort           " sortProp,
    
    -- set test
    testProperty "bytes-set            " setProp,
    
    -- estimate test
    testProperty "bytes-estimate       " estimateProp
  ]

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp          :: Char -> Bytes Int Char -> Bool
basicLinearProp          =  basicLinearTest

deconstructionLinearProp :: Bytes Int Char -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp   :: Char -> Bytes Int Char -> Bool
constructionLinearProp   =  constructionLinearTest

reverseProp              :: Bytes Int Char -> Bool
reverseProp              =  reverseTest

replicateProp            :: TestLinear2 Bytes Int Char
replicateProp            =  replicateTest

concatProp               :: Bytes Int Char -> Bool
concatProp               =  concatTest

--------------------------------------------------------------------------------

{- Split property. -}

splitProp :: TestSplit2 Bytes Int Char
splitProp =  splitTest

--------------------------------------------------------------------------------

{- Indexed property. -}

basicIndexedProp :: TestIndexed2 Bytes Int Char
basicIndexedProp =  basicIndexedTest

assocIndexedProp :: TestIndexed2 Bytes Int Char
assocIndexedProp =  assocIndexedTest

readIndexedProp  :: TestIndexed2 Bytes Int Char
readIndexedProp  =  readIndexedTest

--------------------------------------------------------------------------------

{- Sort property. -}

sortProp :: Bytes Int Char -> Bool
sortProp =  sortTest

--------------------------------------------------------------------------------

{- Set property. -}

setProp :: TestSet1 (Bytes Int) Char
setProp =  setTest

--------------------------------------------------------------------------------

{- Estimate property. -}

estimateProp :: TestEstimate (Bytes Int Int)
estimateProp =  estimateTest



