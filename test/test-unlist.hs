module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.Unrolled.Unlist

import Test.SDP.Indexed
import Test.SDP.Linear
import Test.SDP.Sort

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    -- linear tests
    testProperty "unlist-linear-basic   " basicLinearProp,
    testProperty "unlist-linear-decons  " deconstructionLinearProp,
    testProperty "unlist-linear-cons    " constructionLinearProp,
    testProperty "unlist-linear-reverse " reverseProp,
    testProperty "unlist-linear-concat  " concatProp,
    
    -- split test
    testProperty "unlist-split          " splitProp,
    
    -- indexed tests
    testProperty "unlist-indexed-basic  " basicIndexedProp,
    testProperty "unlist-indexed-assoc  " assocIndexedProp,
    testProperty "unlist-indexed-read   " readIndexedProp,
    
    -- sort test
    testProperty "unlist-sort           " sortProp
    
    -- set test (planned)
  ]

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp          :: Char -> Unlist Char -> Bool
basicLinearProp          =  basicLinearTest

deconstructionLinearProp :: Unlist Char -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp   :: Char -> Unlist Char -> Bool
constructionLinearProp   =  constructionLinearTest

reverseProp              :: Unlist Char -> Bool
reverseProp              =  reverseTest

replicateProp            :: TestLinear1 Unlist Char
replicateProp            =  replicateTest

concatProp               :: Unlist Char -> Bool
concatProp               =  concatTest

--------------------------------------------------------------------------------

{- Split property. -}

splitProp :: TestSplit1 Unlist Char
splitProp =  splitTest

--------------------------------------------------------------------------------

{- Indexed property. -}

basicIndexedProp :: TestIndexed1 Unlist Int Char
basicIndexedProp =  basicIndexedTest

assocIndexedProp :: TestIndexed1 Unlist Int Char
assocIndexedProp =  assocIndexedTest

readIndexedProp  :: TestIndexed1 Unlist Int Char
readIndexedProp  =  readIndexedTest

--------------------------------------------------------------------------------

{- Sort property. -}

sortProp :: Unlist Char -> Bool
sortProp =  sortTest



