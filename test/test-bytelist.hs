module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.ByteList

import Test.SDP.Indexed
import Test.SDP.Linear
import Test.SDP.Sort

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
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
    testProperty "bytelist-sort           " sortProp
    
    -- set test (planned)
  ]

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



