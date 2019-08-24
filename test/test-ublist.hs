module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.ByteList.Ublist

import Test.SDP.Indexed
import Test.SDP.Linear
import Test.SDP.Sort

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    -- linear tests
    testProperty "ublist-linear-basic   " basicLinearProp,
    testProperty "ublist-linear-decons  " deconstructionLinearProp,
    testProperty "ublist-linear-cons    " constructionLinearProp,
    testProperty "ublist-linear-reverse " reverseProp,
    testProperty "ublist-linear-concat  " concatProp,
    
    -- split test
    testProperty "ublist-split          " splitProp,
    
    -- indexed tests
    testProperty "ublist-indexed-basic  " basicIndexedProp,
    testProperty "ublist-indexed-assoc  " assocIndexedProp,
    testProperty "ublist-indexed-read   " readIndexedProp,
    
    -- sort test
    testProperty "ublist-sort           " sortProp
    
    -- set test (planned)
  ]

--------------------------------------------------------------------------------

{- Linear properties. -}

basicLinearProp          :: Char -> Ublist Char -> Bool
basicLinearProp          =  basicLinearTest

deconstructionLinearProp :: Ublist Char -> Bool
deconstructionLinearProp =  deconstructionLinearTest

constructionLinearProp   :: Char -> Ublist Char -> Bool
constructionLinearProp   =  constructionLinearTest

reverseProp              :: Ublist Char -> Bool
reverseProp              =  reverseTest

replicateProp            :: TestLinear1 Ublist Char
replicateProp            =  replicateTest

concatProp               :: Ublist Char -> Bool
concatProp               =  concatTest

--------------------------------------------------------------------------------

{- Split property. -}

splitProp :: TestSplit1 Ublist Char
splitProp =  splitTest

--------------------------------------------------------------------------------

{- Indexed property. -}

basicIndexedProp :: TestIndexed1 Ublist Int Char
basicIndexedProp =  basicIndexedTest

assocIndexedProp :: TestIndexed1 Ublist Int Char
assocIndexedProp =  assocIndexedTest

readIndexedProp  :: TestIndexed1 Ublist Int Char
readIndexedProp  =  readIndexedTest

--------------------------------------------------------------------------------

{- Sort property. -}

sortProp :: Ublist Char -> Bool
sortProp =  sortTest



