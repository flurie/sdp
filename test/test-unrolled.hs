module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import SDP.Unrolled

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
    testProperty "unrolled-eq             " eqProp,
    
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

sortProp :: Unrolled Int Char -> Bool
sortProp =  sortTest

--------------------------------------------------------------------------------

{- Set property. -}

setProp :: TestSet1 (Unrolled Int) Int
setProp =  setTest

--------------------------------------------------------------------------------

{- Estimate property. -}

estimateProp :: TestEstimate (Unrolled Int Int)
estimateProp =  estimateTest



