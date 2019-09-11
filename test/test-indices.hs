module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Int  ( Int8,  Int16,  Int32  )
import Data.Word ( Word8, Word16, Word32 )

import SDP.Index

import Test.SDP.Index

default ()

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [
    testProperty "index-int   " testIndexInt,
    testProperty "index-int8  " testIndexInt8,
    testProperty "index-int16 " testIndexInt16,
    testProperty "index-int32 " testIndexInt32,
    
    testProperty "index-word  " testIndexWord,
    testProperty "index-word8 " testIndexWord8,
    testProperty "index-word16" testIndexWord16,
    testProperty "index-word32" testIndexWord32,
    
    testProperty "index-char  " testIndexChar,
    
    testProperty "index-ind2  " testIndexI2,
    testProperty "index-ind3  " testIndexI3,
    testProperty "index-ind4  " testIndexI4,
    testProperty "index-ind5  " testIndexI5,
    testProperty "index-ind6  " testIndexI6,
    testProperty "index-ind7  " testIndexI7,
    testProperty "index-ind8  " testIndexI8,
    testProperty "index-ind9  " testIndexI9,
    testProperty "index-ind10 " testIndexI10,
    testProperty "index-ind11 " testIndexI11,
    testProperty "index-ind12 " testIndexI12,
    testProperty "index-ind13 " testIndexI13,
    testProperty "index-ind14 " testIndexI14,
    testProperty "index-ind15 " testIndexI15
  ]

--------------------------------------------------------------------------------

{- Int properties. -}

testIndexInt   :: TestIndex Int
testIndexInt   =  indexTest

testIndexInt8  :: TestIndex Int8
testIndexInt8  =  indexTest

testIndexInt16 :: TestIndex Int16
testIndexInt16 =  indexTest

testIndexInt32 :: TestIndex Int32
testIndexInt32 =  indexTest

--------------------------------------------------------------------------------

{- Word properties. -}

testIndexWord   :: TestIndex Word
testIndexWord   =  indexTest

testIndexWord8  :: TestIndex Word8
testIndexWord8  =  indexTest

testIndexWord16 :: TestIndex Word16
testIndexWord16 =  indexTest

testIndexWord32 :: TestIndex Word32
testIndexWord32 =  indexTest

--------------------------------------------------------------------------------

{- N-dimensional properties. -}

testIndexI2  :: TestIndex (I2 Int)
testIndexI2  =  indexTest

testIndexI3  :: TestIndex (I3 Int)
testIndexI3  =  indexTest

testIndexI4  :: TestIndex (I4 Int)
testIndexI4  =  indexTest

testIndexI5  :: TestIndex (I5 Int)
testIndexI5  =  indexTest

testIndexI6  :: TestIndex (I6 Int)
testIndexI6  =  indexTest

testIndexI7  :: TestIndex (I7 Int)
testIndexI7  =  indexTest

testIndexI8  :: TestIndex (I8 Int)
testIndexI8  =  indexTest

testIndexI9  :: TestIndex (I9 Int)
testIndexI9  =  indexTest

testIndexI10 :: TestIndex (I10 Int)
testIndexI10 =  indexTest

testIndexI11 :: TestIndex (I11 Int)
testIndexI11 =  indexTest

testIndexI12 :: TestIndex (I12 Int)
testIndexI12 =  indexTest

testIndexI13 :: TestIndex (I13 Int)
testIndexI13 =  indexTest

testIndexI14 :: TestIndex (I14 Int)
testIndexI14 =  indexTest

testIndexI15 :: TestIndex (I15 Int)
testIndexI15 =  indexTest

--------------------------------------------------------------------------------

{- Other properties. -}

testIndexChar :: TestIndex Char
testIndexChar =  indexTest




