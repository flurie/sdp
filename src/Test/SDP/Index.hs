{-# LANGUAGE TypeOperators #-}

module Test.SDP.Index
(
  module Test.QuickCheck,
  module SDP.Index,
  
  TestIndex,
  testIndex,
)
where

import Test.QuickCheck
import SDP.Index

default ()

--------------------------------------------------------------------------------

type TestIndex i = (i, i) -> i -> Bool

listSizeRestriction :: Int
listSizeRestriction = 65536

{-
  testIndex takes bounds and index.
  May fail with Enum.from exception in Word64 check.
-}
testIndex :: (Index i) => (i, i) -> i -> Bool
testIndex (l, u) i = and
  [
    rank u == rank i, rank l == rank i
    
    , length  (sizes (l, u)) == rank i
    , product (sizes (l, u)) == size (l, u)
    
    , not $ inRange (l, u) i && isOverflow  (l, u) i
    , not $ inRange (l, u) i && isUnderflow (l, u) i
    , not $ inRange (l, u) i && isEmpty     (l, u)
    , not (isEmpty (l, u))   || isOverflow  (l, u) i
    , not (isEmpty (l, u))   || isUnderflow (l, u) i
    
    {- Dumb tests, very long. -}
    
    -- , length  (range (l, u)) == size (l, u)
    
    , prevTest (l, u)
    , nextTest (l, u)
    
  ]
  where
    prevTest :: (Index i) => (i, i) -> Bool
    prevTest bnds = null r || and test
      where
        test = take listSizeRestriction $ zipWith (==) r (tail $ prev bnds <$> r)
        r = range bnds

    nextTest :: (Index i) => (i, i) -> Bool
    nextTest bnds = null r || and test
      where
        test = take listSizeRestriction $ zipWith (==) r (tail $ prev bnds <$> r)
        r = range bnds

