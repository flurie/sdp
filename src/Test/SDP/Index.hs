{-# LANGUAGE TypeOperators #-}

{- |
    Module      :  Test.SDP.Index
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    Stability   :  stable
    
    Test.SDP.Index provides simple set of test for SDP.Index class.
-}

module Test.SDP.Index
(
  TestIndex,
  
  rangeTest,
  inBoundsTest,
  
  prevTest,
  nextTest,
  
  basicIndexTest,
  indexTest,
  
  dumbSizeTest
)
where

import SDP.Index

default ()

--------------------------------------------------------------------------------

-- | TestIndex is service type synonym for more comfortable quickCheck using.
type TestIndex i = (i, i) -> i -> Bool

listSizeRestriction :: Int
listSizeRestriction = 65536

-- | rangeTest checks relations of inRange, isOverflow, isUnderflow and isEmpty.
rangeTest :: (Index i) => (i, i) -> i -> Bool
rangeTest (l, u) i = and
  [
    not $ inRange (l, u) i && isOverflow  (l, u) i,
    not $ inRange (l, u) i && isUnderflow (l, u) i,
    not $ inRange (l, u) i && isEmpty     (l, u),
    
    not (isEmpty  (l, u))  || isOverflow  (l, u) i,
    not (isEmpty  (l, u))  || isUnderflow (l, u) i
  ]

-- | prevTest checks relations of prev and range.
prevTest :: (Index i) => (i, i) -> Bool
prevTest bnds = null r || and test
  where
    test = take listSizeRestriction $ zipWith (==) r (tail $ prev bnds <$> r)
    r = range bnds

-- | nextTest checks relations of next and range.
nextTest :: (Index i) => (i, i) -> Bool
nextTest bnds = null r || and test
  where
    test = take listSizeRestriction $ zipWith (==) r (tail $ prev bnds <$> r)
    r = range bnds

-- | inBoundsTest checks relations of inBounds and other range functions.
inBoundsTest :: (Index i) => (i, i) -> i -> Bool
inBoundsTest bnds i = case inBounds bnds i of
  ER -> isEmpty     bnds
  UR -> isUnderflow bnds i
  IN -> inRange     bnds i
  OR -> isOverflow  bnds i

-- | dumbSizeTest is O(n) (very long) test, that checks relation of size and length.
dumbSizeTest :: (Index i) => (i, i) -> Bool
dumbSizeTest bnds = length (range bnds) == size bnds

-- | basicIndexTest checks the basic functions: rank and sizes.
basicIndexTest :: (Index i) => (i, i) -> i -> Bool
basicIndexTest (l, u) i = and
  [
    rank u == rank i,
    rank l == rank i,
    
    length  (sizes (l, u)) == rank i,
    product (sizes (l, u)) == size (l, u)
  ]

{- |
  indexTest - is complex test, that includes all other tests.
  May crash with very big numbers (Word64, Integer) because the tested functions
  are limited by size of type Int.
  In practice, structures of such sizes would take more memory than the address
  space of computers can accommodate (assuming that the address size does not
  exceed the size of an integer).
-}
indexTest :: (Index i) => (i, i) -> i -> Bool
indexTest bnds i = and
  [
    basicIndexTest bnds i,
    
    rangeTest bnds i,
    
    inBoundsTest bnds i,
    
    prevTest bnds,
    
    nextTest bnds
  ]
