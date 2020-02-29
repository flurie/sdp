{-# LANGUAGE TypeOperators #-}

{- |
    Module      :  Test.SDP.Index
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable modules)
    
    @Test.SDP.Index@ provides basic test suite for 'Index' class.
-}
module Test.SDP.Index
(
  -- * Test type synonym
  TestIndex,
  
  -- * Default test
  indexTest,
  
  -- * Particular tests
  basicIndexTest,
  inBoundsTest,
  rangeTest,
  prevTest,
  nextTest,
  
  dumbSizeTest
)
where

import SDP.Index

default ()

--------------------------------------------------------------------------------

-- | TestIndex is service type synonym for more comfortable quickCheck using.
type TestIndex i = (i, i) -> i -> Bool

lim :: Int
lim = 65536

{- |
  'rangeTest' checks relations of 'inRange', 'isOverflow', 'isUnderflow' and
  'isEmpty'.
-}
rangeTest :: (Index i) => (i, i) -> i -> Bool
rangeTest bnds i = and
  [
    not $ inRange bnds i && isOverflow  bnds i,
    not $ inRange bnds i && isUnderflow bnds i,
    not $ inRange bnds i && isEmpty     bnds,
    
    not (isEmpty  bnds)  || isOverflow  bnds i,
    not (isEmpty  bnds)  || isUnderflow bnds i
  ]

-- | 'prevTest' checks relations of 'prev' and 'range'.
prevTest :: (Index i) => (i, i) -> Bool
prevTest bnds = isEmpty bnds || and test
  where
    test = take lim $ zipWith (==) (range bnds) (tail $ prev bnds <$> range bnds)

-- | 'nextTest' checks relations of 'next' and 'range'.
nextTest :: (Index i) => (i, i) -> Bool
nextTest bnds = isEmpty bnds || and test
  where
    test = take lim $ zipWith (==) (range bnds) (tail $ prev bnds <$> range bnds)

-- | 'inBoundsTest' checks relations of 'inBounds' and other range functions.
inBoundsTest :: (Index i) => (i, i) -> i -> Bool
inBoundsTest bnds i = case inBounds bnds i of
  ER -> isEmpty     bnds
  UR -> isUnderflow bnds i
  IN -> inRange     bnds i
  OR -> isOverflow  bnds i

{- |
  'dumbSizeTest' is O(n) (very long) test, that checks relation of range 'size'
  and 'range' length.
-}
dumbSizeTest :: (Index i) => (i, i) -> Bool
dumbSizeTest bnds = length (range bnds) == size bnds

-- | 'basicIndexTest' checks relations of 'rank', 'size' and 'sizes'.
basicIndexTest :: (Index i) => (i, i) -> i -> Bool
basicIndexTest bnds@(l, u) i = and
  [
    rank u == rank i,
    rank l == rank i,
    
    length  (sizes bnds) == rank i,
    product (sizes bnds) == size bnds
  ]

{- |
  'indexTest' is complex test, that includes all other tests.
  May crash with very big numbers (Word64, Integer) because the tested functions
  are limited by size of type Int.
  In practice, structures of such sizes would take more memory than the address
  space of computers can accommodate.
-}
indexTest :: (Index i) => (i, i) -> i -> Bool
indexTest bnds i = and
  [
    basicIndexTest bnds i,
    
    inBoundsTest bnds i,
    
    rangeTest bnds i,
    
    prevTest bnds, nextTest bnds
  ]


