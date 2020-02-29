{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

{- |
    Module      :  Test.SDP.Gen
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable modules)
  
  @Test.SDP.Gen@ provides newtypes for QuickCheck.
-}
module Test.SDP.Gen
(
  -- * Common newtypes for QuickCheck.
  Short (..), Medium (..), Long (..),
  
  -- * Related functions.
  linearA
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.Linear

default ()

--------------------------------------------------------------------------------

{- |
  Short is newtype of short data structures [0, 100) in QuickCheck properties.
  
  Short is the equivalent of the standard Arbitrary definition:
  @arbitrary = fromList \<$\> arbitrary@.
-}
newtype Short a = Short a deriving ( Eq, Ord, Read, Show )

{- |
  Medium is newtype of medium data structures in QuickCheck properties. The
  Arbitrary Medium instance must create a random-sized structure from the range
  [100, 1024).
  
  Medium is useful in testing as a compromise between speed and reliability:
  structures are too short for stress tests, but still human readable and enough
  fast for auto-testing.
-}
newtype Medium a = Medium a deriving ( Eq, Ord, Read, Show )

{- |
  Long is newtype of large data structures (>= 1024) in QuickCheck properties.
  Since Large can generate really huge numbers, the maximum length is limited by
  16384 - [1024, 16384).
  
  Long is primarily intended for benchmarks, although a large range of lengths
  makes them not very convenient for a pure comparison of several algorithms
  (but for this can be used vector[Of] and generate). Long works well with
  Unrolled and ByteList, since only the shortest of the allowed structures may
  fit in one standard chunk (1024 elements).
-}
newtype Long a = Long a deriving ( Eq, Ord, Read, Show )

--------------------------------------------------------------------------------

instance (Linear l e, Arbitrary e) => Arbitrary (Short l)
  where
    arbitrary = arbitrary >>= \ n -> (Short . fromList) <$> vector n

instance (Linear l e, Arbitrary e) => Arbitrary (Medium l)
  where
    arbitrary = arbitrary >>= \ (Large n) -> (Medium . fromList) <$> vector (100 + n `mod` 914)

instance (Linear l e, Arbitrary e) => Arbitrary (Long l)
  where
    arbitrary = arbitrary >>= \ (Large n) -> (Long . fromList) <$> vector (1024 + n `mod` 15360)

--------------------------------------------------------------------------------

-- | linearA is overloaded 'vector'.
linearA :: (Linear l e, Arbitrary e) => Int -> Gen l
linearA =  fmap fromList . vector


