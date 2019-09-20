{- |
    Module      :  Test.SDP.Tyoes
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
  
  @Test.SDP.Types@ provides newtypes for Arbitrary.
-}
module Test.SDP.Types
(
  -- * Common newtypes for QuickCheck.
  Short (..), Long (..)
)
where

--------------------------------------------------------------------------------

-- | Short is newtype of short data structures in QuickCheck properties.
newtype Short a = Short a deriving (Eq, Ord, Read, Show)

-- | Long  is newtype of large data structures in QuickCheck properties.
newtype Long  a = Long  a deriving (Eq, Ord, Read, Show)


