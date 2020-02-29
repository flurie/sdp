{- |
    Module      :  Control.Exception.SDP
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
  
  @Control.Exception.SDP@ - service module that provide some useful exceptions.
  Note that @SDP.SafePrelude@ doesn't export this module.
-}
module Control.Exception.SDP
(
  -- * Exports
  module Control.Exception,
  
  -- * Exceptions
  UnreachableException (..), IndexException (..), UnexpectedRank (..)
)
where

import Prelude ( Eq (..), Show (..), String, (++) )

import Control.Exception
import Data.Typeable

default ()

--------------------------------------------------------------------------------

{-|
  'IndexException' replaces the less informative 'ArrayException' and has more
  neutral names in relation to other structures.
  
  If the founded error may depend on the check order, then it should be
  indicated in the documentation. For example: first it checks overflow, and
  then underflow. But if an overflow is detected, the underflow may not be
  noticed. Recommended check order is: empty bounds, overflow and underflow.
-}
data IndexException = UndefinedValue String
                    | EmptyRange     String
                    | IndexOverflow  String
                    | IndexUnderflow String
  deriving (Eq, Typeable)

instance Show IndexException
  where
    show (UndefinedValue  s) = "undefined element "        ++ s
    show (EmptyRange      s) = "empty range "              ++ s
    show (IndexOverflow   s) = "index out of upper bound " ++ s
    show (IndexUnderflow  s) = "index out of lower bound " ++ s

instance Exception IndexException

--------------------------------------------------------------------------------

{- |
  'UnexpectedRank' - service exception for "SDP.Finite" @IsList@ instances.
  
  This exception is thrown when the rank of the index doesn't match the number
  of elements in its list representation.
-}
data UnexpectedRank = UnexpectedRank String deriving ( Eq, Typeable )

instance Show UnexpectedRank
  where
    show (UnexpectedRank s) = "unexpected rank " ++ s

instance Exception UnexpectedRank

--------------------------------------------------------------------------------

{- |
   A 'UnreachableException' is used as an exception that should never be thrown.
  Unlike 'ErrorCall' and 'AssertionFailed', which signal an incorrect use of a
  function or an error in its behavior, 'UnreachableException' indicates that
  expression is unreachable.
-}
data UnreachableException = UnreachableException String deriving (Eq, Typeable)

instance Show UnreachableException
  where
    show (UnreachableException s) = "unreachable exception " ++ s

instance Exception UnreachableException

