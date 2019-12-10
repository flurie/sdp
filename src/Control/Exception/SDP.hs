{- |
    Module      :  Control.Exception.SDP
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
  
  @Control.Exception.SDP@ - service module that contain two useful exception
  types and reexports @"Control.Exception"@. @SDP.SafePrelude@ doesn't export
  this module (just like @"Prelude"@ doesn't export @"Control.Exception"@).
-}
module Control.Exception.SDP
(
  -- * Exports
  module Control.Exception,
  
  -- * Exceptions
  UnreachableException (..), IndexException (..)
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
  then underflow. But if an overflow is detected, the underflow may not be\
  noticed.
  
  A check is recommended in the constructors order: empty bounds, overflow and
  underflow.
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

{-|
    'UnreachableException' is a service exception type that should never be
    thrown.
    
    Unlike 'ErrorCall', it doesn't mean "this is an erroneous action", but "as
    intended".
    
    Unlike 'AssertionFailed', it doesn't imply any additional checks - this is a
    fact.
-}
data UnreachableException = UnreachableException String
  deriving (Eq, Typeable)

instance Show UnreachableException
  where
    show (UnreachableException s) = "unreachable exception " ++ s

instance Exception UnreachableException


