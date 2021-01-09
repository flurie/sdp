{-# LANGUAGE Safe #-}

{- |
    Module      :  Control.Exception.SDP
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
  
  "Control.Exception.SDP" - service module that provide some useful exceptions.
  Note that "SDP.SafePrelude" doesn't export this module.
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

{- |
  'IndexException' replaces the less informative 'ArrayException' and has more
  neutral names.
  
  * 'UnacceptableExpansion' - occurs if the desired range exceed the actual size
  * 'UnexpectedRank' - occurs when trying to convert a list into a generalized
  index of inappropriate dimension
  * 'UndefinedValue' - occurs if the value is undefined
  * 'EmptyRange' - occurs if range is empty
  * 'IndexOverflow' - occurs if index overflows range
  * 'IndexUnderflow' - occurs if index underflows range
  
  'Exception' constructors are specified in the order of definition, this is the
  recommended check order.
  
  If the error type may depend on the check order, then it should be indicated
  in the documentation. For example: overflow is checked first, and then
  underflow. But if an overflow is detected, underflow may not be noticed.
-}
data IndexException = UnacceptableExpansion String
                    | UnexpectedRank        String
                    | UndefinedValue        String
                    | EmptyRange            String
                    | IndexOverflow         String
                    | IndexUnderflow        String
  deriving ( Eq, Typeable )

instance Show IndexException
  where
    show (UnacceptableExpansion s) = "unacceptable expansion "   ++ s
    show (UnexpectedRank        s) = "unexpected rank "          ++ s
    show (UndefinedValue        s) = "undefined element "        ++ s
    show (EmptyRange            s) = "empty range "              ++ s
    show (IndexOverflow         s) = "index out of upper bound " ++ s
    show (IndexUnderflow        s) = "index out of lower bound " ++ s

instance Exception IndexException

--------------------------------------------------------------------------------

{- |
   A 'UnreachableException' is used as an exception that should never be thrown.
  Unlike 'ErrorCall' and 'AssertionFailed', which signal an incorrect use of a
  function or an error in its behavior, 'UnreachableException' indicates that
  expression is unreachable by definition.
-}
data UnreachableException = UnreachableException String deriving (Eq, Typeable)

instance Show UnreachableException
  where
    show (UnreachableException s) = "unreachable exception " ++ s

instance Exception UnreachableException

