{- |
    Module      :  Control.Exception.SDP
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    Stability   :  stable
-}

module Control.Exception.SDP
(
  module Control.Exception,
  
  UnreachableException (..),
  IndexException (..)
)
where

import Prelude ( Eq (..), Show (..), String, (++) )

import Control.Exception
import Data.Typeable

--------------------------------------------------------------------------------

{-|
  Exception type IndexException replaces  the less informative 'ArrayException'
  and  has  more  neutral  names  in  relation  to other  indexable  structures.
  
  If the index can overflow and underflow at the same time,  it is recommended
  to indicate in the documentation.
-}

data IndexException = UndefinedValue String
                    | EmptyRange     String
                    | IndexUnderflow String
                    | IndexOverflow  String
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
    UnreachableCodeException exception type is used in expressions that should
    never be evaluated. Is soft version of assert.
-}

data UnreachableException = UnreachableException String
  deriving (Eq, Typeable)

instance Show UnreachableException
  where
    show (UnreachableException s) = "unreachable exception " ++ s

instance Exception UnreachableException

--------------------------------------------------------------------------------
