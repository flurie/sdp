module SDP.Internal.Read
(
  -- * Exports
  module Text.Read,
  
  -- * Common parsers
  readIndexedPrec, readLinearPrec, readSDPPrec,
  
  -- * Enum parsers
  readAsEnum, fromPrec, fromToPrec, fromThenPrec, fromThenToPrec
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed

import Text.Read
import Text.Read.Lex ( expect  )

import GHC.Show      ( appPrec )

default ()

--------------------------------------------------------------------------------

-- | Common 'Indexed' parser.
readIndexedPrec :: (Indexed v i e, Read i, Read e) => String -> ReadPrec v
readIndexedPrec name = readNamedPrec name readAssocsPrec

-- | Common 'Linear' parser.
readLinearPrec :: (Linear l e, Read e) => String -> ReadPrec l
readLinearPrec name = readNamedPrec name (readZeroPrec +++ readAsList +++ readAsListN)

-- | Common Linear and 'Indexed' parser.
readSDPPrec :: (Indexed v i e, Linear v e, Read i, Read e) => String -> ReadPrec v
readSDPPrec name = readNamedPrec name (readZeroPrec +++ readAsList +++ readAsListN +++ readAssocsPrec)

--------------------------------------------------------------------------------

{- Common parsers. -}

-- | 'readZeroPrec' is just 'Z' parser.
readZeroPrec :: (Linear l e) => ReadPrec l
readZeroPrec = parens $ prec appPrec (expectPrec $ Ident "Z") >> return Z

-- | 'readAsList' is 'fromList'-based parser.
readAsList :: (Linear l e, Read e) => ReadPrec l
readAsList = fromList <$> readListPrec

-- | 'readAsListN' is 'fromListN'-based parser.
readAsListN :: (Linear l e, Read e) => ReadPrec l
readAsListN = liftA2 fromListN (step readPrec) (step readPrec)

-- | 'readAssocsPrec' is SDP recommended format 'ReadPrec' parser for 'Indexed'.
readAssocsPrec :: (Indexed v i e, Read i, Read e) => ReadPrec v
readAssocsPrec = parens $ liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Enum parsers. -}

-- | 'Enum' parser.
readAsEnum :: (Linear l e, Read e, Enum e) => ReadPrec l
readAsEnum =  fromList <$> parens' (fromPrec_ +++ fromThenPrec_ +++ fromToPrec_ +++ fromThenToPrec_)

-- | 'enumFrom' parser.
fromPrec :: (Linear l e, Read e, Enum e) => ReadPrec l
fromPrec =  fromList <$> parens' fromPrec_

-- | 'enumFromTo' parser.
fromToPrec :: (Linear l e, Read e, Enum e) => ReadPrec l
fromToPrec =  fromList <$> parens' fromToPrec_

-- | 'enumFromThen' parser.
fromThenPrec :: (Linear l e, Read e, Enum e) => ReadPrec l
fromThenPrec =  fromList <$> parens' fromThenPrec_

-- | 'enumFromThenTo' parser.
fromThenToPrec :: (Linear l e, Read e, Enum e) => ReadPrec l
fromThenToPrec =  fromList <$> parens' fromThenToPrec_

--------------------------------------------------------------------------------

fromPrec_ :: (Read e, Enum e) => ReadPrec [e]
fromPrec_ =  do
  fr <- parens (step readPrec); expectPrec (Punc "..")
  return (enumFrom fr)

fromToPrec_ :: (Read e, Enum e) => ReadPrec [e]
fromToPrec_ =  do
  fr <- parens (step readPrec); expectPrec (Punc "..")
  to <- parens (step readPrec)
  return (enumFromTo fr to)

fromThenPrec_ :: (Read e, Enum e) => ReadPrec [e]
fromThenPrec_ =  do
  fr <- parens (step readPrec); expectPrec (Punc  ",")
  th <- parens (step readPrec); expectPrec (Punc "..")
  return (enumFromThen fr th)

fromThenToPrec_ :: (Read e, Enum e) => ReadPrec [e]
fromThenToPrec_ =  do
  fr <- parens (step readPrec); expectPrec (Punc  ",")
  th <- parens (step readPrec); expectPrec (Punc "..")
  to <- parens (step readPrec)
  return (enumFromThenTo fr th to)

parens' :: ReadPrec e -> ReadPrec e
parens' parser = do
  expectPrec (Punc "[")
  value <- parser
  expectPrec (Punc "]")
  return value

readNamedPrec :: String -> ReadPrec e -> ReadPrec e
readNamedPrec name parser = named +++ parser
  where
    named = prec appPrec (expectPrec $ Ident name) >> parser

expectPrec :: Lexeme -> ReadPrec ()
expectPrec = lift . expect


