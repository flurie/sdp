{- |
    Module      :  SDP.Internal.Read
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable modules)
    
    @SDP.Internal.Read@ provides common 'ReadPrec' parsers and related stuff.
-}
module SDP.Internal.Read
(
  -- * Exports
  module Text.Read,
  
  -- * Common parsers
  readIndexedPrec, readLinearPrec, readSDPPrec, readRawSequencePrec,
  
  readZeroPrec, readAsList, readAsListN, readAssocsPrec,
  
  -- * Common parser combinators
  readRawSequence, readNamedPrec,
  
  -- * Generalized readers
  readBy, readMaybeBy, readEitherBy,
  
  -- * Enum parsers
  readAsEnum, enumFromPrec, enumFromToPrec, enumFromThenPrec, enumFromThenToPrec
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed

import Text.Read
import Text.Read.Lex ( expect  )

import GHC.Show      ( appPrec )

import Text.ParserCombinators.ReadP ( many1, skipSpaces )

default ()

--------------------------------------------------------------------------------

{- |
  @readIndexedPrec ident@ is common parser of 'Indexed' structure with name
  @ident@:
  
  > read "ident (0,1) [(0,0),(1,1)]" == read "(0,1) [(0,0),(1,1)]" == assoc (0,1) [(0,0),(1,1)]
-}
readIndexedPrec :: (Indexed v i e, Read i, Read e) => String -> ReadPrec v
readIndexedPrec name = readNamedPrec name readAssocsPrec

{- |
  @readLinearPrec ident@ is common parser of 'Linear' structure with name
  @ident@:
  
  > read "Z" == read "[]" == []
  > read "['l','g','p','l']" == fromList "lgpl"
  > read "4 [1,5,-1,45,12,6,0,0,12]" == fromListN 4 [1,5,-1,45,12,6,0,0,12]
-}
readLinearPrec :: (Linear l e, Read e) => String -> ReadPrec l
readLinearPrec name = readNamedPrec name (readZeroPrec +++ readAsList +++ readAsListN)

-- | Common 'Linear' and 'Indexed' parser.
readSDPPrec :: (Indexed v i e, Linear v e, Read i, Read e) => String -> ReadPrec v
readSDPPrec name = readNamedPrec name (readZeroPrec +++ readAsList +++ readAsListN +++ readAssocsPrec)

--------------------------------------------------------------------------------

{- Common parsers. -}

-- | 'readZeroPrec' is just 'Z' parser, see 'readLinearPrec'.
readZeroPrec :: (Linear l e) => ReadPrec l
readZeroPrec = parens $ prec appPrec (expectPrec $ Ident "Z") >> return Z

-- | 'readAsList' is 'fromList'-based parser, see 'readLinearPrec'.
readAsList :: (Linear l e, Read e) => ReadPrec l
readAsList = fromList <$> readListPrec

-- | 'readAsListN' is 'fromListN'-based parser, see 'readLinearPrec'.
readAsListN :: (Linear l e, Read e) => ReadPrec l
readAsListN = liftA2 fromListN (step readPrec) (step readPrec)

-- | 'readAssocsPrec' is SDP recommended format 'ReadPrec' parser for 'Indexed'.
readAssocsPrec :: (Indexed v i e, Read i, Read e) => ReadPrec v
readAssocsPrec = parens $ liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Enum parsers. -}

-- | Common 'Enum' parser.
readAsEnum :: (Linear l e, Read e, Enum e) => ReadPrec l
readAsEnum =  fromList <$> parens' (fromPrec_ +++ fromThenPrec_ +++ fromToPrec_ +++ fromThenToPrec_)

{- |
  'enumFrom' parser:
  
  > take 5 (readBy enumFromPrec "[1 ..]") == take 5 [1 ..] = [1,2,3,4,5]
-}
enumFromPrec :: (Linear l e, Read e, Enum e) => ReadPrec l
enumFromPrec =  fromList <$> parens' fromPrec_

{- |
  'enumFromTo' parser:
  
  > readBy enumFromToPrec "[9 .. 12]" == [9 .. 12] == [9,10,11,12]
-}
enumFromToPrec :: (Linear l e, Read e, Enum e) => ReadPrec l
enumFromToPrec =  fromList <$> parens' fromToPrec_

{- |
  'enumFromThen' parser:
  
  > take 4 (readBy enumFromThenPrec "[17, -6 .. ]") == take 4 [17, -6 ..] == [17,-6,-29,-52]
-}
enumFromThenPrec :: (Linear l e, Read e, Enum e) => ReadPrec l
enumFromThenPrec =  fromList <$> parens' fromThenPrec_

{- |
  'enumFromThenTo' parser:
  
  > take 4 (readBy enumFromThenToPrec "[17, -6 .. 4]") == [17, -6 .. 4] == [17]
-}
enumFromThenToPrec :: (Linear l e, Read e, Enum e) => ReadPrec l
enumFromThenToPrec =  fromList <$> parens' fromThenToPrec_

--------------------------------------------------------------------------------

{- Common parser combinators. -}

{- |
  readRawSequencePrec is 'many1'-based combinator (ambiguous):
  
  > readBy readRawSequencePrec "1 2 3 4 5 6 7" :: [Int] == *** Exception ...
  
  but
  
  > readRawSequence "1 2 3 4 5 6 7" == [1 .. 7]
-}
readRawSequencePrec :: (Read e) => ReadPrec [e]
readRawSequencePrec = lift . many1 $ readPrec_to_P readPrec appPrec

-- | @readNamedPrec name readprec@ parses @[name] readprec@.
readNamedPrec :: String -> ReadPrec e -> ReadPrec e
readNamedPrec name parser = named +++ parser
  where
    named = prec appPrec (expectPrec $ Ident name) >> parser

--------------------------------------------------------------------------------

-- | readBy is generalized 'read'.
readBy :: ReadPrec e -> String -> e
readBy parser string = case readEitherBy parser string of
  Left msg -> error msg
  Right  x -> x

-- | readMaybeBy is generalized 'readMaybe'.
readMaybeBy :: ReadPrec e -> String -> Maybe e
readMaybeBy parser string = case readPrec_to_S read' minPrec string of
    [(x, "")] -> Just x
    _         -> Nothing
  where
    read' = do x <- parser; lift skipSpaces; return x

-- | readEitherBy is generalized 'readEither'.
readEitherBy :: ReadPrec e -> String -> Either String e
readEitherBy parser string = case readPrec_to_S read' minPrec string of
    [(x, "")] -> Right x
    []        -> Left "SDP.Internal.Read.readBy: no parse"
    _         -> Left "SDP.Internal.Read.readBy: ambiguous parse"
  where
    read' = do x <- parser; lift skipSpaces; return x

-- | readRawSequence reads the most complete sequence.
readRawSequence :: (Read e) => String -> [e]
readRawSequence string = fst (last cases)
  where
    read' = do x <- readRawSequencePrec; lift skipSpaces; return x
    cases = readPrec_to_S read' minPrec string

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

expectPrec :: Lexeme -> ReadPrec ()
expectPrec = lift . expect




