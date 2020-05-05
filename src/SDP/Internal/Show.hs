{- |
    Module      :  SDP.Internal.Show
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable modules)
    
    @SDP.Internal.Show@ provides common 'ShowS' stuff.
-}
module SDP.Internal.Show
(
  -- * Common show templates
  assocsPrec, showsRaw, showsRawLinear
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed

import GHC.Show ( appPrec )

default ()

--------------------------------------------------------------------------------

-- | 'assocsPrec' is 'showsPrec' template.
assocsPrec :: (Indexed v i e, Show i, Show e) => String -> Int -> v -> ShowS
assocsPrec name = \ p es -> showParen (p > appPrec) $ showString name
                                                    . shows (bounds es)
                                                    . showChar ' '
                                                    . shows (assocs es)

{- |
  'showsRaw' is a primitive list-to-string conversion pattern.
  Note that attempting to parse the resulting string with standard @ReadS@-based
  functions will cause an error (ambiguous parse). To properly parse a string,
  use the @readRawSequence@ function from the @SDP.Internal.Read@ module.
-}
showsRaw :: (Show e) => Int -> [e] -> ShowS
showsRaw _    []    = id
showsRaw p (x : xs) = showParen (p > appPrec) stream
  where
    stream = shows x . foldr (\ e rest -> showChar ' ' . shows e . rest) id xs

-- | Just 'showsRaw' version for 'Linear'.
showsRawLinear :: (Linear l e, Show e) => Int -> l -> ShowS
showsRawLinear p = showsRaw p . listL

