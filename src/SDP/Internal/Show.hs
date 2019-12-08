module SDP.Internal.Show ( assocsPrec ) where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed

import GHC.Show ( appPrec )

default ()

--------------------------------------------------------------------------------

-- | 'assocsPrec' is 'showsPrec' template.
assocsPrec :: (Bordered v i e, Show i, Show e) => String -> Int -> v -> ShowS
assocsPrec name = \ p es -> showParen (p > appPrec) $ showString name
                                                    . shows (bounds es)
                                                    . showChar ' '
                                                    . shows (assocs es)

