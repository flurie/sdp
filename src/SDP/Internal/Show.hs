{- |
    Module      :  SDP.Internal.Show
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable modules)
    
    @SDP.Internal.Show@ provides common 'ShowS' stuff.
-}
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


