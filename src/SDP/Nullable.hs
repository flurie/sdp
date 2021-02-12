{-# LANGUAGE Trustworthy, PatternSynonyms, ViewPatterns, MagicHash #-}

{- |
    Module      :  SDP.Nullable
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Nullable" provides 'Nullable' - class of types with empty values.
-}
module SDP.Nullable
(
  Nullable (..),
  
  pattern NULL
)
where

import Foreign.Ptr

import GHC.Stable
import GHC.Base

default ()

--------------------------------------------------------------------------------

-- | 'Nullable' is class of types which value may be empty.
class Nullable e
  where
    -- | Empty value.
    lzero  :: e
    
    -- | Is value empty?
    isNull :: e -> Bool

-- | Originally defined in @sdp-ctypes@ (now @sdp-foreign@), same as @Z@ now.
pattern NULL :: (Nullable e) => e
pattern NULL <- (isNull -> True) where NULL = lzero

--------------------------------------------------------------------------------

instance Nullable (Maybe e)
  where
    isNull Nothing = True
    isNull       _ = False
    
    lzero  = Nothing

instance Nullable [e]
  where
    isNull = null
    lzero  = []

instance Nullable (Ptr e)
  where
    isNull = (== nullPtr)
    lzero  = nullPtr

instance Nullable (StablePtr e)
  where
    isNull = (== lzero)
    lzero  = StablePtr (unsafeCoerce# 0#)

