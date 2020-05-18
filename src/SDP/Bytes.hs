{-# LANGUAGE Unsafe, MagicHash, TypeFamilies, FlexibleInstances #-}

{- |
    Module      :  SDP.Bytes
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    SDP.Bytes provides 'Bytes' - immutable strict unboxed array type.
-}
module SDP.Bytes
(
  -- * Exports
  module SDP.Unboxed,
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Bytes
  Bytes
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Scan
import SDP.Set

import SDP.Templates.AnyBorder
import SDP.Prim.SBytes

import Text.Show.SDP
import Text.Read.SDP

default ()

--------------------------------------------------------------------------------

-- | Bytes - unboxed array.
type Bytes = AnyBorder SBytes#

instance {-# OVERLAPPABLE #-} (Index i, Unboxed e, Show i, Show e) => Show (Bytes i e)
  where
    showsPrec = assocsPrec "bytes "

instance (Index i, Show i) => Show (Bytes i Char)
  where
    showsPrec = shows ... const listL

instance (Index i, Unboxed e, Read i, Read e) => Read (Bytes i e)
  where
    readList = readListDefault
    readPrec = indexedPrec' "bytes"


