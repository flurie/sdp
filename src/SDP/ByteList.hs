{-# LANGUAGE Unsafe, FlexibleInstances #-}

{- |
    Module      :  SDP.ByteList
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.ByteList" provides 'ByteList' - strict unboxed unrolled linked list.
-}
module SDP.ByteList
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Unboxed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * ByteList
  ByteList,
  
  -- * Ublist
  Ublist
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
import SDP.ByteList.Ublist

import Text.Show.SDP
import Text.Read.SDP

default ()

--------------------------------------------------------------------------------

-- | 'ByteList' is bordered strict unboxed unrolled linked list.
type ByteList = AnyBorder Ublist

instance {-# OVERLAPPABLE #-} (Index i, Show i, Unboxed e, Show e) => Show (ByteList i e)
  where
    showsPrec = assocsPrec "bytelist "

instance (Index i, Show i) => Show (ByteList i Char)
  where
    showsPrec = shows ... const listL

instance (Index i, Read i, Unboxed e, Read e) => Read (ByteList i e)
  where
    readList = readListDefault
    readPrec = indexedPrec' "bytelist"




