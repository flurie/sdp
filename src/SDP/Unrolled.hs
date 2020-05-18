{-# LANGUAGE Unsafe, TypeFamilies, FlexibleInstances #-}

{- |
    Module      :  SDP.Unrolled
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Unrolled@ provides 'Unrolled' - lazy boxed unrolled linked list.
-}
module SDP.Unrolled
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Unrolled
  Unrolled,
  
  -- * Unlist
  Unlist
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Indexed
import SDP.Sort
import SDP.Scan
import SDP.Set

import SDP.Templates.AnyBorder
import SDP.Unrolled.Unlist

import Text.Show.SDP
import Text.Read.SDP

default ()

--------------------------------------------------------------------------------

-- | Unrolled is bordered unrolled linked list.
type Unrolled = AnyBorder Unlist

instance {-# OVERLAPPABLE #-} (Index i, Show i, Show e) => Show (Unrolled i e)
  where
    showsPrec = assocsPrec "unrolled "

instance (Index i, Show i) => Show (Unrolled i Char)
  where
    showsPrec = shows ... const listL

instance (Index i, Read i, Read e) => Read (Unrolled i e)
  where
    readList = readListDefault
    readPrec = indexedPrec' "unrolled"

