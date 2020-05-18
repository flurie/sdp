{-# LANGUAGE Unsafe, MagicHash, TypeFamilies, FlexibleInstances #-}

{- |
    Module      :  SDP.Array
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Array@ provides 'Array' - immutable lazy boxed array type.
-}
module SDP.Array
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Array
  Array
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Indexed
import SDP.Sort
import SDP.Scan
import SDP.Set

import SDP.Templates.AnyBorder
import SDP.Prim.SArray

import Text.Show.SDP
import Text.Read.SDP

default ()

--------------------------------------------------------------------------------

-- | Array - boxed array.
type Array = AnyBorder SArray#

instance {-# OVERLAPPABLE #-} (Index i, Show i, Show e) => Show (Array i e)
  where
    showsPrec = assocsPrec "array "

instance (Index i, Show i) => Show (Array i Char)
  where
    showsPrec = shows ... const listL

instance (Index i, Read i, Read e) => Read (Array i e)
  where
    readList = readListDefault
    readPrec = indexedPrec' "array"




