{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MagicHash, Unsafe #-}

{- |
    Module      :  SDP.ByteList.Ublist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.ByteList.Ublist@ provides 'Ublist' - strict boxed unrolled linked list.
-}
module SDP.ByteList.Ublist
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Unboxed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Ublist
  Ublist
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Templates.AnyChunks
import SDP.ByteList.STUblist
import SDP.Prim.SBytes

import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Scan
import SDP.Set

import SDP.SortM.Tim

import Text.Show.SDP
import Text.Read.SDP

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | 'Ublist' is unrolled linked list of unboxed values.
type Ublist = AnyChunks SBytes#

--------------------------------------------------------------------------------

{- Show instance. -}

instance {-# OVERLAPPABLE #-} (Show e, Unboxed e) => Show (Ublist e)
  where
    showsPrec = assocsPrec "ublist "

instance Show (Ublist Char)
  where
    showsPrec = shows ... const listL

instance (Read e, Unboxed e) => Read (Ublist e)
  where
    readPrec = indexedPrec' "ublist"
    readList = readListDefault

--------------------------------------------------------------------------------

{- Sort instance. -}

instance (Unboxed e) => Sort (Ublist e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: (Unboxed e) => STBytes# s e -> ST s (Ublist e)
done =  unsafeFreeze



