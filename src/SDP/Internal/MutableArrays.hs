{-# LANGUAGE MagicHash, RoleAnnotations #-}

module SDP.Internal.MutableArrays
(
  STArray  (..), STUArray (..)
)
where

import Prelude  ()
import GHC.Base ( MutableArray#, MutableByteArray#, Int (..) )

--------------------------------------------------------------------------------

-- | STArray - service type, that needed for SDP.Array and SDP.Unrolled definitions.
data STArray s i e = STArray !i !i {-# UNPACK #-} !Int (MutableArray# s e)

type role STArray nominal nominal representational

--------------------------------------------------------------------------------

-- | STUArray is a poor service type that lift the primitive ByteArray to the level of boxed types.
data STUArray s i e = STUArray !i !i !Int (MutableByteArray# s)

type role STUArray nominal nominal nominal

