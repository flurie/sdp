{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

module SDP.Array.Mutable
(
  STArray  (..),
  STUArray (..),
  fill
)
where

import Prelude ()
import SDP.SafePrelude

import GHC.Base
  (
    MutableArray#, MutableByteArray#, Int (..),
    
    writeArray#, sameMutableArray#, isTrue#
  )

import GHC.ST   ( STRep )

--------------------------------------------------------------------------------

-- | STArray - service type, that needed for SDP.Array and SDP.Unrolled definitions.
data STArray s i e = STArray !i !i {-# UNPACK #-} !Int (MutableArray# s e)

type role STArray nominal nominal representational

instance Eq (STArray s i e)
  where
    (STArray _ _ _ a1#) == (STArray _ _ _ a2#) = isTrue# (sameMutableArray# a1# a2#)

-- | fill - service function, that needed for writing to MutableArray#.
{-# INLINE fill #-}
fill :: MutableArray# s e -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) next = \ s1# -> case writeArray# marr# i# e s1# of s2# -> next s2#

--------------------------------------------------------------------------------

{- |
  STUArray is a poor service type that lift the primitive ByteArray to the
  level of boxed types.
-}
data STUArray s i e = STUArray !i !i !Int (MutableByteArray# s)

type role STUArray nominal nominal nominal

