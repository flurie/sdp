{-# LANGUAGE Unsafe, CPP, MagicHash, UnboxedTuples, BangPatterns #-}
{-# LANGUAGE RoleAnnotations #-}

{- |
    Module      :  SDP.Unboxed
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
  
  This module provide service class Unboxed, that needed for SDP.UArray.
  This module partially based on code Data.Array.Base (array).
-}

module SDP.Unboxed ( Unboxed (..), newUnboxedByteArray, safe_scale ) where

import Prelude ()
import SDP.SafePrelude

import GHC.Stable ( StablePtr(..) )
import GHC.Base   ( divInt# )
import GHC.Exts
import GHC.ST     ( ST (..) )

import GHC.Int  ( Int  (..), Int8  (..), Int16  (..), Int32  (..), Int64  (..) )
import GHC.Word ( Word (..), Word8 (..), Word16 (..), Word32 (..), Word64 (..) )

#include "MachDeps.h"

--------------------------------------------------------------------------------

{- |
  Unboxed is a class that allows creating, writing and reading from byte arrays.
  It partly repeats the functionality of the MArray from the array,
  but solves a narrower range of tasks.
-}

class (Eq e) => Unboxed e
  where
    -- | (marr# !# i#) reads element of marr# with index i#.
    (!#)            :: ByteArray# -> Int# -> e
    
    -- | writeByteArray# marr# i# e writes e to marr# with index i#.
    writeByteArray# :: MutableByteArray# s -> Int# -> e -> State# s -> State# s
    
    fillByteArray#  :: MutableByteArray# s -> Int# -> e -> State# s -> State# s
    fillByteArray# marr# n# e = \ s1# -> case sequence_
        [
          ST $ \ sn# -> case writeByteArray# marr# i# e sn# of
            sn1# -> (# sn1#, () #) | (I# i#) <- [0 .. (I# n#)]
        ]
        of ST (rep) -> case rep s1# of (# s2#, () #) -> s2#
    
    -- | newUnboxed e n# creates new MutableByteArray. First argument used as type variable.
    newUnboxed      :: e -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
    
    newUnboxed'     :: e -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
    newUnboxed' e n# = \ s1# -> case newUnboxed e n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# e s2# of s3# -> (# s3#, marr# #)

--------------------------------------------------------------------------------

{- Int instances. -}

instance Unboxed Int
  where
    bytes# !# i# = I#   (indexIntArray#   bytes# i#)
    newUnboxed _ = newUnboxedByteArray word_scale
    
    writeByteArray# marr#    n# (I# e#)   = \ s1# -> writeIntArray# marr#    n# e# s1#

instance Unboxed Int8
  where
    bytes# !# i# = I8#  (indexInt8Array#  bytes# i#)
    newUnboxed _ = newUnboxedByteArray (\ x -> x)
    
    writeByteArray# marr#    n# (I8# e#)  = \ s1# -> writeInt8Array# marr#   n# e# s1#

instance Unboxed Int16
  where
    bytes# !# i# = I16# (indexInt16Array# bytes# i#)
    newUnboxed _ = newUnboxedByteArray (safe_scale 2#)
    
    writeByteArray# marr#    n# (I16# e#) = \ s1# -> writeInt16Array# marr#  n# e# s1#

instance Unboxed Int32
  where
    bytes# !# i# = I32# (indexInt32Array# bytes# i#)
    newUnboxed _ = newUnboxedByteArray (safe_scale 4#)
    
    writeByteArray# marr#    n# (I32# e#) = \ s1# -> writeInt32Array# marr#  n# e# s1#

instance Unboxed Int64
  where
    bytes# !# i# = I64# (indexInt64Array# bytes# i#)
    newUnboxed _ = newUnboxedByteArray (safe_scale 8#)
    
    writeByteArray# marr#    n# (I64# e#) = \ s1# -> writeInt64Array# marr#  n# e# s1#

{- Word instances. -}

instance Unboxed Word
  where
    bytes# !# i# = W#   (indexWordArray#   bytes# i#)
    newUnboxed _ = newUnboxedByteArray word_scale
    
    writeByteArray# marr#    n# (W# e#)   = \ s1# -> writeWordArray#   marr# n# e# s1#

instance Unboxed Word8
  where
    bytes# !# i# = W8#  (indexWord8Array#  bytes# i#)
    newUnboxed _ = newUnboxedByteArray (\ x -> x)
    
    writeByteArray# marr#    n# (W8# e#)  = \ s1# -> writeWord8Array#  marr# n# e# s1#

instance Unboxed Word16
  where
    bytes# !# i# = W16# (indexWord16Array# bytes# i#)
    newUnboxed _ = newUnboxedByteArray (safe_scale 2#)

    writeByteArray# marr#    n# (W16# e#) = \ s1# -> writeWord16Array# marr# n# e# s1#

instance Unboxed Word32
  where
    bytes# !#   i# = W32# (indexWord32Array# bytes# i#)
    newUnboxed  _  = newUnboxedByteArray (safe_scale 4#)
    
    writeByteArray# marr#    n# (W32# e#) = \ s1# -> writeWord32Array# marr# n# e# s1#

instance Unboxed Word64
  where
    bytes# !#   i# = W64# (indexWord64Array# bytes# i#)
    newUnboxed  _  = newUnboxedByteArray (safe_scale 8#)
    
    writeByteArray# marr#    n# (W64# e#) = \ s1# -> writeWord64Array# marr# n# e# s1#

{- Pointer instances. -}

instance Unboxed (Ptr a)
  where
    bytes# !#   i# = Ptr        (indexAddrArray# bytes# i#)
    newUnboxed  _  = newUnboxedByteArray word_scale

    writeByteArray# marr#    n# (Ptr e#)       = \ s1# -> writeAddrArray# marr# n# e# s1#

instance Unboxed (FunPtr a)
  where
    bytes# !#   i# = FunPtr    (indexAddrArray# bytes# i#)
    newUnboxed  _  = newUnboxedByteArray word_scale
    
    writeByteArray# marr#    n# (FunPtr e#)    = \ s1# -> writeAddrArray# marr# n# e# s1#

instance Unboxed (StablePtr a)
  where
    bytes# !#   i# = StablePtr (indexStablePtrArray# bytes# i#)
    newUnboxed  _  = newUnboxedByteArray word_scale
    
    writeByteArray# marr#    n# (StablePtr e#) = \ s1# -> writeStablePtrArray# marr# n# e# s1#

{- Other instances. -}

instance Unboxed Bool
  where
    bytes# !# i# = isTrue# ((indexWordArray# bytes# (bool_index i#) `and#` bool_bit i#) `neWord#` int2Word# 0#)
    newUnboxed _ = newUnboxedByteArray bool_scale
    
    writeByteArray# marr# n# e = \ s1# -> case readWordArray# marr# i# s1# of
        (# s2#, old_byte# #) -> writeWordArray# marr# i# (bitWrite old_byte#) s2#
      where
        bitWrite old_byte# = if e then old_byte# `or#` bool_bit n# else old_byte# `and#` bool_not_bit n#
        i# = bool_index n#
    
    fillByteArray# marr# n# e = \ s1# -> setByteArray# marr# 0# (bool_scale n#) byte# s1#
      where
        !(I# byte#) = e ? 0xff $ 0

instance Unboxed Char
  where
    bytes# !# i# = C# (indexWideCharArray# bytes# i#)
    newUnboxed _ = newUnboxedByteArray (safe_scale 4#)
    
    writeByteArray# marr# n# (C# e#) = \ s1# -> writeWideCharArray# marr# n# e# s1#

instance Unboxed Float
  where
    bytes# !# i# = F# (indexFloatArray# bytes# i#)
    newUnboxed _ = newUnboxedByteArray float_scale
    
    writeByteArray# marr# n# (F# e#) = \ s1# -> writeFloatArray# marr# n# e# s1#

instance Unboxed Double
  where
    bytes# !# i# = D# (indexDoubleArray# bytes# i#)
    newUnboxed _ = newUnboxedByteArray double_scale
    
    writeByteArray# marr# n# (D# e#) = \ s1# -> writeDoubleArray# marr# n# e# s1#

--------------------------------------------------------------------------------

-- | newUnboxedByteArray is service function for ordinary newUnboxed decrarations.
{-# INLINE newUnboxedByteArray #-}
newUnboxedByteArray :: (Int# -> Int#) -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
newUnboxedByteArray n2l = \ n# s1# -> newByteArray# (n2l n#) s1#

{-# INLINE safe_scale #-}
{- |
  safe_scale is a service function that converts the scale and number of
  elements to length in bytes.
-}
safe_scale :: Int# -> (Int# -> Int#)
safe_scale scale# n# = if overflow then error "in SDP.Unboxed.safe_scale" else res#
  where
    !overflow = case maxBound of (I# maxN#) -> isTrue# (maxN# `divInt#` scale# <# n#)
    !res#     = scale# *# n#

bool_scale   :: Int# -> Int#
bool_scale   n# = (n# +# 7#) `uncheckedIShiftRA#` 3#

word_scale   :: Int# -> Int#
word_scale   n# = safe_scale scale# n# where !(I# scale#) = SIZEOF_HSWORD

float_scale  :: Int# -> Int#
float_scale  n# = safe_scale scale# n# where !(I# scale#) = SIZEOF_HSFLOAT

double_scale :: Int# -> Int#
double_scale n# = safe_scale scale# n# where !(I# scale#) = SIZEOF_HSDOUBLE

bool_bit        :: Int# -> Word#
bool_bit n#     =  case (SIZEOF_HSWORD * 8 - 1) of !(W# mask#) -> int2Word# 1# `uncheckedShiftL#` (word2Int# (int2Word# n# `and#` mask#))

bool_not_bit    :: Int# -> Word#
bool_not_bit n# =  case maxBound of !(W# mb#) -> bool_bit n# `xor#` mb#

bool_index :: Int# -> Int#
#if   SIZEOF_HSWORD == 4
bool_index = (`uncheckedIShiftRA#` 5#)
#elif SIZEOF_HSWORD == 8
bool_index = (`uncheckedIShiftRA#` 6#)
#endif

