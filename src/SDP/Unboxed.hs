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

module SDP.Unboxed
  (
    Unboxed (..),
    newUnboxedByteArray,
    safe_scale
  )
where

import Prelude ()
import SDP.SafePrelude

import GHC.Stable ( StablePtr (..) )
import GHC.Base   ( divInt# )
import GHC.Exts
import GHC.ST     ( ST (..), STRep )

import GHC.Int  ( Int  (..), Int8  (..), Int16  (..), Int32  (..), Int64  (..) )
import GHC.Word ( Word (..), Word8 (..), Word16 (..), Word32 (..), Word64 (..) )
import GHC.Ptr  ( nullPtr, nullFunPtr )

#include "MachDeps.h"

default ()

--------------------------------------------------------------------------------

{- |
  Unboxed is a class that allows creating, writing and reading from byte arrays.
  It partly repeats the functionality of the MArray from the array,
  but solves a narrower range of tasks.
-}

class (Eq e) => Unboxed e
  where
    -- | Unsafe ByteList reader with overloaded result type.
    (!#)            :: ByteArray# -> Int# -> e
    
    -- | Unsafe MutableByteArray reader with overloaded result type.
    (!>#)           :: MutableByteArray# s -> Int# -> STRep s e
    
    -- | Unsafe MutableByteArray writer.
    writeByteArray# :: MutableByteArray# s -> Int# -> e -> State# s -> State# s
    
    {-# INLINE fillByteArray# #-}
    -- | Procedure for filling the array with the default value (like calloc).
    fillByteArray#  :: MutableByteArray# s -> Int# -> e -> State# s -> State# s
    fillByteArray# marr# n# e = \ s1# -> case sequence_
        [
          ST $ \ sn# -> case writeByteArray# marr# i# e sn# of
            sn1# -> (# sn1#, () #) | (I# i#) <- [0 .. (I# n#) - 1]
        ]
        of ST rep -> case rep s1# of (# s2#, () #) -> s2#
    
    {- |
      newUnboxed creates new MutableByteArray.
      First argument used as type variable.
    -}
    newUnboxed      :: e -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
    
    {-# INLINE newUnboxed' #-}
    {- |
      new Unboxed' is strict version of array, that use first argument as initial
      value. May fail when trying to write error or undefined.
    -}
    newUnboxed'     :: e -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
    newUnboxed' e n# = \ s1# -> case newUnboxed e n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# e s2# of
        s3# -> (# s3#, marr# #)

--------------------------------------------------------------------------------

{- Int instances. -}

instance Unboxed Int
  where
    bytes#  !#  i# = I# (indexIntArray# bytes# i#)
    mbytes# !># i# = \ s1# -> case readIntArray# mbytes# i# s1# of (# s2#, e# #) -> (# s2#, I# e# #)
    
    writeByteArray# marr# n# (I# e#) = writeIntArray# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray word_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Int) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Int8
  where
    bytes#  !#  i# = I8# (indexInt8Array# bytes# i#)
    mbytes# !># i# = \ s1# -> case readInt8Array# mbytes# i# s1# of (# s2#, e# #) -> (# s2#, I8# e# #)
    
    writeByteArray# marr# n# (I8#  e#) = writeInt8Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (\ x -> x) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Int8) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Int16
  where
    bytes#  !#  i# = I16# (indexInt16Array# bytes# i#)
    mbytes# !># i# = \ s1# -> case readInt16Array# mbytes# i# s1# of (# s2#, e# #) -> (# s2#, I16# e# #)
    
    writeByteArray# marr# n# (I16# e#) = writeInt16Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (safe_scale 2#) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Int16) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Int32
  where
    bytes#  !#  i# = I32# (indexInt32Array# bytes# i#)
    mbytes# !># i# = \ s1# -> case readInt32Array# mbytes# i# s1# of (# s2#, e# #) -> (# s2#, I32# e# #)
    
    writeByteArray# marr# n# (I32# e#) = writeInt32Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (safe_scale 4#) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Int32) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Int64
  where
    bytes#  !#  i# = I64# (indexInt64Array# bytes# i#)
    mbytes# !># i# = \ s1# -> case readInt64Array# mbytes# i# s1# of (# s2#, e# #) -> (# s2#, I64# e# #)
    
    writeByteArray# marr# n# (I64# e#) = writeInt64Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (safe_scale 8#) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Int64) s2# of
        s3# -> (# s3#, marr# #)

{- Word instances. -}

instance Unboxed Word
  where
    bytes#  !#  i# = W# (indexWordArray# bytes# i#)
    mbytes# !># i# = \ s1# -> case readWordArray# mbytes# i# s1# of (# s2#, e# #) -> (# s2#, W# e# #)
    
    writeByteArray# marr# n# (W#   e#) = writeWordArray# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray word_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Word) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Word8
  where
    bytes#  !#  i# = W8# (indexWord8Array# bytes# i#)
    mbytes# !># i# = \ s1# -> case readWord8Array# mbytes# i# s1# of (# s2#, e# #) -> (# s2#, W8# e# #)
    
    writeByteArray# marr# n# (W8#  e#) = writeWord8Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (\ x -> x) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Word8) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Word16
  where
    bytes#  !#  i# = W16# (indexWord16Array# bytes# i#)
    mbytes# !># i# = \ s1# -> case readWord16Array# mbytes# i# s1# of (# s2#, e# #) -> (# s2#, W16# e# #)
    
    writeByteArray# marr# n# (W16# e#) = writeWord16Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (safe_scale 2#) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Word16) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Word32
  where
    bytes#  !#  i# = W32# (indexWord32Array# bytes# i#)
    mbytes# !># i# = \ s1# -> case readWord32Array# mbytes# i# s1# of (# s2#, e# #) -> (# s2#, W32# e# #)
    
    writeByteArray# marr# n# (W32# e#) = writeWord32Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (safe_scale 4#) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Word32) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Word64
  where
    bytes#  !#  i# = W64# (indexWord64Array# bytes# i#)
    mbytes# !># i# = \ s1# -> case readWord64Array# mbytes# i# s1# of (# s2#, e# #) -> (# s2#, W64# e# #)
    
    writeByteArray# marr# n# (W64# e#) = writeWord64Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (safe_scale 8#) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Word64) s2# of
        s3# -> (# s3#, marr# #)

{- Pointer instances. -}

instance Unboxed (Ptr a)
  where
    bytes#  !#  i# = Ptr (indexAddrArray# bytes# i#)
    mbytes# !># i# = \ s1# -> case readAddrArray# mbytes# i# s1# of (# s2#, e# #) -> (# s2#, Ptr e# #)
    
    writeByteArray# marr# n# (Ptr e) = writeAddrArray# marr# n# e
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray word_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# nullPtr s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed (FunPtr a)
  where
    bytes#  !#  i# = FunPtr (indexAddrArray# bytes# i#)
    mbytes# !># i# = \ s1# -> case readAddrArray# mbytes# i# s1# of (# s2#, e# #) -> (# s2#, FunPtr e# #)
    
    writeByteArray# marr# n# (FunPtr e) = writeAddrArray# marr# n# e
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray word_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# nullFunPtr s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed (StablePtr a)
  where
    bytes#  !#  i# = StablePtr (indexStablePtrArray# bytes# i#)
    mbytes# !># i# = \ s1# -> case readStablePtrArray# mbytes# i# s1# of (# s2#, e# #) -> (# s2#, StablePtr e# #)
    
    writeByteArray# marr# n# (StablePtr e) = writeStablePtrArray# marr# n# e
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray word_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# nullStablePtr s2# of
        s3# -> (# s3#, marr# #)

{- Other instances. -}

instance Unboxed Bool
  where
    bytes#  !#  i# = isTrue# ((indexWordArray# bytes# (bool_index i#) `and#` bool_bit i#) `neWord#` int2Word# 0#)
    mbytes# !># i# = \ s1# -> case readWordArray# mbytes# (bool_index i#) s1# of (# s2#, e# #) -> (# s2#, isTrue# ((e# `and#` bool_bit i#) `neWord#` int2Word# 0#) #)
    
    writeByteArray# marr# n# e = \ s1# -> case readWordArray# marr# i# s1# of
        (# s2#, old_byte# #) -> writeWordArray# marr# i# (bitWrite old_byte#) s2#
      where
        bitWrite old_byte# = if e then old_byte# `or#` bool_bit n# else old_byte# `and#` bool_not_bit n#
        i# = bool_index n#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray word_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# False s2# of
        s3# -> (# s3#, marr# #)
    
    fillByteArray# marr# n# e = \ s1# -> setByteArray# marr# 0# (bool_scale n#) byte# s1#
      where
        !(I# byte#) = e ? 0xff $ 0

instance Unboxed Char
  where
    bytes#  !#  i# = C# (indexWideCharArray# bytes# i#)
    mbytes# !># i# = \ s1# -> case readWideCharArray# mbytes# i# s1# of (# s2#, c# #) -> (# s2#, C# c# #)
    
    writeByteArray# marr# n# (C# e#) = \ s1# -> writeWideCharArray# marr# n# e# s1#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (safe_scale 4#) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# '\0' s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Float
  where
    bytes#  !#  i# = F# (indexFloatArray# bytes# i#)
    mbytes# !># i# = \ s1# -> case readFloatArray# mbytes# i# s1# of (# s2#, f# #) -> (# s2#, F# f# #)
    
    writeByteArray# marr# n# (F# e#) = \ s1# -> writeFloatArray# marr# n# e# s1#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray float_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Float) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Double
  where
    bytes#  !#  i# = D# (indexDoubleArray# bytes# i#)
    mbytes# !># i# = \ s1# -> case readDoubleArray# mbytes# i# s1# of (# s2#, d# #) -> (# s2#, D# d# #)
    
    writeByteArray# marr# n# (D# e#) = \ s1# -> writeDoubleArray# marr# n# e# s1#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray double_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Double) s2# of
        s3# -> (# s3#, marr# #)

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

nullStablePtr :: StablePtr a
nullStablePtr =  StablePtr (unsafeCoerce# 0#)

--------------------------------------------------------------------------------

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

