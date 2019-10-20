{-# LANGUAGE Unsafe, CPP, MagicHash, UnboxedTuples, BangPatterns #-}
{-# LANGUAGE RoleAnnotations #-}

{- |
    Module      :  SDP.Unboxed
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
  
  @SDP.Unboxed@ provide service class 'Unboxed', that needed for "SDP.Bytes",
  "SDP.ByteList" and "SDP.ByteList.Ublist".
-}
module SDP.Unboxed
  (
    -- * Unboxed
    Unboxed (..),
    
    -- * Overloaded operations with primitives
    cloneUnboxed#, copyUnboxed#, copyUnboxedM#,
    
    -- * Related functions
    newUnboxedByteArray, safe_scale
  )
where

import Prelude ()
import SDP.SafePrelude

import GHC.Stable ( StablePtr (..) )
import GHC.Base   ( divInt# )
import GHC.Exts
import GHC.ST     ( runST, ST (..), STRep )

import GHC.Int  ( Int  (..), Int8  (..), Int16  (..), Int32  (..), Int64  (..) )
import GHC.Word ( Word (..), Word8 (..), Word16 (..), Word32 (..), Word64 (..) )
import GHC.Ptr  ( nullPtr, nullFunPtr )

#include "MachDeps.h"

default ()

--------------------------------------------------------------------------------

{- |
  Unboxed is an class for structures that use ByteArray and MutableByteArray
  primitives to store data. Unboxed is a simplified analogue of Storable.
  
  Unboxed is abstracted from a specific data structure and contains only those
  functions that are related to working with primitives. It does the most
  trivial part of the job, leaving all the high-level operations and
  implementation details to other classes.
  
  Unlike MArray (array), it doesn't try to do everything at once, turning the
  instance declaration of representatives into a boilerplate.
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
      newUnboxed creates new MutableByteArray of given count of elements.
      First argument used as type variable.
    -}
    newUnboxed :: e -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
    
    {-# INLINE newUnboxed' #-}
    {- |
      new Unboxed' is strict version of array, that use first argument as initial
      value. May fail when trying to write error or undefined.
    -}
    newUnboxed' :: e -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
    newUnboxed' e n# = \ s1# -> case newUnboxed e n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# e s2# of
        s3# -> (# s3#, marr# #)

--------------------------------------------------------------------------------

{- Int instances. -}

instance Unboxed Int
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = I# (indexIntArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readIntArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I# e# #)
    
    writeByteArray# marr# n# (I# e#) = writeIntArray# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray word_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Int) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Int8
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = I8# (indexInt8Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readInt8Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I8# e# #)
    
    writeByteArray# marr# n# (I8#  e#) = writeInt8Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (\ x -> x) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Int8) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Int16
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = I16# (indexInt16Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readInt16Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I16# e# #)
    
    writeByteArray# marr# n# (I16# e#) = writeInt16Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (safe_scale 2#) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Int16) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Int32
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = I32# (indexInt32Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readInt32Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I32# e# #)
    
    writeByteArray# marr# n# (I32# e#) = writeInt32Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (safe_scale 4#) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Int32) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Int64
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = I64# (indexInt64Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readInt64Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I64# e# #)
    
    writeByteArray# marr# n# (I64# e#) = writeInt64Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (safe_scale 8#) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Int64) s2# of
        s3# -> (# s3#, marr# #)

--------------------------------------------------------------------------------

{- Word instances. -}

instance Unboxed Word
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = W# (indexWordArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readWordArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W# e# #)
    
    writeByteArray# marr# n# (W#   e#) = writeWordArray# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray word_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Word) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Word8
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = W8# (indexWord8Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readWord8Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W8# e# #)
    
    writeByteArray# marr# n# (W8#  e#) = writeWord8Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (\ x -> x) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Word8) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Word16
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = W16# (indexWord16Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readWord16Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W16# e# #)
    
    writeByteArray# marr# n# (W16# e#) = writeWord16Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (safe_scale 2#) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Word16) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Word32
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = W32# (indexWord32Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readWord32Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W32# e# #)
    
    writeByteArray# marr# n# (W32# e#) = writeWord32Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (safe_scale 4#) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Word32) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Word64
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = W64# (indexWord64Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readWord64Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W64# e# #)
    
    writeByteArray# marr# n# (W64# e#) = writeWord64Array# marr# n# e#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (safe_scale 8#) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Word64) s2# of
        s3# -> (# s3#, marr# #)

--------------------------------------------------------------------------------

{- Pointer instances. -}

instance Unboxed (Ptr a)
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = Ptr (indexAddrArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readAddrArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, Ptr e# #)
    
    writeByteArray# marr# n# (Ptr e) = writeAddrArray# marr# n# e
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray word_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# nullPtr s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed (FunPtr a)
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = FunPtr (indexAddrArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readAddrArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, FunPtr e# #)
    
    writeByteArray# marr# n# (FunPtr e) = writeAddrArray# marr# n# e
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray word_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# nullFunPtr s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed (StablePtr a)
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = StablePtr (indexStablePtrArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readStablePtrArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, StablePtr e# #)
    
    writeByteArray# marr# n# (StablePtr e) = writeStablePtrArray# marr# n# e
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray word_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# nullStablePtr s2# of
        s3# -> (# s3#, marr# #)

nullStablePtr :: StablePtr a
nullStablePtr =  StablePtr (unsafeCoerce# 0#)

--------------------------------------------------------------------------------

{- Other instances. -}

instance Unboxed ()
  where
    {-# INLINE (!#) #-}
    (!#) = \ _ _ -> ()
    _ !># _ = \ s# -> (# s#, () #)
    
    newUnboxed  _ _ = newByteArray# 0#
    newUnboxed' _ _ = newByteArray# 0#
    
    writeByteArray# _ _ = \ _ s# -> s#
    fillByteArray#  _ _ = \ _ s# -> s#

instance Unboxed Bool
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = isTrue# ((indexWordArray# bytes# (bool_index i#) `and#` bool_bit i#) `neWord#` int2Word# 0#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readWordArray# mbytes# (bool_index i#) s1# of
      (# s2#, e# #) -> (# s2#, isTrue# ((e# `and#` bool_bit i#) `neWord#` int2Word# 0#) #)
    
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
    {-# INLINE (!#) #-}
    bytes#  !#  i# = C# (indexWideCharArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readWideCharArray# mbytes# i# s1# of
      (# s2#, c# #) -> (# s2#, C# c# #)
    
    writeByteArray# marr# n# (C# e#) = \ s1# -> writeWideCharArray# marr# n# e# s1#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray (safe_scale 4#) n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# '\0' s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Float
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = F# (indexFloatArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readFloatArray# mbytes# i# s1# of
      (# s2#, f# #) -> (# s2#, F# f# #)
    
    writeByteArray# marr# n# (F# e#) = \ s1# -> writeFloatArray# marr# n# e# s1#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray float_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Float) s2# of
        s3# -> (# s3#, marr# #)

instance Unboxed Double
  where
    {-# INLINE (!#) #-}
    bytes#  !#  i# = D# (indexDoubleArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readDoubleArray# mbytes# i# s1# of
      (# s2#, d# #) -> (# s2#, D# d# #)
    
    writeByteArray# marr# n# (D# e#) = \ s1# -> writeDoubleArray# marr# n# e# s1#
    
    newUnboxed _ n# = \ s1# -> case newUnboxedByteArray double_scale n# s1# of
      (# s2#, marr# #) -> case fillByteArray# marr# n# (0 :: Double) s2# of
        s3# -> (# s3#, marr# #)

--------------------------------------------------------------------------------

-- Just a wrapper, used once to lift ByteArray# from ST.
data Wrap = Wrap { unwrap :: ByteArray# }

{- |
  @cloneUnboxed\# e o\# c\#@ creates byte array with @c\#@ elements of same type
  as @e@ beginning from @o\#@ elements.
-}
cloneUnboxed# :: (Unboxed e) => e -> ByteArray# -> Int# -> Int# -> ByteArray#
cloneUnboxed# e arr# o# c# = unwrap $ runST $ ST $
  \ s1# -> case newUnboxed e c# s1# of
    (# s2#, marr# #) -> case copyUnboxed# e arr# o# marr# 0# c# s2# of
      s3# -> case unsafeFreezeByteArray# marr# s3# of
        (# s4#, arr'# #) -> (# s4#, (Wrap arr'#) #)

{- |
  @copyUnboxed\# e arr\# o1\# marr\# o2\# n\#@ writes elements from @arr\#@'s
  @[o1\# .. o1\# +\# n\#]@ range to @marr\#@'s @[o2\# .. o2\# +\# n\#]@ range.
-}
copyUnboxed# :: (Unboxed e) => e -> ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyUnboxed# _  _    _    _    _  0# = \ sn# -> sn#
copyUnboxed# e arr# o1# marr# o2# c# = \ s1# ->
  case writeByteArray# marr# o2# ((arr# !# o1#) `asTypeOf` e) s1# of
    s2# -> copyUnboxed# e arr# (o1# +# 1#) marr# (o2# +# 1#) (c# -# 1#) s2#

{- |
  @copyUnboxed\# e msrc\# o1# marr\# o2# n\#@ writes elements from @msrc\#@'s
  @[o1\# .. o1\# +\# n\#]@ range to @marr\#@'s @[02\# .. o2\# +\# n\#]@ range.
-}
copyUnboxedM# :: (Unboxed e) => e -> MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
copyUnboxedM# _  _    _    _    _  0# = \ sn# -> sn#
copyUnboxedM# e src# o1# marr# o2# n# = \ s1# -> case (!>#) src# o1# s1# of
  (# s2#, x #) -> case writeByteArray# marr# o2# (x `asTypeOf` e) s2# of
    s3# -> copyUnboxedM# e src# (o1# +# 1#) marr# (o2# +# 1#) (n# -# 1#) s3#

--------------------------------------------------------------------------------

{- Related. -}

{- |
  newUnboxedByteArray is service function for ordinary newUnboxed decrarations.
  
  @newUnboxedByteArray f i\#@ creates new MutableByteArray\# of real
  length (f i\#), where i\# - count of element, f - non-negative function
  (e.g. @newUnboxedByteArray double_scale == newUnboxed@ for 'Float').
-}
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

--------------------------------------------------------------------------------

{- Scales. -}

{-# INLINE bool_scale #-}
bool_scale   :: Int# -> Int#
bool_scale   n# = (n# +# 7#) `uncheckedIShiftRA#` 3#

{-# INLINE word_scale #-}
word_scale   :: Int# -> Int#
word_scale   n# = safe_scale scale# n# where !(I# scale#) = SIZEOF_HSWORD

{-# INLINE float_scale #-}
float_scale  :: Int# -> Int#
float_scale  n# = safe_scale scale# n# where !(I# scale#) = SIZEOF_HSFLOAT

{-# INLINE double_scale #-}
double_scale :: Int# -> Int#
double_scale n# = safe_scale scale# n# where !(I# scale#) = SIZEOF_HSDOUBLE

{-# INLINE bool_bit #-}
bool_bit        :: Int# -> Word#
bool_bit n#     =  case (SIZEOF_HSWORD * 8 - 1) of
  !(W# mask#) -> int2Word# 1# `uncheckedShiftL#` (word2Int# (int2Word# n# `and#` mask#))

{-# INLINE bool_not_bit #-}
bool_not_bit    :: Int# -> Word#
bool_not_bit n# =  case maxBound of !(W# mb#) -> bool_bit n# `xor#` mb#

{-# INLINE bool_index #-}
bool_index :: Int# -> Int#
#if   SIZEOF_HSWORD == 4
bool_index = (`uncheckedIShiftRA#` 5#)
#elif SIZEOF_HSWORD == 8
bool_index = (`uncheckedIShiftRA#` 6#)
#endif




