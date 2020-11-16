{-# LANGUAGE Unsafe, CPP, MagicHash, UnboxedTuples, BangPatterns #-}

{- |
    Module      :  SDP.Unboxed
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Unboxed" provide service class 'Unboxed', that needed for
    "SDP.Prim.SBytes"-based structures.
-}
module SDP.Unboxed
  (
    -- * Unboxed
    Unboxed (..), cloneUnboxed#, sizeof#,
    
    -- ** Proxy
    psizeof, pnewUnboxed, pcopyUnboxed, pcopyUnboxedM,
    pnewUnboxed1, pcopyUnboxed1, pcopyUnboxedM1,
    
    -- *** Proxy helpers
    fromProxy, fromProxy1
  )
where

import Prelude ()
import SDP.SafePrelude
import SDP.Ratio

import GHC.Stable
import GHC.Base
import GHC.Word
import GHC.Int
import GHC.Ptr
import GHC.ST

import Data.Complex

import Foreign.C.Types

#include "MachDeps.h"

default ()

--------------------------------------------------------------------------------

{- |
  'Unboxed' is a layer between untyped raw data and parameterized unboxed data
  structures. Also it prevents direct interaction with primitives.
-}
class (Eq e) => Unboxed e
  where
    {-# MINIMAL sizeof, (!#), (!>#), writeByteArray#, newUnboxed #-}
    
    {- |
      @sizeof e n@ returns the length (in bytes) of primitive, where @n@ - count
      of elements, @e@ - type parameter.
    -}
    sizeof :: e -> Int -> Int
    
    -- | Unsafe 'ByteArray#' reader with overloaded result type.
    (!#) :: ByteArray# -> Int# -> e
    
    -- | Unsafe 'MutableByteArray#' reader with overloaded result type.
    (!>#) :: MutableByteArray# s -> Int# -> State# s -> (# State# s, e #)
    
    -- | Unsafe 'MutableByteArray#' writer.
    writeByteArray# :: MutableByteArray# s -> Int# -> e -> State# s -> State# s
    
    {-# INLINE fillByteArray# #-}
    -- | Procedure for filling the array with the default value (like calloc).
    fillByteArray# :: MutableByteArray# s -> Int# -> e -> State# s -> State# s
    fillByteArray# mbytes# n# e = isTrue# (n# <# 1#) ? (\ s1# -> s1#) $
      \ s1# -> case writeByteArray# mbytes# (n# -# 1#) e s1# of
        s2# -> fillByteArray# mbytes# (n# -# 1#) e s2#
    
    {- |
      'newUnboxed' creates new 'MutableByteArray#' of given count of elements.
      First argument used as type variable.
    -}
    newUnboxed :: e -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
    
    {-# INLINE newUnboxed' #-}
    {- |
      'newUnboxed'' is version of 'newUnboxed', that use first argument as
      initial value. May fail when trying to write 'error' or 'undefined'.
    -}
    newUnboxed' :: e -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
    newUnboxed' e n# = \ s1# -> case newUnboxed e n# s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# e s2# of
        s3# -> (# s3#, mbytes# #)
    
    {- |
      @'copyUnboxed#' e bytes\# o1\# mbytes\# o2\# n\#@ unsafely writes elements
      from @bytes\#@ to @mbytes\#@, where o1\# and o2\# - offsets (element
      count), @n\#@ - count of elements to copy.
    -}
    copyUnboxed# :: e -> ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
    copyUnboxed# e bytes# o1# mbytes# o2# n# = copyByteArray# bytes# (sizeof# e o1#) mbytes# (sizeof# e o2#) (sizeof# e n#)
    
    {- |
      @'copyUnboxedM#' e msrc\# o1\# mbytes\# o2\# n\#@ unsafely writes elements
      from @msrc\#@ to @mbytes\#@, where o1\# and o2\# - offsets (element
      count), @n\#@ - count of elements to copy.
    -}
    copyUnboxedM# :: e -> MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
    copyUnboxedM# e msrc# o1# mbytes# o2# n# = copyMutableByteArray# msrc# (sizeof# e o1#) mbytes# (sizeof# e o2#) (sizeof# e n#)

--------------------------------------------------------------------------------

-- | 'sizeof#' is unboxed 'sizeof'.
{-# INLINE sizeof# #-}
sizeof# :: (Unboxed e) => e -> Int# -> Int#
sizeof# =  \ e c# -> case sizeof e (I# c#) of I# n# -> n#

-- | 'psizeof' is 'Proxy' 'sizeof'.
{-# INLINE psizeof #-}
psizeof :: (Unboxed e) => proxy e -> Int -> Int
psizeof =  sizeof . fromProxy

-- | @(* -> *)@ kind proxy version of 'newUnboxed'.
pnewUnboxed :: (Unboxed e) => proxy e -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
pnewUnboxed =  newUnboxed . fromProxy

-- | @(* -> *)@ kind proxy version if 'copyUnboxed#'.
pcopyUnboxed :: (Unboxed e) => proxy e -> ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
pcopyUnboxed =  copyUnboxed# . fromProxy

-- | Proxy version if 'copyUnboxedM1'.
pcopyUnboxedM :: (Unboxed e) => proxy e -> MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
pcopyUnboxedM =  copyUnboxedM# . fromProxy

fromProxy :: proxy e -> e
fromProxy =  const undefined

-- | @(* -> * -> *)@ kind proxy version of 'newUnboxed'.
pnewUnboxed1 :: (Unboxed e) => m (proxy e) -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
pnewUnboxed1 =  newUnboxed . fromProxy1

-- | @(* -> * -> *)@ kind proxy version if 'copyUnboxed#'.
pcopyUnboxed1 :: (Unboxed e) => m (proxy e) -> ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
pcopyUnboxed1 =  copyUnboxed# . fromProxy1

-- | @(* -> * -> *)@ kind proxy version if 'copyUnboxedM1'.
pcopyUnboxedM1 :: (Unboxed e) => m (proxy e) -> MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
pcopyUnboxedM1 =  copyUnboxedM# . fromProxy1

fromProxy1 :: m (proxy e) -> e
fromProxy1 =  const undefined

--------------------------------------------------------------------------------

{- Int instances. -}

instance Unboxed Int
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * SIZEOF_HSWORD
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I# (indexIntArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readIntArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I# e# #)
    
    writeByteArray# mbytes# n# (I# e#) = writeIntArray# mbytes# n# e#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# (0 :: Int) s2# of
        s3# -> (# s3#, mbytes# #)

instance Unboxed Int8
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I8# (indexInt8Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readInt8Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I8# e# #)
    
    writeByteArray# mbytes# n# (I8#  e#) = writeInt8Array# mbytes# n# e#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# (0 :: Int8) s2# of
        s3# -> (# s3#, mbytes# #)

instance Unboxed Int16
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * 2
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I16# (indexInt16Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readInt16Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I16# e# #)
    
    writeByteArray# mbytes# n# (I16# e#) = writeInt16Array# mbytes# n# e#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# (0 :: Int16) s2# of
        s3# -> (# s3#, mbytes# #)

instance Unboxed Int32
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * 4
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I32# (indexInt32Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readInt32Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I32# e# #)
    
    writeByteArray# mbytes# n# (I32# e#) = writeInt32Array# mbytes# n# e#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# (0 :: Int32) s2# of
        s3# -> (# s3#, mbytes# #)

instance Unboxed Int64
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * 8
    
    {-# INLINE (!#) #-}
    bytes# !# i# = I64# (indexInt64Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readInt64Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, I64# e# #)
    
    writeByteArray# mbytes# n# (I64# e#) = writeInt64Array# mbytes# n# e#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# (0 :: Int64) s2# of
        s3# -> (# s3#, mbytes# #)

--------------------------------------------------------------------------------

{- Word instances. -}

instance Unboxed Word
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * SIZEOF_HSWORD
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W# (indexWordArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readWordArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W# e# #)
    
    writeByteArray# mbytes# n# (W#   e#) = writeWordArray# mbytes# n# e#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# (0 :: Word) s2# of
        s3# -> (# s3#, mbytes# #)

instance Unboxed Word8
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W8# (indexWord8Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readWord8Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W8# e# #)
    
    writeByteArray# mbytes# n# (W8#  e#) = writeWord8Array# mbytes# n# e#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# (0 :: Word8) s2# of
        s3# -> (# s3#, mbytes# #)

instance Unboxed Word16
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * 2
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W16# (indexWord16Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readWord16Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W16# e# #)
    
    writeByteArray# mbytes# n# (W16# e#) = writeWord16Array# mbytes# n# e#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# (0 :: Word16) s2# of
        s3# -> (# s3#, mbytes# #)

instance Unboxed Word32
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * 4
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W32# (indexWord32Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readWord32Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W32# e# #)
    
    writeByteArray# mbytes# n# (W32# e#) = writeWord32Array# mbytes# n# e#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# (0 :: Word32) s2# of
        s3# -> (# s3#, mbytes# #)

instance Unboxed Word64
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * 8
    
    {-# INLINE (!#) #-}
    bytes# !# i# = W64# (indexWord64Array# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readWord64Array# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, W64# e# #)
    
    writeByteArray# mbytes# n# (W64# e#) = writeWord64Array# mbytes# n# e#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# (0 :: Word64) s2# of
        s3# -> (# s3#, mbytes# #)

--------------------------------------------------------------------------------

{- Pointer instances. -}

instance Unboxed (Ptr a)
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * SIZEOF_HSWORD
    
    {-# INLINE (!#) #-}
    bytes# !# i# = Ptr (indexAddrArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readAddrArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, Ptr e# #)
    
    writeByteArray# mbytes# n# (Ptr e) = writeAddrArray# mbytes# n# e
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# nullPtr s2# of
        s3# -> (# s3#, mbytes# #)

instance Unboxed (FunPtr a)
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * SIZEOF_HSWORD
    
    {-# INLINE (!#) #-}
    bytes#  !#  i# = FunPtr (indexAddrArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readAddrArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, FunPtr e# #)
    
    writeByteArray# mbytes# n# (FunPtr e) = writeAddrArray# mbytes# n# e
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# nullFunPtr s2# of
        s3# -> (# s3#, mbytes# #)

instance Unboxed (StablePtr a)
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * SIZEOF_HSWORD
    
    {-# INLINE (!#) #-}
    bytes# !# i# = StablePtr (indexStablePtrArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readStablePtrArray# mbytes# i# s1# of
      (# s2#, e# #) -> (# s2#, StablePtr e# #)
    
    writeByteArray# mbytes# n# (StablePtr e) = writeStablePtrArray# mbytes# n# e
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# nullStablePtr s2# of
        s3# -> (# s3#, mbytes# #)

nullStablePtr :: StablePtr a
nullStablePtr =  StablePtr (unsafeCoerce# 0#)

--------------------------------------------------------------------------------

{- Foreign C instances. -}

#define SDP_DERIVE_FOREIGN_UNBOXED(Type)\
instance Unboxed Type where\
{\
  sizeof e = sizeof (unpackUndefined e Type);\
  arr# !# i# = Type ( arr# !# i# );\
  marr# !># i# = \ s1# -> case (!>#) marr# i# s1# of {(# s2#, e #) -> (# s2#, Type e #)};\
  writeByteArray# marr# i# (Type e) = writeByteArray# marr# i# e;\
  fillByteArray#  marr# i# (Type e) = fillByteArray#  marr# i# e;\
  newUnboxed  (Type e) = newUnboxed  e;\
  newUnboxed' (Type e) = newUnboxed' e;\
}

SDP_DERIVE_FOREIGN_UNBOXED(CChar)
SDP_DERIVE_FOREIGN_UNBOXED(CSChar)
SDP_DERIVE_FOREIGN_UNBOXED(CWchar)
SDP_DERIVE_FOREIGN_UNBOXED(CShort)
SDP_DERIVE_FOREIGN_UNBOXED(CUShort)

SDP_DERIVE_FOREIGN_UNBOXED(CInt)
SDP_DERIVE_FOREIGN_UNBOXED(CUInt)
SDP_DERIVE_FOREIGN_UNBOXED(CLong)
SDP_DERIVE_FOREIGN_UNBOXED(CULong)
SDP_DERIVE_FOREIGN_UNBOXED(CLLong)
SDP_DERIVE_FOREIGN_UNBOXED(CULLong)
SDP_DERIVE_FOREIGN_UNBOXED(CIntPtr)
SDP_DERIVE_FOREIGN_UNBOXED(CUIntPtr)
SDP_DERIVE_FOREIGN_UNBOXED(CIntMax)
SDP_DERIVE_FOREIGN_UNBOXED(CUIntMax)
SDP_DERIVE_FOREIGN_UNBOXED(CPtrdiff)

SDP_DERIVE_FOREIGN_UNBOXED(CTime)
SDP_DERIVE_FOREIGN_UNBOXED(CClock)
SDP_DERIVE_FOREIGN_UNBOXED(CUSeconds)
SDP_DERIVE_FOREIGN_UNBOXED(CSUSeconds)

SDP_DERIVE_FOREIGN_UNBOXED(CSize)
SDP_DERIVE_FOREIGN_UNBOXED(CBool)
SDP_DERIVE_FOREIGN_UNBOXED(CFloat)
SDP_DERIVE_FOREIGN_UNBOXED(CDouble)
SDP_DERIVE_FOREIGN_UNBOXED(CSigAtomic)

#undef SDP_DERIVE_FOREIGN_UNBOXED

--------------------------------------------------------------------------------

{- Other instances. -}

instance Unboxed ()
  where
    {-# INLINE sizeof #-}
    sizeof _ _ = 0
    
    {-# INLINE (!#) #-}
    (!>#) = \ _ _ s# -> (# s#, () #)
    (!#)  = \ _ _ -> ()
    
    newUnboxed  _ _ = newByteArray# 0#
    newUnboxed' _ _ = newByteArray# 0#
    
    writeByteArray# _ _ = \ _ s# -> s#
    fillByteArray#  _ _ = \ _ s# -> s#

instance Unboxed Bool
  where
    {-# INLINE sizeof #-}
    sizeof _ c = d == 0 ? n $ n + 1 where (n, d) = max 0 c `divMod` 8
    
    {-# INLINE (!#) #-}
    bytes# !# i# = isTrue# ((indexWordArray# bytes# (bool_index i#) `and#` bool_bit i#) `neWord#` int2Word# 0#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readWordArray# mbytes# (bool_index i#) s1# of
      (# s2#, e# #) -> (# s2#, isTrue# ((e# `and#` bool_bit i#) `neWord#` int2Word# 0#) #)
    
    writeByteArray# mbytes# n# e = \ s1# -> case readWordArray# mbytes# i# s1# of
        (# s2#, old_byte# #) -> writeWordArray# mbytes# i# (bitWrite old_byte#) s2#
      where
        bitWrite old_byte# = if e then old_byte# `or#` bool_bit n# else old_byte# `and#` bool_not_bit n#
        i# = bool_index n#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# False s2# of
        s3# -> (# s3#, mbytes# #)
    
    fillByteArray# mbytes# n# e = setByteArray# mbytes# 0# (bool_scale n#) byte#
      where
        byte# = if e then 0xff# else 0#
    
    copyUnboxed# e bytes# o1# mbytes# o2# c# = isTrue# (c# <# 1#) ? (\ s1# -> s1#) $
      \ s1# -> case writeByteArray# mbytes# o2# ((bytes# !# o1#) `asTypeOf` e) s1# of
        s2# -> copyUnboxed# e bytes# (o1# +# 1#) mbytes# (o2# +# 1#) (c# -# 1#) s2#
    
    copyUnboxedM# e src# o1# mbytes# o2# n# = \ s1# -> case (!>#) src# o1# s1# of
      (# s2#, x #) -> case writeByteArray# mbytes# o2# (x `asTypeOf` e) s2# of
        s3# -> copyUnboxedM# e src# (o1# +# 1#) mbytes# (o2# +# 1#) (n# -# 1#) s3#

instance Unboxed Char
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * 4
    
    {-# INLINE (!#) #-}
    bytes# !# i# = C# (indexWideCharArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readWideCharArray# mbytes# i# s1# of
      (# s2#, c# #) -> (# s2#, C# c# #)
    
    writeByteArray# mbytes# n# (C# e#) = writeWideCharArray# mbytes# n# e#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# '\0' s2# of
        s3# -> (# s3#, mbytes# #)

instance Unboxed Float
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * SIZEOF_HSFLOAT
    
    {-# INLINE (!#) #-}
    bytes# !# i# = F# (indexFloatArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readFloatArray# mbytes# i# s1# of
      (# s2#, f# #) -> (# s2#, F# f# #)
    
    writeByteArray# mbytes# n# (F# e#) = writeFloatArray# mbytes# n# e#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# (0 :: Float) s2# of
        s3# -> (# s3#, mbytes# #)

instance Unboxed Double
  where
    {-# INLINE sizeof #-}
    sizeof _ n = max 0 n * SIZEOF_HSDOUBLE
    
    {-# INLINE (!#) #-}
    bytes# !# i# = D# (indexDoubleArray# bytes# i#)
    
    {-# INLINE (!>#) #-}
    mbytes# !># i# = \ s1# -> case readDoubleArray# mbytes# i# s1# of
      (# s2#, d# #) -> (# s2#, D# d# #)
    
    writeByteArray# mbytes# n# (D# e#) = writeDoubleArray# mbytes# n# e#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# (0 :: Double) s2# of
        s3# -> (# s3#, mbytes# #)

instance (Unboxed a, Integral a) => Unboxed (Ratio a)
  where
    sizeof e n = 2 * psizeof e n
    
    bytes# !# i# = bytes# !# i2# :% (bytes# !# (i2# +# 1#)) where i2# = 2# *# i#
    
    mbytes# !># i# = let i2# = 2# *# i# in \ s1# -> case (!>#) mbytes# i2# s1# of
      (# s2#, n #) -> case (!>#) mbytes# (i2# +# 1#) s2# of
        (# s3#, d #) -> (# s3#, n :% d #)
    
    writeByteArray# mbytes# i# (n :% d) = let i2# = 2# *# i# in
      \ s1# -> case writeByteArray# mbytes# i2# n s1# of
        s2# -> writeByteArray# mbytes# (i2# +# 1#) d s2#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# ((0 :% 0) `asTypeOf` e) s2# of
        s3# -> (# s3#, mbytes# #)

instance (Unboxed a, Num a) => Unboxed (Complex a)
  where
    sizeof e n = 2 * psizeof e n
    
    bytes# !# i# = bytes# !# i2# :+ (bytes# !# (i2# +# 1#)) where i2# = 2# *# i#
    
    mbytes# !># i# = let i2# = 2# *# i# in \ s1# -> case (!>#) mbytes# i2# s1# of
      (# s2#, n #) -> case (!>#) mbytes# (i2# +# 1#) s2# of
        (# s3#, d #) -> (# s3#, n :+ d #)
    
    writeByteArray# mbytes# i# (n :+ d) = let i2# = 2# *# i# in
      \ s1# -> case writeByteArray# mbytes# i2# n s1# of
        s2# -> writeByteArray# mbytes# (i2# +# 1#) d s2#
    
    newUnboxed e n# = \ s1# -> case newByteArray# (sizeof# e n#) s1# of
      (# s2#, mbytes# #) -> case fillByteArray# mbytes# n# ((0 :+ 0) `asTypeOf` e) s2# of
        s3# -> (# s3#, mbytes# #)

--------------------------------------------------------------------------------

-- Just a wrapper, used once to lift ByteArray# from ST.
data Wrap = Wrap { unwrap :: ByteArray# }

{- |
  @cloneUnboxed\# e o\# c\#@ creates byte array with @c\#@ elements of same type
  as @e@ beginning from @o\#@ elements.
-}
cloneUnboxed# :: (Unboxed e) => e -> ByteArray# -> Int# -> Int# -> ByteArray#
cloneUnboxed# e bytes# o# c# = unwrap $ runST $ ST $
  \ s1# -> case newUnboxed e c# s1# of
    (# s2#, mbytes# #) -> case copyUnboxed# e bytes# o# mbytes# 0# c# s2# of
      s3# -> case unsafeFreezeByteArray# mbytes# s3# of
        (# s4#, bytes'# #) -> (# s4#, (Wrap bytes'#) #)

--------------------------------------------------------------------------------

{-# INLINE bool_scale #-}
bool_scale :: Int# -> Int#
bool_scale n# = (n# +# 7#) `uncheckedIShiftRA#` 3#

{-# INLINE bool_bit #-}
bool_bit :: Int# -> Word#
bool_bit n# =  case (SIZEOF_HSWORD * 8 - 1) of
  !(W# mask#) -> int2Word# 1# `uncheckedShiftL#` word2Int# (int2Word# n# `and#` mask#)

{-# INLINE bool_not_bit #-}
bool_not_bit    :: Int# -> Word#
bool_not_bit n# =  case maxBound of !(W# mb#) -> bool_bit n# `xor#` mb#

{-# INLINE bool_index #-}
bool_index :: Int# -> Int#
#if   SIZEOF_HSWORD == 4
bool_index =  (`uncheckedIShiftRA#` 5#)
#elif SIZEOF_HSWORD == 8
bool_index =  (`uncheckedIShiftRA#` 6#)
#endif

unpackUndefined :: b -> (a -> b) -> a
unpackUndefined =  \ _ _ -> undefined




