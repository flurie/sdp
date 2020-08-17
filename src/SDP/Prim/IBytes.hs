{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, BangPatterns #-}

{- |
    Module      : SDP.Prim.IBytes
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.Prim.IBytes@ provides 'IOBytes\#' - pseudo-primitive unboxed array.
-}
module SDP.Prim.IBytes
(
  -- * Export
  module SDP.Unboxed,
  
  -- * Pseudo-primitive types
  IOBytes# ( IOBytes# )
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Prim.SBytes

import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM

import SDP.SortM.Tim

import Data.Function
import Data.Coerce

import Control.Monad.ST
import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | Primitive int-indexed unboxed array in monad 'IO'.
newtype IOBytes# e = IOBytes# { unpack :: STBytes# RealWorld e } deriving ( Eq )

--------------------------------------------------------------------------------

{- Estimate, Bordered, BorderedM, LinearM and SplitM instances. -}

instance Estimate (IOBytes# e)
  where
    (<==>) = on (<=>) sizeOf
    
    (.>.)  = on (>)  sizeOf
    (.<.)  = on (<)  sizeOf
    (.<=.) = on (<=) sizeOf
    (.>=.) = on (>=) sizeOf
    
    (<.=>) = (<=>) . sizeOf
    (.>)   = (>)   . sizeOf
    (.<)   = (<)   . sizeOf
    (.>=)  = (>=)  . sizeOf
    (.<=)  = (<=)  . sizeOf

instance Bordered (IOBytes# e) Int
  where
    sizeOf (IOBytes# es) = sizeOf es
    bounds (IOBytes# es) = bounds es

instance BorderedM IO (IOBytes# e) Int
  where
    getIndexOf = stToIO ... getIndexOf . unpack
    getIndices = stToIO . getIndices . unpack
    getBounds  = stToIO . getBounds . unpack
    getSizeOf  = stToIO . getSizeOf . unpack
    getUpper   = stToIO . getUpper . unpack
    getLower _ = return 0

instance (Unboxed e) => LinearM IO (IOBytes# e) e
  where
    newNull = pack' newNull
    singleM = pack'  . singleM
    nowNull = stToIO . nowNull . unpack
    getHead = stToIO . getHead . unpack
    getLast = stToIO . getLast . unpack
    
    prepend e = pack' . prepend e . unpack
    append es = pack' . append (unpack es)
    
    newLinear     = pack' . newLinear
    newLinearN    = pack' ... newLinearN
    fromFoldableM = pack' . fromFoldableM
    
    copied   = pack'  . copied   . unpack
    getLeft  = stToIO . getLeft  . unpack
    getRight = stToIO . getRight . unpack
    reversed = pack'  . reversed . unpack
    
    copied' es = pack' ... copied' (unpack es)
    
    merged = pack'  .  merged . foldr ((:) . unpack) []
    filled = pack' ... filled
    
    copyTo src so trg to = stToIO . copyTo (unpack src) so (unpack trg) to

instance (Unboxed e) => SplitM IO (IOBytes# e) e
  where
    takeM n = pack' . takeM n . unpack
    dropM n = pack' . dropM n . unpack
    keepM n = pack' . keepM n . unpack
    sansM n = pack' . sansM n . unpack
    
    prefixM f = stToIO . prefixM f . unpack
    suffixM f = stToIO . suffixM f . unpack
    
    mprefix p es = go 0
      where
        go i = i >= c ? return i $ do e <- es !#> i; p e ?^ go (i + 1) $ return i
        
        c = sizeOf es
    
    msuffix p es = go (max 0 (c - 1))
      where
        go i = i == 0 ? return c $ do e <- es !#> i; p e ?^ go (i - 1) $ return (c - i - 1)
        
        c = sizeOf es

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Unboxed e) => IndexedM IO (IOBytes# e) Int e
  where
    fromAssocs  bnds = pack'  .  fromAssocs  bnds
    fromAssocs' bnds = pack' ... fromAssocs' bnds
    
    (!#>) = stToIO ... (!#>) . unpack
    (>!)  = (!#>)
    
    writeM_ = writeM
    
    writeM es = stToIO ... writeM (unpack es)
    overwrite = pack' ... overwrite . unpack
    
    fromIndexed' = pack' . fromIndexed'
    
    fromIndexedM es = do
      n    <- getSizeOf es
      copy <- filled n (unreachEx "fromIndexedM")
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy

instance (Unboxed e) => IFoldM IO (IOBytes# e) Int e
  where
    ifoldrM f base arr =
      let go i = sizeOf arr == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f i
      in  go 0
    
    ifoldlM f base arr =
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f i
      in  go (sizeOf arr - 1)
    
    i_foldrM f base arr =
      let go i = sizeOf arr == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f
      in  go 0
    
    i_foldlM f base arr =
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f
      in  go (sizeOf arr - 1)

instance (Unboxed e) => SortM IO (IOBytes# e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

instance (Unboxed e) => Thaw IO (SBytes# e) (IOBytes# e)
  where
    unsafeThaw = pack' . unsafeThaw
    thaw       = pack' . thaw

instance (Unboxed e) => Freeze IO (IOBytes# e) (SBytes# e)
  where
    unsafeFreeze = stToIO . unsafeFreeze . unpack
    freeze       = stToIO . freeze . unpack

--------------------------------------------------------------------------------

{-# INLINE pack' #-}
pack' :: ST RealWorld (STBytes# RealWorld e) -> IO (IOBytes# e)
pack' =  stToIO . coerce

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Prim.IArray."


