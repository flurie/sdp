{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, BangPatterns #-}

{- |
    Module      :  SDP.Prim.IArray
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.Prim.IArray@ provides 'IOArray\#' - pseudo-primitive boxed array.
-}
module SDP.Prim.IArray
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * Pseudo-primitive types
  IOArray# ( IOArray# ),
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Prim.SArray

import SDP.IndexedM
import SDP.SortM

import SDP.SortM.Tim

import Data.Function
import Data.Coerce

import Control.Monad.ST
import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | Primitive int-indexed unboxed array in monad 'IO'.
newtype IOArray# e = IOArray# { unpack :: STArray# RealWorld e} deriving ( Eq )

--------------------------------------------------------------------------------

{- Estimate, Bordered, BorderedM, LinearM and SplitM instances. -}

instance Estimate (IOArray# e)
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

instance Bordered (IOArray# e) Int
  where
    sizeOf (IOArray# es) = sizeOf es
    bounds (IOArray# es) = bounds es

instance BorderedM IO (IOArray# e) Int
  where
    getIndexOf = stToIO ... getIndexOf . unpack
    getIndices = stToIO . getIndices . unpack
    getBounds  = stToIO . getBounds . unpack
    getSizeOf  = stToIO . getSizeOf . unpack
    getUpper   = stToIO . getUpper . unpack
    getLower _ = return 0

instance LinearM IO (IOArray# e) e
  where
    newNull = pack' newNull
    singleM = pack' . singleM
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
    
    merged = pack' . merged . foldr ((:) . unpack) []
    filled = pack' ... filled
    
    copyTo src so trg to = stToIO . copyTo (unpack src) so (unpack trg) to

instance SplitM IO (IOArray# e) e
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

instance IndexedM IO (IOArray# e) Int e
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

instance IFoldM IO (IOArray# e) Int e
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

instance SortM IO (IOArray# e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

instance Thaw IO (SArray# e) (IOArray# e)
  where
    unsafeThaw = pack' . unsafeThaw
    thaw       = pack' . thaw

instance Freeze IO (IOArray# e) (SArray# e)
  where
    unsafeFreeze = stToIO . unsafeFreeze . unpack
    freeze       = stToIO . freeze . unpack

--------------------------------------------------------------------------------

{-# INLINE pack' #-}
pack' :: ST RealWorld (STArray# RealWorld e) -> IO (IOArray# e)
pack' =  stToIO . coerce

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Prim.IArray."


