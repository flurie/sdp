{-# LANGUAGE Unsafe, MagicHash, MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  SDP.Unrolled.Unlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Unrolled.Unlist" provides 'Unlist' - lazy boxed unrolled linked list.
-}
module SDP.Unrolled.Unlist
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Unlist
  Unlist
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Indexed
import SDP.Sort
import SDP.Scan
import SDP.Set

import SDP.Templates.AnyChunks
import SDP.Prim.SArray

import SDP.SortM.Tim

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | 'Unlist' is unrolled linked list of boxed values.
type Unlist = AnyChunks SArray#

--------------------------------------------------------------------------------

{- Eq1 and Ord1 instances. -}

instance Eq1 Unlist
  where
    liftEq _ Z Z = True
    liftEq f xs@(AnyChunks (x : xs')) ys@(AnyChunks (y : ys')) = if n1 > n2
        then liftEq f (take n2 x) y && liftEq f (drop n2 xs) (AnyChunks ys')
        else liftEq f x (take n1 y) && liftEq f (AnyChunks xs') (drop n1 ys)
      where
        n1 = sizeOf x
        n2 = sizeOf y
    liftEq _ _ _ = False

instance Ord1 Unlist
  where
    liftCompare _ Z Z = EQ
    liftCompare f xs@(AnyChunks (x : xs')) ys@(AnyChunks (y : ys')) = if n1 > n2
        then liftCompare f (take n2 x) y <> liftCompare f (drop n2 xs) (AnyChunks ys')
        else liftCompare f x (take n1 y) <> liftCompare f (AnyChunks xs') (drop n1 ys)
      where
        n1 = sizeOf x
        n2 = sizeOf y
    liftCompare _ Z _ = LT
    liftCompare _ _ _ = GT

instance Zip Unlist
  where
    zipWith  f as bs             = fromList $ zipWith  f (toList as) (toList bs)
    zipWith3 f as bs cs          = fromList $ zipWith3 f (toList as) (toList bs) (toList cs)
    zipWith4 f as bs cs ds       = fromList $ zipWith4 f (toList as) (toList bs) (toList cs) (toList ds)
    zipWith5 f as bs cs ds es    = fromList $ zipWith5 f (toList as) (toList bs) (toList cs) (toList ds) (toList es)
    zipWith6 f as bs cs ds es fs = fromList $ zipWith6 f (toList as) (toList bs) (toList cs) (toList ds) (toList es) (toList fs)

instance Sort (Unlist e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: STArray# s e -> ST s (Unlist e)
done =  unsafeFreeze

