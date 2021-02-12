{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.SortM.Tim
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable modules)
    
    "SDP.SortM.Tim" provides @InsertionSort@ and @TimSort@ algorithms.
-}
module SDP.SortM.Tim
(
  -- * Insertion Sort
  insertionSort, insertionSortBy, insertionSortOn,
  
  -- * TimSort
  timSort, timSortBy, timSortOn, minrunTS
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM

import SDP.SortM.Insertion

import Data.Bits

import Control.Monad.Rope

default ()

--------------------------------------------------------------------------------

-- | 'timSort' is just synonym for @'timSortBy' 'compare'@.
{-# INLINE timSort #-}
timSort :: (LinearM m v e, BorderedM m v i, Ord e) => v -> m ()
timSort =  timSortBy compare

{- |
  'timSortOn' is a version of 'timSortBy' that uses a conversion function to
  compare elements.
-}
{-# INLINE timSortOn #-}
timSortOn :: (LinearM m v e, BorderedM m v i, Ord o) => (e -> o) -> v -> m ()
timSortOn =  timSortBy . comparing

{- |
  'timSortBy' is a sorting procedure for mutable random access data structures
  using any comparison function and having @O(nlogn)@ complexity in the worst
  case.
-}
{-# INLINE timSortBy #-}
timSortBy :: (LinearM m v e, BorderedM m v i) => Compare e -> v -> m ()
timSortBy cmp = timSort' $ \ x y -> case x `cmp` y of {GT -> True; _ -> False}

--------------------------------------------------------------------------------

{-
  timSort' is a sorting procedure for mutable random access data structures
  using any comparison function and having O(n * log n) complexity in the worst
  case.
-}
{-# INLINE timSort' #-}
timSort' :: (LinearM m v e, BorderedM m v i) => (e -> e -> Bool) -> v -> m ()
timSort' gt es = getSizeOf es >>= sort'
  where
    sort' n
      |  n < 0  = return ()
      | n <= 64 = unsafeInsertionSort gt es 0 0 (n - 1)
      |   True  = evalInit (ascSubs 0 n) 3 >>= uncurry mergeAll
    
    ascSubs o n = case n - o of
        0 -> RopeEnd
        1 -> RopeM $ return ((o, 1), RopeEnd)
        2 -> RopeM $ do
            e0 <- es !#> o
            e1 <- es !#> o + 1
            when (e0 `gt` e1) $ swapM es o (o + 1)
            return ((o, 2), RopeEnd)
        _ -> RopeM $ do
            end <- normalized =<< actual
            return ((o, end - o), ascSubs end n)
      where
        actual = (es !#> o) >>=<< (es !#> o + 1) $
          \ e0 e1 -> e0 `gt` e1 ? desc e1 (o + 2) $ asc e1 (o + 2)
          where
            asc  p i = do c <- es !#> i; p `gt` c ? return i $ i /= n - 1 ? asc  c (i + 1) $ return (i + 1)
            desc p i = do c <- es !#> i; c `gt` p ? rev' o i $ i /= n - 1 ? desc c (i + 1) $ rev' o (i + 1)
            rev  f l = when (f < l) $ do swapM es f l; rev (f + 1) (l - 1)
            rev' f l = do rev f (l - 1); return l
        
        normalized s = do
          let ex = min n (o + minrunTS n) -- minimal expected ending
          when (ex > s) $ unsafeInsertionSort gt es o (s - 1) (ex - 1)
          return $ max ex s
    
    mergeAll [x@(bx, sx), y@(by, sy), z@(_, sz)] r = if rules || sz <= sx
        then do merge y z; (nxt, r') <- nextR r; mergeAll ([x, (by, sy + sz)] ++ nxt) r'
        else do merge x y; (nxt, r') <- nextR r; mergeAll ([(bx, sx + sy), z] ++ nxt) r'
      where
        rules = sx > sy + sz && sy > sz
    mergeAll [x, y] _ = merge x y
    mergeAll    _   _ = return ()
    
    merge (bx, sx) (by, sy) = copied' es bx sx >>= mergeGo bx 0 by
      where
        mergeGo ic il ir left
          | il >= lb = return () -- at least left is empty, merge is completed.
          | ir >= rb = copyTo left il es ic (lb - il)
          |   True   = (left !#> il) >>=<< (es !#> ir) $
            \ l r -> if r `gt` l
              then writeM es ic l >> mergeGo (ic + 1) (il + 1) ir left
              else writeM es ic r >> mergeGo (ic + 1) il (ir + 1) left
        rb = by + sy; lb = sx

--------------------------------------------------------------------------------

{-# INLINE minrunTS #-}
-- | 'minrunTS' returns @Timsort@ chunk size.
minrunTS :: Int -> Int
minrunTS i = mr i 0 where mr n r = n >= 64 ? mr (shiftR n 1) (n .&. 1) $ n + r


