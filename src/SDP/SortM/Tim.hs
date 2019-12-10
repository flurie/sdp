{-# LANGUAGE MultiParamTypeClasses #-}

{- |
    Module      :  SDP.SortM.Tim
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable modules)
    
    @SDP.SortM.Tim@ provides @InsertionSort@ and @TimSort@ algorithms.
-}
module SDP.SortM.Tim
(
  -- * Insertion Sort
  insertionSort, insertionSortBy, insertionSortOn,
  
  -- * TimSort
  timSort, timSortBy, timSortOn,
  
  minrunTS
)
where

import Prelude ()

import SDP.SafePrelude
import SDP.IndexedM
import SDP.Simple

import Control.Monad.Rope

import Data.Bits

default ()

--------------------------------------------------------------------------------

{- Insertion sort. -}

-- | insertionSort is just synonym for insertionSortBy compare.
insertionSort :: (BorderedM m v i e, IndexedM m v i e, Ord e) => v -> m ()
insertionSort es = insertionSortBy compare es

{- |
  insertionSortOn is a version of insertionSortBy that uses a cast function to
  compare elements.
-}
insertionSortOn :: (BorderedM m v i e, IndexedM m v i e, Ord o) => (e -> o) -> v -> m ()
insertionSortOn f es = insertionSortBy (compare `on` f) es

{- |
  insertionSortBy is naive service sorting procedure, that have O(n^2)
  complexity in all cases.
-}
insertionSortBy :: (BorderedM m v i e, IndexedM m v i e) => Compare e -> v -> m ()
insertionSortBy cmp es = do n <- getSizeOf es; insertionSort_ cmp es 0 0 (n - 1)

{-
  insertionSort_ cmp es b s e is internal sorting procedure, where
  cmp - comparator, es - data structure, [b .. s] - sorted fragment,
  [b .. e] - sortable fragment.
-}
insertionSort_ :: (IndexedM m v i e) => Compare e -> v -> Int -> Int -> Int -> m ()
insertionSort_ cmp es b s e' = mapM_ insert [s + 1 .. e']
  where
    insert u = do j <- snext (b, u - 1) u; mapM_ (swapM es u) [j .. u - 1]
    
    snext (l, u) i = l > u ? return i $ bindM2 (es !#> l) (es !#> i) $
      \ c e -> case cmp e c of {GT -> snext (l + 1, u) i; _ -> return l}

--------------------------------------------------------------------------------

{- Monadic TimSort. -}

-- | timsort is just synonym for timSortBy compare
timSort :: (LinearM m v e, BorderedM m v i e, IndexedM m v i e, Ord e) => v -> m ()
timSort es = timSortBy compare es

{- |
  timSortOn is a version of timSortBy that uses a conversion function to compare
  elements.
-}
timSortOn :: (LinearM m v e, BorderedM m v i e, IndexedM m v i e, Ord o) => (e -> o) -> v -> m ()
timSortOn f es = timSortBy (compare `on` f) es

{- |
  timSortBy is a sorting procedure for mutable random access data structures
  using any comparison function and having O(n * log n) complexity in the worst
  case.
-}
timSortBy :: (BorderedM m v i e, LinearM m v e, IndexedM m v i e) => Compare e -> v -> m ()
timSortBy cmp es = getSizeOf es >>= timSort'
  where
    timSort' n
      |  n < 0  = return ()
      | n <= 64 = insertionSortBy cmp es
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
        actual = bindM2 (es !#> o) (es !#> o + 1) $
          \ e0 e1 -> e0 `gt` e1 ? desc e1 (o + 2) $ asc e1 (o + 2)
          where
            asc  p i = do c <- es !#> i; p `gt` c ? return i $ i /= n - 1 ? asc  c (i + 1) $ return (i + 1)
            desc p i = do c <- es !#> i; p `lt` c ? rev' o i $ i /= n - 1 ? desc c (i + 1) $ rev' o (i + 1)
            rev  f l = when (f < l) $ do swapM es f l; rev (f + 1) (l - 1)
            rev' f l = do rev f (l - 1); return l
        
        normalized s = do
          let ex = min n (o + minrunTS n) -- minimal expected ending
          when (ex > s) $ insertionSort_ cmp es o (s - 1) (ex - 1)
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
          | ir >= rb = arrcopy left es il ic (lb - il)
          |   True   = bindM2 (left !#> il) (es !#> ir) $
            \ l r -> if l `lt` r
              then writeM_ es ic l >> mergeGo (ic + 1) (il + 1) ir left
              else writeM_ es ic r >> mergeGo (ic + 1) il (ir + 1) left
        rb = by + sy; lb = sx
    
    gt x y = case x `cmp` y of {GT -> True; _ -> False}
    lt x y = case x `cmp` y of {LT -> True; _ -> False}

--------------------------------------------------------------------------------

{-# INLINE minrunTS #-}
-- | minrunTS returns Timsort chunk size.
minrunTS :: Int -> Int
minrunTS i = mr i 0 where mr n r = n >= 64 ? mr (shiftR n 1) (n .&. 1) $ n + r

{-
  arrcopy is a utility function that copies a fragment of the first array to the
  second array. This function is not intended for copying to an adjacent memory
  area. The first 2 Int arguments are the offsets in the first and second
  arrays, respectively, the third is the number of elements to be copied.
-}
arrcopy :: (IndexedM m v i e) => v -> v -> Int -> Int -> Int -> m ()
arrcopy xs ys ix iy count = copy ix iy (max 0 count)
  where
    copy _  _  0 = return ()
    copy ox oy c = xs !#> ox >>= writeM_ ys oy >> copy (ox + 1) (oy + 1) (c - 1)


