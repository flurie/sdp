{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{- |
    Module      :  SDP.SortM.Stuff
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    SDP.SortM.Stuff provides some sorting algorithms.
-}
module SDP.SortM.Stuff
(
  insertionSort, insertionSortBy, insertionSortOn,
  
  timSort, timSortBy, timSortOn,
  
  sorted, sortedM, ascending,
  
  minrunTS
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM

import Data.Bits

import SDP.Simple

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
  insertionSortBy is naive service sorting procedure, that have O(n) complexity
  in all cases.
-}
insertionSortBy :: (BorderedM m v i e, IndexedM m v i e) => (e -> e -> Ordering) -> v -> m ()
insertionSortBy cmp es = do n <- getSizeOf es; insertionSort_ cmp es 0 0 (n - 1)

{-
  insertionSort_ cmp es b s e is internal sorting procedure, where
  cmp - comparator, es - data structure, [b .. s] - sorted fragment,
  [b .. e] - sortable fragment.
-}
insertionSort_ :: (IndexedM m v i e) => (e -> e -> Ordering) -> v -> Int -> Int -> Int -> m ()
insertionSort_ cmp es b s e' = mapM_ insert [s + 1 .. e']
  where
    insert u = do j <- snext (b, u - 1) u; mapM_ (swap es u) [j .. u - 1]
    
    snext (l, u) i = if l > u then return i else bindM2 (es !#> l) (es !#> i) $
      \ c e -> case cmp e c of {GT -> snext (l + 1, u) i; _ -> return l}

--------------------------------------------------------------------------------

{- Monadic TimSort. -}

-- | timsort is just synonym for timSortBy compare
timSort :: (LinearM m v e, BorderedM m v i e, IndexedM m v i e, Ord e) => v -> m ()
timSort es = timSortBy compare es

{- |
  timSortOn is a version of timSortBy that uses a cast function to compare
  elements.
-}
timSortOn :: (LinearM m v e, BorderedM m v i e, IndexedM m v i e, Ord o) => (e -> o) -> v -> m ()
timSortOn f es = timSortBy (compare `on` f) es

{- |
  timSortBy is a sorting procedure for mutable random access data structures
  using any comparison function and having O (nlogn) complexity in the worst
  case.
-}
timSortBy :: (BorderedM m v i e, LinearM m v e, IndexedM m v i e) => (e -> e -> Ordering) -> v -> m ()
timSortBy cmp es = getSizeOf es >>= timSort'
  where
    timSort' n
      |  n < 0  = return ()
      | n <= 64 = insertionSortBy cmp es
      |   True  = ascSubs n 0 >>= mergeAll
    
    ascSubs n o
        |   o   == n = return []
        | o + 1 == n = return [(o, 1)]
        | o + 2 == n = bindM2 (es !#> o) (es !#> o + 1) $
          \ e0 e1 -> do when (e0 `gt` e1) $ swap es o (o + 1); return [(o, 2)]
        |    True    = do
          end  <- normalized =<< actual
          nxts <- ascSubs n end
          return $ (o, end - o) : nxts
      where
        actual = bindM2 (es !#> o) (es !#> o + 1) $
          \ e0 e1 -> if e0 `gt` e1 then desc e1 (o + 2) else asc e1 (o + 2)
          where
            asc  p i = do c <- es !#> i; if p `gt` c then return i else i /= n - 1 ? asc  c (i + 1) $ return (i + 1)
            desc p i = do c <- es !#> i; if p `lt` c then rev' o i else i /= n - 1 ? desc c (i + 1) $ rev' o (i + 1)
            rev  f l = when (f < l) $ do swap es f l; rev (f + 1) (l - 1)
            rev' f l = rev f (l - 1) >> return l
        
        normalized s = do when (ex > s) $ insertionSort_ cmp es o (s - 1) (ex - 1); return (max ex s)
        ex = min n (o + minrunTS n) -- expected ending
    
    mergeAll (x@(bx, sx) : y@(by, sy) : z@(_, sz) : bnds) = if rules || sz <= sx
        then do merge y z; mergeAll $ x : (by, sy + sz) : bnds
        else do merge x y; mergeAll $ (bx, sx + sy) : z : bnds
      where
        rules = sx > sy + sz && sy > sz
    mergeAll [x,y] = merge x y
    mergeAll   _   = return ()
    
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

{-# INLINE swap #-}
swap :: (IndexedM m v i e) => v -> Int -> Int -> m ()
swap es i j = do a <- es !#> i; b <- es !#> j; writeM_ es j a; writeM_ es i b

-- | sorted is a function that checks for sorting.
sorted :: (Linear l e, Ord e) => l -> Bool
sorted Z  = True
sorted es = and $ zipWith (<=) es' (tail es') where es' = listL es

-- | sortedM is a procedure that checks for sorting.
sortedM :: (LinearM m l e, Ord e) => l -> m Bool
sortedM es = flip fmap (getLeft es) $
  \ left -> case left of {[] -> True; _ -> and $ zipWith (<=) left (tail left)}

{- |
  ascending is a function that checks if sequences of elements given in pairs
  (start, length) are sorted in ascending order.
-}
ascending :: (Split s e, Ord e) => s -> [(Int, Int)] -> Bool
ascending es ss = sorted `all` splits (snds ss) es



