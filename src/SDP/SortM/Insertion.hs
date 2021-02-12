{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.SortM.Insertion
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.SortM.Insertion" provides insertion sort - simple sorting algorithm.
-}
module SDP.SortM.Insertion
(
  -- * Insertion Sort
  insertionSort, insertionSortBy, insertionSortOn, unsafeInsertionSort
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM

default ()

--------------------------------------------------------------------------------

-- | 'insertionSort' is just synonym for @'insertionSortBy' 'compare'@.
{-# INLINE insertionSort #-}
insertionSort :: (LinearM m v e, BorderedM m v i, Ord e) => v -> m ()
insertionSort =  insertionSortBy compare

{- |
  'insertionSortOn' is a version of 'insertionSortBy' that uses a cast function
  to 'compare' elements.
-}
{-# INLINE insertionSortOn #-}
insertionSortOn :: (LinearM m v e, BorderedM m v i, Ord o) => (e -> o) -> v -> m ()
insertionSortOn =  insertionSortBy . comparing

{- |
  'insertionSortBy' is naive service sorting procedure, that have @O(n^2)@
  complexity in all cases.
-}
{-# INLINE insertionSortBy #-}
insertionSortBy :: (LinearM m v e, BorderedM m v i) => Compare e -> v -> m ()
insertionSortBy cmp es =
  let gt = \ x y -> case x `cmp` y of {GT -> True; _ -> False}
  in  do n <- getSizeOf es; unsafeInsertionSort gt es 0 0 (n - 1)

--------------------------------------------------------------------------------

{- |
  unsafeInsertionSort cmp es b s e is internal sorting procedure, where
  @cmp@ - @('>')@, @es@ - data structure, @[b .. s]@ - sorted fragment,
  @[b .. e]@ - sortable fragment.
-}
unsafeInsertionSort :: (LinearM m v e) => (e -> e -> Bool) -> v -> Int -> Int -> Int -> m ()
unsafeInsertionSort gt es b s e' = mapM_ insert_ [s + 1 .. e']
  where
    insert_ u = do j <- snext (b, u - 1) u; mapM_ (swapM es u) [j .. u - 1]
    
    snext (l, u) i = l > u ? return i $ (es !#> l) >>=<< (es !#> i) $
      \ c e -> if e `gt` c then snext (l + 1, u) i else return l


