{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

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
import SDP.Linear

import Data.Bits

import SDP.Simple

default ()

--------------------------------------------------------------------------------

{- Insertion sort. -}

insertionSort :: (BorderedM m v i e, IndexedM m v i e, Ord e) => v -> m ()
insertionSort es = insertionSortBy compare es

insertionSortOn :: (BorderedM m v i e, IndexedM m v i e, Ord o) => (e -> o) -> v -> m ()
insertionSortOn f es = insertionSortBy (compare `on` f) es

insertionSortBy :: (BorderedM m v i e, IndexedM m v i e) => Compare e -> v -> m ()
insertionSortBy cmp es = do n <- getSizeOf es; insertionSort_ cmp es 0 0 (n - 1)

insertionSort_ :: (IndexedM m v i e) => Compare e -> v -> Int -> Int -> Int -> m ()
insertionSort_ cmp es b s e' = mapM_ insert [s + 1 .. e']
  where
    insert u = do j <- snext (b, u - 1) u; mapM_ (swap es u) [j .. u - 1]
    
    snext (l, u) i = if l > u then return i else bindM2 (es !#> l) (es !#> i) $
      \ c e -> case cmp e c of {GT -> snext (l + 1, u) i; _ -> return l}

--------------------------------------------------------------------------------

{- Monadic TimSort. -}

timSort :: (LinearM m v e, BorderedM m v i e, IndexedM m v i e, Ord e) => v -> m ()
timSort es = timSortBy compare es

timSortOn :: (LinearM m v e, BorderedM m v i e, IndexedM m v i e, Ord o) => (e -> o) -> v -> m ()
timSortOn f es = timSortBy (compare `on` f) es

timSortBy :: (BorderedM m v i e, LinearM m v e, IndexedM m v i e) => Compare e -> v -> m ()
timSortBy cmp es = getSizeOf es >>= timSort'
  where
    timSort' n
      | n <  0 = return ()
      | n > 64 = ascSubs n 0 >>= mergeAll
      |  True  = insertionSortBy cmp es
    
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
            rev' f l = rev f (l - 1) >> return l
            rev  f l = when (f < l) $ do swap es f l; rev (f + 1) (l - 1)
        
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
        rb = by + sy
        lb = sx
    
    gt x y = case x `cmp` y of {GT -> True; _ -> False}
    lt x y = case x `cmp` y of {LT -> True; _ -> False}

--------------------------------------------------------------------------------

{-# INLINE minrunTS #-}
-- minrunTS returns Timsort chunk size.
minrunTS :: Int -> Int
minrunTS i = mr i 0 where mr n r = n >= 64 ? mr (shiftR n 1) (n .&. 1) $ n + r

{-# INLINE swap #-}
swap :: (IndexedM m v i e) => v -> Int -> Int -> m ()
swap es i j = do a <- es !#> i; b <- es !#> j; writeM_ es j a; writeM_ es i b

sorted :: (Linear l e, Ord e) => l -> Bool
sorted Z  = True
sorted es = and $ zipWith (<=) es' (tail es') where es' = listL es

sortedM :: (LinearM m l e, Ord e) => l -> m Bool
sortedM es = do
  left <- getLeft es
  return $ case left of {[] -> True; _ -> and $ zipWith (<=) left (tail left)}

ascending :: (Split s e, Ord e) => s -> [(Int, Int)] -> Bool
ascending es ss = sorted `all` splits (snds ss) es


