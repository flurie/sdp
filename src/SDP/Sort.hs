{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

{- |
    Module      :  SDP.Index
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    Stability   :  experimental
    
    This module provides Sort (class of sortable one-parametric types).
-}

module SDP.Sort ( Sort (..), sort, sortOn ) where

import Prelude ()
import SDP.SafePrelude

import SDP.Simple

-- | Sort - is class of types that can be sorted.
class Sort s e | s -> e
  where
    -- | The sortBy function is the non-overloaded version of sort.
    sortBy :: (e -> e -> Ordering) -> s -> s

-- | The sort function implements a stable sorting algorithm.
sort :: (Sort s e, Ord e) => s -> s
sort = sortBy compare

-- | Sort by comparing the results of a key function applied to each element.
sortOn :: (Sort s e, Ord o) => (e -> o) -> s -> s
sortOn f = sortBy (compare `on` f)

--------------------------------------------------------------------------------

instance Sort [a] a
  where
    {-
      Code of sortBy "stolen" from Data.List (base-4.12.0.0, BSD3-style).
      
      This code originally contributed to the nhc12 compiler by Thomas Nordin
      in 2002.
      Rumoured to have been based on code by Lennart Augustsson, e.g.
      http://www.mail-archive.com/haskell@haskell.org/msg01822.html
      and possibly to bear similarities to a 1982 paper by Richard O'Keefe:
      "A smooth applicative merge sort".
    -}
    sortBy cmp = mergeAll . sequences
      where
        sequences (a : b : xs) = case a `cmp` b of
          GT -> descending b   [a]  xs
          _  -> ascending  b (a : ) xs
        sequences xs = [xs]
        
        descending a as (b : bs) | a `cmp` b == GT = descending b (a : as) bs
        descending a as bs = (a : as) : sequences bs
        
        ascending  a as (b : bs) | a `cmp` b /= GT = ascending b (\ ys -> as (a : ys)) bs
        ascending  a as bs = let !x = as [a] in x : sequences bs
        
        mergeAll [x] = x
        mergeAll xs  = mergeAll (mergePairs xs)
        
        mergePairs (a : b : xs) = let !x = merge a b in x : mergePairs xs
        mergePairs xs = xs
        
        merge as@(a : as') bs@(b : bs') = case a `cmp` b of
          GT -> b : merge as  bs'
          _  -> a : merge as' bs
        merge [] bs = bs
        merge as [] = as

