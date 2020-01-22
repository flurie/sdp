{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DefaultSignatures #-}

{- |
    Module      :  SDP.LinearM
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable modules)
  
  @SDP.LinearM@ is a module that provides 'BorderedM' and 'LinearM' classes.
-}
module SDP.LinearM
  (
    -- * Exports
    module SDP.Linear,
    
    -- * BorderedM class
    BorderedM (..),
    
    -- * LinearM class
    LinearM   (..),
    
    -- * Related section
    sortedM
  )
where

import Prelude ()
import SDP.SafePrelude

import SDP.Linear

import SDP.Internal.Commons

default ()

--------------------------------------------------------------------------------

-- | BorderedM is 'Bordered' version for mutable data structures.
class (Monad m, Index i) => BorderedM m b i e | b -> m, b -> i, b -> e
  where
    {-# MINIMAL (getBounds|getLower, getUpper) #-}
    
    -- | getBounds returns 'bounds' of mutable data structure.
    {-# INLINE getBounds #-}
    getBounds :: b -> m (i, i)
    getBounds es = liftA2 (,) (getLower es) (getUpper es)
    
    -- | getLower returns 'lower' bound of mutable data structure.
    {-# INLINE getLower #-}
    getLower :: b -> m i
    getLower =  fsts . getBounds
    
    -- | getUpper returns 'upper' bound of mutable data structure.
    {-# INLINE getUpper #-}
    getUpper :: b -> m i
    getUpper =  snds . getBounds
    
    -- | getSizeOf returns size of mutable data structure.
    {-# INLINE getSizeOf #-}
    getSizeOf :: b -> m Int
    getSizeOf =  fmap size . getBounds
    
    -- | getIndxeOf is 'indexOf' version for mutable data structures.
    {-# INLINE getIndexOf #-}
    getIndexOf :: b -> i -> m Bool
    getIndexOf =  flip $ \ i -> fmap (`inRange` i) . getBounds
    
    -- | getIndices returns 'indices' of mutable data structure.
    {-# INLINE getIndices #-}
    getIndices :: b -> m [i]
    getIndices =  fmap range . getBounds
    
    -- | getAssocs returns 'assocs' of mutable data structure.
    getAssocs :: b -> m [(i, e)]
    
    default getAssocs :: (LinearM m b e) => b -> m [(i, e)]
    getAssocs es = liftA2 zip (getIndices es) (getLeft es)

--------------------------------------------------------------------------------

-- | LinearM is 'Linear' version for mutable data structures.
class (Monad m) => LinearM m l e | l -> m, l -> e
  where
    {-# MINIMAL (newLinear|newLinearN), (getLeft|getRight), reversed, filled #-}
    
    {- |
      Prepend new element to the start of the structure (monadic 'toHead').
      Like most size-changing operations, @prepend@ doesn't guarantee the
      correctness of the original structure after conversion.
    -}
    prepend :: e -> l -> m l
    prepend e es = newLinear . (e :) =<< getLeft es
    
    {- |
      Append new element to the end of the structure (monadic 'toLast').
      Like most size-changing operations, @append@ doesn't guarantee the
      correctness of the original structure after conversion.
    -}
    append  :: l -> e -> m l
    append es e = newLinear . (:< e) =<< getLeft es
    
    -- | Create new mutable line from list.
    {-# INLINE newLinear #-}
    newLinear :: [e] -> m l
    newLinear es = newLinearN (length es) es
    
    -- | Create new mutable line from limited list.
    {-# INLINE newLinearN #-}
    newLinearN :: Int -> [e] -> m l
    newLinearN n = newLinear . take n
    
    -- | Create new mutable line from foldable structure.
    {-# INLINE fromFoldableM #-}
    fromFoldableM :: (Foldable f) => f e -> m l
    fromFoldableM =  newLinear . toList
    
    -- | Return left view of line.
    {-# INLINE getLeft #-}
    getLeft :: l -> m [e]
    getLeft =  fmap reverse . getRight
    
    -- | Return right view of line.
    {-# INLINE getRight #-}
    getRight :: l -> m [e]
    getRight =  fmap reverse . getLeft
    
    -- | Create copy of structure.
    copied :: l -> m l
    copied es = getLeft es >>= newLinear
    
    -- | @copied' es begin count@ return the slice of line.
    copied' :: l -> Int -> Int -> m l
    copied' es l n = getLeft es >>= newLinearN n . drop l
    
    -- | In-place reverse of line.
    reversed :: l -> m l
    
    -- | Monadic version of replicate.
    filled :: Int -> e -> m l

--------------------------------------------------------------------------------

-- | sortedM is a procedure that checks for sorting.
sortedM :: (LinearM m l e, Ord e) => l -> m Bool
sortedM =  fmap f . getLeft
  where
    f Z  = True
    f es = and $ zipWith (<=) es (tail es)

