{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DefaultSignatures #-}

{- |
    Module      :  SDP.LinearM
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC-extensions)
  
  LinearM is a module that provides several convenient interfaces for working
  with mutable linear data structures.
-}
module SDP.LinearM
  (
    module SDP.Linear,
    
    BorderedM (..),
    LinearM   (..),
    
    sortedM
  )
where

import Prelude ()
import SDP.SafePrelude

import SDP.Linear

default ()

--------------------------------------------------------------------------------

-- | BorderedM is Bordered version for mutable data structures.
class (Monad m, Index i) => BorderedM m b i e | b -> m, b -> i, b -> e
  where
    {-# MINIMAL (getBounds|getLower, getUpper) #-}
    
    -- | getBounds returns bounds of mutable data structure.
    {-# INLINE getBounds #-}
    getBounds    :: b -> m (i, i)
    getBounds es =  liftA2 (,) (getLower es) (getUpper es)
    
    -- | getBounds returns lower bound of mutable data structure.
    {-# INLINE getLower #-}
    getLower    :: b -> m i
    getLower es =  fst <$> getBounds es
    
    -- | getBounds returns upper bound of mutable data structure.
    {-# INLINE getUpper #-}
    getUpper    :: b -> m i
    getUpper es =  snd <$> getBounds es
    
    -- | getSizeOf returns size of mutable data structure.
    {-# INLINE getSizeOf #-}
    getSizeOf    :: b -> m Int
    getSizeOf es =  size <$> getBounds es
    
    -- | getIndxeOf is indexOf version for mutable data structures.
    {-# INLINE getIndexOf #-}
    getIndexOf :: b -> i -> m Bool
    getIndexOf es i = (`inRange` i) <$> getBounds es
    
    -- | getBounds returns indices of mutable data structure.
    {-# INLINE getIndices #-}
    getIndices    :: b -> m [i]
    getIndices es =  range <$> getBounds es
    
    -- | getAssocs returns assocs of mutable data structure.
    getAssocs  :: b -> m [(i, e)]
    
    default getAssocs :: (LinearM m b e) => b -> m [(i, e)]
    getAssocs es = liftA2 zip (getIndices es) (getLeft es)

--------------------------------------------------------------------------------

-- | LinearM is Linear version for mutable data structures.
class (Monad m) => LinearM m l e | l -> m, l -> e
  where
    {-# MINIMAL (newLinear|newLinearN), (getLeft|getRight), reversed, filled #-}
    
    -- | Creates new mutable line from list.
    {-# INLINE newLinear #-}
    newLinear    :: [e] -> m l
    newLinear es =  length es `newLinearN` es
    
    -- | Creates new mutable line from limited list.
    {-# INLINE newLinearN #-}
    newLinearN      :: Int -> [e] -> m l
    newLinearN n es =  newLinear $ take n es
    
    -- | Creates new mutable line from foldable structure.
    {-# INLINE fromFoldableM #-}
    fromFoldableM    :: (Foldable f) => f e -> m l
    fromFoldableM es =  newLinear $ toList es
    
    -- | Returns left view of line.
    {-# INLINE getLeft #-}
    getLeft     :: l -> m [e]
    getLeft es  =  reverse <$> getRight es
    
    -- | Returns right view of line.
    {-# INLINE getRight #-}
    getRight    :: l -> m [e]
    getRight es =  reverse <$> getLeft es
    
    -- | copied creates copy of structure.
    copied    :: l -> m l
    copied es = getLeft es >>= newLinear
    
    {- |
      copied' es l u (where l - begining, u - count of elements)
      returns the slice of line.
    -}
    copied' :: l -> Int -> Int -> m l
    copied' es l u = getLeft es >>= newLinear . take u . drop l
    
    -- | In-place reverse of line.
    reversed :: l -> m l
    
    -- | Monadic version of replicate.
    filled   :: Int -> e -> m l

--------------------------------------------------------------------------------

-- | sortedM is a procedure that checks for sorting.
sortedM :: (LinearM m l e, Ord e) => l -> m Bool
sortedM es = flip fmap (getLeft es) $
  \ left -> case left of {[] -> True; _ -> and $ zipWith (<=) left (tail left)}


