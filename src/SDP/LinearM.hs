{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

{- |
    Module      :  SDP.LinearM
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    Stability   :  stable
  
  LinearM is a module that provides several convenient interfaces for working
  with mutable linear data structures.
-}

module SDP.LinearM
  (
    module SDP.Index,
    
    BorderedM (..),
    LinearM   (..)
  )
where

import Prelude ( take, reverse )
import SDP.SafePrelude

import SDP.Index

default ()

--------------------------------------------------------------------------------

-- | BorderedM is Bordered version for mutable data structures.
class (Monad m, Index i) => BorderedM m b i e | b -> m, b -> i, b -> e
  where
    {-# MINIMAL (getBounds|getLower, getUpper), getAssocs #-}
    
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
    
    -- | In-place reverse of line.
    reversed :: l -> m ()
    
    -- | Fills mutable structure by element.
    filled   :: l -> e -> m ()

--------------------------------------------------------------------------------
