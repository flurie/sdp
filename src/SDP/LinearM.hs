{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds, DefaultSignatures #-}

{- |
    Module      :  SDP.LinearM
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
  
  @SDP.LinearM@ is a module that provides 'BorderedM' and 'LinearM' classes.
-}
module SDP.LinearM
  (
    -- * Exports
    module SDP.Linear,
    
    -- * BorderedM class
    BorderedM (..), BorderedM1, BorderedM2,
    
    -- * LinearM class
    LinearM (..), LinearM1,
    
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
    
    -- | getIndexOf is 'indexOf' version for mutable data structures.
    {-# INLINE getIndexOf #-}
    getIndexOf :: b -> i -> m Bool
    getIndexOf =  \ es i -> (`inRange` i) <$> getBounds es
    
    -- | getIndices returns 'indices' of mutable data structure.
    {-# INLINE getIndices #-}
    getIndices :: b -> m [i]
    getIndices =  fmap range . getBounds
    
    -- | getAssocs returns 'assocs' of mutable data structure.
    default getAssocs :: (LinearM m b e) => b -> m [(i, e)]
    getAssocs :: b -> m [(i, e)]
    getAssocs es = liftA2 zip (getIndices es) (getLeft es)

--------------------------------------------------------------------------------

-- | LinearM is 'Linear' version for mutable data structures.
class (Monad m) => LinearM m l e | l -> m, l -> e
  where
    {-# MINIMAL nowNull, getHead, getLast, (newLinear|fromFoldableM), (getLeft|getRight) #-}
    
    -- | 'nowNull' is monadic version of 'isNull'.
    nowNull :: l -> m Bool
    
    -- | 'singleM' is monadic version of 'single'.
    singleM :: e -> m l
    singleM =  newLinear . single
    
    {- |
      'getHead' is monadic version of 'head'. This procedure mustn't modify the
      source structure or return references to its mutable fields.
    -}
    getHead :: l -> m e
    getHead =  fmap head . getLeft
    
    {- |
      'getLast' is monadic version of 'last'. This procedure mustn't modify the
      source structure or return references to its mutable fields.
    -}
    getLast :: l -> m e
    getLast =  fmap head . getRight
    
    {- |
      Prepends new element to the start of the structure (monadic 'toHead').
      Like most size-changing operations, @prepend@ doesn't guarantee the
      correctness of the original structure after conversion.
    -}
    prepend :: e -> l -> m l
    prepend e es = newLinear . (e :) =<< getLeft es
    
    {- |
      Appends new element to the end of the structure (monadic 'toLast').
      Like most size-changing operations, @append@ doesn't guarantee the
      correctness of the original structure after conversion.
    -}
    append  :: l -> e -> m l
    append es e = newLinear . (:< e) =<< getLeft es
    
    -- | Creates new mutable line from list.
    {-# INLINE newLinear #-}
    newLinear :: [e] -> m l
    newLinear =  fromFoldableM
    
    -- | Creates new mutable line from limited list.
    {-# INLINE newLinearN #-}
    newLinearN :: Int -> [e] -> m l
    newLinearN =  newLinear ... take
    
    -- | Creates new mutable line from foldable structure.
    {-# INLINE fromFoldableM #-}
    fromFoldableM :: (Foldable f) => f e -> m l
    fromFoldableM =  newLinear . toList
    
    -- | Returns left view of line.
    {-# INLINE getLeft #-}
    getLeft :: l -> m [e]
    getLeft =  fmap reverse . getRight
    
    -- | Return right view of line.
    {-# INLINE getRight #-}
    getRight :: l -> m [e]
    getRight =  fmap reverse . getLeft
    
    -- | Creates copy of structure.
    {-# INLINE copied #-}
    copied :: l -> m l
    copied =  getLeft >=> newLinear
    
    -- | @copied' es l n@ returns the slice of @es@ from @l@ of length @n@.
    {-# INLINE copied' #-}
    copied' :: l -> Int -> Int -> m l
    copied' es l n = getLeft es >>= newLinearN n . drop l
    
    -- | Mutable version of 'reverse' (ideally, in-place).
    {-# INLINE reversed #-}
    reversed :: l -> m l
    reversed =  newLinear <=< getRight
    
    -- | Monadic version of 'replicate'.
    {-# INLINE filled #-}
    filled :: Int -> e -> m l
    filled n = newLinearN n . replicate n

--------------------------------------------------------------------------------

-- | Rank (* -> *) 'LinearM' structure.
type LinearM1 m l e = LinearM m (l e) e

-- | Rank (* -> *) 'BorderedM' structure.
type BorderedM1 m l i e = BorderedM m (l e) i e

-- | Rank (* -> * -> *) 'BorderedM' structure.
type BorderedM2 m l i e = BorderedM m (l i e) i e

--------------------------------------------------------------------------------

-- | sortedM is a procedure that checks for sorting.
sortedM :: (LinearM m l e, Ord e) => l -> m Bool
sortedM =  fmap (\ es -> null es || and (zipWith (<=) es (tail es))) . getLeft

