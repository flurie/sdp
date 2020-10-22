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
    
    -- * SplitM class
    SplitM (..), SplitM1,
    
    -- * Related section
    sortedM, swapM
  )
where

import Prelude ()
import SDP.SafePrelude
import SDP.Internal
import SDP.Linear

default ()

infixl 5 !#>

--------------------------------------------------------------------------------

-- | 'BorderedM' is 'Bordered' version for mutable data structures.
class (Monad m, Index i) => BorderedM m b i | b -> m, b -> i
  where
    {-# MINIMAL (getBounds|getLower, getUpper) #-}
    
    -- | 'getBounds' returns 'bounds' of mutable data structure.
    getBounds :: b -> m (i, i)
    getBounds es = liftA2 (,) (getLower es) (getUpper es)
    
    -- | 'getLower' returns 'lower' bound of mutable data structure.
    getLower :: b -> m i
    getLower =  fsts . getBounds
    
    -- | 'getUpper' returns 'upper' bound of mutable data structure.
    getUpper :: b -> m i
    getUpper =  snds . getBounds
    
    -- | 'getSizeOf' returns size of mutable data structure.
    getSizeOf :: b -> m Int
    getSizeOf =  fmap size . getBounds
    
    -- | 'nowIndexIn' is 'indexIn' version for mutable structures.
    nowIndexIn :: b -> i -> m Bool
    nowIndexIn es i = (`inRange` i) <$> getBounds es
    
    -- | 'getOffsetOf' is 'offsetOf' version for mutable structures.
    getOffsetOf :: b -> i -> m Int
    getOffsetOf es i = (`offset` i) <$> getBounds es
    
    -- | 'getIndexOf' is 'indexOf' version for mutable structures.
    getIndexOf :: b -> Int -> m i
    getIndexOf es i = (`index` i) <$> getBounds es
    
    -- | 'getIndices' returns 'indices' of mutable data structure.
    getIndices :: b -> m [i]
    getIndices =  fmap range . getBounds

--------------------------------------------------------------------------------

-- | 'LinearM' is 'Linear' version for mutable data structures.
class (Monad m) => LinearM m l e | l -> m, l -> e
  where
    {-# MINIMAL (newLinear|fromFoldableM), (getLeft|getRight), (!#>), writeM, copyTo #-}
    
    -- | Monadic 'single'.
    newNull :: m l
    newNull =  newLinear []
    
    -- | Monadic 'isNull'.
    nowNull :: l -> m Bool
    nowNull =  fmap isNull . getLeft
    
    -- | Monadic 'single'.
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
    
    -- | Monadic 'fromList'.
    {-# INLINE newLinear #-}
    newLinear :: [e] -> m l
    newLinear =  fromFoldableM
    
    -- | Monadic 'fromListN'.
    {-# INLINE newLinearN #-}
    newLinearN :: Int -> [e] -> m l
    newLinearN =  newLinear ... take
    
    -- | Monadic 'fromFoldable'.
    {-# INLINE fromFoldableM #-}
    fromFoldableM :: (Foldable f) => f e -> m l
    fromFoldableM =  newLinear . toList
    
    -- | Left view of line.
    {-# INLINE getLeft #-}
    getLeft :: l -> m [e]
    getLeft =  fmap reverse . getRight
    
    -- | Right view of line.
    {-# INLINE getRight #-}
    getRight :: l -> m [e]
    getRight =  fmap reverse . getLeft
    
    -- | (!#>) is unsafe monadic offset-based reader.
    (!#>) :: l -> Int -> m e
    
    -- | Unsafe monadic offset-based writer.
    writeM :: l -> Int -> e -> m ()
    
    -- | Create copy.
    {-# INLINE copied #-}
    copied :: l -> m l
    copied =  getLeft >=> newLinear
    
    -- | @copied' es l n@ returns the slice of @es@ from @l@ of length @n@.
    {-# INLINE copied' #-}
    copied' :: l -> Int -> Int -> m l
    copied' es l n = getLeft es >>= newLinearN n . drop l
    
    -- | Monadic 'reverse'.
    {-# INLINE reversed #-}
    reversed :: l -> m l
    reversed =  newLinear <=< getRight
    
    -- | Monadic 'concat'.
    merged :: (Foldable f) => f l -> m l
    merged =  newLinear . concat <=< sequence . foldr ((:) . getLeft) []
    
    -- | Monadic version of 'replicate'.
    {-# INLINE filled #-}
    filled :: Int -> e -> m l
    filled n = newLinearN n . replicate n
    
    {- |
      @copyTo source soff target toff count@ writes @count@ elements of @source@
      from @soff@ to @target@ starting with @toff@.
    -}
    copyTo :: l -> Int -> l -> Int -> Int -> m ()

--------------------------------------------------------------------------------

-- | 'SplitM' is 'SplitM' version for mutable data structures.
class (LinearM m s e) => SplitM m s e
  where
    {-# MINIMAL (takeM|sansM), (dropM|keepM), prefixM, suffixM, mprefix, msuffix #-}
    
    {- |
      @takeM n es@ returns a reference to the @es@, keeping first @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    takeM :: Int -> s -> m s
    default takeM :: (BorderedM m s i) => Int -> s -> m s
    takeM n es = do s <- getSizeOf es; sansM (s - n) es
    
    {- |
      @dropM n es@ returns a reference to the @es@, discarding first @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    dropM :: Int -> s -> m s
    default dropM :: (BorderedM m s i) => Int -> s -> m s
    dropM n es = do s <- getSizeOf es; keepM (s - n) es
    
    {- |
      @keepM n es@ returns a reference to the @es@, keeping last @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    keepM :: Int -> s -> m s
    default keepM :: (BorderedM m s i) => Int -> s -> m s
    keepM n es = do s <- getSizeOf es; dropM (s - n) es
    
    {- |
      @sansM n es@ returns a reference to the @es@, discarding last @n@ elements.
      Changes in the source and result must be synchronous.
    -}
    sansM :: Int -> s -> m s
    default sansM :: (BorderedM m s i) => Int -> s -> m s
    sansM n es = do s <- getSizeOf es; takeM (s - n) es
    
    {- |
      @splitM n es@ returns pair of references to the @es@: keeping and
      discarding first @n@ elements. Changes in the source and result must be
      synchronous.
    -}
    splitM  :: Int -> s -> m (s, s)
    splitM n es = liftA2 (,) (takeM n es) (dropM n es)
    
    {- |
      @divideM n es@ returns pair of references to the @es@: discarding and
      keeping last @n@ elements. Changes in the source and results must be
      synchronous.
    -}
    divideM :: Int -> s -> m (s, s)
    divideM n es = liftA2 (,) (sansM n es) (keepM n es)
    
    {- |
      @splitM ns es@ returns the sequence of @es@ prefix references of length
      @n <- ns@. Changes in the source and results must be synchronous.
    -}
    splitsM :: (Foldable f) => f Int -> s -> m [s]
    splitsM ns es = reverse <$> foldl (\ ds' n -> do ds <- ds'; (d, d') <- splitM n (head ds); return (d' : d : ds)) (return [es]) ns
    
    {- |
      @dividesM ns es@ returns the sequence of @es@ suffix references of length
      @n <- ns@. Changes in the source and results must be synchronous.
    -}
    dividesM :: (Foldable f) => f Int -> s -> m [s]
    dividesM ns es = foldr (\ n ds' -> do ds <- ds'; (d, d') <- divideM n (head ds); return (d' : d : ds)) (return [es]) ns
    
    {- |
      @partsM n es@ returns the sequence of @es@ prefix references, splitted by
      offsets in @es@. Changes in the source and results must be synchronous.
    -}
    partsM :: (Foldable f) => f Int -> s -> m [s]
    partsM =  splitsM . go . toList where go is = zipWith (-) is (0 : is)
    
    {- |
      @chunksM n es@ returns the sequence of @es@ prefix references of length
      @n@. Changes in the source and results must be synchronous.
    -}
    chunksM :: Int -> s -> m [s]
    chunksM n es = do (t, d) <- splitM n es; nowNull d ?^ return [t] $ (t :) <$> chunksM n d
    
    {- |
      @eachM n es@ returns new sequence of @es@ elements with step @n@. eachM
      shouldn't return references to @es@.
    -}
    eachM :: Int -> s -> m s
    eachM n = newLinearN n . each n <=< getLeft
    
    -- | @prefixM p es@ returns the longest @es@ prefix size, satisfying @p@.
    prefixM :: (e -> Bool) -> s -> m Int
    
    -- | @suffixM p es@ returns the longest @es@ suffix size, satisfying @p@.
    suffixM :: (e -> Bool) -> s -> m Int
    
    -- | @mprefix p es@ returns the longest @es@ prefix size, satisfying @p@.
    mprefix :: (e -> m Bool) -> s -> m Int
    
    -- | @msuffix p es@ returns the longest @es@ suffix size, satisfying @p@.
    msuffix :: (e -> m Bool) -> s -> m Int

--------------------------------------------------------------------------------

-- | Kind (* -> *) 'SplitM' structure.
type SplitM1 m l e = SplitM m (l e) e

-- | Kind (* -> *) 'LinearM' structure.
type LinearM1 m l e = LinearM m (l e) e

-- | Kind (* -> *) 'BorderedM' structure.
type BorderedM1 m l i e = BorderedM m (l e) i

-- | Kind (* -> * -> *) 'BorderedM' structure.
type BorderedM2 m l i e = BorderedM m (l i e) i

--------------------------------------------------------------------------------

-- | sortedM checks if structure is sorted.
sortedM :: (LinearM m l e, Ord e) => l -> m Bool
sortedM =  fmap (\ es -> null es || and (zipWith (<=) es (tail es))) . getLeft

-- | Just swap two elements.
{-# INLINE swapM #-}
swapM :: (LinearM m v e) => v -> Int -> Int -> m ()
swapM es i j = do ei <- es !#> i; writeM es i =<< es !#> j; writeM es j ei





