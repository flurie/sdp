{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ConstraintKinds #-}

{- |
    Module      :  SDP.IndexedM
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
  
  @SDP.IndexedM@ provides 'IndexedM', 'IFoldM', 'Freeze' and 'Thaw' classes.
-}
module SDP.IndexedM
  (
    -- * Exports
    module SDP.LinearM,
    module SDP.Indexed,
    
    -- * IndexedM
    IndexedM (..), IndexedM1, IndexedM2,
    
    -- * IFoldM
    IFoldM (..), IFoldM1, IFoldM2,
    
    -- * Freeze and Thaw
    Freeze (..), Thaw (..),
    
    -- * Related functions
    swapM
  )
where

import Prelude ()
import SDP.SafePrelude

import SDP.LinearM
import SDP.Indexed

import SDP.Internal

default ()

infixl 5 !#>, >!, !>, !?>

--------------------------------------------------------------------------------

-- | Class for work with mutable indexed structures.
class (LinearM m v e, BorderedM m v i e) => IndexedM m v i e
  where
    {-# MINIMAL fromAssocs', fromIndexed', fromIndexedM, overwrite, ((>!)|(!?>)) #-}
    
    -- | getAssocs returns 'assocs' of mutable data structure.
    getAssocs :: v -> m [(i, e)]
    getAssocs es = liftA2 zip (getIndices es) (getLeft es)
    
    {-# INLINE fromAssocs #-}
    {- |
      @fromAssocs bnds ascs@ creates new structure from list of associations,
      without default element. Note that @bnds@ is @ascs@ bounds and may not
      match with the result bounds (not always possible).
    -}
    fromAssocs :: (i, i) -> [(i, e)] -> m v
    fromAssocs =  (`fromAssocs'` undEx "fromAssocs")
    
    {- |
      @fromAssocs' bnds defvalue ascs@ creates new structure from list of
      associations, with default element. Note that @bnds@ is @ascs@ bounds and
      may not match with the result bounds (not always possible).
    -}
    fromAssocs' :: (i, i) -> e -> [(i, e)] -> m v
    
    -- | (!#>) is unsafe monadic offset-based reader.
    {-# INLINE (!#>) #-}
    (!#>) :: v -> Int -> m e
    es !#> i = do bnds <- getBounds es; es >! index bnds i
    
    -- | (>!) is unsafe monadic reader.
    {-# INLINE (>!) #-}
    (>!) :: v -> i -> m e
    (>!) =  fmap (fromMaybe $ undEx "(!) {default}") ... (!?>)
    
    -- | (!>) is well-safe monadic reader.
    {-# INLINE (!>) #-}
    (!>) :: v -> i -> m e
    (!>) es i = getBounds es >>= \ bnds -> case inBounds bnds i of
        IN -> es >! i
        ER -> throw $ EmptyRange     msg
        OR -> throw $ IndexOverflow  msg
        UR -> throw $ IndexUnderflow msg
      where
        msg = "in SDP.IndexedM.(!>) {default}"
    
    -- | (!?>) is completely safe monadic reader.
    (!?>) :: v -> i -> m (Maybe e)
    (!?>) es i = do b <- getIndexOf es i; b ? Just <$> (es >! i) $ return empty
    
    writeM_ :: v -> Int -> e -> m ()
    writeM_ es i e = do bnds <- getBounds es; void $ overwrite es [(index bnds i, e)]
    
    writeM :: v -> i -> e -> m ()
    writeM es i e = do b <- getIndexOf es i; when b . void $ overwrite es [(i, e)]
    
    -- | fromIndexed' is overloaded version of thaw.
    fromIndexed' :: (Indexed v' j e) => v' -> m v
    
    -- | fromIndexed converts one mutable structure to other.
    fromIndexedM :: (IndexedM m v' j e) => v' -> m v
    
    {- |
      This function designed to overwrite large enough fragments of the
      structure (unlike 'writeM' and 'writeM_'). In addition to the main
      conversion it can be moved, cleaned and optimized in any possible way. So,
      the original structure may become incorrect and its change may lead to
      undesirable consequences. Therefore, overwrite explicitly return the
      result and the original structure should never been used.
      
      All standard SDP structures support secure in-place @overwrite@.
      
      If the structure uses unmanaged memory, when all unused fragments should
      be cleared regardless of the reachability from the original structure
      (after rewriting) and its correctness.
      
      Please note that @overwrite@ require a list of associations with indices
      in the current structure bounds and ignore any other, therefore
      
      > (fromAssocs bnds ascs) /= (fromAssocs bnds ascs >>= (`overwrite` ascs))
    -}
    overwrite :: v -> [(i, e)] -> m v
    
    -- | reshaped creates new indexed structure from old with reshaping function.
    reshaped :: (IndexedM m v' j e) => (i, i) -> v' -> (i -> j) -> m v
    reshaped bnds es f = fromAssocs bnds =<< range bnds `forM` \ i -> (,) i <$> es !> f i
    
    {- |
      @'fromAccum' f es ies@ create a new structure from @es@ elements
      selectively updated by function @f@ and @ies@ associations list.
    -}
    fromAccum :: (e -> e' -> e) -> v -> [(i, e')] -> m v
    fromAccum f es ascs = getBounds es >>=<< ies $ fromAssocs
      where
        ies = sequence [ do e <- es !> i; return (i, f e e') | (i, e') <- ascs ]
    
    -- | updateM updates elements with specified indices by function.
    updateM :: v -> [i] -> (i -> e -> e) -> m v
    updateM es is f = sequence [ do e <- es !> i; return (i, f i e) | i <- is ] >>= overwrite es
    
    -- | (.?) is monadic version of (.$).
    (.?) :: (e -> Bool) -> v -> m (Maybe i)
    (.?) =  fmap listToMaybe ... (*?)
    
    -- | (*?) is monadic version of (*$).
    (*?) :: (e -> Bool) -> v -> m [i]
    (*?) p es = select (p . snd ?+ fst) <$> getAssocs es

--------------------------------------------------------------------------------

-- | IFoldM is monadic version of IFold.
class (Monad m) => IFoldM m v i e | v -> m, v -> i, v -> e
  where
    {-# MINIMAL ifoldrM, ifoldlM #-}
    
    -- | ifoldrM is right monadic fold with index
    ifoldrM  :: (i -> e -> r -> m r) -> r -> v -> m r
    
    -- | ifoldlM is left  monadic fold with index
    ifoldlM  :: (i -> r -> e -> m r) -> r -> v -> m r
    
    -- | i_foldrM is just foldrM in IFoldM context
    i_foldrM :: (e -> r -> m r) -> r -> v -> m r
    i_foldrM =  ifoldrM . const
    
    -- | i_foldlM is just foldlM in IFoldM context
    i_foldlM :: (r -> e -> m r) -> r -> v -> m r
    i_foldlM =  ifoldlM . const

--------------------------------------------------------------------------------

-- | Service class of immutable-mutable converters.
class (Monad m) => Thaw m v v' | v' -> m
  where
    {- |
      @thaw@ is safe way to convert a immutable structure to a mutable. @thaw@
      should copy the old structure or ensure that it will not be used after the
      procedure calling.
    -}
    thaw :: v -> m v'
    
    {- |
      @unsafeThaw@ is unsafe version of 'thaw'. @unsafeThaw@ doesn't guarantee
      that the structure will be copied or locked. It only guarantees that if
      the old structure isn't used, no error will occur.
    -}
    unsafeThaw :: v -> m v'
    unsafeThaw =  thaw

--------------------------------------------------------------------------------

-- | Service class of mutable-immutable converters.
class (Monad m) => Freeze m v' v | v' -> m
  where
    {- |
      @freeze@ is a safe way to convert a mutable structure to a immutable.
      @freeze@ should copy the old structure or ensure that it will not be used
      after calling the procedure.
    -}
    freeze :: v' -> m v
    
    {- |
      @unsafeFreeze@ is unsafe version of 'freeze'. @unsafeFreeze@ doesn't
      guarantee that the structure will be copied or locked. It only guarantees
      that if the old structure isn't used, no error will occur.
    -}
    unsafeFreeze :: v' -> m v
    unsafeFreeze =  freeze

--------------------------------------------------------------------------------

-- | Rank (* -> *) 'IndexedM' structure.
type IndexedM1 m v i e = IndexedM m (v e) i e

-- | Rank (* -> * -> *) 'IndexedM' structure.
type IndexedM2 m v i e = IndexedM m (v i e) i e

-- | Rank (* -> *) 'IFoldM' structure.
type IFoldM1 m v i e = IFoldM m (v e) i e

-- | Rank (* -> * -> *) 'IFoldM' structure.
type IFoldM2 m v i e = IFoldM m (v i e) i e

--------------------------------------------------------------------------------

{-# INLINE swapM #-}
-- | Just swap two elements.
swapM :: (IndexedM m v i e) => v -> Int -> Int -> m ()
swapM es i j = do ei <- es !#> i; writeM_ es i =<< es !#> j; writeM_ es j ei

--------------------------------------------------------------------------------

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.IndexedM."

