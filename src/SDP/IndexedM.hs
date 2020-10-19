{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures, ConstraintKinds #-}

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
    module SDP.MapM,
    
    -- * IndexedM
    IndexedM (..), IndexedM1, IndexedM2,
    
    -- * IFoldM
    IFoldM (..), IFoldM1, IFoldM2,
    
    -- * Thaw
    Thaw (..), Thaw1,
    
    -- * Freeze
    Freeze (..), Freeze1,
    
    -- * Related functions
    swapM
  )
where

import Prelude ()
import SDP.SafePrelude
import SDP.Internal
import SDP.LinearM
import SDP.Indexed
import SDP.MapM

default ()

--------------------------------------------------------------------------------

-- | Class for work with mutable indexed structures.
class (LinearM m v e, BorderedM m v i, MapM m v i e) => IndexedM m v i e
  where
    {-# MINIMAL fromAssocs', fromIndexed', fromIndexedM #-}
    
    {-# INLINE fromAssocs #-}
    {- |
      @fromAssocs bnds ascs@ creates new structure from list of associations,
      without default element. Note that @bnds@ is @ascs@ bounds and may not
      match with the result bounds (not always possible).
    -}
    fromAssocs :: (i, i) -> [(i, e)] -> m v
    fromAssocs =  flip fromAssocs' (undEx "fromAssocs")
    
    {- |
      @fromAssocs' bnds defvalue ascs@ creates new structure from list of
      associations, with default element. Note that @bnds@ is @ascs@ bounds and
      may not match with the result bounds (not always possible).
    -}
    fromAssocs' :: (i, i) -> e -> [(i, e)] -> m v
    
    writeM_ :: v -> Int -> e -> m ()
    writeM_ es i e = do bnds <- getBounds es; void $ overwrite es [(index bnds i, e)]
    
    {- |
      @'writeM' map key e@ writes element @e@ to @key@ position safely (if @key@
      is out of @map@ range, do nothing). The 'writeM' function is intended to
      overwrite only existing values, so its behavior is identical for
      structures with both static and dynamic boundaries.
      
      By default, implemented via 'overwrite'.
    -}
    writeM :: v -> i -> e -> m ()
    writeM es i e = do b <- nowIndexIn es i; when b . void $ overwrite es [(i, e)]
    
    -- | fromIndexed' is overloaded version of thaw.
    fromIndexed' :: (Indexed v' j e) => v' -> m v
    
    -- | fromIndexed converts one mutable structure to other.
    fromIndexedM :: (IndexedM m v' j e) => v' -> m v
    
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

--------------------------------------------------------------------------------

-- | IFoldM is monadic version of IFold.
class (Monad m, Index i) => IFoldM m v i e | v -> m, v -> i, v -> e
  where
    {-# MINIMAL (ifoldrM | ofoldrM), (ifoldlM | ofoldlM) #-}
    
    -- | 'ifoldrM' is right monadic fold with index
    default ifoldrM :: (BorderedM m v i) => (i -> e -> r -> m r) -> r -> v -> m r
    ifoldrM :: (i -> e -> r -> m r) -> r -> v -> m r
    ifoldrM f base es = do bnds <- getBounds es; ofoldrM (f . index bnds) base es
    
    -- | 'ifoldlM' is left  monadic fold with index
    default ifoldlM :: (BorderedM m v i) => (i -> r -> e -> m r) -> r -> v -> m r
    ifoldlM :: (i -> r -> e -> m r) -> r -> v -> m r
    ifoldlM f base es = do bnds <- getBounds es; ofoldlM (f . index bnds) base es
    
    -- | 'ofoldrM' is right monadic fold with offset
    default ofoldrM  :: (BorderedM m v i) => (Int -> e -> r -> m r) -> r -> v -> m r
    ofoldrM  :: (Int -> e -> r -> m r) -> r -> v -> m r
    ofoldrM f base es = ifoldrM (\ i e r -> do o <- getOffsetOf es i; f o e r) base es
    
    -- | 'ofoldlM' is left  monadic fold with offset
    default ofoldlM  :: (BorderedM m v i) => (Int -> r -> e -> m r) -> r -> v -> m r
    ofoldlM  :: (Int -> r -> e -> m r) -> r -> v -> m r
    ofoldlM f base es = ifoldlM (\ i r e -> do o <- getOffsetOf es i; f o r e) base es
    
    -- | 'i_foldrM' is just 'foldrM' in 'IFoldM' context
    i_foldrM :: (e -> r -> m r) -> r -> v -> m r
    i_foldrM =  ifoldrM . const
    
    -- | 'i_foldlM' is just 'foldlM' in 'IFoldM' context
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

-- | Kind (* -> *) 'IndexedM'.
type IndexedM1 m v i e = IndexedM m (v e) i e

-- | Kind (* -> * -> *) 'IndexedM'.
type IndexedM2 m v i e = IndexedM m (v i e) i e

-- | Kind (* -> *) 'IFoldM'.
type IFoldM1 m v i e = IFoldM m (v e) i e

-- | Kind (* -> * -> *) 'IFoldM'.
type IFoldM2 m v i e = IFoldM m (v i e) i e

-- | Kind (* -> *) 'Thaw'.
type Thaw1 m v v' e = Thaw m (v e) (v' e)

-- | Kind (* -> *) 'Freeze'.
type Freeze1 m v' v e = Freeze m (v' e) (v e)

--------------------------------------------------------------------------------

{-# INLINE swapM #-}
-- | Just swap two elements.
swapM :: (IndexedM m v i e) => v -> Int -> Int -> m ()
swapM es i j = do ei <- es !#> i; writeM_ es i =<< es !#> j; writeM_ es j ei

--------------------------------------------------------------------------------

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.IndexedM."




