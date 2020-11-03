{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures, ConstraintKinds #-}

{- |
    Module      :  SDP.IndexedM
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.IndexedM" provides 'IndexedM' and 'Thaw' classes.
-}
module SDP.IndexedM
(
  -- * Exports
  module SDP.LinearM,
  module SDP.Indexed,
  module SDP.MapM,
  
  -- * IndexedM
  IndexedM (..), IndexedM1, IndexedM2,
  
  -- * Thaw
  Thaw (..), Thaw1
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
    {-# MINIMAL fromIndexed', fromIndexedM #-}
    
    {- |
      @fromAssocs bnds ascs@ creates new structure from list of associations,
      without default element. Note that @bnds@ is @ascs@ bounds and may not
      match with the result bounds (not always possible).
    -}
    fromAssocs :: (i, i) -> [(i, e)] -> m v
    fromAssocs =  flip fromAssocs' (undEx "fromAssocs {default}")
    
    {- |
      @fromAssocs' bnds defvalue ascs@ creates new structure from list of
      associations, with default element. Note that @bnds@ is @ascs@ bounds and
      may not match with the result bounds (not always possible).
    -}
    fromAssocs' :: (i, i) -> e -> [(i, e)] -> m v
    fromAssocs' bnds defvalue = newMap' defvalue . filter (inRange bnds . fst)
    
    {- |
      @'writeM' map key e@ writes element @e@ to @key@ position safely (if @key@
      is out of @map@ range, do nothing). The 'writeM' function is intended to
      overwrite only existing values, so its behavior is identical for
      structures with both static and dynamic boundaries.
    -}
    writeM' :: v -> i -> e -> m ()
    writeM' es i e = do bnds <- getBounds es; writeM es (offset bnds i) e
    
    -- | Just swap two elements.
    swapM' :: v -> i -> i -> m ()
    swapM' es i j = do ei <- es >! i; writeM' es i =<< es >! j; writeM' es j ei
    
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
    
    -- | Update element by given function.
    updateM' :: v -> (e -> e) -> i -> m ()
    updateM' es f i = writeM' es i . f =<< es >! i
    
    -- | Update elements by mapping with indices.
    updatesM' :: v -> (i -> e -> e) -> m v
    updatesM' es f = kfoldrM (\ i e go -> go <$ writeM' es i (f i e)) es es

--------------------------------------------------------------------------------

-- | Service class of immutable to mutable conversions.
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

-- | Kind @(* -> *)@ 'IndexedM'.
type IndexedM1 m v i e = IndexedM m (v e) i e

-- | Kind @(* -> * -> *)@ 'IndexedM'.
type IndexedM2 m v i e = IndexedM m (v i e) i e

-- | Kind @(* -> *)@ 'Thaw'.
type Thaw1 m v v' e = Thaw m (v e) (v' e)

--------------------------------------------------------------------------------

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.IndexedM."

