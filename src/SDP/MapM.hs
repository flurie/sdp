{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures, ConstraintKinds, BangPatterns #-}

{- |
    Module      :  SDP.MapM
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.MapM@ provides 'MapM' - class of mutable dictionaries.
-}
module SDP.MapM
(
  -- * Mutable maps
  MapM (..), MapM1, MapM2
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Internal
import SDP.LinearM

default ()

infixl 5 >!, !>, !?>

--------------------------------------------------------------------------------

-- | Class for work with mutable indexed structures.
class (Monad m) => MapM m map key e | map -> m, map -> key, map -> e
  where
    {-# MINIMAL newMap', overwrite, ((>!)|(!?>)), kfoldrM, kfoldlM #-}
    
    -- | Create new mutable map from list of associations.
    newMap :: [(key, e)] -> m map
    newMap =  newMap' (undEx "newMap {default}")
    
    -- | Create new mutable map from list of associations.
    newMap' :: e -> [(key, e)] -> m map
    
    -- | getAssocs returns 'assocs' of mutable data structure.
    default getAssocs :: (LinearM m map e) => map -> m [(key, e)]
    getAssocs :: map -> m [(key, e)]
    getAssocs es = liftA2 zip (getKeys es) (getLeft es)
    
    -- | (>!) is unsafe monadic reader.
    {-# INLINE (>!) #-}
    (>!) :: map -> key -> m e
    (>!) =  fmap (fromMaybe $ undEx "(!) {default}") ... (!?>)
    
    -- | (!>) is well-safe monadic reader.
    {-# INLINE (!>) #-}
    default (!>) :: (BorderedM m map key) => map -> key -> m e
    (!>) :: map -> key -> m e
    es !> i = do
      let msg = "(!>) {default}"
      bnds <- getBounds es
      case inBounds bnds i of
        IN -> es >! i
        ER -> empEx   msg
        OR -> overEx  msg
        UR -> underEx msg
    
    -- | (!?>) is completely safe monadic reader.
    (!?>) :: map -> key -> m (Maybe e)
    es !?> i = do b <- memberM' es i; b ? Just <$> (es >! i) $ return empty
    
    {- |
      This function designed to overwrite large enough fragments of the
      structure (unlike 'writeM' and 'writeM_')
      
      In addition to write operations, 'overwrite' can perform move and cleanup
      operations, various optimization of data presentation, depending on the
      implementation of a particular structure. Since the reference to the
      original structure may not be the same as reference to the result (which
      implementation is undesirable, but acceptable), the original reference
      (argument) shouldn't be used after 'overwrite'.
      
      All standard SDP structures support secure in-place 'overwrite'.
      
      If the structure uses unmanaged memory, then all unused fragments in the
      resulting structure must be deallocated, regardless of reachability by
      original reference (argument).
      
      Please note that @overwrite@ require a list of associations with indices
      in the current structure bounds and ignore any other, therefore:
      
      > fromAssocs bnds ascs /= (fromAssocs bnds ascs >>= flip overwrite ascs)
    -}
    overwrite :: map -> [(key, e)] -> m map
    
    -- | Checks if key in map.
    default memberM' :: (BorderedM m map key) => map -> key -> m Bool
    memberM' :: map -> key -> m Bool
    memberM' =  nowIndexIn
    
    -- | Returns list of map keys.
    default getKeys :: (BorderedM m map key) => map -> m [key]
    getKeys :: map -> m [key]
    getKeys =  getIndices
    
    -- | (.?) is monadic version of (.$).
    (.?) :: (e -> Bool) -> map -> m (Maybe key)
    (.?) =  fmap listToMaybe ... (*?)
    
    -- | (*?) is monadic version of (*$).
    (*?) :: (e -> Bool) -> map -> m [key]
    (*?) p = (select (p . snd ?+ fst) <$>) . getAssocs
    
    -- | 'kfoldrM' is right monadic fold with key.
    kfoldrM :: (key -> e -> acc -> m acc) -> acc -> map -> m acc
    
    -- | 'kfoldlM' is left monadic fold with key.
    kfoldlM :: (key -> acc -> e -> m acc) -> acc -> map -> m acc
    
    -- | 'kfoldrM'' is strict version of 'kfoldrM'.
    kfoldrM' :: (key -> e -> acc -> m acc) -> acc -> map -> m acc
    kfoldrM' f = kfoldrM (\ !i e !r -> f i e r)
    
    -- | 'kfoldlM'' is strict version of 'kfoldlM'.
    kfoldlM' :: (key -> acc -> e -> m acc) -> acc -> map -> m acc
    kfoldlM' f = kfoldlM (\ !i !r e -> f i r e)

--------------------------------------------------------------------------------

-- | Rank @(* -> *)@ 'MapM'.
type MapM1 m map key e = MapM m (map e) key e

-- | Rank @(* -> * -> *)@ 'MapM'.
type MapM2 m map key e = MapM m (map key e) key e

--------------------------------------------------------------------------------

empEx :: String -> a
empEx =  throw . EmptyRange . showString "in SDP.MapM."

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.MapM."

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.MapM."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.MapM."




