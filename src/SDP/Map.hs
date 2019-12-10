{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{- |
    Module      :  SDP.Map
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (imports SDP.Set)
  
  @SDP.Map@ provides 'Map' - class for dictionaries.
-}
module SDP.Map
(
  -- * Exports
  module SDP.Set,
  
  -- * Map
  Map (..),
  
  -- * Related functions
  union',  intersection', unions'
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Set

import qualified Data.List as L
import Data.Maybe

import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | Map is class of dictionaries. It's unstable, provisional implementation.
class (Ord k) => Map m k e | m -> k, m -> e
  where
    {-# MINIMAL mapAssocs, listMap, unionWith', intersectionWith', differenceWith', unionsWith' #-}
    
    {-# INLINE fromSet #-}
    -- | fromSet creates map using elements of (correct) 'set' as keys.
    fromSet :: (Set s k) => (k -> e) -> s -> m
    fromSet f = mapAssocs . fromSet f
    
    -- | mapAssocs creates map from list of 'assocs' @(key, element)@.
    mapAssocs :: [(k, e)] -> m
    
    {-# INLINE toMap #-}
    -- | toMap creates correct map from any data.
    toMap :: m -> m
    toMap = mapAssocs . listMap
    
    -- | listMap is just 'assocs'.
    listMap   :: m -> [(k, e)]
    
    {-# INLINE filterMap #-}
    -- | @filterMap f@ is same as @'mapAssocs' . 'filter' ('uncurry' f) . 'listMap'@
    filterMap :: (k -> e -> Bool) -> m -> m
    filterMap f = mapAssocs . filter (uncurry f) . listMap
    
    {-# INLINE update' #-}
    {- |
      @update' upd key new map@ overwrites @(key, old)@ by @(key, upd old new)@
      or just write @new@ if in @map@ is no @key@.
    -}
    update' :: (e -> e -> e) -> k -> e -> m -> m
    update' f = \ k e -> mapAssocs . update' f k e . listMap
    
    {-# INLINE adjust #-}
    -- | @adjust upd key map@ overwrites @(key, elem)@ by @(key, upd elem)@
    adjust :: (e -> e) -> k -> m -> m
    adjust f k = mapAssocs . adjust f k . listMap
    
    {-# INLINE insert' #-}
    -- | @insert' key e@ is just @'insert' (k, e)@.
    insert' :: k -> e -> m -> m
    insert' k e = \ me -> union' me $ mapAssocs [(k, e)]
    
    {-# INLINE delete' #-}
    -- | delete' removes element with given key.
    delete' :: k -> m -> m
    delete' k = mapAssocs . delete' k . listMap
    
    {-# INLINE lookup #-}
    {-
      lookup tries to find element in map by it's key. Requires 'Ord', so may
      work in O(log n) but less general than 'Data.List.lookup'.
    -}
    lookup :: k -> m -> Maybe e
    lookup k = lookup k . listMap
    
    {-# INLINE lookup' #-}
    -- | lookup' is lookup with default value.
    lookup' :: e -> k -> m -> e
    lookup' e k = fromMaybe e . lookup k
    
    {-# INLINE lookup_ #-}
    -- | lookup_ is unsafe lookup, may fail.
    lookup_ :: k -> m -> e
    lookup_ k = fromJust . lookup k
    
    {-# INLINE keySet #-}
    -- | @keySet@ is generic 'keys' (just uses fromList).
    keySet :: (Set s k) => m -> s
    keySet =  fromList . keys
    
    {-# INLINE keys #-}
    -- | Return list of keys.
    keys :: m -> [k]
    keys = fsts . listMap
    
    {-# INLINE isMapElem #-}
    -- | isMapElem is just 'isContainedIn' for maps.
    isMapElem :: k -> m -> Bool
    isMapElem k = isMapElem k . listMap
    
    {-# INLINE lookupLT' #-}
    -- | lookupLT' is just 'lookupLT' for maps.
    lookupLT' :: k -> m -> Maybe (k, e)
    lookupLT' k = lookupLT' k . listMap
    
    {-# INLINE lookupGT' #-}
    -- | lookupGT' is just 'lookupGT' for maps.
    lookupGT' :: k -> m -> Maybe (k, e)
    lookupGT' k = lookupGT' k . listMap
    
    {-# INLINE lookupLE' #-}
    -- | lookupLE' is just 'lookupLE' for maps.
    lookupLE' :: k -> m -> Maybe (k, e)
    lookupLE' k me = case lookup k me of {Just e -> Just (k, e); _ -> lookupLT' k me}
    
    {-# INLINE lookupGE' #-}
    -- | lookupGE' is just 'lookupGE' for maps.
    lookupGE' :: k -> m -> Maybe (k, e)
    lookupGE' k me = case lookup k me of {Just e -> Just (k, e); _ -> lookupGT' k me}
    
    {- |
      unionWith' is 'groupSetWith' for maps but works with real groups of
      elements, not with consequentive equal elements.
      
      unionWith' merges/chooses elements with equal keys from two maps.
    -}
    unionWith' :: (e -> e -> e) -> m -> m -> m
    
    {- |
      @differenceWith' comb mx my@ applies @comb@ to values with equal keys.
      If @comp x y@ (where @(k1, x) <- mx@, @(k2, y) <- my@, @k1 == k2@) is
      'Nothing', element isn't included to result map.
      
      Note that diffenenceWith is poorer than a similar function from
      Data.[Int]Map[.Lazy] (containers), .
    -}
    differenceWith' :: (e -> e -> Maybe e) -> m -> m -> m
    
    {- |
      @intersectionWith' f mx my@ combines elements of 'intersection'' by @f@:
      if @'isJust' (f x y)@ (where @(k1, x) <- mx, (k2, y) <- my, k1 == k2@),
      then element is added to result map.
    -}
    intersectionWith' :: (e -> e -> e) -> m -> m -> m
    
    -- | unionsWith' is left fold by unionWith'.
    unionsWith' :: (Foldable f) => (e -> e -> e) -> f m -> m

--------------------------------------------------------------------------------

{-# INLINE union' #-}
-- | union' is just @unionWith' const@.
union' :: (Map m k e) => m -> m -> m
union' =  unionWith' const

{-# INLINE intersection' #-}
-- | intersection' is just @intersectionWith' const@.
intersection' :: (Map m k e) => m -> m -> m
intersection' =  intersectionWith' const

{-# INLINE unions' #-}
-- | unions is just @unionsWith' const@.
unions' :: (Map m k e, Foldable f) => f m -> m
unions' =  unionsWith' const

--------------------------------------------------------------------------------

instance (Ord k) => Map [(k, e)] k e
  where
    mapAssocs = L.sortBy cmpfst
    toMap     = L.sortBy cmpfst
    keys      = fsts
    listMap   = id
    
    filterMap f = filter (uncurry f)
    fromSet   f = map (\ e -> (e, f e)) . listL
    isMapElem k = isContainedIn cmpfst (k, unreachEx  "isMapElem"  )
    lookupLT' k = lookupLTWith  cmpfst (k, unreachEx "lookupLTWith")
    lookupGT' k = lookupGTWith  cmpfst (k, unreachEx "lookupGTWith")
    lookupLE' k = lookupLEWith  cmpfst (k, unreachEx "lookupLEWith")
    lookupGE' k = lookupGEWith  cmpfst (k, unreachEx "lookupGEWith")
    
    lookup = L.lookup
    
    update' upd k e = go
      where
        go [] = []
        go es@(m@(k', x) : ms) = case k <=> k' of
          EQ -> (k', upd x e) : ms
          LT -> m : go ms
          GT -> es
    
    insert' k e [] = [(k, e)]
    insert' k e es@(m : ms) = case k <=> fst m of
      LT -> m : insert' k e ms
      GT -> (fst m, e) : es
      EQ -> es
    
    delete' _ [] = []
    delete' k (m : ms) = case k <=> fst m of
      LT -> m : delete' k ms
      GT -> m : ms
      EQ -> ms
    
    adjust _ _ [] = []
    adjust f k (m@(k', x) : ms) = case k <=> k' of
      LT -> m : adjust f k ms
      EQ -> (k', f x) : ms
      GT -> m : ms
    
    unionWith' f xs@((kx, x) : mx) ys@((ky, y) : my) = case kx <=> ky of
      LT -> (kx, x) : unionWith' f mx ys
      EQ -> (kx, f x y) : unionWith' f mx my
      GT -> (ky, y) : unionWith' f xs my
    unionWith' _ xs ys = xs ++ ys
    
    differenceWith' f xs@((kx, x) : mx) ys@((ky, y) : my) = case kx <=> ky of
      LT -> (kx, x) : differenceWith' f mx ys
      EQ -> let d = differenceWith' f mx my in case f x y of {Just z -> (kx, z) : d; _ -> d}
      GT -> differenceWith' f xs my
    differenceWith' _ xs _ = xs
    
    intersectionWith' f xs@((kx, x) : mx) ys@((ky, y) : my) = case kx <=> ky of
      LT -> intersectionWith' f mx ys
      EQ -> (kx, f x y) : intersectionWith' f mx my
      GT -> intersectionWith' f xs my
    intersectionWith' _ _ _ = []
    
    unionsWith' f = unionWith' f `foldl` Z

--------------------------------------------------------------------------------

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.Map." ++ msg ++ " (List)"

