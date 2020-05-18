{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

{- |
    Module      :  SDP.Map
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
  
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

import SDP.Internal

default ()

--------------------------------------------------------------------------------

-- | Map is class of dictionaries. It's unstable, provisional implementation.
class (Ord k) => Map m k e | m -> k, m -> e
  where
    {-# MINIMAL (fromSet|mapAssocs), unionWith', intersectionWith', differenceWith' #-}
    
    {-# INLINE fromSet #-}
    -- | fromSet creates map using elements of (correct) 'set' as keys.
    fromSet :: (Set s k) => (k -> e) -> s -> m
    fromSet =  mapAssocs ... fromSet
    
    -- | mapAssocs creates map from list of assocs @(key, element)@.
    mapAssocs :: [(k, e)] -> m
    mapAssocs ies = (`lookup_` ies) `fromSet` fsts ies
    
    {-# INLINE toMap #-}
    -- | toMap creates correct map from arbitrary data.
    toMap :: m -> m
    toMap =  mapAssocs . listMap
    
    -- | listMap is just assocs.
    default listMap :: (Bordered m k e, Linear m e) => m -> [(k, e)]
    listMap :: m -> [(k, e)]
    listMap es = indices es `zip` listL es
    
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
    insert' =  curry (union' . mapAssocs . single)
    
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
    lookup' e = fromMaybe e ... lookup
    
    {-# INLINE lookup_ #-}
    -- | lookup_ is unsafe lookup, may fail.
    lookup_ :: k -> m -> e
    lookup_ =  fromJust ... lookup
    
    {-# INLINE keySet #-}
    -- | @keySet@ is generic 'keys'.
    keySet :: (Set s k) => m -> s
    keySet =  fromList . keys
    
    {-# INLINE keys #-}
    -- | Return list of keys.
    keys :: m -> [k]
    keys =  fsts . listMap
    
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
    lookupLE' k me = (,) k <$> lookup k me <|> lookupLT' k me
    
    {-# INLINE lookupGE' #-}
    -- | lookupGE' is just 'lookupGE' for maps.
    lookupGE' :: k -> m -> Maybe (k, e)
    lookupGE' k me = (,) k <$> lookup k me <|> lookupGT' k me
    
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
      
      Note that diffenenceWith is poorer than a similar functions in containers.
    -}
    differenceWith' :: (e -> e -> Maybe e) -> m -> m -> m
    
    {- |
      @intersectionWith' f mx my@ combines elements of 'intersection'' by @f@:
      if @'isJust' (f x y)@ (where @(k1, x) <- mx, (k2, y) <- my, k1 == k2@),
      then element is added to result map.
    -}
    intersectionWith' :: (e -> e -> e) -> m -> m -> m
    
    -- | unionsWith' is right fold by unionWith'.
    unionsWith' :: (Foldable f) => (e -> e -> e) -> f m -> m
    unionsWith' =  foldr1 . unionWith'

--------------------------------------------------------------------------------

-- | union' is just @unionWith' const@.
{-# INLINE union' #-}
union' :: (Map m k e) => m -> m -> m
union' =  unionWith' const

-- | intersection' is just @intersectionWith' const@.
{-# INLINE intersection' #-}
intersection' :: (Map m k e) => m -> m -> m
intersection' =  intersectionWith' const

-- | unions is just @unionsWith' const@.
{-# INLINE unions' #-}
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
    isMapElem k = isContainedIn cmpfst (k, unreachEx    "isMapElem")
    lookupLT' k = lookupLTWith  cmpfst (k, unreachEx "lookupLTWith")
    lookupGT' k = lookupGTWith  cmpfst (k, unreachEx "lookupGTWith")
    lookupLE' k = lookupLEWith  cmpfst (k, unreachEx "lookupLEWith")
    lookupGE' k = lookupGEWith  cmpfst (k, unreachEx "lookupGEWith")
    
    lookup = L.lookup
    
    update' upd k e = go
      where
        go es@(m@(k', x) : ms) = case k <=> k' of
          EQ -> (k', upd x e) : ms
          LT -> m : go ms
          GT -> es
        go _ = []
    
    insert' k e es@(m : ms) = case k <=> fst m of
      LT -> m : insert' k e ms
      GT -> (fst m, e) : es
      EQ -> es
    insert' k e _ = [(k, e)]
    
    delete' k (m : ms) = case k <=> fst m of
      LT -> m : delete' k ms
      GT -> m : ms
      EQ -> ms
    delete' _ _ = []
    
    adjust f k (m@(k', x) : ms) = case k <=> k' of
      LT -> m : adjust f k ms
      EQ -> (k', f x) : ms
      GT -> m : ms
    adjust _ _ _ = []
    
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
    
    unionsWith' = (`foldl` Z) . unionWith'

--------------------------------------------------------------------------------

unreachEx :: String -> a
unreachEx = throw . UnreachableException . showString "in SDP.Map."




