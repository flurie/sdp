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
  union',  difference',  intersection', unions'
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

{-
  Map is class of dictionaries. It's unstable class that will expand.
  I intend to stabilize it in sdp-0.3.
-}
class (Ord k) => Map m k e | m -> k, m -> e
  where
    {-# MINIMAL toMap, listMap, unionWith', intersectionWith', differenceWith', unionsWith' #-}
    
    -- | fromSet creates map using elements of (correct) 'set' as keys.
    fromSet   :: (Set s k) => (k -> e) -> s -> m
    fromSet f =  toMap . fromSet f
    
    -- | toMap creates map from list of 'assocs' @(key, element)@.
    toMap     :: [(k, e)] -> m
    
    -- | listMap is just 'assocs'.
    listMap   :: m -> [(k, e)]
    
    -- | @filterMap f@ is same as @'toMap' . 'filter' ('uncurry' f) . 'listMap'@
    filterMap :: (k -> e -> Bool) -> m -> m
    filterMap f =  toMap . filterMap f . listMap
    
    {- |
      @update' upd key new map@ overwrites @(key, old)@ by @(key, upd old new)@
      or just write @new@ if in @map@ is no @key@.
    -}
    update' :: (e -> e -> e) -> k -> e -> m -> m
    update' f k e = toMap . update' f k e . listMap
    
    -- | @adjust upd key map@ overwrites @(key, elem)@ by @(key, upd elem)@
    adjust :: (e -> e) -> k -> m -> m
    adjust f k = toMap . adjust f k . listMap
    
    -- | @insert' key e@ is just @'insert' (k, e)@.
    insert' :: k -> e -> m -> m
    insert' k e me = union' me $ toMap [(k, e)]
    
    -- | delete' removes element with given key.
    delete' :: k -> m -> m
    delete' k = toMap . delete' k . listMap
    
    {-
      lookup tries to find element in map by it's key. Requires 'Ord', so may
      work in O(log n) but less general than 'Data.List.lookup'.
    -}
    lookup  :: k -> m -> Maybe e
    lookup k = lookup k . listMap
    
    -- | lookup' is lookup with default value.
    lookup' :: e -> k -> m -> e
    lookup' e k = fromMaybe e . lookup k
    
    -- | lookup_ is unsafe lookup, may fail.
    lookup_ :: k -> m -> e
    lookup_ k = fromJust . lookup k
    
    -- | @keySet@ is generic 'keys' (just uses fromList).
    keySet :: (Set s k) => m -> s
    keySet =  fromList . keys
    
    -- | Return list of keys.
    keys :: m -> [k]
    keys = fsts . listMap
    
    -- | isMapElem is just 'isContainedIn' for maps.
    isMapElem :: k -> m -> Bool
    isMapElem k = isMapElem k . listMap
    
    -- | lookupLT' is just 'lookupLT' for maps.
    lookupLT' :: k -> m -> Maybe (k, e)
    lookupLT' k = lookupLT' k . listMap
    
    -- | lookupGT' is just 'lookupGT' for maps.
    lookupGT' :: k -> m -> Maybe (k, e)
    lookupGT' k = lookupGT' k . listMap
    
    -- | lookupLE' is just 'lookupLE' for maps.
    lookupLE' :: k -> m -> Maybe (k, e)
    lookupLE' k me = case lookup k me of {Just e -> Just (k, e); _ -> lookupLT' k me}
    
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
      @intersectionWith' f mx my@ combines elements of 'intersection' by @f@:
      if @'isJust' (f x y)@ (where @(k1, x) <- mx, (k2, y) <- my, k1 == k2@),
      then element is added to result map.
    -}
    intersectionWith' :: (e -> e -> e) -> m -> m -> m
    
    -- | unionsWith' is left fold by unionWith'.
    unionsWith' :: (Foldable f) => (e -> e -> e) -> f m -> m

--------------------------------------------------------------------------------

-- | union' is just @unionWith' const@.
union' :: (Map m k e) => m -> m -> m
union' =  unionWith' const

-- | difference' is just @difference' const@.
difference' :: (Map m k e) => m -> m -> m
difference' =  undefined

-- | intersection' is just @intersectionWith' const@.
intersection' :: (Map m k e) => m -> m -> m
intersection' =  intersectionWith' const

-- | unions is just @unionsWith' const@.
unions' :: (Map m k e, Foldable f) => f m -> m
unions' =  unionsWith' const

--------------------------------------------------------------------------------

instance (Ord k) => Map [(k, e)] k e
  where
    fromSet f se = [ (e, f e) | e <- listL se ]
    
    toMap   = L.sortBy cmpfst
    listMap = id
    
    keys = fsts
    
    filterMap f = filter (uncurry f)
    isMapElem k = isContainedIn cmpfst (k, unreachEx "isMapElem")
    
    lookupLT' k = lookupLTWith cmpfst (k, unreachEx "lookupLTWith")
    lookupGT' k = lookupGTWith cmpfst (k, unreachEx "lookupGTWith")
    lookupLE' k = lookupLEWith cmpfst (k, unreachEx "lookupLEWith")
    lookupGE' k = lookupGEWith cmpfst (k, unreachEx "lookupGEWith")
    
    lookup = L.lookup
    
    update' upd k e = go
      where
        go [] = []
        go es@(m@(k', x) : ms) = case k <=> k' of {LT -> m : go ms; EQ -> (k', upd x e) : ms; GT -> es}
    
    insert' k e [] = [(k, e)]
    insert' k e es@(m : ms) = case k <=> fst m of {LT -> m : insert' k e ms; EQ -> es; GT -> (fst m, e) : es}
    
    delete' _ [] = []
    delete' k (m : ms) = case k <=> fst m of {LT -> m : delete' k ms; EQ -> ms; GT -> m : ms}
    
    adjust _ _ [] = []
    adjust f k (m@(k', x) : ms) = case k <=> k' of {LT -> m : adjust f k ms; EQ -> (k', f x) : ms; GT -> m : ms}
    
    unionWith'         = undefined
    differenceWith'    = undefined
    intersectionWith'  = undefined
    
    unionsWith' f = unionWith' f `foldl` Z

--------------------------------------------------------------------------------

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.Map." ++ msg ++ " (List)"


