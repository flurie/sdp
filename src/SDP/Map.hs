{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{- |
    Module      :  SDP.Map
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (imports SDP.Set)
  
  @SDP.Map@ provides Map class for dictionaries.
-}
module SDP.Map
(
  -- * Exports
  module SDP.Set,
  
  -- * Map
  Map (..),
  
  -- * Related functions
  union',  symdiff',  difference',  intersection',
  unions', symdiffs', differences', intersections'
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
    fromSet   :: (Set s k) => (k -> e) -> s -> m
    listMap   :: m -> [(k, e)]
    toMap     :: [(k, e)] -> m
    
    filterMap :: (k -> e -> Bool) -> m -> m
    
    update' :: (e -> e -> e) -> k -> e -> m -> m
    adjust' :: (e -> e) -> k -> m -> m
    insert' :: k -> e -> m -> m
    delete' :: k -> m -> m
    
    alter   :: (Maybe e -> Maybe e) -> k -> m -> m
    lookup  :: k -> m -> Maybe e
    lookup' :: e -> k -> m -> e
    lookup_ :: k -> m -> e
    
    keySet :: (Set s k) => m -> s
    keySet =  fromList . keys
    
    keys   :: m -> [k]
    
    isMapElem :: k -> m -> Bool
    
    lookupLT' :: k -> m -> Maybe (k, e)
    lookupGT' :: k -> m -> Maybe (k, e)
    lookupLE' :: k -> m -> Maybe (k, e)
    lookupGE' :: k -> m -> Maybe (k, e)
    
    unionWith'         :: (e -> e -> e) -> m -> m -> m
    symdiffWith'       :: (e -> e -> e) -> m -> m -> m
    differenceWith'    :: (e -> e -> e) -> m -> m -> m
    intersectionWith'  :: (e -> e -> e) -> m -> m -> m
    
    unionsWith'        :: (Foldable f) => (e -> e -> e) -> f m -> m
    symdiffsWith'      :: (Foldable f) => (e -> e -> e) -> f m -> m
    differencesWith'   :: (Foldable f) => (e -> e -> e) -> f m -> m
    intersectionsWith' :: (Foldable f) => (e -> e -> e) -> f m -> m

--------------------------------------------------------------------------------

union' :: (Map m k e) => m -> m -> m
union' =  unionWith' const

symdiff' :: (Map m k e) => m -> m -> m
symdiff' =  symdiffWith' const

difference' :: (Map m k e) => m -> m -> m
difference' =  differenceWith' const

intersection' :: (Map m k e) => m -> m -> m
intersection' =  intersectionWith' const

unions' :: (Map m k e, Foldable f) => f m -> m
unions' =  unionsWith' const

symdiffs' :: (Map m k e, Foldable f) => f m -> m
symdiffs' =  symdiffsWith' const

differences' :: (Map m k e, Foldable f) => f m -> m
differences' =  differencesWith' const

intersections' :: (Map m k e, Foldable f) => f m -> m
intersections' =  intersectionsWith' const

--------------------------------------------------------------------------------

instance (Ord k) => Map [(k, e)] k e
  where
    fromSet f se = [ (e, f e) | e <- listL se ]
    toMap   = L.sortBy cmpfst
    listMap = id
    
    keys = fsts
    
    filterMap f = filter (uncurry f)
    
    isMapElem = undefined
    
    lookupLT' k = lookupLTWith cmpfst (k, unreachEx "lookupLTWith")
    lookupGT' k = lookupGTWith cmpfst (k, unreachEx "lookupGTWith")
    lookupLE' k = lookupLEWith cmpfst (k, unreachEx "lookupLEWith")
    lookupGE' k = lookupGEWith cmpfst (k, unreachEx "lookupGEWith")
    
    lookup = L.lookup
    
    lookup_   k = fromJust    . lookup k
    lookup' e k = fromMaybe e . lookup k
    
    update' = undefined
    adjust' = undefined
    insert' = undefined
    delete' = undefined
    alter   = undefined
    
    unionWith'         = undefined
    symdiffWith'       = undefined
    differenceWith'    = undefined
    intersectionWith'  = undefined
    
    unionsWith'        f = unionWith' f `foldl` Z
    symdiffsWith'      f = symdiffWith' f `foldl` Z
    differencesWith'   f = differenceWith' f `foldl` Z
    intersectionsWith' f = intersectionWith' f `foldl` Z

--------------------------------------------------------------------------------

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.Map." ++ msg ++ " (List)"


