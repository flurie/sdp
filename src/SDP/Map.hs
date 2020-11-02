{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures, ConstraintKinds, BangPatterns #-}

{- |
    Module      :  SDP.Map
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Map@ provides 'Map' - class of immutable dictionaries.
-}
module SDP.Map
(
  -- * Exports
  module SDP.Set,
  
  -- * Map
  Map (..), Map1, Map2
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Internal
import SDP.Linear
import SDP.Set

import Data.List ( findIndex, findIndices )

default ()

infixl 9 .!, !, !?

--------------------------------------------------------------------------------

{- |
  'Map' is a class of dictionaries, simple associative arrays with an arbitrary
  (implementation-dependent) key.
  
  In the current implementation, Map (since sdp-0.2) is a superclass of Indexed.
  Map provides a set of operations on associative arrays that are not specific
  to linear data structures and are not limited by the Bordered context (doesn't
  impose significant restrictions on the type of the key, its properties).
  
  The disadvantage of this implementation is the impossibility of simultaneous
  implementation of associative arrays of the form:
  
  > instance Map [e] Int e          -- current implementation
  > instance (Eq k) => Map [(k, e)] -- previous implementation
-}
class (Nullable map, Eq key) => Map map key e | map -> key, map -> e
  where
    {-# MINIMAL toMap', kfoldl, kfoldr, ((.!) | (!?)) #-}
    
    -- | List of associations (index, element).
    default assocs :: (Bordered map key, Linear map e) => map -> [(key, e)]
    assocs :: map -> [(key, e)]
    assocs es = indices es `zip` listL es
    
    {- |
      A less specific version of @assoc@ that creates a new associative array.
      For 'Linear' structures without gaps, it may be less effective.
      
      > Z // ascs = toMap -- forall ascs
    -}
    toMap :: [(key, e)] -> map
    toMap =  toMap' (undEx "toMap {default}")
    
    {- |
      Strict version of 'toMap' with default value.
      
      Note that the default value is only set for uninitialized elements in
      structures with gaps. When new gaps appear, their elements may be
      overwritten with the other values.
    -}
    toMap' :: e -> [(key, e)] -> map
    
    {- |
      @insert' key e map@ inserts @e@ with @key@ to @map@. If @map@ already
      contains an element with @key@, the element will be overwritten.
      
      If @map@ doesn't allow gaps, then the missing elements should be filled
      with default values.
    -}
    default insert' :: (Bordered map key) => key -> e -> map -> map
    insert' :: key -> e -> map -> map
    insert' k e es = toMap $ assocs es :< (k, e)
    
    {- |
      delete' removes element with given key.
      
      If the structure has boundaries, when removed from the beginning (end),
      they should change accordingly. If the structure doesn't allow gaps, then
      when removed from the middle, the actual value should be replaced with the
      default value.
    -}
    delete' :: key -> map -> map
    delete' k = toMap . except ((== k) . fst) . assocs
    
    -- | @member' k map@ checks if there is an element with key @k@ in @map@.
    default member' :: (Bordered map key) => key -> map -> Bool
    member' :: key -> map -> Bool
    member' =  flip indexIn
    
    -- | Update elements of immutable structure (by copying).
    (//) :: map -> [(key, e)] -> map
    (//) =  toMap ... (++) . assocs
    
    -- | (.!) is unsafe reader, can be faster ('!') by skipping checks.
    {-# INLINE (.!) #-}
    (.!) :: map -> key -> e
    (.!) =  fromMaybe (undEx "(.!)") ... (!?)
    
    -- | (!) is well-safe reader. Must 'throw' 'IndexException'.
    default (!) :: (Bordered map key) => map -> key -> e
    (!) :: map -> key -> e
    (!) es i = case inBounds (bounds es) i of
        IN -> es .! i
        ER -> empEx   msg
        OR -> overEx  msg
        UR -> underEx msg
      where
        msg = "(!) {default}"
    
    -- | (!?) is completely safe, but very boring function.
    (!?) :: map -> key -> Maybe e
    (!?) es = not . flip member' es ?- (es .!)
    
    -- | Filter with key.
    filter' :: (key -> e -> Bool) -> map -> map
    filter' f = toMap . filter (uncurry f) . assocs
    {-
    {- |
      'union'' is 'groupSetWith' for maps but works with real groups of
      elements, not with consequentive equal elements.
      
      'union'' merges/chooses elements with equal keys from two maps.
    -}
    union' :: (e -> e -> e) -> m -> m -> m
    
    {- |
      @'difference'' comb mx my@ applies @comb@ to values with equal keys.
      If @comp x y@ (where @(k1, x) <- mx@, @(k2, y) <- my@, @k1 == k2@) is
      'Nothing', element isn't included to result map.
      
      Note that 'diffenence'' is poorer than a similar functions in containers.
    -}
    difference' :: (e -> e -> Maybe e) -> m -> m -> m
    
    {- |
      @'intersection'' f mx my@ combines elements of 'intersection'' by @f@:
      if @'isJust' (f x y)@ (where @(k1, x) <- mx, (k2, y) <- my, k1 == k2@),
      then element is added to result map.
    -}
    intersection' :: (e -> e -> e) -> m -> m -> m
    -}
    -- | Update function, by default uses ('!') and may throw 'IndexException'.
    update :: map -> [key] -> (key -> e -> e) -> map
    update es is f = es // [ (i, f i (es!i)) | i <- is ]
    
    {- |
      @lookupLT' k map@ finds pair @(key, value)@ with smallest @key@, where
      @key < k@ (if any). @k@ may not be a @map@ element.
    -}
    lookupLT' :: (Ord key) => key -> map -> Maybe (key, e)
    lookupLT' k = lookupLTWith cmpfst (k, unreachEx "lookupLT'") . assocs
    
    {- |
      @lookupGT' k map@ finds pair @(key, value)@ with greatest @key@, where
      @key > k@ (if any). @k@ may not be a @map@ element.
    -}
    lookupGT' :: (Ord key) => key -> map -> Maybe (key, e)
    lookupGT' k = lookupGTWith cmpfst (k, unreachEx "lookupGT'") . assocs
    
    {- |
      @lookupLE' k map@ finds pair @(key, value)@ with smallest @key@, where
      @key <= k@ (if any). If @k@ is a @map@ element, returns @(k, e)@.
    -}
    lookupLE' :: (Ord key) => key -> map -> Maybe (key, e)
    lookupLE' k me = (,) k <$> (me !? k) <|> lookupLEWith cmpfst (k, unreachEx "lookupLE'") (assocs me)
    
    {- |
      @lookupGE' k map@ finds pair @(key, value)@ with  @key@, where
      @key >= k@ (if any).
    -}
    lookupGE' :: (Ord key) => key -> map -> Maybe (key, e)
    lookupGE' k me = (,) k <$> (me !? k) <|> lookupGEWith cmpfst (k, unreachEx "lookupGE'") (assocs me)
    
    -- | Keys of map elements.
    default keys :: (Bordered map key) => map -> [key]
    keys :: map -> [key]
    keys =  indices
    
    -- | Searches the index of first matching element.
    (.$) :: (e -> Bool) -> map -> Maybe key
    (.$) =  null ?- head ... (*$)
    
    -- | Searches the indices of all matching elements.
    (*$) :: (e -> Bool) -> map -> [key]
    (*$) f = select (f . snd ?+ fst) . assocs
    
    {- Folds with index. -}
    
    -- | 'kfoldr' is right fold with key.
    kfoldr :: (key -> e -> b -> b) -> b -> map -> b
    
    -- | 'kfoldl' is left  fold with key.
    kfoldl :: (key -> b -> e -> b) -> b -> map -> b
    
    -- | 'kfoldr'' is strict version of 'kfoldr'.
    kfoldr' :: (key -> e -> b -> b) -> b -> map -> b
    kfoldr' f = kfoldr (\ !i e !b -> f i e b)
    
    -- | 'kfoldl'' is strict version of 'kfoldl'.
    kfoldl' :: (key -> b -> e -> b) -> b -> map -> b
    kfoldl' f = kfoldl (\ !i !b e -> f i b e)

--------------------------------------------------------------------------------

-- | Kind @(* -> *)@ 'Map' structure.
type Map1 map key e = Map (map e) key e

-- | Kind @(* -> * -> *)@ 'Map' structure.
type Map2 map key e = Map (map key e) key e

--------------------------------------------------------------------------------

instance Map [e] Int e
  where
    toMap' e = snds . fill . setWith cmpfst
      where
        fill (ix@(i1, _) : iy@(i2, _) : ies) =
          let rest = i1 + 1 == i2 ? iy : ies $ (i1 + 1, e) : iy : ies
          in  ix : fill rest
        fill xs = xs
    
    assocs = zip [0 ..] . listL
    
    insert' k e es = k < 0 ? es $ go k es
      where
        go 0    xs    = isNull xs ? [e] $ e : tail xs
        go i    []    = err : go (i - 1) []
        go i (x : xs) = x : go (i - 1) xs
        
        err = undEx "insert'"
    
    (x : xs) .! n = n == 0 ? x $ xs .! (n - 1)
    _        .! _ = error "in SDP.Map.(.!)"
    
    (!) [] _ = empEx "(!)"
    (!) es n = n >= 0 ? es !# n $ underEx "(!)"
      where
        []       !# _  = overEx "(!)"
        (x : xs) !# n' = n' == 0 ? x $ xs !# (n' - 1)
    
    []       !? _ = Nothing
    (x : xs) !? n = case n <=> 0 of
      GT -> xs !? (n - 1)
      EQ -> Just x
      LT -> Nothing
    
    xs // es = snds $ unionWith cmpfst (setWith cmpfst es) (assocs xs)
    
    (.$) = findIndex
    (*$) = findIndices
    
    kfoldr f base =
      let go i es = case es of {(x : xs) -> f i x $ go (i + 1) xs; _ -> base}
      in  go 0
    
    kfoldl f =
      let go i e es = case es of {(x : xs) -> go (i + 1) (f i e x) xs; _ -> e}
      in  go 0

--------------------------------------------------------------------------------

empEx :: String -> a
empEx =  throw . EmptyRange . showString "in SDP.Map."

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.Map."

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Map."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Map."

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Map."




