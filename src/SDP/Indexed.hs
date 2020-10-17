{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE BangPatterns, DefaultSignatures, ConstraintKinds #-}

{- |
    Module      :  SDP.Indexed
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
  
  @SDP.Indexed@ provides 'Indexed' - class of indexed data structures.
-}
module SDP.Indexed
(
  -- * Exports
  module SDP.Linear,
  module SDP.Map,
  
  -- * Indexed
  Indexed (..), Indexed1, Indexed2,
  
  -- * IFold
  IFold (..), IFold1, IFold2,
  
  -- * Related functions
  binaryContain
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Internal
import SDP.Linear
import SDP.Map

default ()

--------------------------------------------------------------------------------

-- | Class of indexed data structures.
class (Linear v e, Bordered v i, Map v i e) => Indexed v i e | v -> i, v -> e
  where
    {-# MINIMAL fromIndexed #-}
    
    {- |
      @assoc bnds ascs@ create new structure from list of associations, without
      default element. Note that @bnds@ is @ascs@ bounds and may not match with
      the result bounds (not always possible).
    -}
    assoc :: (i, i) -> [(i, e)] -> v
    assoc =  flip assoc' (undEx "assoc {default}")
    
    {- |
      @assoc' bnds def ascs@ creates new structure from list of associations
      with default element. Note that @bnds@ is @ascs@ bounds and may not match
      with the result bounds (not always possible).
    -}
    assoc' :: (i, i) -> e -> [(i, e)] -> v
    assoc' bnds defvalue = toMap' defvalue . filter (inRange bnds . fst)
    
    -- | 'fromIndexed' converts this indexed structure to another one.
    fromIndexed :: (Indexed m j e) => m -> v
    
    {- |
      @'accum' f es ies@ create a new structure from @es@ elements selectively
      updated by function @f@ and @ies@ associations list.
    -}
    accum :: (e -> e' -> e) -> v -> [(i, e')] -> v
    accum f es ies = bounds es `assoc` [ (i, es ! i `f` e') | (i, e') <- ies ]
    
    -- | 'imap' creates new indexed structure from old with reshaping.
    imap :: (Map m j e) => (i, i) -> m -> (i -> j) -> v
    imap bnds es f = assoc bnds [ (i, es ! f i) | i <- range bnds ]
    
    -- | Create new structure from old by indexed mapping.
    (/>) :: v -> (i -> e -> e) -> v
    (/>) es f = assoc (bounds es) [ (i, f i (es ! i)) | i <- indices es ]

--------------------------------------------------------------------------------

{- |
  IFold class for folds with index. The main reason for creating this class is
  the Foldable extension to containers with a restriction on the type of
  elements - monomorphic, Storable, Unboxed, etc.
-}
class (Index i) => IFold v i e | v -> i, v -> e
  where
    {-# MINIMAL (ifoldr | ofoldr), (ifoldl | ofoldr) #-}
    
    {- Folds with index. -}
    
    -- | 'ifoldr' is right fold with index.
    default ifoldr :: (Bordered v i) => (i -> e -> r -> r) -> r -> v -> r
    ifoldr :: (i -> e -> r -> r) -> r -> v -> r
    ifoldr f base es = let bnds = bounds es in ofoldr (f . index bnds) base es
    
    -- | 'ifoldl' is left  fold with index.
    default ifoldl :: (Bordered v i) => (i -> r -> e -> r) -> r -> v -> r
    ifoldl :: (i -> r -> e -> r) -> r -> v -> r
    ifoldl f base es = let bnds = bounds es in ofoldl (f . index bnds) base es
    
    -- | 'ifoldr'' is strict version of 'ifoldr'.
    ifoldr' :: (i -> e -> r -> r) -> r -> v -> r
    ifoldr' f = ifoldr (\ !i e !r -> f i e r)
    
    -- | 'ifoldl'' is strict version of 'ifoldl'.
    ifoldl' :: (i -> r -> e -> r) -> r -> v -> r
    ifoldl' f = ifoldl (\ !i !r e -> f i r e)
    
    {- Folds with offset. -}
    
    -- | 'ofoldr' is right fold with offset.
    ofoldr :: (Int -> e -> r -> r) -> r -> v -> r
    default ofoldr :: (Bordered v i) => (Int -> e -> r -> r) -> r -> v -> r
    ofoldr f base es = ifoldr (f . offsetOf es) base es
    
    -- | 'ofoldl' is left fold with offset.
    default ofoldl :: (Bordered v i) => (Int -> r -> e -> r) -> r -> v -> r
    ofoldl :: (Int -> r -> e -> r) -> r -> v -> r
    ofoldl f base es = ifoldl (f . offsetOf es) base es
    
    -- | 'ofoldr'' is strict version of 'ofoldr'.
    default ofoldr' :: (Bordered v i) => (Int -> e -> r -> r) -> r -> v -> r
    ofoldr' :: (Int -> e -> r -> r) -> r -> v -> r
    ofoldr' f base es = ifoldr' (f . offsetOf es) base es
    
    -- | 'ofoldl'' is strict version of 'ofoldl'.
    default ofoldl' :: (Bordered v i) => (Int -> r -> e -> r) -> r -> v -> r
    ofoldl' :: (Int -> r -> e -> r) -> r -> v -> r
    ofoldl' f base es = ifoldl' (f . offsetOf es) base es
    
    {- 'Foldable' crutches. -}
    
    -- | 'i_foldr' is just 'foldr' in 'IFold' context.
    i_foldr :: (e -> r -> r) -> r -> v -> r
    i_foldr =  ifoldr  . const
    
    -- | 'i_foldl' is just 'foldl' in 'IFold' context.
    i_foldl :: (r -> e -> r) -> r -> v -> r
    i_foldl =  ifoldl  . const
    
    -- | 'i_foldr'' is just 'foldr'' in 'IFold' context.
    i_foldr' :: (e -> r -> r) -> r -> v -> r
    i_foldr' =  ifoldr' . const
    
    -- | 'i_foldl'' is just 'foldl'' in 'IFold' context.
    i_foldl' :: (r -> e -> r) -> r -> v -> r
    i_foldl' =  ifoldl' . const

--------------------------------------------------------------------------------

instance Indexed [e] Int e
  where
    assoc' bnds e = toMap' e . filter (inRange bnds . fst)
    
    fromIndexed es = (es !) <$> indices es

instance IFold [e] Int e
  where
    ifoldr f base =
      let go i es = case es of {(x : xs) -> f i x $ go (i + 1) xs; _ -> base}
      in  go 0
    
    ifoldl f =
      let go i e es = case es of {(x : xs) -> go (i + 1) (f i e x) xs; _ -> e}
      in  go 0

--------------------------------------------------------------------------------

-- | Kind (* -> *) 'Indexed' structure.
type Indexed1 v i e = Indexed (v e) i e

-- | Kind (* -> * -> *) 'Indexed' structure.
type Indexed2 v i e = Indexed (v i e) i e

-- | Kind (* -> *) 'IFold' structure.
type IFold1 v i e = IFold (v e) i e

-- | Kind (* -> * -> *) 'IFold' structure.
type IFold2 v i e = IFold (v i e) i e

--------------------------------------------------------------------------------

-- | binaryContain checks that sorted structure has equal element.
binaryContain :: (Linear v e, Bordered v i) => Compare e -> e -> v -> Bool
binaryContain _ _ Z  = False
binaryContain f e es =
  let
    contain l u = not (l > u) && case f e (es !^ j) of
        LT -> contain l (j - 1)
        EQ -> True
        GT -> contain (j + 1) u
      where
        j = u - l `div` 2 + l
  in  f e (head es) /= LT && f e (last es) /= GT && contain 0 (sizeOf es - 1)

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.Indexed."


