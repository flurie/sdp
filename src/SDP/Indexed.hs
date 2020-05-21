{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE BangPatterns, ConstraintKinds #-}

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
  
  -- * Indexed
  Indexed (..), Indexed1, Indexed2,
  
  -- * IFold
  IFold (..), IFold1, IFold2,
  
  -- * Related functions
  binaryContain, (>/>)
)
where

import SDP.SafePrelude
import Prelude ()

import Data.List ( findIndex, findIndices )

import SDP.Linear
import SDP.Map

import SDP.Internal

default ()

infixl 9 !^, .!, !, !?

--------------------------------------------------------------------------------

-- | Class of indexed data structures.
class (Linear v e, Bordered v i) => Indexed v i e
  where
    {-# MINIMAL assoc', fromIndexed, (//), ((.!)|(!?)) #-}
    
    -- | list of associations (index, element).
    assocs :: v -> [(i, e)]
    assocs es = indices es `zip` listL es
    
    {- Global operations. -}
    
    {- |
      @assoc bnds ascs@ create new structure from list of associations, without
      default element. Note that @bnds@ is @ascs@ bounds and may not match with
      the result bounds (not always possible).
    -}
    assoc :: (i, i) -> [(i, e)] -> v
    assoc =  (`assoc'` undEx "assoc {default}")
    
    {- |
      @assoc' bnds defvalue ascs@ creates new structure from list of
      associations with default element. Note that @bnds@ is @ascs@ bounds and
      may not match with the result bounds (not always possible).
    -}
    assoc' :: (i, i) -> e -> [(i, e)] -> v
    
    -- | 'assocMap' is 'assoc' where @ascs@ represented by 'Map'.
    assocMap :: (Map map i e) => (i, i) -> map -> v
    assocMap =  (`assocMap'` undEx "assocMap {default}")
    
    -- | 'assocMap'' is 'assoc'' where @ascs@ represented by 'Map'.
    assocMap' :: (Map map i e) => (i, i) -> e -> map -> v
    assocMap' bnds defvalue = assoc' bnds defvalue . listMap
    
    -- | 'fromIndexed' converts this indexed structure to another one.
    fromIndexed :: (Indexed v' j e) => v' -> v
    
    -- | 'imap' creates new indexed structure from old with reshaping function.
    imap :: (Indexed v' j e) => (i, i) -> v' -> (i -> j) -> v
    imap bnds es f = assoc bnds [ (i, es ! f i) | i <- range bnds ]
    
    {- |
      @'accum' f es ies@ create a new structure from @es@ elements selectively
      updated by function @f@ and @ies@ associations list.
    -}
    accum :: (e -> e' -> e) -> v -> [(i, e')] -> v
    accum f es ies = bounds es `assoc` [ (i, es ! i `f` e') | (i, e') <- ies ]
    
    -- | Update elements of immutable structure (by copying).
    (//) :: v -> [(i, e)] -> v
    
    -- | Update function, by default uses ('!') and may throw 'IndexException'.
    update :: v -> [i] -> (i -> e -> e) -> v
    update es is f = es // [ (i, f i (es ! i)) | i <- is ]
    
    -- | Create new structure from old by indexed mapping.
    (/>) :: v -> (i -> e -> e) -> v
    (/>) es f = assoc (bounds es) [ (i, f i (es ! i)) | i <- indices es ]
    
    {- Elementwise operations. -}
    
    -- | (!^) is completely unsafe reader. Must work as fast, as possible.
    (!^) :: v -> Int -> e
    (!^) es = (es .!) . indexOf es
    
    -- | (.!) is unsafe reader, but on bit faster of ('!').
    {-# INLINE (.!) #-}
    (.!) :: v -> i -> e
    (.!) =  fromMaybe (undEx "(.!)") ... (!?)
    
    -- | (!) is well-safe reader. Must 'throw' 'IndexException'.
    (!) :: v -> i -> e
    (!) es i = case inBounds (bounds es) i of
        IN -> es .! i
        ER -> throw $ EmptyRange     msg
        OR -> throw $ IndexOverflow  msg
        UR -> throw $ IndexUnderflow msg
      where
        msg = "in SDP.Indexed.(!) {default}"
    
    -- | (!?) is completely safe, but very boring function.
    (!?) :: v -> i -> Maybe e
    (!?) es = not . indexIn es ?- (es .!)
    
    -- |  Write one element to structure.
    write_ :: v -> Int -> e -> v
    write_ es i e = es // [ (indexOf es i, e) ]
    
    -- |  Write one element to structure.
    write :: v -> i -> e -> v
    write es i e = es // [(i, e)]
    
    -- | Searches the index of first matching element.
    (.$) :: (e -> Bool) -> v -> Maybe i
    (.$) =  null ?- head ... (*$)
    
    -- | Searches the indices of all matching elements.
    (*$) :: (e -> Bool) -> v -> [i]
    (*$) f = select (f . snd ?+ fst) . assocs

--------------------------------------------------------------------------------

{- |
  IFold class for folds with index. The main reason for creating this class is
  the Foldable extension to containers with a restriction on the type of
  elements - monomorphic, Storable, Unboxed, etc.
-}
class IFold v i e | v -> i, v -> e
  where
    {-# MINIMAL ifoldr, ifoldl #-}
    
    -- | 'ifoldr' is right fold with index
    ifoldr   :: (i -> e -> r -> r) -> r -> v -> r
    
    -- | 'ifoldl' is left  fold with index
    ifoldl   :: (i -> r -> e -> r) -> r -> v -> r
    
    -- | 'ifoldr'' is strict version of 'ifoldr'
    ifoldr'  :: (i -> e -> r -> r) -> r -> v -> r
    
    -- | 'ifoldl'' is strict version of 'ifoldl'
    ifoldl'  :: (i -> r -> e -> r) -> r -> v -> r
    
    -- | 'i_foldr' is just 'foldr' in 'IFold' context
    i_foldr  :: (e -> r -> r) -> r -> v -> r
    
    -- | 'i_foldl' is just 'foldl' in 'IFold' context
    i_foldl  :: (r -> e -> r) -> r -> v -> r
    
    -- | 'i_foldr'' is just 'foldl'' with 'IFold' context
    i_foldr' :: (e -> r -> r) -> r -> v -> r
    
    -- | 'i_foldl'' is just 'foldl'' with 'IFold' context
    i_foldl' :: (r -> e -> r) -> r -> v -> r
    
    ifoldr'  f = ifoldr (\ !i e !r -> f i e r)
    ifoldl'  f = ifoldl (\ !i !r e -> f i r e)
    
    i_foldr  = ifoldr  . const
    i_foldl  = ifoldl  . const
    i_foldr' = ifoldr' . const
    i_foldl' = ifoldl' . const

--------------------------------------------------------------------------------

instance Indexed [e] Int e
  where
    assocs es = [0 ..] `zip` listL es
    
    assoc' bnds e = take (size bnds) . snds . fill . setWith cmpfst . filter (inRange bnds . fst)
      where
        fill (ie1@(i1, _) : ie2@(i2, _) : xs) = ie1 : fill rest
          where
            rest = nx /= i2 ? (nx, e) : ie2 : xs $ ie2 : xs
            nx   = next bnds i1
        fill xs  = xs
    
    (!^) = (.!)
    
    (x : xs) .! n = n == 0 ? x $ xs .! (n - 1)
    _        .! _ = error "in SDP.Indexed.(.!)"
    
    (!) [] _ = throw $ EmptyRange "in SDP.Indexed.(!)"
    (!) es n = n >= 0 ? es !# n $ throw $ IndexUnderflow "in SDP.Indexed.(!)"
      where
        []       !# _  = throw $ IndexOverflow "in SDP.Indexed.(!)"
        (x : xs) !# n' = n' == 0 ? x $ xs !# (n' - 1)
    
    (x : xs) !? n = case n <=> 0 of {LT -> Nothing; EQ -> Just x; GT -> xs !? (n - 1)}
    _        !? _ = Nothing
    
    fromIndexed es = (es !) <$> indices es
    
    xs // es = snds $ unionWith cmpfst (assocs xs) (setWith cmpfst es)
    
    (.$) = findIndex
    (*$) = findIndices

instance IFold [e] Int e
  where
    ifoldr f base =
      let go i es = case es of {(x : xs) -> f i x $ go (i + 1) xs; _ -> base}
      in  go 0
    
    ifoldl f =
      let go i e es = case es of {(x : xs) -> go (i + 1) (f i e x) xs; _ -> e}
      in  go 0

--------------------------------------------------------------------------------

-- | Rank (* -> *) 'Indexed' structure.
type Indexed1 v i e = Indexed (v e) i e

-- | Rank (* -> * -> *) 'Indexed' structure.
type Indexed2 v i e = Indexed (v i e) i e

-- | Rank (* -> *) 'IFold' structure.
type IFold1 v i e = IFold (v e) i e

-- | Rank (* -> * -> *) 'IFold' structure.
type IFold2 v i e = IFold (v i e) i e

--------------------------------------------------------------------------------

-- | binaryContain checks that sorted structure has equal element.
binaryContain :: (Indexed v i e) => Compare e -> e -> v -> Bool
binaryContain f e es = and [ s /= 0, f e (es !^ 0) /= LT, f e (es !^ u') /= GT, contain 0 u' ]
  where
    contain l u = l > u ? False $ case f e (es !^ j) of
        LT -> contain l (j - 1)
        EQ -> True
        GT -> contain (j + 1) u
      where
        j = l + (u - l `div` 2)
    s  = sizeOf es
    u' = s - 1

-- | Update some elements in structure (without indices).
{-# DEPRECATED (>/>) "use (/>) or update instead" #-}
(>/>) :: (Indexed v i e) => v -> [i] -> (e -> e) -> v
es >/> is = update es is . const

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.Indexed."


