{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

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
  Indexed (..),
  
  -- * IFold
  IFold (..),
  
  -- * Related functions
  binaryContain, (>/>)
)
where

import SDP.SafePrelude
import Prelude ()

import Data.List ( findIndex, findIndices )

import SDP.Linear
import SDP.Map

import SDP.Internal.Commons

default ()

infixl 9 !^, .!, !, !?

--------------------------------------------------------------------------------

-- | Class of indexed data structures.
class (Index i) => Indexed v i e | v -> i, v -> e
  where
    {-# MINIMAL assoc', fromIndexed, (//), ((.!)|(!?)), (*$) #-}
    
    {- Global operations. -}
    
    {- |
      assoc creates new structure from list of associations [(index, element)],
      where default element is 'IndexException' ('UndefinedValue').
    -}
    assoc :: (i, i) -> [(i, e)] -> v
    assoc bnds = assoc' bnds (undEx "assoc (default)")
    
    -- | assoc' is safe version of assoc.
    assoc' :: (i, i) -> e -> [(i, e)] -> v
    
    assocMap :: (Map map i e) => (i, i) -> map -> v
    assocMap =  (`assocMap'` undEx "assocMap (default)")
    
    assocMap' :: (Map map i e) => (i, i) -> e -> map -> v
    assocMap' bnds defvalue = assoc' bnds defvalue . listMap
    
    -- | fromIndexed converts indexed structure to another one.
    fromIndexed :: (Bordered v' j e, Indexed v' j e) => v' -> v
    
    -- | imap creates new indexed structure from old with reshaping function.
    imap :: (Indexed v' j e) => (i, i) -> v' -> (i -> j) -> v
    imap bnds es f = assoc bnds [ (i, es ! f i) | i <- range bnds ]
    
    -- | accum creates a new structure from the old and the assocs list.
    default accum :: (Bordered v i e) => (e -> e' -> e) -> v -> [(i, e')] -> v
    accum :: (e -> e' -> e) -> v -> [(i, e')] -> v
    accum f es ies = bounds es `assoc` [ (i, es ! i `f` e') | (i, e') <- ies ]
    
    -- | Writes elements to immutable structure (by copying).
    (//) :: v -> [(i, e)] -> v
    
    -- | Update function, by default uses ('!') and may throw 'IndexException'.
    update :: v -> [i] -> (i -> e -> e) -> v
    update es is f = es // [ (i, f i (es ! i)) | i <- is ]
    
    -- | Create new structure from old by indexed mapping.
    default (/>) :: (Bordered v i e) => v -> (i -> e -> e) -> v
    (/>) :: v -> (i -> e -> e) -> v
    (/>) es f = assoc (bounds es) [ (i, f i (es ! i)) | i <- indices es ]
    
    {- Elementwise operations. -}
    
    -- | (!^) is completely unsafe reader. Must work as fast, as possible.
    default (!^) :: (Bordered v i e) => v -> Int -> e
    (!^) :: v -> Int -> e
    (!^) es = (es .!) . indexOf es
    
    -- | (.!) is unsafe reader, but on bit faster of ('!').
    {-# INLINE (.!) #-}
    (.!) :: v -> i -> e
    (.!) es = fromMaybe (undEx "(.!)") . (es !?)
    
    -- | (!) is well-safe reader. Must throw 'IndexException'.
    default (!) :: (Bordered v i e) => v -> i -> e
    (!) :: v -> i -> e
    (!) es i = case inBounds (bounds es) i of
        IN -> es .! i
        ER -> throw $ EmptyRange     msg
        OR -> throw $ IndexOverflow  msg
        UR -> throw $ IndexUnderflow msg
      where
        msg = "in SDP.Indexed.(!) [default]"
    
    -- | (!?) is completely safe, but very boring function.
    default (!?) :: (Bordered v i e) => v -> i -> Maybe e
    (!?) :: v -> i -> Maybe e
    (!?) es = (not . indexIn es) ?: (es .!)
    
    -- |  Write one element to structure.
    default write_ :: (Bordered v i e) => v -> Int -> e -> v
    write_ :: v -> Int -> e -> v
    write_ es i e = es // [ (indexOf es i, e) ]
    
    -- |  Write one element to structure.
    write :: v -> i -> e -> v
    write es i e = es // [(i, e)]
    
    -- | Searches the index of first matching element.
    (.$) :: (e -> Bool) -> v -> Maybe i
    (.$) f es = null ?: head $ f *$ es
    
    -- | Searches the indices of all matching elements.
    default (*$) :: (Bordered v i e) => (e -> Bool) -> v -> [i]
    (*$) :: (e -> Bool) -> v -> [i]
    (*$) f = fsts . filter (f . snd) . assocs

--------------------------------------------------------------------------------

{- |
  IFold class for folds with index. The main reason for creating this class is
  the Foldable extension to containers with a restriction on the type of
  elements - monomorphic, Storable, Unboxed, etc.
-}
class (Indexed v i e) => IFold v i e
  where
    {-# MINIMAL ifoldr, ifoldl #-}
    
    -- | ifoldr is right fold with index
    ifoldr   :: (i -> e -> r -> r) -> r -> v -> r
    
    -- | ifoldl is left  fold with index
    ifoldl   :: (i -> r -> e -> r) -> r -> v -> r
    
    -- | ifoldr' is strict version of ifoldr
    ifoldr'  :: (i -> e -> r -> r) -> r -> v -> r
    
    -- | ifoldl' is strict version of ifoldl
    ifoldl'  :: (i -> r -> e -> r) -> r -> v -> r
    
    -- | i_foldr is just foldr with IFold context
    i_foldr  :: (e -> r -> r) -> r -> v -> r
    
    -- | i_foldl is just foldl with IFold context
    i_foldl  :: (r -> e -> r) -> r -> v -> r
    
    -- | i_foldr' is just foldl' with IFoldl context
    i_foldr' :: (e -> r -> r) -> r -> v -> r
    
    -- | i_foldl' is just foldl' with IFold context
    i_foldl' :: (r -> e -> r) -> r -> v -> r
    
    ifoldr'  f = ifoldr (\ i e r -> id $! f i e r)
    ifoldl'  f = ifoldl (\ i r e -> id $! f i r e)
    
    i_foldr  f = ifoldr  (const f)
    i_foldl  f = ifoldl  (const f)
    i_foldr' f = ifoldr' (const f)
    i_foldl' f = ifoldl' (const f)

--------------------------------------------------------------------------------

instance Indexed [e] Int e
  where
    assoc' bnds e = toResultList . normalAssocs
      where
        toResultList = fromListN (size bnds) . snds
        normalAssocs = fill . setWith cmpfst . filter (inRange bnds . fst)
        
        fill (ie1@(i1, _) : ie2@(i2, _) : xs) = ie1 : fill rest
          where
            rest = nx /= i2 ? (nx, e) : ie2 : xs $ ie2 : xs
            nx   = next bnds i1
        fill xs  = xs
    
    (!^) = (.!)
    
    []       .! _ = error "in SDP.Indexed.(.!)"
    (x : xs) .! n = n == 0 ? x $ xs .! (n - 1)
    
    [] ! _ = throw $ EmptyRange "in SDP.Indexed.(!)"
    es ! n = n >= 0 ? es !# n $ throw $ IndexUnderflow "in SDP.Indexed.(!)"
      where
        []       !# _  = throw $ IndexOverflow "in SDP.Indexed.(!)"
        (x : xs) !# n' = n' == 0 ? x $ xs !# (n' - 1)
    
    (x : xs) !? n = case n <=> 0 of {LT -> Nothing; EQ -> Just x; GT -> xs !? (n - 1)}
    []       !? _ = Nothing
    
    fromIndexed es = (es !) <$> indices es
    
    xs // es = snds $ unionWith cmpfst (assocs xs) (setWith cmpfst es)
    
    (.$) = findIndex
    (*$) = findIndices

instance IFold [e] Int e
  where
    ifoldr f base = go 0
      where
        go _    []    = base
        go i (x : xs) = f i x $ go (i + 1) xs
    
    ifoldl f = go 0
      where
        go _ e    []    = e
        go i e (x : xs) = go (i + 1) (f i e x) xs

--------------------------------------------------------------------------------

-- | binaryContain checks that sorted structure has equal element.
binaryContain :: (Bordered v i e, Indexed v i e) => Compare e -> e -> v -> Bool
binaryContain f e es = s /= 0 && f e (es !^ 0) /= LT && f e (es !^ u') /= GT && contain 0 u'
  where
    contain l u = l > u ? False $ case f e (es !^ j) of
        LT -> contain l (j - 1)
        EQ -> True
        GT -> contain (j + 1) u
      where
        j = l + (u - l `div` 2)
    s = sizeOf es; u' = s - 1

-- | Update some elements in structure (without indices).
{-# DEPRECATED (>/>) "use (/>) or update instead" #-}
(>/>) :: (Indexed v i e) => v -> [i] -> (e -> e) -> v
(>/>) es is = update es is . const

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.Indexed." ++ msg

