{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

{- |
    Module      :  SDP.Indexed
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
  
  Indexed is one of the main classes of SDP, designed to read and write immutable
  indexable data structures.
-}

module SDP.Indexed
(
  module SDP.Linear,
  
  Indexed (..),
  
  (>/>)
)
where

import SDP.SafePrelude
import Prelude ()

import Data.List ( findIndex, findIndices )

import SDP.Linear
import SDP.Set

import SDP.Simple

default ()

infixl 9 !^, .!, !, !?

--------------------------------------------------------------------------------

-- | Class of indexed data structures.
class (Linear v e, Index i) => Indexed v i e | v -> i, v -> e
  where
    {-# MINIMAL assoc', (//), ((!)|(!?)), (*$) #-}
    
    {- |
      assoc creates new structure from list of associations [(index, element)],
      where default element is IndexOverflow Exception.
    -}
    assoc :: (i, i) -> [(i, e)] -> v
    assoc bnds =  assoc' bnds err
      where
        err = throw $ UndefinedValue "in SDP.Indexed.assoc (default)"
    
    -- | assoc' is safe version of assoc.
    assoc' :: (i, i) -> e -> [(i, e)] -> v
    
    (!^) :: v -> Int -> e
    
    {- |
      (.!) is unsafe, but on bit faster version of (!).
      Use (.!) only if you are really sure that you will not go beyond the bounds.
    -}
    {-# INLINE (.!) #-}
    (.!) :: v -> i -> e
    dat .! ix = dat ! ix
    
    -- | (!) is pretty safe function, that returns elements by index. Must throw IndexException.
    {-# INLINE (!) #-}
    (!)  :: v -> i -> e
    (!) dat ix = fromMaybe err $ dat !? ix
      where
        err = throw . UndefinedValue $ "in SDP.Indexed.(!)"
    
    -- | (!?) is completely safe, but so boring function.
    (!?) :: v -> i -> Maybe e
    
    -- |  Write one element to structure.
    default write_ :: (Bordered v i e) => v -> Int -> e -> v
    write_ :: v -> Int -> e -> v
    write_ es i e = es // [(index (bounds es) i, e)]
    
    -- |  Write one element to structure.
    write :: v -> i -> e -> v
    write es i e = es // [(i, e)]
    
    -- | Writes elements to (immutable) structure.
    (//) :: v -> [(i, e)] -> v
    
    -- | Update function. Uses (!) and may throw IndexException.
    (/>) :: v -> [i] -> (i -> e -> e) -> v
    (/>) es is f = es // [ (i, f i (es ! i)) | i <- is ]
    
    -- | Searches the index of first matching element.
    (.$) :: (e -> Bool) -> v -> Maybe i
    (.$) f es = null ?: head $ f *$ es
    
    -- | Searches the indices of all matching elements.
    (*$) :: (e -> Bool) -> v -> [i]
    
    default (*$) :: (Bordered v i e) => (e -> Bool) -> v -> [i]
    f *$ es      =  fsts . filter (f . snd) $ assocs es
    
    default (!^) :: (Bordered v i e) => v -> Int -> e
    es !^ i = es .! index (bounds es) i
    
    default (!?) :: (Bordered v i e) => v -> i -> Maybe e
    {-# INLINE (!?) #-}
    (!?) dat     = \ i -> (not . indexOf dat) ?: (dat !) $ i

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
    
    xs !^ i = xs .! i
    
    []       .! _ = error "in SDP.Indexed.(.!)"
    (x : xs) .! n = n == 0 ? x $ xs .! (n - 1)
    
    [] ! _ = throw $ EmptyRange "in SDP.Indexed.(!)"
    es ! n = n >= 0 ? es !# n $ throw $ IndexUnderflow "in SDP.Indexed.(!)"
      where
        []       !# _  = throw $ IndexOverflow "in SDP.Indexed.(!)"
        (x : xs) !# n' = n' == 0 ? x $ xs !# (n' - 1)
    
    {-# INLINE (!?) #-}
    []       !? _ = Nothing
    (x : xs) !? n = case n <=> 0 of {LT -> Nothing; EQ -> Just x; GT -> xs !? (n - 1)}
    
    {-# INLINE (//) #-}
    xs // es = snds $ unionWith cmpfst (assocs xs) (setWith cmpfst es)
    
    {-# INLINE (.$) #-}
    p .$ es = findIndex p es
    
    {-# INLINE (*$) #-}
    p *$ es = findIndices p es

--------------------------------------------------------------------------------

-- | Update one element in structure.
{-# INLINE (>/>) #-}
(>/>) :: (Indexed v i e) => v -> [i] -> (e -> e) -> v
(>/>) es is = (es /> is) . const

