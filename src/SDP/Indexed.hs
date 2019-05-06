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
  
  write, (>/>)
)
where

import SDP.SafePrelude
import Prelude ()

import Data.List ( findIndex, findIndices )

import SDP.Linear
import SDP.Set

import SDP.Simple

--------------------------------------------------------------------------------

-- | Class of indexed data structures.

class (Linear v e, Index i) => Indexed v i e | v -> i, v -> e
  where
    {-# MINIMAL assoc', (//), ((!)|(!?)), (*$) #-}
    
    {- Create functions. -}
    
    {- |
      assoc creates new structure from list of associations [(index, element)],
      where default element is IndexOverflow Exception.
    -}
    assoc           :: (i, i) -> [(i, e)] -> v
    assoc bnds      =  assoc' bnds defvalue
      where
        defvalue = throw $ IndexOverflow "in SDP.Indexed.assoc (List)"
    
    -- | assoc' is safe version of assoc.
    assoc'          :: (i, i) -> e -> [(i, e)] -> v
    
    {- Read functions. -}
    
    {- |
      (.!) is unsafe, but on bit faster version of (!).
      Use (.!) only if you are really sure that you will not go beyond the bounds.
    -}
    (.!)        :: v -> i -> e
    dat .! ix   =  fromMaybe undefined $ dat !? ix
    
    {- |
      (!) is pretty safe function, that returns elements by index.
      Throws IndexException.
    -} 
    (!)          :: v -> i -> e
    (!) dat ix   =  fromMaybe err $ dat !? ix
      where
        err = throw . UndefinedValue $ "SDP.Indexed.(!)"
    
    default (!?) :: (Bordered v i e) => v -> i -> Maybe e
    
    -- | (!?) is completely safe, but so boring function.
    (!?)         :: v -> i -> Maybe e
    (!?) dat     =  (inRange $ bounds dat) ?: (dat !)
    
    {- Write and update functions -}
    
    -- | Writes elements to (immutable) structure.
    (//)         :: v -> [(i, e)] -> v
    
    -- | Update function. Uses (!) and may throw IndexException.
    (/>)         :: v -> [i] -> (i -> e -> e) -> v
    (/>) es is f = es // [ (i, f i (es ! i)) | i <- is ]
    
    {- Search functions. -}
    
    -- | Searches the index of first matching element.
    (.$) :: (e -> Bool) -> v -> Maybe i
    (.$) f es = null ?: head $ f *$ es
    
    -- | Searches the indices of all matching elements.
    (*$) :: (e -> Bool) -> v -> [i]
    
    default (*$) :: (Bordered v i e) => (e -> Bool) -> v -> [i]
    
    f *$ es = fsts . filter (f . snd) $ assocs es

-- |  Write one element to structure.
write        :: (Indexed v i e) => v -> i -> e -> v
write es i e = es // [(i, e)]

-- | Update one element in structure.
(>/>)        :: (Indexed v i e) => v -> [i] -> (e -> e) -> v
(>/>) es  is = (es /> is) . const

instance Indexed [e] Int e
  where
    assoc' bnds e = toResultList . normalAssocs
      where
        fill (ie1@(i1, _) : ie2@(i2, _) : xs) = ie1 : fill rest
          where
            rest = nx /= i2 ? (nx, e) : ie2 : xs $ ie2 : xs
            nx   = next bnds i1
        fill xs  = xs
        
        toResultList = fromListN (size bnds) . snds
        normalAssocs = fill . setWith cmpfst . filter (inRange bnds . fst)
    
    (x : xs) .! n = (n == 0) ? x $ xs .! (n - 1)
    _ .! _ = error "nice try but you still finished badly in SDP.Indexed.(.!) (List)"
    
    (!) [] n = throw $ (n < 0 ? IndexUnderflow $ IndexOverflow) "in SDP.Indexed.(!) (List)"
    (x : xs) ! n = case n <=> 0 of
      GT -> xs .! (n - 1)
      EQ -> x
      LT -> throw $ IndexUnderflow "in SDP.Indexed.(!) (List)"
    
    [] !? _ = Nothing
    (x : xs) !? n = case n <=> 0 of
      GT -> xs !? (n - 1)
      EQ -> Just x
      LT -> Nothing
    
    xs // es = snds $ unionWith cmpfst xs' es'
      where
        es' = setWith cmpfst es
        xs' = assocs xs
    
    (.$) = findIndex
    (*$) = findIndices

--------------------------------------------------------------------------------
