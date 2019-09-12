{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, RoleAnnotations #-}

{- |
    Module      :  SDP.Unrolled.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    This module provides service type STUnrolled - mutable version of
    SDP.Unrolled.
-}

module SDP.Unrolled.ST
(
  module SDP.IndexedM,
  module SDP.SortM,
  
  STUnrolled (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.SortM

import GHC.Base ( Int (..) )
import GHC.ST   ( ST )

import SDP.Unrolled.STUnlist
import SDP.SortM.Tim
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | STUnlist is mutable version Unlist.
data STUnrolled s i e = STUnrolled !i !i (STUnlist s e)

type role STUnrolled nominal nominal representational

--------------------------------------------------------------------------------

{- Eq instance. -}

instance (Index i, Eq e) => Eq (STUnrolled s i e)
  where
    (STUnrolled l1 u1 xs) == (STUnrolled l2 u2 ys) = e1 && e2 || xs == ys
      where
        e1 = isEmpty (l1, u1)
        e2 = isEmpty (l2, u2)

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance (Index i) => BorderedM (ST s) (STUnrolled s i e) i e
  where
    getLower  (STUnrolled l _ _) = return l
    getUpper  (STUnrolled _ u _) = return u
    getBounds (STUnrolled l u _) = return (l, u)
    getSizeOf (STUnrolled l u _) = return $ size (l, u)

instance (Index i) => LinearM (ST s) (STUnrolled s i e) e
  where
    newLinear es = STUnrolled l u <$> newLinear es
      where
        (l, u) = defaultBounds $ sizeOf es
    
    filled n e = STUnrolled l u <$> filled n e where (l, u) = defaultBounds n
    
    getLeft  (STUnrolled l u es) = take (size (l, u)) <$> getLeft es
    copied   (STUnrolled l u es) = STUnrolled l u <$> copied  es
    copied'  (STUnrolled l u es) = \ l' u' -> STUnrolled l u <$> copied' es l' u'
    reversed (STUnrolled l u es) = STUnrolled l u <$> reversed es

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Index i) => IndexedM (ST s) (STUnrolled s i e) i e
  where
    {-# INLINE fromAssocs' #-}
    fromAssocs' (l, u) defvalue ascs = STUnrolled l u <$> fromAssocs' bnds defvalue ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    (STUnrolled _ _ es) !#> i = es !#> i
    
    (STUnrolled l u es) >! i = es >! offset (l, u) i
    (STUnrolled l u es) !> i = case inBounds (l, u) i of
        ER -> throw $ EmptyRange     msg
        UR -> throw $ IndexUnderflow msg
        IN -> es >! offset (l, u) i
        OR -> throw $ IndexOverflow  msg
      where
        msg = "in SDP.Unrolled.ST.(!>)"
    
    writeM_ (STUnrolled _ _ es) = writeM es
    writeM  (STUnrolled l u es) = writeM es . offset (l, u)
    
    overwrite es [] = return es
    overwrite (STUnrolled l u es) ascs = if isEmpty (l, u)
        then fromAssocs (l', u') ascs
        else STUnrolled l u <$> overwrite es ies
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        l'  = fst $ minimumBy cmpfst ascs
        u'  = fst $ maximumBy cmpfst ascs
    
    fromIndexed' es = STUnrolled l u <$> fromIndexed' es
      where
        (l, u) = defaultBounds $ sizeOf es
    
    fromIndexedM es = do
      es' <- fromIndexedM es
      n   <- getSizeOf es'
      return $ let (l, u) = defaultBounds n in STUnrolled l u es'

instance (Index i) => IFoldM (ST s) (STUnrolled s i e) i e
  where
    ifoldrM f e (STUnrolled l u es) = ifoldrM (\ i -> f (index (l, u) i)) e es
    ifoldlM f e (STUnrolled l u es) = ifoldlM (\ i -> f (index (l, u) i)) e es
    
    i_foldrM f e (STUnrolled _ _ es) = i_foldrM f e es
    i_foldlM f e (STUnrolled _ _ es) = i_foldlM f e es

instance (Index i) => SortM (ST s) (STUnrolled s i e) e where sortMBy = timSortBy


