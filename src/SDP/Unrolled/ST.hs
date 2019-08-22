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
  
  STUnrolled (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM

import GHC.Base ( Int (..) )

import GHC.ST ( ST (..) )

import SDP.Unrolled.STUnlist
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
    getSizeOf (STUnrolled l u _) = return $ size (l, u)

instance (Index i) => LinearM (ST s) (STUnrolled s i e) e
  where
    newLinear = fromFoldableM
    
    fromFoldableM es = STUnrolled l u <$> fromFoldableM es
      where
        (l, u) = unsafeBounds $ length es
    
    getLeft (STUnrolled l u es) = take (size (l, u)) <$> getLeft es
    
    copied  (STUnrolled l u es)       = STUnrolled l u <$> copied  es
    copied' (STUnrolled l u es) l' u' = STUnrolled l u <$> copied' es l' u'
    
    reversed es = liftA2 zip (getIndices es) (getRight es) >>= overwrite es
    filled n e  = STUnrolled l u <$> filled n e where (l, u) = unsafeBounds n

--------------------------------------------------------------------------------

{- IndexedM instance. -}

instance (Index i) => IndexedM (ST s) (STUnrolled s i e) i e
  where
    {-# INLINE fromAssocs' #-}
    fromAssocs' bnds defvalue ascs = size bnds `filled` defvalue >>= (`overwrite` ascs)
    
    (STUnrolled _ _ es) !#> i = es !#> i
    
    (STUnrolled l u es) >! i = es >! offset (l, u) i
    (STUnrolled l u es) !> i = case inBounds (l, u) i of
        ER -> throw $ EmptyRange     msg
        UR -> throw $ IndexUnderflow msg
        IN -> es >! offset (l, u) i
        OR -> throw $ IndexOverflow  msg
      where
        msg = "in SDP.Unrolled.ST.(!>)"
    
    writeM_ (STUnrolled _ _ es) i = writeM es i
    
    writeM  (STUnrolled l u es) i = writeM es $ offset (l, u) i
    
    overwrite es [] = return es
    overwrite (STUnrolled l u es) ascs = if isEmpty (l, u)
        then fromAssocs (l', u') ascs
        else STUnrolled l u <$> overwrite es ies
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        l'  = fst $ minimumBy cmpfst ascs
        u'  = fst $ maximumBy cmpfst ascs
    
    fromIndexedM es = do
      es' <- fromIndexedM es
      n   <- getSizeOf es'
      return $ let (l, u) = unsafeBounds n in STUnrolled l u es'



