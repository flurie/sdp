{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, RoleAnnotations #-}

{- |
    Module      :  SDP.STUblist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    This module provides service type STByteList - mutable version of
    SDP.ByteList.
-}

module SDP.ByteList.ST
(
  module SDP.IndexedM,
  
  STByteList (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Unboxed
import SDP.Linear

import GHC.Base ( Int (..) )
import GHC.ST   ( ST  (..) )

import SDP.ByteList.STUblist
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | STByteList is mutable version of ByteList.
data STByteList s i e = STByteList !i !i (STUblist s e)

type role STByteList nominal nominal representational

--------------------------------------------------------------------------------

{- Eq instance. -}

instance (Index i, Unboxed e) => Eq (STByteList s i e)
  where
    (STByteList l1 u1 xs) == (STByteList l2 u2 ys) = e1 && e2 || xs == ys
      where
        e1 = isEmpty (l1, u1)
        e2 = isEmpty (l2, u2)

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance (Index i, Unboxed e) => BorderedM (ST s) (STByteList s i e) i e
  where
    getLower   (STByteList l _ _)   = return l
    getUpper   (STByteList _ u _)   = return u
    getSizeOf  (STByteList l u _)   = return $ size    (l, u)
    getIndices (STByteList l u _)   = return $ range   (l, u)
    getIndexOf (STByteList l u _) i = return $ inRange (l, u) i

instance (Index i, Unboxed e) => LinearM (ST s) (STByteList s i e) e
  where
    newLinear = fromFoldableM
    
    fromFoldableM es = STByteList l u <$> fromFoldableM es
      where
        (l, u) = unsafeBounds (length es)
    
    getLeft (STByteList l u es) = take (size (l, u)) <$> getLeft es
    
    copied  (STByteList l u es)       = STByteList l u <$> copied  es
    copied' (STByteList l u es) l' u' = STByteList l u <$> copied' es l' u'
    
    {-# INLINE reversed #-}
    reversed es = liftA2 zip (getIndices es) (getRight es) >>= overwrite es
    
    {-# INLINE filled #-}
    filled n e = STByteList l u <$> filled n e where (l, u) = unsafeBounds n

--------------------------------------------------------------------------------

{- IndexedM instance. -}

instance (Index i, Unboxed e) => IndexedM (ST s) (STByteList s i e) i e
  where
    {-# INLINE fromAssocs' #-}
    fromAssocs' bnds defvalue ascs = size bnds `filled` defvalue >>= (`overwrite` ascs)
    
    (STByteList _ _ es) !#> i = es !#> i
    (STByteList l u es) >!  i = es >! offset  (l, u) i
    (STByteList l u es) !>  i = case inBounds (l, u) i of
        ER -> throw $ EmptyRange     msg
        UR -> throw $ IndexUnderflow msg
        IN -> es >! offset (l, u) i
        OR -> throw $ IndexOverflow  msg
      where
        msg = "in SDP.ByteList.ST.(!>)"
    
    writeM_ (STByteList _ _ es) i = writeM es i
    writeM  (STByteList l u es) i = writeM es $ offset (l, u) i
    
    overwrite es [] = return es
    overwrite (STByteList l u es) ascs
      | isEmpty (l, u) = fromAssocs (l', u') ascs
      | True = STByteList l u <$> overwrite es ies
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        l'  = fst $ minimumBy cmpfst ascs
        u'  = fst $ maximumBy cmpfst ascs




