{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

{- |
    Module      :  SDP.Unrolled.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.Unrolled.ST@ provides 'STUnrolled' - mutable boxed lazy unrolled linked
    list.
-}

module SDP.Unrolled.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STUnrolled
  STUnrolled (..), STArray#, fromPseudoMutableArray#
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.SortM

import GHC.Base ( Int (..) )
import GHC.ST   ( ST  (..) )

import SDP.Unrolled.STUnlist
import SDP.SortM.Tim

default ()

--------------------------------------------------------------------------------

-- | STUnlist is mutable version Unlist.
data STUnrolled s i e = STUnrolled !i !i (STUnlist s e)

type role STUnrolled nominal nominal representational

--------------------------------------------------------------------------------

{- Eq instance. -}

instance (Index i) => Eq (STUnrolled s i e)
  where
    (STUnrolled l1 u1 xs) == (STUnrolled l2 u2 ys) =
      isEmpty (l1, u1) && isEmpty (l2, u2) || xs == ys

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
    
    getLeft  (STUnrolled _ _ unr#) = getLeft unr#
    copied   (STUnrolled l u unr#) = STUnrolled l u <$> copied unr#
    copied'  (STUnrolled l u unr#) = \ l' u' -> STUnrolled l u <$> copied' unr# l' u'
    reversed (STUnrolled l u unr#) = STUnrolled l u <$> reversed unr#

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Index i) => IndexedM (ST s) (STUnrolled s i e) i e
  where
    fromAssocs' (l, u) defvalue ascs = STUnrolled l u <$> fromAssocs' bnds defvalue ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    {-# INLINE (!#>) #-}
    (STUnrolled _ _ es) !#> i = es !#> i
    
    {-# INLINE (>!) #-}
    (STUnrolled l u es) >! i = es !#> offset (l, u) i
    
    {-# INLINE writeM_ #-}
    writeM_ (STUnrolled _ _ es) = writeM es
    
    {-# INLINE writeM #-}
    writeM  (STUnrolled l u es) = writeM es . offset (l, u)
    
    overwrite es [] = return es
    overwrite (STUnrolled l u unr#) ascs = if isEmpty (l, u)
        then fromAssocs (l', u') ascs
        else STUnrolled l u <$> overwrite unr# ies
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
    ifoldrM f e (STUnrolled l u es) = ifoldrM (f . index (l, u)) e es
    ifoldlM f e (STUnrolled l u es) = ifoldlM (f . index (l, u)) e es
    
    i_foldrM f e (STUnrolled _ _ es) = i_foldrM f e es
    i_foldlM f e (STUnrolled _ _ es) = i_foldlM f e es

instance (Index i) => SortM (ST s) (STUnrolled s i e) e
  where
    sortMBy = timSortBy




