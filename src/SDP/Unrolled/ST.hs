{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

{- |
    Module      :  SDP.Unrolled.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Unrolled.ST@ provides 'STUnrolled' - mutable boxed lazy unrolled linked
    list.
-}

module SDP.Unrolled.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STUnrolled
  STUnrolled (..)
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Unrolled.STUnlist

import SDP.IndexedM

import SDP.SortM
import SDP.SortM.Tim

import Control.Exception.SDP
import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | STUnrolled is mutable version Unrolled.
data STUnrolled s i e = STUnrolled !i !i (STUnlist s e)

type role STUnrolled nominal nominal representational

--------------------------------------------------------------------------------

{- Eq instance. -}

instance (Index i) => Eq (STUnrolled s i e)
  where
    (STUnrolled l1 u1 xs) == (STUnrolled l2 u2 ys) =
      isEmpty (l1, u1) && isEmpty (l2, u2) || xs == ys

--------------------------------------------------------------------------------

{- BorderedM, LinearM and SplitM instances. -}

instance (Index i) => BorderedM (ST s) (STUnrolled s i e) i e
  where
    getLower  (STUnrolled l _ _) = return l
    getUpper  (STUnrolled _ u _) = return u
    getBounds (STUnrolled l u _) = return (l, u)
    getSizeOf (STUnrolled l u _) = return $ size (l, u)

instance (Index i) => LinearM (ST s) (STUnrolled s i e) e
  where
    newNull = let (l, u) = defaultBounds 0 in STUnrolled l u <$> newNull
    
    nowNull (STUnrolled l u es) = isEmpty (l, u) ? return True $ nowNull es
    getHead (STUnrolled l u es) = isEmpty (l, u) ? empEx "getHead" $ getHead es
    getLast (STUnrolled l u es) = isEmpty (l, u) ? empEx "getLast" $ getLast es
    
    prepend e = withBounds <=< prepend e . unpack
    append es = withBounds <=< append (unpack es)
    newLinear = withBounds <=< newLinear
    filled  n = withBounds <=< filled n
    
    getLeft   = getLeft  . unpack
    getRight  = getRight . unpack
    
    copied   (STUnrolled l u es) = STUnrolled l u <$> copied es
    copied'  (STUnrolled l u es) = (STUnrolled l u <$>) ... copied' es
    reversed (STUnrolled l u es) = STUnrolled l u <$> reversed es
    
    copyTo src os trg ot n = copyTo (unpack src) os (unpack trg) ot n

instance (Index i) => SplitM (ST s) (STUnrolled s i e) e
  where
    takeM n es@(STUnrolled l u marr#)
        | n <= 0 = newNull
        | n >= c = return es
        |  True  = STUnrolled l (index (l, u) n) <$> takeM n marr#
      where
        c = size (l, u)
    
    dropM n es@(STUnrolled l u marr#)
        | n >= c = newNull
        | n <= 0 = return es
        |  True  = STUnrolled (index (l, u) n) u <$> dropM n marr#
      where
        c = size (l, u)
    
    keepM n es@(STUnrolled l u marr#)
        | n <= 0 = newNull
        | n >= c = return es
        |  True  = STUnrolled (index (l, u) (c - n)) u <$> keepM n marr#
      where
        c = size (l, u)
    
    sansM n es@(STUnrolled l u marr#)
        | n >= c = newNull
        | n <= 0 = return es
        |  True  = STUnrolled (index (l, u) (c - n)) u <$> sansM n marr#
      where
        c = size (l, u)
    
    splitM n es@(STUnrolled l u marr#)
        | n <= 0 = do e' <- newNull; return (e', es)
        | n >= c = do e' <- newNull; return (es, e')
        |  True  = do (take#, drop#) <- splitM n marr#; return (STUnrolled l i take#, STUnrolled i u drop#)
      where
        i = index (l, u) n
        c = size  (l, u)
    
    divideM n es@(STUnrolled l u marr#)
        | n <= 0 = do e' <- newNull; return (es, e')
        | n >= c = do e' <- newNull; return (e', es)
        |  True  = do (sans#, keep#) <- divideM n marr#; return (STUnrolled l i sans#, STUnrolled i u keep#)
      where
        i = index (l, u) (c - n)
        c = size  (l, u)
    
    prefixM p (STUnrolled _ _ es) = prefixM p es
    suffixM p (STUnrolled _ _ es) = suffixM p es
    mprefix p (STUnrolled _ _ es) = mprefix p es
    msuffix p (STUnrolled _ _ es) = msuffix p es

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Index i) => IndexedM (ST s) (STUnrolled s i e) i e
  where
    fromAssocs' (l, u) defvalue ascs = STUnrolled l u <$> fromAssocs' bnds defvalue ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    {-# INLINE (!#>) #-}
    (!#>) es = (unpack es !#>)
    
    {-# INLINE (>!) #-}
    (>!) (STUnrolled l u es) = (es !#>) . offset (l, u)
    
    {-# INLINE writeM_ #-}
    writeM_ = writeM . unpack
    
    {-# INLINE writeM #-}
    writeM  (STUnrolled l u es) = writeM es . offset (l, u)
    
    overwrite es [] = return es
    overwrite (STUnrolled l u es) ascs = if isEmpty (l, u)
        then fromAssocs (l', u') ascs
        else STUnrolled l u <$> overwrite es ies
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        l'  = fst $ minimumBy cmpfst ascs
        u'  = fst $ maximumBy cmpfst ascs
    
    fromIndexed' = withBounds <=< fromIndexed'
    fromIndexedM = withBounds <=< fromIndexedM

instance (Index i) => IFoldM (ST s) (STUnrolled s i e) i e
  where
    ifoldrM f e (STUnrolled l u es) = ifoldrM (f . index (l, u)) e es
    ifoldlM f e (STUnrolled l u es) = ifoldlM (f . index (l, u)) e es
    
    i_foldrM f e = i_foldrM f e . unpack
    i_foldlM f e = i_foldlM f e . unpack

instance (Index i) => SortM (ST s) (STUnrolled s i e) e
  where
    sortMBy = timSortBy

--------------------------------------------------------------------------------

withBounds :: (Index i) => STUnlist s e -> ST s (STUnrolled s i e)
withBounds es = do
  n <- getSizeOf es
  let (l, u) = defaultBounds n
  return (STUnrolled l u es)

empEx :: String -> a
empEx =  throw . EmptyRange . showString "in SDP.Unrolled.ST."

unpack :: STUnrolled s i e -> STUnlist s e
unpack =  \ (STUnrolled _ _ es) -> es

