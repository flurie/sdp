{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

{- |
    Module      :  SDP.Array.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Array.ST@ provides 'STArray' - mutable lazy boxed array type.
-}
module SDP.Array.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STArray
  STArray (..)
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Prim.SArray

import SDP.IndexedM

import GHC.ST ( ST (..) )

import SDP.SortM
import SDP.SortM.Tim

import SDP.Internal.Commons

default ()

--------------------------------------------------------------------------------

-- | STArray is mutable version of Array.
data STArray s i e = STArray !i !i (STArray# s e)

type role STArray nominal nominal representational

--------------------------------------------------------------------------------

instance (Index i) => Eq (STArray s i e)
  where
    (STArray l1 u1 arr1#) == (STArray l2 u2 arr2#) =
      isEmpty (l1, u1) == isEmpty (l2, u2) || arr1# == arr2#

--------------------------------------------------------------------------------

{- BorderedM, LinearM and SplitM instances. -}

instance (Index i) => BorderedM (ST s) (STArray s i e) i e
  where
    getLower   (STArray l _ _) = return l
    getUpper   (STArray _ u _) = return u
    getBounds  (STArray l u _) = return (l, u)
    getIndices (STArray l u _) = return $ range (l, u)
    getIndexOf (STArray l u _) = return . inRange (l, u)
    
    getSizeOf  (STArray _ _ marr#) = getSizeOf marr#

instance (Index i) => LinearM (ST s) (STArray s i e) e
  where
    newNull = let (l, u) = defaultBounds 0 in STArray l u <$> newNull
    
    nowNull (STArray l u _) = return $ isEmpty (l, u)
    
    getHead es = do s <- getSizeOf es; s < 1 ? empEx "getHead" $ es !#> 0
    getLast es = do s <- getSizeOf es; s < 1 ? empEx "getLast" $ es !#> (s - 1)
    
    prepend e es = withBounds =<< prepend e (unpack es)
    append  es e = withBounds =<< append  (unpack es) e
    
    newLinear     = fromFoldableM
    newLinearN    = newLinearN   >>=> withBounds
    fromFoldableM = fromFoldableM >=> withBounds
    
    getLeft  = getLeft  . unpack
    getRight = getRight . unpack
    
    reversed (STArray l u marr#) = STArray l u <$> reversed marr#
    copied   (STArray l u marr#) = STArray l u <$> copied marr#
    copied'  (STArray _ _ marr#) = copied' marr# >>=> withBounds
    
    filled = filled >>=> withBounds
    
    copyTo src os trg ot n = copyTo (unpack src) os (unpack trg) ot n

instance (Index i) => SplitM (ST s) (STArray s i e) e
  where
    takeM n es@(STArray l u marr#)
        | n <= 0 = newNull
        | n >= c = return es
        |  True  = STArray l (index (l, u) n) <$> takeM n marr#
      where
        c = size (l, u)
    
    dropM n es@(STArray l u marr#)
        | n >= c = newNull
        | n <= 0 = return es
        |  True  = STArray (index (l, u) n) u <$> dropM n marr#
      where
        c = size (l, u)
    
    keepM n es@(STArray l u marr#)
        | n <= 0 = newNull
        | n >= c = return es
        |  True  = STArray (index (l, u) (c - n)) u <$> keepM n marr#
      where
        c = size (l, u)
    
    sansM n es@(STArray l u marr#)
        | n >= c = newNull
        | n <= 0 = return es
        |  True  = STArray (index (l, u) (c - n)) u <$> sansM n marr#
      where
        c = size (l, u)
    
    splitM n es@(STArray l u marr#)
        | n <= 0 = do e' <- newNull; return (e', es)
        | n >= c = do e' <- newNull; return (es, e')
        |  True  = do (take#, drop#) <- splitM n marr#; return (STArray l i take#, STArray i u drop#)
      where
        i = index (l, u) n
        c = size  (l, u)
    
    divideM n es@(STArray l u marr#)
        | n <= 0 = do e' <- newNull; return (es, e')
        | n >= c = do e' <- newNull; return (e', es)
        |  True  = do (sans#, keep#) <- divideM n marr#; return (STArray l i sans#, STArray i u keep#)
      where
        i = index (l, u) (c - n)
        c = size  (l, u)
    
    prefixM p (STArray _ _ es) = prefixM p es
    suffixM p (STArray _ _ es) = suffixM p es
    mprefix p (STArray _ _ es) = mprefix p es
    msuffix p (STArray _ _ es) = msuffix p es

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Index i) => IndexedM (ST s) (STArray s i e) i e
  where
    fromAssocs' bs@(l, u) defvalue ascs = STArray l u <$> marr#
      where
        marr# = defaultBounds (size bs) `fromAssocs'` defvalue $ ies
        ies   = [ (offset bs i, e) | (i, e) <- ascs, inRange bs i ]
    
    {-# INLINE (!#>) #-}
    (!#>) = (!#>) . unpack
    
    {-# INLINE (>!) #-}
    (>!) (STArray l u marr#) = (marr# !#>) . offset (l, u)
    
    {-# LANGUAGE writeM_ #-}
    writeM_ = writeM_ . unpack
    
    {-# INLINE writeM #-}
    writeM  (STArray l u marr#) = writeM_ marr# . offset (l, u)
    
    overwrite es@(STArray l u _) ascs = mapM_ (uncurry $ writeM_ es) ies >> return es
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
    
    fromIndexed' = fromIndexed' >=> withBounds
    fromIndexedM = fromIndexedM >=> withBounds

instance (Index i) => IFoldM (ST s) (STArray s i e) i e
  where
    ifoldrM f base (STArray l u marr#) = ifoldrM (f . index (l, u)) base marr#
    ifoldlM f base (STArray l u marr#) = ifoldlM (f . index (l, u)) base marr#
    
    i_foldrM f base = i_foldrM f base . unpack
    i_foldlM f base = i_foldlM f base . unpack

instance (Index i) => SortM (ST s) (STArray s i e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

{-# INLINE withBounds #-}
withBounds :: (Index i) => STArray# s e -> ST s (STArray s i e)
withBounds marr# = do
  (l, u) <- defaultBounds <$> getSizeOf marr#
  return (STArray l u marr#)

unpack :: (STArray s i e) -> STArray# s e
unpack =  \ (STArray _ _ arr#) -> arr#

empEx :: String -> a
empEx =  throw . EmptyRange . showString "in SDP.Array.ST."



