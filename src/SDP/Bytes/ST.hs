{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

{- |
    Module      :  SDP.Bytes.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Bytes.ST@ provides 'STBytes' - mutable lazy boxed array type.
-}
module SDP.Bytes.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STBytes
  STBytes (..)
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Internal.SBytes

import SDP.IndexedM
import SDP.Unboxed

import SDP.SortM
import SDP.SortM.Tim

import SDP.Internal.Commons

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | STBytes is mutable version of Bytes.
data STBytes s i e = STBytes !i !i (STBytes# s e)

type role STBytes nominal nominal representational

--------------------------------------------------------------------------------

instance (Index i) => Eq (STBytes s i e)
  where
    (STBytes l1 u1 arr1#) == (STBytes l2 u2 arr2#) =
      isEmpty (l1, u1) == isEmpty (l2, u2) || arr1# == arr2#

--------------------------------------------------------------------------------

{- BorderedM, LinearM and SplitM instances. -}

instance (Index i, Unboxed e) => BorderedM (ST s) (STBytes s i e) i e
  where
    getLower   (STBytes l _ _) = return l
    getUpper   (STBytes _ u _) = return u
    getBounds  (STBytes l u _) = return (l, u)
    getIndices (STBytes l u _) = return $ range (l, u)
    getIndexOf (STBytes l u _) = return . inRange (l, u)
    
    getSizeOf = getSizeOf . unpack

instance (Index i, Unboxed e) => LinearM (ST s) (STBytes s i e) e
  where
    newNull = let (l, u) = defaultBounds 0 in STBytes l u <$> newNull
    
    nowNull (STBytes l u _) = return $ isEmpty (l, u)
    
    getHead es = do s <- getSizeOf es; s < 1 ? empEx "getHead" $ es !#> 0
    getLast es = do s <- getSizeOf es; s < 1 ? empEx "getLast" $ es !#> (s - 1)
    
    prepend e es = withBounds =<< prepend e (unpack es)
    append  es e = withBounds =<< append  (unpack es) e
    
    newLinear     = fromFoldableM
    newLinearN    = newLinearN   >>=> withBounds
    fromFoldableM = fromFoldableM >=> withBounds
    
    getLeft  = getLeft  . unpack
    getRight = getRight . unpack
    filled   = filled >>=> withBounds
    
    reversed (STBytes l u mbytes#) = STBytes l u <$> reversed mbytes#
    copied   (STBytes l u mbytes#) = STBytes l u <$> copied mbytes#
    copied'  (STBytes _ _ mbytes#) = copied' mbytes# >>=> withBounds
    
    copyTo src os trg ot n = copyTo (unpack src) os (unpack trg) ot n

instance (Index i, Unboxed e) => SplitM (ST s) (STBytes s i e) e
  where
    takeM n es@(STBytes l u marr#)
        | n <= 0 = newNull
        | n >= c = return es
        |  True  = STBytes l (index (l, u) n) <$> takeM n marr#
      where
        c = size (l, u)
    
    dropM n es@(STBytes l u marr#)
        | n >= c = newNull
        | n <= 0 = return es
        |  True  = STBytes (index (l, u) n) u <$> dropM n marr#
      where
        c = size (l, u)
    
    keepM n es@(STBytes l u marr#)
        | n <= 0 = newNull
        | n >= c = return es
        |  True  = STBytes (index (l, u) (c - n)) u <$> keepM n marr#
      where
        c = size (l, u)
    
    sansM n es@(STBytes l u marr#)
        | n >= c = newNull
        | n <= 0 = return es
        |  True  = STBytes (index (l, u) (c - n)) u <$> sansM n marr#
      where
        c = size (l, u)
    
    splitM n es@(STBytes l u marr#)
        | n <= 0 = do e' <- newNull; return (e', es)
        | n >= c = do e' <- newNull; return (es, e')
        |  True  = do (take#, drop#) <- splitM n marr#; return (STBytes l i take#, STBytes i u drop#)
      where
        i = index (l, u) n
        c = size  (l, u)
    
    divideM n es@(STBytes l u marr#)
        | n <= 0 = do e' <- newNull; return (es, e')
        | n >= c = do e' <- newNull; return (e', es)
        |  True  = do (sans#, keep#) <- divideM n marr#; return (STBytes l i sans#, STBytes i u keep#)
      where
        i = index (l, u) (c - n)
        c = size  (l, u)
    
    prefixM p (STBytes _ _ es) = prefixM p es
    suffixM p (STBytes _ _ es) = suffixM p es
    mprefix p (STBytes _ _ es) = mprefix p es
    msuffix p (STBytes _ _ es) = msuffix p es

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Index i, Unboxed e) => IndexedM (ST s) (STBytes s i e) i e
  where
    fromAssocs bs@(l, u) ascs = STBytes l u <$> mbytes#
      where
        mbytes# = defaultBounds (size bs) `fromAssocs` ies
        ies     = [ (offset bs i, e) | (i, e) <- ascs, inRange bs i ]
    
    fromAssocs' bs@(l, u) defvalue ascs = STBytes l u <$> mbytes#
      where
        mbytes# = defaultBounds (size bs) `fromAssocs'` defvalue $ ies
        ies     = [ (offset bs i, e) | (i, e) <- ascs, inRange bs i ]
    
    {-# INLINE (!#>) #-}
    (!#>) = (!#>) . unpack
    
    {-# INLINE (>!) #-}
    (>!) (STBytes l u mbytes#) = (mbytes# !#>) . offset (l, u)
    
    {-# INLINE writeM_ #-}
    writeM_ = writeM_ . unpack
    
    {-# INLINE writeM #-}
    writeM (STBytes l u mbytes#) = writeM mbytes# . offset (l, u)
    
    overwrite es@(STBytes l u _) ascs = mapM_ (uncurry $ writeM_ es) ies >> return es
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
    
    fromIndexed' = fromIndexed' >=> withBounds
    fromIndexedM = fromIndexedM >=> withBounds

instance (Index i, Unboxed e) => IFoldM (ST s) (STBytes s i e) i e
  where
    ifoldrM f base = \ (STBytes l u mbytes#) -> ifoldrM (f . index (l, u)) base mbytes#
    ifoldlM f base = \ (STBytes l u mbytes#) -> ifoldlM (f . index (l, u)) base mbytes#
    
    i_foldrM f base = i_foldrM f base . unpack
    i_foldlM f base = i_foldlM f base . unpack

instance (Index i, Unboxed e) => SortM (ST s) (STBytes s i e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

{-# INLINE withBounds #-}
withBounds :: (Index i, Unboxed e) => STBytes# s e -> ST s (STBytes s i e)
withBounds marr# = do
  (l, u) <- defaultBounds <$> getSizeOf marr#
  return (STBytes l u marr#)

unpack :: STBytes s i e -> STBytes# s e
unpack =  \ (STBytes _ _ bytes#) -> bytes#

empEx :: String -> a
empEx =  throw . EmptyRange . showString "in SDP.Bytes.ST."



