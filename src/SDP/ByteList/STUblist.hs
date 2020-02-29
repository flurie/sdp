{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, BangPatterns #-}

{- |
    Module      :  SDP.ByteList.STUblist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.ByteList.STUblist@ provides 'STUblist' - mutable unboxed strict
    unrolled linked list.
-}
module SDP.ByteList.STUblist
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STUblist
  STUblist (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Unboxed

import SDP.SortM.Tim
import SDP.SortM

import Data.Coerce

import GHC.Base ( Int (..) )
import GHC.ST   ( ST  (..) )

import SDP.Internal.Commons
import SDP.Internal.SBytes

default ()

--------------------------------------------------------------------------------

-- | This STUblist is mutable version of Ublist.
newtype STUblist s e = STUblist [STBytes# s e] deriving ( Eq )

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance (Unboxed e) => BorderedM (ST s) (STUblist s e) Int e
  where
    getLower _  = return 0
    getUpper es = do n <- getSizeOf es; return (n - 1)
    
    getSizeOf (STUblist es) = foldr (liftA2 (+) . getSizeOf) (return 0) es
    
    getIndices es = do n <- getSizeOf es; return [0 .. n - 1]
    getIndexOf es = \ i -> i < 0 ? return False $ do n <- getSizeOf es; return (i < n)

instance (Unboxed e) => LinearM (ST s) (STUblist s e) e
  where
    prepend e' es' = fmap STUblist . go e' =<< unpack' es'
      where
        go e es@(x : xs) = do n <- getSizeOf x; n < lim ? (: xs) <$> prepend e x $ (: es) <$> newLinear [e]
        go e _ = pure <$> newLinear [e]
    
    append es' e' = fmap STUblist . go e' =<< unpack' es'
      where
        go e es@(xs :< x) = do n <- getSizeOf x; n < lim ? (xs :<) <$> append x e $ (es :<) <$> newLinear [e]
        go e _ = pure <$> newLinear [e]
    
    newLinear = fmap STUblist . mapM newLinear . chunks lim
    
    getLeft  (STUblist es) = concat <$> mapM getLeft es
    getRight (STUblist es) = (concat . reverse) <$> mapM getRight es
    reversed (STUblist es) = (STUblist . reverse) <$> mapM reversed es
    
    filled c e = STUblist <$> sequence (replicate d (filled lim e) :< filled n e)
      where
        (d, n) = c `divMod` lim

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Unboxed e) => IndexedM (ST s) (STUblist s e) Int e
  where
    fromAssocs bnds@(l, _) ascs = do
        ubl <- filled' (size bnds)
        STUblist ubl `overwrite` [ (i - l, e) | (i, e) <- ascs ]
      where
        filled' n = n < 1 ? return [] $ liftA2 (:) ch' rs'
          where
            ch' = filled_ (min n lim)
            rs' = filled' (n - lim)
    
    fromAssocs' bnds@(l, _) defvalue ascs = do
      arr <- size bnds `filled` defvalue
      arr `overwrite` [ (i - l, e) | (i, e) <- ascs, inRange bnds i ]
    
    (!#>) (STUblist es) = go es
      where
        go (x : xs) i = do n <- getSizeOf x; i < n ? x !#> i $ go xs (i - n)
        go _ _ = throw $ IndexOverflow "in SDP.ByteList.STUblist.(>!)"
    
    {-# INLINE (>!) #-}
    (>!) es i = i < 0 ? err $ es !#> i
      where
        err = throw $ IndexUnderflow "in SDP.ByteList.STUblist.(>!)"
    
    {-# INLINE writeM_ #-}
    writeM_ (STUblist es) = go es
      where
        go (x : xs) i e = do n <- getSizeOf x; i < n ? writeM_ x i e $ go xs (i - n) e
        go _ _ _ = return ()
    
    {-# INLINE writeM #-}
    writeM es i e = (i < 0) `unless` writeM_ es i e
    
    overwrite es@(STUblist []) ascs = isNull ascs ? return es $ fromAssocs (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    overwrite (STUblist es) ascs = STUblist <$> go es ies'
      where
        go :: (Unboxed e) => [STBytes# s e] -> [(Int, e)] -> ST s [STBytes# s e]
        go xs ies = do
          fs <- mapM (fmap (flip $ (<) . fst) . getSizeOf) xs
          sequence $ zipWith overwrite xs (partitions fs ies)
        
        ies' = filter ((>= 0) . fst) ascs
    
    fromIndexed' = newLinear . listL
    fromIndexedM = newLinear <=< getLeft

instance (Unboxed e) => IFoldM (ST s) (STUblist s e) Int e
  where
    ifoldrM f base = ifoldrCh 0 f base . coerce
    ifoldlM f base = ifoldlCh 0 f base . coerce
    
    i_foldrM f base (STUblist es) = foldr g (return base) es
      where
        g = \ e -> (flip (i_foldrM f) e =<<)
    
    i_foldlM f base (STUblist es) = foldl g (return base) es
      where
        g = flip $ \ e -> (flip (i_foldlM f) e =<<)

instance (Unboxed e) => SortM (ST s) (STUblist s e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

ifoldrCh :: (Unboxed e) =>  Int -> (Int -> e -> r -> ST s r) -> r -> [STBytes# s e] -> ST s r
ifoldrCh !o f base (x : xs) = do
  n   <- getSizeOf x
  xs' <- ifoldrCh (o + n) f base xs
  ifoldrM (f . (o +)) xs' x
ifoldrCh _ _ base _ = return base

ifoldlCh :: (Unboxed e) => Int -> (Int -> r -> e -> ST s r) -> r -> [STBytes# s e] -> ST s r
ifoldlCh !o f base (x : xs) = do
  n  <- getSizeOf x
  x' <- ifoldlM (f . (o +)) base x
  ifoldlCh (o + n) f x' xs
ifoldlCh _ _ base _ = return base

unpack' :: (Unboxed e) => STUblist s e -> ST s [STBytes# s e]
unpack' (STUblist es) = go es
  where
    go (x : xs) = do n <- getSizeOf x; n < 1 ? go xs $ (x :) <$> go xs
    go _ = return []

lim :: Int
lim =  1024



