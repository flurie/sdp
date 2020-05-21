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
import SDP.Prim.SBytes

import SDP.IndexedM
import SDP.Unboxed

import SDP.SortM.Tim
import SDP.SortM

import SDP.Internal

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | This STUblist is mutable version of Ublist.
newtype STUblist s e = STUblist [STBytes# s e] deriving ( Eq )

--------------------------------------------------------------------------------

{- BorderedM, LinearM and SplitM instances. -}

instance (Unboxed e) => BorderedM (ST s) (STUblist s e) Int
  where
    getLower _  = return 0
    getUpper es = do n <- getSizeOf es; return (n - 1)
    
    getSizeOf (STUblist es) = foldr (liftA2 (+) . getSizeOf) (return 0) es
    
    getIndices es = do n <- getSizeOf es; return [0 .. n - 1]
    getIndexOf es = \ i -> i < 0 ? return False $ do n <- getSizeOf es; return (i < n)

instance (Unboxed e) => LinearM (ST s) (STUblist s e) e
  where
    newNull = return (STUblist [])
    nowNull = fmap null . unpack'
    getHead = getHead . head <=< unpack'
    getLast = getLast . last <=< unpack'
    
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
    
    copyTo src os trg ot c = when (c > 0) $ do
        when (os < 0 || ot < 0) $ underEx "copyTo"
        src' <- dropM os src
        trg' <- dropM ot trg
        go c src' trg'
      where
        go n xs@(STUblist (x : _)) ys@(STUblist (y : _)) = do
          n1 <- getSizeOf x
          n2 <- getSizeOf y
          let n' = minimum [n1, n2, n]
          
          copyTo x 0 y 0 n'
          xs' <- dropM n' xs
          ys' <- dropM n' ys
          go (n - n') xs' ys'
        go n _ _ = when (n > 0) $ overEx "copyTo"

instance (Unboxed e) => SplitM (ST s) (STUblist s e) e
  where
    takeM n (STUblist (e : es)) = n < 1 ? newNull $ do
      c <- getSizeOf e
      case n <=> c of
        EQ -> return (STUblist [e])
        LT -> do t <- takeM n e; return (STUblist [t])
        GT -> do (STUblist ts) <- takeM (n - c) (STUblist es); return $ STUblist (e : ts)
    takeM _ es = return es
    
    dropM n es'@(STUblist (e : es)) = n < 1 ? return es' $ do
      c <- getSizeOf e
      case n <=> c of
        EQ -> return (STUblist es)
        GT -> dropM (n - c) (STUblist es)
        LT -> do d <- dropM n e; return $ STUblist (d : es)
    dropM _ es = return es
    
    prefixM f (STUblist es) = foldr (\ e p -> do n <- getSizeOf e; c <- prefixM f e; c == n ? (+ c) <$> p $ return c) (return 0) es
    suffixM f (STUblist es) = foldl (\ p e -> do n <- getSizeOf e; c <- suffixM f e; c == n ? (+ c) <$> p $ return c) (return 0) es
    mprefix f (STUblist es) = foldr (\ e p -> do n <- getSizeOf e; c <- mprefix f e; c == n ? (+ c) <$> p $ return c) (return 0) es
    msuffix f (STUblist es) = foldl (\ p e -> do n <- getSizeOf e; c <- msuffix f e; c == n ? (+ c) <$> p $ return c) (return 0) es

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Unboxed e) => IndexedM (ST s) (STUblist s e) Int e
  where
    fromAssocs bnds@(l, _) ascs = do
        ubl <- liftA2 (:<) (replicateM c (filled_ lim)) (filled_ d)
        STUblist ubl `overwrite` [ (i - l, e) | (i, e) <- ascs ]
      where
        (c, d) = size bnds `divMod` lim
    
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

instance (Unboxed e) => SortM (ST s) (STUblist s e) e
  where
    sortMBy = timSortBy

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

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.ByteList.STUblist."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.ByteList.STUblist."

lim :: Int
lim =  1024

