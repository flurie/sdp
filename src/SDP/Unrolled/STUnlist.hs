{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric #-}
{-# LANGUAGE Unsafe, MagicHash, BangPatterns #-}

{- |
    Module      :  SDP.Unrolled.STUnlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Unrolled.STUnlist@ provides 'STUnlist' - mutable boxed lazy unrolled
    linked list.
-}
module SDP.Unrolled.STUnlist
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STUnlist
  STUnlist (..)
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Prim.SArray

import SDP.IndexedM

import SDP.SortM.Tim
import SDP.SortM

import GHC.Generics

import Data.Typeable

import SDP.Internal

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

-- | This STUnlist is mutable version of Unlist.
newtype STUnlist s e = STUnlist [STArray# s e]
  deriving ( Eq, Typeable, Generic )

--------------------------------------------------------------------------------

{- BorderedM, LinearM and SplitM instances. -}

instance BorderedM (ST s) (STUnlist s e) Int
  where
    getLower _  = return 0
    getUpper es = do n <- getSizeOf es; return (n - 1)
    
    getSizeOf (STUnlist es) = foldr (liftA2 (+) . getSizeOf) (return 0) es
    
    getIndices es = do n <- getSizeOf es; return [0 .. n - 1]
    getIndexOf es = \ i -> i < 0 ? return False $ do n <- getSizeOf es; return (i < n)

instance LinearM (ST s) (STUnlist s e) e
  where
    newNull = return (STUnlist [])
    nowNull = fmap null . unpack'
    getHead = getHead . head <=< unpack'
    getLast = getLast . last <=< unpack'
    
    prepend e' es' = fmap STUnlist . go e' =<< unpack' es'
      where
        go e es@(x : xs) = do n <- getSizeOf x; n < lim ? (: xs) <$> prepend e x $ (: es) <$> newLinear [e]
        go e _ = pure <$> newLinear [e]
    
    append es' e' = fmap STUnlist . go e' =<< unpack' es'
      where
        go e es@(xs :< x) = do n <- getSizeOf x; n < lim ? (xs :<) <$> append x e $ (es :<) <$> newLinear [e]
        go e _ = pure <$> newLinear [e]
    
    newLinear = fmap STUnlist . mapM newLinear . chunks lim
    
    getLeft  (STUnlist es) = concat <$> mapM getLeft es
    getRight (STUnlist es) = (concat . reverse) <$> mapM getRight es
    reversed (STUnlist es) = (STUnlist . reverse) <$> mapM reversed es
    
    filled c e = STUnlist <$> sequence (replicate d (filled lim e) :< filled n e)
      where
        (d, n) = c `divMod` lim
    
    copyTo src os trg ot c = when (c > 0) $ do
        when (os < 0 || ot < 0) $ underEx "copyTo"
        src' <- dropM os src
        trg' <- dropM ot trg
        go c src' trg'
      where
        go n xs@(STUnlist (x : _)) ys@(STUnlist (y : _)) = do
          n1 <- getSizeOf x
          n2 <- getSizeOf y
          let n' = minimum [n1, n2, n]
          
          copyTo x 0 y 0 n'
          xs' <- dropM n' xs
          ys' <- dropM n' ys
          go (n - n') xs' ys'
        go n _ _ = when (n > 0) $ overEx "copyTo"

instance SplitM (ST s) (STUnlist s e) e
  where
    takeM n (STUnlist (e : es)) = n < 1 ? newNull $ do
      c <- getSizeOf e
      case n <=> c of
        EQ -> return (STUnlist [e])
        LT -> do t <- takeM n e; return (STUnlist [t])
        GT -> do (STUnlist ts) <- takeM (n - c) (STUnlist es); return $ STUnlist (e : ts)
    takeM _ es = return es
    
    dropM n es'@(STUnlist (e : es)) = n < 1 ? return es' $ do
      c <- getSizeOf e
      case n <=> c of
        EQ -> return (STUnlist es)
        GT -> dropM (n - c) (STUnlist es)
        LT -> do d <- dropM n e; return $ STUnlist (d : es)
    dropM _ es = return es
    
    prefixM f (STUnlist es) = foldr (\ e p -> do n <- getSizeOf e; c <- prefixM f e; c == n ? (+ c) <$> p $ return c) (return 0) es
    suffixM f (STUnlist es) = foldl (\ p e -> do n <- getSizeOf e; c <- suffixM f e; c == n ? (+ c) <$> p $ return c) (return 0) es
    mprefix f (STUnlist es) = foldr (\ e p -> do n <- getSizeOf e; c <- mprefix f e; c == n ? (+ c) <$> p $ return c) (return 0) es
    msuffix f (STUnlist es) = foldl (\ p e -> do n <- getSizeOf e; c <- msuffix f e; c == n ? (+ c) <$> p $ return c) (return 0) es

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance IndexedM (ST s) (STUnlist s e) Int e
  where
    fromAssocs' bnds@(l, _) defvalue ascs = do
      arr <- size bnds `filled` defvalue
      arr `overwrite` [ (i - l, e) | (i, e) <- ascs, inRange bnds i ]
    
    (!#>) (STUnlist es) = go es
      where
        go (x : xs) i = do n <- getSizeOf x; i < n ? x !#> i $ go xs (i - n)
        go _ _ = overEx "(>!)"
    
    {-# INLINE (>!) #-}
    (>!) es i = i < 0 ? overEx "(>!)" $ es !#> i
    
    {-# INLINE writeM_ #-}
    writeM_ (STUnlist es) = go es
      where
        go (x : xs) i e = do n <- getSizeOf x; i < n ? writeM_ x i e $ go xs (i - n) e
        go _ _ _ = return ()
    
    {-# INLINE writeM #-}
    writeM es i e = (i < 0) `unless` writeM_ es i e
    
    overwrite es@(STUnlist []) ascs = isNull ascs ? return es $ fromAssocs (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    overwrite (STUnlist es) ascs = STUnlist <$> go es ies'
      where
        go :: [STArray# s e] -> [(Int, e)] -> ST s [STArray# s e]
        go xs ies = do
          fs <- mapM (fmap (flip $ (<) . fst) . getSizeOf) xs
          sequence $ zipWith overwrite xs (partitions fs ies)
        ies' = filter ((>= 0) . fst) ascs
    
    fromIndexed' = newLinear . listL
    fromIndexedM = newLinear <=< getLeft

instance IFoldM (ST s) (STUnlist s e) Int e
  where
    ifoldrM f base = ifoldrCh 0 f base . coerce
    ifoldlM f base = ifoldlCh 0 f base . coerce
    
    i_foldrM f base (STUnlist es) = foldr g (return base) es
      where
        g = \ e -> (flip (i_foldrM f) e =<<)
    
    i_foldlM f base (STUnlist es) = foldl g (return base) es
      where
        g = flip $ \ e -> (flip (i_foldlM f) e =<<)

instance SortM (ST s) (STUnlist s e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

ifoldrCh :: Int -> (Int -> e -> r -> ST s r) -> r -> [STArray# s e] -> ST s r
ifoldrCh !o f base (x : xs) = do
  n   <- getSizeOf x
  xs' <- ifoldrCh (o + n) f base xs
  ifoldrM (f . (o +)) xs' x
ifoldrCh _ _ base _ = return base

ifoldlCh :: Int -> (Int -> r -> e -> ST s r) -> r -> [STArray# s e] -> ST s r
ifoldlCh !o f base (x : xs) = do
  n  <- getSizeOf x
  x' <- ifoldlM (f . (o +)) base x
  ifoldlCh (o + n) f x' xs
ifoldlCh _ _ base _ = return base

unpack' :: STUnlist s e -> ST s [STArray# s e]
unpack' (STUnlist es) = go es
  where
    go (x : xs) = do n <- getSizeOf x; n < 1 ? go xs $ (x :) <$> go xs
    go _ = return []

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Unrolled.STUnlist."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Unrolled.STUnlist."

lim :: Int
lim =  1024



