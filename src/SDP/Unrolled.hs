{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, RoleAnnotations #-}

{- |
    Module      :  SDP.Unrolled
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    This module provides Unrolled - unrolled linked list.
-}

module SDP.Unrolled
(
  Unrolled (..),
  Unlist, -- type Unlist is abstract.
  
  module SDP.Indexed,
  module SDP.Scan,
  module SDP.Set
)
where

import Prelude ()
import SDP.SafePrelude
import Test.QuickCheck

import GHC.Base ( Int (..) )
import GHC.Show ( appPrec )

import Text.Read
import Text.Read.Lex ( expect )

import SDP.Indexed

import SDP.Unrolled.Unlist

import SDP.Scan
import SDP.Set

import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | Bordered unrolled linked list.
data Unrolled i e = Unrolled !i !i (Unlist e)

type role Unrolled nominal representational

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Eq e, Index i) => Eq (Unrolled i e) where (==) = eq1

instance (Index i) => Eq1 (Unrolled i)
  where
    liftEq f unr1@(Unrolled l1 u1 xs) unr2@(Unrolled l2 u2 ys) = res
      where
        res = null xs && null ys || l1 == l2 && u1 == u2 && liftEq f xs' ys'
        xs' = toList unr1
        ys' = toList unr2

--------------------------------------------------------------------------------

{- Ord and Ord1 innstances. -}

instance (Ord e, Index i) => Ord (Unrolled i e) where compare = compare1

instance (Index i) => Ord1 (Unrolled i)
  where
    liftCompare cmp unr1 unr2 = liftCompare cmp' (assocs unr1) (assocs unr2)
      where
        cmp' (ix, x) (iy, y) = (ix <=> iy) <> (cmp x y)

--------------------------------------------------------------------------------

{- Show and Read instances -}

instance (Index i, Show i, Show e) => Show (Unrolled i e)
  where
    showsPrec p unr@(Unrolled l u _) = showParen (p > appPrec) shows'
      where
        shows' = showString "unrolled " . shows (l, u) . showChar ' ' . shows (assocs unr)

instance (Index i, Read i, Read e) => Read (Unrolled i e)
  where
    readList = readListDefault
    readPrec = parens $ prec appPrec (lift . expect $ Ident "unrolled") >> liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances -}

instance (Index i) => Functor (Unrolled i)
  where
    fmap f (Unrolled l u es) = Unrolled l u (f <$> es)

instance (Index i) => Zip (Unrolled i)
  where
    zipWith  f as bs             = fromList $ zipWith  f (toList as) (toList bs)
    zipWith3 f as bs cs          = fromList $ zipWith3 f (toList as) (toList bs) (toList cs)
    zipWith4 f as bs cs ds       = fromList $ zipWith4 f (toList as) (toList bs) (toList cs) (toList ds)
    zipWith5 f as bs cs ds es    = fromList $ zipWith5 f (toList as) (toList bs) (toList cs) (toList ds) (toList es)
    zipWith6 f as bs cs ds es fs = fromList $ zipWith6 f (toList as) (toList bs) (toList cs) (toList ds) (toList es) (toList fs)

instance (Index i) => Applicative (Unrolled i)
  where
    pure = single
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable, Scan and Traversable instances -}

instance (Index i) => Foldable (Unrolled i)
  where
    foldr  f base (Unrolled l u es) = foldr  f base $ take (size (l, u)) es
    foldl  f base (Unrolled l u es) = foldl  f base $ take (size (l, u)) es
    
    foldr' f base (Unrolled l u es) = foldr' f base $ take (size (l, u)) es
    foldl' f base (Unrolled l u es) = foldl' f base $ take (size (l, u)) es
    
    foldr1 f (Unrolled l u es) = foldr1 f $ take (size (l, u)) es
    foldl1 f (Unrolled l u es) = foldl1 f $ take (size (l, u)) es
    
    toList   (Unrolled l u es) = take n $ toList es where n = size (l, u)
    elem e   (Unrolled l u es) = elem e $ take (size (l, u)) es
    null     (Unrolled l u es) = null es || isEmpty (l, u)
    length   (Unrolled l u  _) = size (l, u)

-- instance (Index i) => Scan (Unrolled i)

instance (Index i) => Traversable (Unrolled i)
  where
    traverse f arr = fromList <$> traverse f (toList arr)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i) => Linear (Unrolled i e) e
  where
    isNull = null
    listL  = toList
    
    fromListN n es = Unrolled l u $ fromListN n es
      where
        l = unsafeIndex 0
        u = unsafeIndex $ max 0 n - 1
    
    uncons Z = throw $ EmptyRange "in SDP.Unrolled.(:>)"
    uncons (Unrolled l u es) = (x, es <. 2 ? Z $ Unrolled l1 u xs)
      where
        (x, xs) = uncons es
        l1 = next (l, u) l
    
    unsnoc Z = throw $ EmptyRange "in SDP.Unrolled.(:<)"
    unsnoc (Unrolled l u es) = (es <. 2 ? Z $ Unrolled l u1 xs, x)
      where
        (xs, x) = unsnoc es
        u1 = prev (l, u) u
    
    concat xss = Unrolled l u res
      where
        (n', res) = foldr f (0, Z) xss
        
        f = \ (Unrolled _ _ xs) (len, ys) -> (len + length xs, xs ++ ys)
        
        l = unsafeIndex 0
        u = unsafeIndex $ max 0 n' - 1
    
    intersperse e (Unrolled _ _ es) = Unrolled l1 u1 (intersperse e es)
      where
        n1 = case n <=> 0 of {LT -> -2; EQ -> 0; GT -> 2 * n - 2}; n = length es
        u1 = unsafeIndex n1
        l1 = unsafeIndex 0

instance (Index i) => Split (Unrolled i e) e
  where
    take n unr@(Unrolled l u es)
        | n <= 0 = Z
        | n >= c = unr
        |  True  = Unrolled l u' (take n es)
      where
        u' = index (l, u) $ n - 1
        c  = size  (l, u)
    
    drop n unr@(Unrolled l u es)
        | n <= 0 = unr
        | n >= c = Z
        |  True  = Unrolled l' u (drop n es)
      where
        l' = index (l, u) n
        c  = size  (l, u)
    
    split n unr@(Unrolled l u es)
        | n <= 0 = (Z, unr)
        | n >= c = (unr, Z)
        |  True  = (Unrolled l u' take', Unrolled l' u drop')
      where
        u' = index (l, u) $ n - 1
        l' = index (l, u)   n
        c  = size  (l, u)
        
        (take', drop') = split n es
    
    prefix f = prefix f . toList
    suffix f = suffix f . toList
    
    isPrefixOf = isPrefixOf `on` toList
    isInfixOf  = isInfixOf  `on` toList
    isSuffixOf = isSuffixOf `on` toList

instance (Index i) => Bordered (Unrolled i e) i e
  where
    indices (Unrolled l u _) = range (l, u)
    bounds  (Unrolled l u _) = (l, u)
    lower   (Unrolled l _ _) = l
    upper   (Unrolled _ u _) = u
    
    sizeOf  (Unrolled l u _) = size (l, u)

--------------------------------------------------------------------------------

instance (Index i) => Indexed (Unrolled i e) i e
  where
    -- [internal]: it's correct, but completly inneficient (Set []). Rewrite.
    assoc' bnds e ies = fromListN n sorted
      where
        sorted = snds $ unionWith cmpfst (setWith cmpfst ies) filler
        filler = zip (range bnds) (replicate n e)
        n = size bnds
    
    Z  // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    (Unrolled l u es) // ascs = Unrolled l' u' es'
      where
        es' = es // [ (offset (l, u) i, e) | (i, e) <- ascs ]
        u'  = unsafeIndex $ upper es'
        l'  = unsafeIndex 0
    
    {-# INLINE (!) #-}
    (!)  (Unrolled l u es) i = es ! (offset (l, u) i)
    
    p .$ (Unrolled l u es) = index (l, u) <$> (p .$ es)
    p *$ (Unrolled l u es) = index (l, u) <$> (p *$ es)

--------------------------------------------------------------------------------

instance (Index i, Arbitrary e) => Arbitrary (Unrolled i e)
  where
    arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

instance (Index i) => Estimate (Unrolled i)
  where
    (Unrolled l1 u1 _) <==> (Unrolled l2 u2 _) = compare s1 s2
      where
        s1 = size (l1, u1)
        s2 = size (l2, u2)

instance (Index i) => LineS (Unrolled i e) e where stream = stream . toList

instance (Index i) => Default (Unrolled i e) where def = Z
