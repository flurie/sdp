{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, TypeFamilies, RoleAnnotations #-}

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
  module SDP.Indexed,
  module SDP.Scan,
  module SDP.Set,
  
  Unrolled (..), Unlist
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed
import SDP.Scan
import SDP.Set

import Test.QuickCheck

import GHC.Base ( Int (..) )
import GHC.Show ( appPrec  )
import GHC.ST   (    ST    )

import Text.Read hiding ( pfail )
import Text.Read.Lex    ( expect )

import qualified GHC.Exts as E
import Data.String ( IsString (..) )

import SDP.Unrolled.Unlist
import SDP.Unrolled.ST
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
    liftEq eq (Unrolled l1 u1 xs) (Unrolled l2 u2 ys) = s1 == s2 && liftEq eq xs ys
      where
        s1 = size (l1, u1)
        s2 = size (l2, u2)

--------------------------------------------------------------------------------

{- Ord and Ord1 innstances. -}

instance (Ord e, Index i) => Ord (Unrolled i e) where compare = compare1

instance (Index i) => Ord1 (Unrolled i)
  where
    liftCompare cmp (Unrolled l1 u1 xs) (Unrolled l2 u2 ys) = (size (l1, u1) <=> size (l2, u2)) <> liftCompare cmp xs ys

--------------------------------------------------------------------------------

{- Show and Read instances -}

instance (Index i, Show i, Show e) => Show (Unrolled i e)
  where
    showsPrec p unr@(Unrolled l u _) = showParen (p > appPrec) $ showString "unrolled "
                                                               . shows (l, u)
                                                               . showChar ' '
                                                               . shows (assocs unr)

instance (Index i, Read i, Read e) => Read (Unrolled i e)
  where
    readList = readListDefault
    readPrec = parens $ do
      prec appPrec (lift . expect $ Ident "unrolled")
      liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Semigroup, Monoid and Default instances. -}

instance (Index i) => Semigroup (Unrolled i e) where xs <> ys = xs ++ ys

instance (Index i) => Monoid (Unrolled i e) where mempty = def

instance (Index i) => Default (Unrolled i e) where def = Z

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances -}

instance (Index i) => Functor (Unrolled i) where fmap f (Unrolled l u es) = Unrolled l u (f <$> es)

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
    {-# INLINE foldr #-}
    foldr  f base (Unrolled l u es) = foldr  f base $ size (l, u) `take` es
    
    {-# INLINE foldl #-}
    foldl  f base (Unrolled l u es) = foldl  f base $ size (l, u) `take` es
    
    {-# INLINE foldr' #-}
    foldr' f base (Unrolled l u es) = foldr' f base $ size (l, u) `take` es
    
    {-# INLINE foldl' #-}
    foldl' f base (Unrolled l u es) = foldl' f base $ size (l, u) `take` es
    
    {-# INLINE foldr1 #-}
    foldr1 f (Unrolled l u es) = foldr1 f $ size (l, u) `take` es
    
    {-# INLINE foldl1 #-}
    foldl1 f (Unrolled l u es) = foldl1 f $ size (l, u) `take` es
    
    elem e   (Unrolled l u es) = elem e $ size (l, u) `take` es
    toList   (Unrolled l u es) = size (l, u) `take` toList es
    null     (Unrolled l u es) = isEmpty (l, u) || null es
    length   (Unrolled l u  _) = size (l, u)

-- instance (Index i) => Scan (Unrolled i)

instance (Index i) => Traversable (Unrolled i)
  where
    traverse f arr = fromList <$> foldr (\ x ys -> liftA2 (:) (f x) ys) (pure []) arr

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i) => Linear (Unrolled i e) e
  where
    isNull es = null es
    
    lzero = def
    
    uncons Z = pfail "(:>)"
    uncons (Unrolled l u es) = (x, es <. 2 ? Z $ Unrolled l1 u xs)
      where
        (x, xs) = uncons es
        l1 = next (l, u) l
    
    head Z = pfail "(:>)"
    head (Unrolled _ _ es) = head es
    
    tail Z = pfail "(:>)"
    tail (Unrolled l u es) = Unrolled l' u $ tail es where l' = next (l, u) l
    
    unsnoc Z = pfail "(:<)"
    unsnoc (Unrolled l u es) = (es <. 2 ? Z $ Unrolled l u1 xs, x)
      where
        (xs, x) = unsnoc es
        u1 = prev (l, u) u
    
    last Z = pfail "(:<)"
    last (Unrolled _ _ es) = last es
    
    init Z = pfail "(:<)"
    init (Unrolled l u es) = Unrolled l u' $ init es where u' = prev (l, u) u
    
    fromList = fromFoldable
    
    fromFoldable es = let (l, u) = unsafeBounds (length es) in Unrolled l u $ fromFoldable es
    
    Z  ++ ys = ys
    xs ++  Z = xs
    (Unrolled l1 u1 xs) ++ (Unrolled l2 u2 ys) = Unrolled l u $ xs ++ ys
      where
        (l, u) = unsafeBounds $ size (l1, u1) + size (l2, u2)
    
    concat xss = Unrolled l' u' res
      where
        res = foldr (\ (Unrolled l u xs) ys -> size (l, u) `take` xs ++ ys) Z xss
        n   = foldr (\ es count -> sizeOf es + count) 0 xss
        
        (l', u') = unsafeBounds n
    
    intersperse e (Unrolled _ _ es) = Unrolled l1 u1 (intersperse e es)
      where
        (l1, u1) = unsafeBounds $ case n <=> 0 of {GT -> 2 * n - 1; _ -> 0}
        n = length es
    
    listL  (Unrolled l u bytes) = listL $ size (l, u) `take` bytes
    listR  (Unrolled l u bytes) = listR $ size (l, u) `take` bytes
    
    partitions ps es = fromList <$> partitions ps (toList es)

instance (Index i) => Split (Unrolled i e) e
  where
    {-# INLINE take #-}
    take n unr@(Unrolled l u es)
        | n <= 0 = Z
        | n >= c = unr
        |  True  = Unrolled l u' (take n es)
      where
        u' = index (l, u) (n - 1)
        c  = size  (l, u)
    
    {-# INLINE drop #-}
    drop n unr@(Unrolled l u es)
        | n <= 0 = unr
        | n >= c = Z
        |  True  = Unrolled l' u (drop n es)
      where
        l' = index (l, u) n
        c  = size  (l, u)
    
    {-# INLINE split #-}
    split n unr@(Unrolled l u es)
        | n <= 0 = (Z, unr)
        | n >= c = (unr, Z)
        |  True  = (Unrolled l u' take', Unrolled l' u drop')
      where
        u' = index (l, u) $ n - 1
        l' = index (l, u)   n
        c  = size  (l, u)
        
        (take', drop') = split n es
    
    isPrefixOf xs ys = toList xs `isPrefixOf` toList ys
    isInfixOf  xs ys = toList xs `isInfixOf`  toList ys
    isSuffixOf xs ys = toList xs `isSuffixOf` toList ys
    
    prefix p (Unrolled l u es) = prefix p es `min` size (l, u)
    suffix p (Unrolled l u es) = prefix p es `min` size (l, u)

instance (Index i) => Bordered (Unrolled i e) i e
  where
    sizeOf (Unrolled l u _) = size (l, u)
    bounds (Unrolled l u _) = (l, u)
    lower  (Unrolled l _ _) = l
    upper  (Unrolled _ u _) = u

--------------------------------------------------------------------------------

instance (Index i) => Indexed (Unrolled i e) i e
  where
    {-# INLINE assoc' #-}
    assoc' (l, u) defvalue ascs = Unrolled l u $ assoc' bnds defvalue ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    {-# INLINE (//) #-}
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    (Unrolled l u es) // ascs = Unrolled l' u' es'
      where
        es' = es // [ (offset (l, u) i, e) | (i, e) <- ascs ]
        (l', u') = unsafeBounds $ length es'
    
    fromIndexed es = let (l, u) = unsafeBounds (sizeOf es) in Unrolled l u (fromIndexed es)
    
    {-# INLINE (!^) #-}
    (Unrolled _ _ es) !^ i = es !^ i
    
    {-# INLINE (!) #-}
    (!) (Unrolled l u es) i = es ! offset (l, u) i
    
    p .$ (Unrolled l u es) = index (l, u) <$> p .$ es
    p *$ (Unrolled l u es) = index (l, u) <$> p *$ es

--------------------------------------------------------------------------------

instance (Index i) => E.IsList (Unrolled i e)
  where
    type Item (Unrolled i e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = toList

instance (Index i) => IsString (Unrolled i Char) where fromString = fromList

instance (Index i) => Estimate (Unrolled i) where xs <==> ys = length xs <=> length ys

instance (Index i, Arbitrary e) => Arbitrary (Unrolled i e) where arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

instance (Index i) => Thaw (ST s) (Unrolled i e) (STUnrolled s i e)
  where
    thaw (Unrolled l u es) = STUnrolled l u <$> thaw es

instance (Index i) => Freeze (ST s) (STUnrolled s i e) (Unrolled i e)
  where
    freeze (STUnrolled l u es) = Unrolled l u <$> freeze es

--------------------------------------------------------------------------------

pfail :: String -> a
pfail msg = throw . PatternMatchFail $ "in SDP.Unrolled." ++ msg


