{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.Unrolled
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    Stability   :  stable
    
    This module provides ByteList - strict boxed unrolled linked list.
-}

module SDP.ByteList
(
  ByteList (..),
  Ublist, -- type Ublist exported as abstract.
  
  module SDP.Indexed,
  module SDP.Set
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.Indexed
import SDP.Unboxed
import SDP.Set

import GHC.Base ( Int (..) )
import GHC.Show ( appPrec )

import Text.Read
import Text.Read.Lex ( expect )

import SDP.ByteList.Ublist
import SDP.Simple

--------------------------------------------------------------------------------

-- | Bordered strict unboxed unrolled linked list.
data ByteList i e = ByteList !i !i (Ublist e)

type role ByteList nominal representational

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Eq e,  Unboxed e, Index i) => Eq  (ByteList i e)
  where
    xs == ys = (assocs xs) == (assocs ys)

instance (Ord e, Unboxed e, Index i) => Ord (ByteList i e)
  where
    compare xs ys = (assocs xs) <=> (assocs ys)

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Index i, Show i, Unboxed e, Show e) => Show (ByteList i e)
  where
    showsPrec p unr@(ByteList l u _) = showParen (p > appPrec) shows'
      where
        shows' = showString "bytelist " . shows (l, u) . showChar ' ' . shows (assocs unr)

instance (Index i, Read i, Unboxed e, Read e) => Read (ByteList i e)
  where
    readList = readListDefault
    readPrec = parens $ prec appPrec (lift . expect $ Ident "bytelist") >> liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i, Unboxed e) => Linear (ByteList i e) e
  where
    isNull (ByteList l u bytes) = isEmpty (l, u) || isNull bytes
    
    uncons Z = throw $ EmptyRange "in SDP.ByteList.(:>)"
    uncons (ByteList l u es) = (x, sizeOf es < 2 ? Z $ ByteList l1 u xs)
      where
        (x, xs) = uncons es
        l1 = next (l, u) l
    
    unsnoc Z = throw $ EmptyRange "in SDP.ByteList.(:<)"
    unsnoc (ByteList l u es) = (sizeOf es < 2 ? Z $ ByteList l u1 xs, x)
      where
        (xs, x) = unsnoc es
        u1 = prev (l, u) u
    
    fromListN n es = ByteList l u $ fromListN n es
      where
        (l, u) = unsafeBounds $ max 0 n
    
    replicate n e = ByteList l u $ replicate n e
      where
        (l, u) = unsafeBounds $ max 0 n
    
    concat xss = ByteList l u res
      where
        res = foldr  (\ (ByteList l' u' xs) ublist -> take (size (l', u')) xs ++ ublist) Z xss
        n   = foldr' (\ (ByteList l' u' _) count -> size (l', u') + count) 0 xss
        
        (l, u) = unsafeBounds $ max 0 n
    
    intersperse e (ByteList _ _ es) = ByteList l1 u1 $ intersperse e es
      where
        n1 = case n <=> 0 of {LT -> -1; EQ -> 1; GT -> 2 * n - 1}; n = sizeOf es
        
        (l1, u1) = unsafeBounds $ max 0 n1
    
    listL  (ByteList l u bytes) = listL $ take (size (l, u)) bytes
    listR  (ByteList l u bytes) = listR $ take (size (l, u)) bytes

instance (Index i, Unboxed e) => Split (ByteList i e) e
  where
    prefix p (ByteList l u es) = min (prefix p es) $ size (l, u)
    suffix p (ByteList l u es) = min (prefix p es) $ size (l, u)
    
    take n list@(ByteList l u es)
        |      n <= 0      = Z
        | n >= size (l, u) = list
        |       True       = ByteList l u' $ take n es
      where
        u' = prev (l, u) u
    
    drop n list@(ByteList l u es)
        | n <= 0 = list
        | n >= size (l, u) = Z
        | True = ByteList l' u $ drop n es
      where
        l' = next (l, u) l

instance (Index i, Unboxed e) => Bordered (ByteList i e) i e
  where
    sizeOf  (ByteList l u _) = size (l, u)
    lower   (ByteList l _ _) = l
    upper   (ByteList _ u _) = u

--------------------------------------------------------------------------------

instance (Index i, Unboxed e) => Indexed (ByteList i e) i e
  where
    -- [internal]: it's correct, but completly inneficient (Set []), rewrite.
    assoc' bnds e ies = fromListN n sorted
      where
        sorted = snds $ unionWith cmpfst (setWith cmpfst ies) filler
        filler = zip (range bnds) (replicate n e)
        n = size bnds
    
    Z  // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    (ByteList l u es) // ascs = ByteList l' u' es'
      where
        es' = es // [ (offset (l, u) i, e) | (i, e) <- ascs ]
        
        (l', u') = unsafeBounds $ sizeOf es'
    
    (!)  (ByteList l u es) i = es ! (offset (l, u) i)
    p .$ (ByteList l u es)   = index (l, u) <$> (p .$ es)
    p *$ (ByteList l u es)   = index (l, u) <$> (p *$ es)

--------------------------------------------------------------------------------

instance (Index i, Unboxed e, Arbitrary e) => Arbitrary (ByteList i e) where arbitrary = fromList <$> arbitrary

instance (Index i) => Default (ByteList i e) where def = case unsafeBounds 0 of (l, u) -> ByteList l u def

