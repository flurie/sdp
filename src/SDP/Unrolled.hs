{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module SDP.Unrolled () where

import Prelude ()
import SDP.SafePrelude
import Test.QuickCheck

import GHC.Base
  (
    Array#, Int (..),
    newArray#, unsafeFreezeArray#, writeArray#, indexArray#,
    isTrue#, (+#), (-#), (==#)
  )
import GHC.ST   ( ST (..), runST )

import GHC.Show ( appPrec )

import Text.Read
import Text.Read.Lex ( expect )

import SDP.Array.Mutable
import SDP.Indexed
import SDP.Scan
import SDP.Set

import SDP.UNList
import SDP.Simple

--------------------------------------------------------------------------------

{- Unrolled type section. Free for public use. -}

data Unrolled i e = Unrolled
                  !i                  {- lower  bound -}
                  !i                  {- upper  bound -}
                  {-# UNPACK #-} !Int {-     size     -}
                  (UNList e)          {-  container   -}

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Eq e, Index i) => Eq (Unrolled i e) where (==) = eq1

instance (Index i) => Eq1 (Unrolled i)
  where
    liftEq f unr1 unr2 = (null unr1 && null unr2) || (l1 == l2 && u1 == u2 && n1 == n2 && liftEq f xs ys)
      where
        (Unrolled l1 u1 n1 xs) = unr1
        (Unrolled l2 u2 n2 ys) = unr2

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
    showsPrec p arr@(Unrolled l u _ _) = showParen (p > appPrec) shows'
      where
        shows' = showString "unrolled " . shows (l, u) . showChar ' ' . shows (assocs arr)

instance (Index i, Read i, Read e) => Read (Unrolled i e)
  where
    readList = readListDefault
    readPrec = parens $ prec appPrec (lift . expect $ Ident "unrolled") >> liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances -}

instance (Index i) => Functor (Unrolled i)
  where
    fmap f (Unrolled l u n es) = Unrolled l u n (f <$> es)

-- instance (Index i) => Zip (Unrolled i)

instance (Index i) => Applicative (Unrolled i)
  where
    pure = single
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable, Scan and Traversable instances -}

instance (Index i) => Foldable (Unrolled i)
  where
    foldr  f base (Unrolled _ _ _ es) = foldr  f base es
    foldl  f base (Unrolled _ _ _ es) = foldl  f base es
    
    foldr' f base (Unrolled _ _ _ es) = foldr' f base es
    foldl' f base (Unrolled _ _ _ es) = foldl' f base es
    
    foldr1 f (Unrolled _ _ _ es) = foldr1 f es
    foldl1 f (Unrolled _ _ _ es) = foldl1 f es
    
    length (Unrolled _ _ n  _) = n
    toList (Unrolled _ _ _ es) = toList es
    elem e (Unrolled _ _ _ es) = e `elem` es
    null   (Unrolled l u n es) = null es || n < 1 || isEmpty (l, u)

-- instance (Index i) => Scan (Unrolled i)

instance (Index i) => Traversable (Unrolled i)
  where
    traverse f arr = fromList <$> traverse f (toList arr)

--------------------------------------------------------------------------------

{- Linear and Bordered instances. -}

instance (Index i) => Linear (Unrolled i)
  where
    fromList es = fromListN (length es) es
    
    fromListN n es = Unrolled l u n unlist
      where
        l = unsafeIndex 0
        u = unsafeIndex $ max 0 n - 1
        
        unlist = fromListN n es
    
    uncons Z = throw $ EmptyRange "in SDP.Unrolled.(:>)"
    uncons (Unrolled l u n es) = (x, n < 2 ? Z $ Unrolled l1 u n1 xs)
      where
        (x, xs) = uncons es
        l1 = next (l, u) l
        n1 = n - 1
    
    unsnoc Z = throw $ EmptyRange "in SDP.Unrolled.(:<)"
    unsnoc (Unrolled l u n es) = (n < 2 ? Z $ Unrolled l u1 n1 xs, x)
      where
        (xs, x) = unsnoc es
        u1 = prev (l, u) u
        n1 = n - 1
    
    concat xss = Unrolled l u n res
      where
        (n, res) = foldr (\ (Unrolled _ _ n xs) (len, ys) -> (len + n, xs ++ ys)) (0, Z) xss
        
        l = unsafeIndex 0
        u = unsafeIndex $ max 0 n - 1
    
    -- filter, partition
    
    -- takeWhile, dropWhile, takeEnd, dropEnd, span, break
    
    -- isSubseqOf, isPrefixOf, isSuffixOf, isInfixOf
    
    intersperse e (Unrolled l u n es) = Unrolled l1 u1 n1 (intersperse e es)
      where
        n1 = case n <=> 0 of {LT -> -1; EQ -> 1; GT -> 2 * n - 1}
        u1 = unsafeIndex (n1 - 1)
        l1 = unsafeIndex 0

instance (Index i) => Bordered (Unrolled i) i
  where
    indices (Unrolled l u _  _) = range (l, u)
    bounds  (Unrolled l u _  _) = (l, u)
    lower   (Unrolled l _ _  _) = l
    upper   (Unrolled _ u _  _) = u

--------------------------------------------------------------------------------

instance (Index i) => Indexed (Unrolled i) i
  where
    -- [internal]: it's correct, but completly inneficient (Set []). Rewrite.
    assoc' bnds e ies = fromListN n $ snds ixset
      where
        ixset  = unionWith cmpfst (setWith cmpfst ies) filler
        filler = zip (range bnds) (replicate n e)
        n = size bnds
    
    Z  // []   = Z
    Z  // ascs = assoc (l, u) ascs
      where
        l = minimum $ fsts ascs
        u = maximum $ fsts ascs
    
    (Unrolled l u _ es) // ascs = Unrolled l' u' n es'
      where
        ascs' = (\ (i, e) -> (offset (l, u) i, e)) <$> ascs
        l'    = unsafeIndex $ lower es'
        u'    = unsafeIndex $ upper es'
        n     = size (l', u')
        es'   = es // ascs'
    
    (Unrolled l u _ es)   .! i = es !# (offset (l, u) i)
    
    (!) (Unrolled l u _ es)  i = es !# (offset (l, u) i)
    
    (Unrolled l u _ arrs) !? i = inRange (l, u) i ? Just e $ Nothing
      where
        e = arrs !# offset (l, u) i
    
    predicate .$ (Unrolled l u _ es) = index (l, u) <$> (predicate .$ es)
    predicate *$ (Unrolled l u _ es) = Unrolled l' u' n es'
      where
        es' = index (l, u) <$> (predicate *$ es)
        u'  = index (l, u) (n - 1)
        l'  = index (l, u) 0
        n   = length es'

--------------------------------------------------------------------------------

instance (Index i) => Estimate (Unrolled i)
  where
    (Unrolled _ _ n1 _) <==> (Unrolled _ _ n2 _) = n1 <=> n2

instance (Index i, Arbitrary e) => Arbitrary (Unrolled i e)
  where
    arbitrary = fromList <$> arbitrary

_UNLIST_CHUNK_MAX_SIZE_ :: Int
_UNLIST_CHUNK_MAX_SIZE_ =  1024

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.Unrolled." ++ msg

