{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{-# LANGUAGE TypeFamilies #-}

{- |
    Module      :  SDP.Unrolled
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    This module provides ByteList - strict boxed unrolled linked list.
-}
module SDP.ByteList
(
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Set,
  
  ByteList (..), Ublist
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Set

import Test.QuickCheck

import GHC.Base ( Int  (..) )
import GHC.Show (  appPrec  )
import GHC.ST   ( runST, ST )

import Text.Read hiding ( pfail )
import Text.Read.Lex    ( expect )

import qualified GHC.Exts as E
import Data.String ( IsString (..) )

import SDP.ByteList.Ublist
import SDP.ByteList.ST
import SDP.SortM.Stuff
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | Bordered strict unboxed unrolled linked list.
data ByteList i e = ByteList !i !i (Ublist e)

type role ByteList nominal representational

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Eq e, Unboxed e, Index i) => Eq (ByteList i e)
  where
    (ByteList l1 u1 xs) == (ByteList l2 u2 ys) = s1 == s2 && xs == ys
      where
        s1 = size (l1, u1)
        s2 = size (l2, u2)

instance (Ord e, Unboxed e, Index i) => Ord (ByteList i e)
  where
    compare (ByteList l1 u1 xs) (ByteList l2 u2 ys) = (s1 <=> s2) <> (xs <=> ys)
      where
        s1 = size (l1, u1)
        s2 = size (l2, u2)

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Index i, Show i, Unboxed e, Show e) => Show (ByteList i e)
  where
    showsPrec p es@(ByteList l u _) = showParen (p > appPrec) $ showString "bytelist "
                                                              . shows (l, u)
                                                              . showChar ' '
                                                              . shows (assocs es)

instance (Index i, Read i, Unboxed e, Read e) => Read (ByteList i e)
  where
    readList = readListDefault
    readPrec = parens $ do
      prec appPrec (lift . expect $ Ident "bytelist")
      liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Semigroup, Monoid and Default instances. -}

instance (Index i, Unboxed e) => Semigroup (ByteList i e) where xs <> ys = xs ++ ys

instance (Index i, Unboxed e) => Monoid (ByteList i e) where mempty = def

instance (Index i) => Default (ByteList i e) where def = let (l, u) = unsafeBounds 0 in ByteList l u def

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i, Unboxed e) => Linear (ByteList i e) e
  where
    isNull (ByteList l u bytes) = isEmpty (l, u) || isNull bytes
    
    lzero = def
    
    toHead e (ByteList l u es) = ByteList l' u' (e :> take n es)
      where
        (l', u') = unsafeBounds (n + 1)
        n = size (l, u)
    
    uncons Z = pfail "(:>)"
    uncons (ByteList l u es) = (x, sizeOf es < 2 ? Z $ ByteList l' u xs)
      where
        (x, xs) = uncons es
        l' = next (l, u) l
    
    head Z = pfail "(:>)"
    head (ByteList _ _ es) = head es
    
    tail Z = pfail "(:>)"
    tail (ByteList l u es) = ByteList l' u $ tail es where l' = next (l, u) l
    
    toLast (ByteList l u es) e = ByteList l' u' (take n es :< e)
      where
        (l', u') = unsafeBounds $ n + 1
        n = size (l, u)
    
    unsnoc Z = pfail "(:<)"
    unsnoc (ByteList l u es) = (sizeOf es < 2 ? Z $ ByteList l u' xs, x)
      where
        (xs, x) = unsnoc es
        u' = prev (l, u) u
    
    last Z = pfail "(:<)"
    last (ByteList _ _ es) = last es
    
    init Z = pfail "(:<)"
    init (ByteList l u es) = ByteList l u' $ init es where u' = prev (l, u) u
    
    fromList = fromFoldable
    
    fromFoldable es = let (l, u) = unsafeBounds (length es) in ByteList l u $ fromFoldable es
    
    replicate n e = let (l, u) = unsafeBounds (max 0 n) in ByteList l u $ replicate n e
    
    concat xss = ByteList l' u' res
      where
        res = foldr  (\ (ByteList l u xs) ublist -> size (l, u) `take` xs ++ ublist) Z xss
        n   = foldr' (\ (ByteList l u _) count -> size (l, u) + count) 0 xss
        
        (l', u') = unsafeBounds (max 0 n)
    
    intersperse e (ByteList _ _ es) = ByteList l u $ intersperse e es
      where
        (l, u) = unsafeBounds $ case n <=> 0 of {GT -> 2 * n - 1; _ -> 0}
        n = sizeOf es
    
    Z  ++ ys = ys
    xs ++  Z = xs
    (ByteList l1 u1 xs) ++ (ByteList l2 u2 ys) = ByteList l u $ xs ++ ys
      where
        (l, u) = unsafeBounds $ size (l1, u1) + size (l2, u2)
    
    listL (ByteList l u bytes) = listL $ size (l, u) `take` bytes
    listR (ByteList l u bytes) = listR $ size (l, u) `take` bytes
    
    partitions ps es = fromList <$> partitions ps (listL es)

instance (Index i, Unboxed e) => Split (ByteList i e) e
  where
    {-# INLINE take #-}
    take n xs@(ByteList l u es)
        | n <= 0 = Z
        | n >= c = xs
        |  True  = ByteList l u' (take n es)
      where
        u' = index (l, u) (n - 1)
        c  = size  (l, u)
    
    {-# INLINE drop #-}
    drop n list@(ByteList l u es)
        | n <= 0 = list
        | n >= c = Z
        |  True  = ByteList l' u (drop n es)
      where
        l' = index (l, u) n
        c  = size  (l, u)
    
    isPrefixOf xs ys = listL xs `isPrefixOf` listL ys
    isInfixOf  xs ys = listL xs `isInfixOf`  listL ys
    isSuffixOf xs ys = listL xs `isSuffixOf` listL ys
    
    prefix p (ByteList l u es) = prefix p es `min` size (l, u)
    suffix p (ByteList l u es) = prefix p es `min` size (l, u)

instance (Index i, Unboxed e) => Bordered (ByteList i e) i e
  where
    sizeOf (ByteList l u _) = size (l, u)
    bounds (ByteList l u _) = (l, u)
    lower  (ByteList l _ _) = l
    upper  (ByteList _ u _) = u

--------------------------------------------------------------------------------

{- Indexed, IFold, Set and Sort instances. -}

instance (Index i, Unboxed e) => Indexed (ByteList i e) i e
  where
    {-# INLINE assoc #-}
    assoc (l, u) ascs = ByteList l u $ assoc bnds ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    {-# INLINE assoc' #-}
    assoc' (l, u) defvalue ascs = ByteList l u $ assoc' bnds defvalue ies
      where
        ies  = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
        bnds = (0, size (l, u) - 1)
    
    {-# INLINE (//) #-}
    Z  // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    (ByteList l u es) // ascs = ByteList l' u' es'
      where
        es' = es // [ (offset (l, u) i, e) | (i, e) <- ascs ]
        (l', u') = unsafeBounds $ sizeOf es'
    
    fromIndexed es = let (l, u) = unsafeBounds (sizeOf es) in ByteList l u (fromIndexed es)
    
    (ByteList _ _ es) !^ i = es ! i
    
    {-# INLINE (!) #-}
    (!) (ByteList l u es) i = es ! offset (l, u) i
    
    p .$ (ByteList l u es) = index (l, u) <$> p .$ es
    p *$ (ByteList l u es) = index (l, u) <$> p *$ es

instance (Index i, Unboxed e) => IFold (ByteList i e) i e
  where
    ifoldr f base (ByteList l u es) = ifoldr (\ i -> f (index (l, u) i)) base $ size (l, u) `take` es
    ifoldl f base (ByteList l u es) = ifoldl (\ i -> f (index (l, u) i)) base $ size (l, u) `take` es
    
    i_foldr f base (ByteList l u es) = i_foldr f base $ size (l, u) `take` es
    i_foldl f base (ByteList l u es) = i_foldl f base $ size (l, u) `take` es

instance (Index i, Unboxed e) => Set (ByteList i e) e
  where
    setWith f (ByteList _ _ es) = ByteList l u es'
      where
        es'    = setWith f es
        (l, u) = unsafeBounds (sizeOf es')
    
    intersectionWith f (ByteList _ _ xs) (ByteList _ _ ys) = ByteList l u es
      where
        es     = intersectionWith f xs ys
        (l, u) = unsafeBounds (sizeOf es)
    
    unionWith f (ByteList _ _ xs) (ByteList _ _ ys) = ByteList l u es
      where
        es     = unionWith f xs ys
        (l, u) = unsafeBounds (sizeOf es)
    
    differenceWith f (ByteList _ _ xs) (ByteList _ _ ys) = ByteList l u es
      where
        es     = differenceWith f xs ys
        (l, u) = unsafeBounds (sizeOf es)
    
    symdiffWith f (ByteList _ _ xs) (ByteList _ _ ys) = ByteList l u es
      where
        es     = symdiffWith f xs ys
        (l, u) = unsafeBounds (sizeOf es)
    
    insertWith f e (ByteList _ _ es) = ByteList l u es'
      where
        es'    = insertWith f e es
        (l, u) = unsafeBounds (sizeOf es')
    
    deleteWith f e (ByteList _ _ es) = ByteList l u es'
      where
        es'    = deleteWith f e es
        (l, u) = unsafeBounds (sizeOf es')
    
    isContainedIn f e (ByteList _ _ es) = isContainedIn f e es
    
    isSubsetWith f (ByteList _ _ xs) (ByteList _ _ ys) = isSubsetWith f xs ys

instance (Index i, Unboxed e) => Sort (ByteList i e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

instance (Index i, Unboxed e) => E.IsList (ByteList i e)
  where
    type Item (ByteList i e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = listL

instance (Index i) => IsString (ByteList i Char) where fromString = fromList

instance (Index i, Unboxed e, Arbitrary e) => Arbitrary (ByteList i e) where arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

instance (Index i, Unboxed e) => Thaw (ST s) (ByteList i e) (STByteList s i e)
  where
    thaw (ByteList l u es) = STByteList l u <$> thaw es

instance (Index i, Unboxed e) => Freeze (ST s) (STByteList s i e) (ByteList i e)
  where
    freeze (STByteList l u es) = ByteList l u <$> freeze es

--------------------------------------------------------------------------------

done :: (Index i, Unboxed e) => STByteList s i e -> ST s (ByteList i e)
done = freeze

pfail :: String -> a
pfail msg = throw . PatternMatchFail $ "in SDP.ByteList." ++ msg

