{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{-# LANGUAGE TypeFamilies #-}

{- |
    Module      :  SDP.Bytes
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    SDP.Bytes provides immutable strict unboxed array type.
    This implementation of UArray no much different from Data.Array.Unboxed (array),
    but incopatible with it.
    The main difference is the Index class instead of Ix.
-}
module SDP.Bytes
(
  module SDP.Unboxed,
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Set,
  
  Bytes (..)
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Set

import Text.Read
import Text.Read.Lex ( expect )

import GHC.Base ( Int (..), ByteArray#, newByteArray#, unsafeFreezeByteArray# )
import GHC.Show ( appPrec )

import GHC.ST   ( runST, ST (..) )

import qualified GHC.Exts as E
import Data.String ( IsString (..) )

import SDP.SortM.Stuff
import SDP.Bytes.ST

import SDP.Simple

default ()

--------------------------------------------------------------------------------

{- |
  This Bytes type definition is no different from the standard Data.Array.Unboxed,
  but I have to redefine it because of the limitation of the Ix class.
-}

data Bytes i e = Bytes !i !i {-# UNPACK #-} !Int ByteArray#

type role Bytes nominal representational

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Index i, Unboxed e) => Eq (Bytes i e)
  where
    Z == Z  = True
    xs == ys = l1 == l2 && u1 == u2 && n1 == n2 && eq' 0
      where
        !(Bytes l1 u1 n1 _) = xs
        !(Bytes l2 u2 n2 _) = ys
        
        eq' i = i == n1 || (xs !^ i == ys !^ i) && eq' (i + 1)

instance (Index i, Unboxed e, Ord e) => Ord (Bytes i e)
  where
    compare Z  Z  = EQ
    compare xs ys = (n1 <=> n2) <> cmp' 0
      where
        cmp' i = if i == n1 then EQ else (xs !^ i <=> ys !^ i) <> cmp' (i + 1)
        n1 = sizeOf xs
        n2 = sizeOf ys

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Index i, Show i, Unboxed e, Show e) => Show (Bytes i e)
  where
    showsPrec p es@(Bytes l u _ _) = showParen (p > appPrec) $ showString "bytes "
                                                             . shows (l, u)
                                                             . showChar ' '
                                                             . shows (assocs es)

instance (Index i, Read i, Unboxed e, Read e) => Read (Bytes i e)
  where
    readList = readListDefault
    readPrec = parens $ do
      prec appPrec (lift . expect $ Ident "bytes")
      liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Semigroup, Monoid and Default instances. -}

instance (Index i, Unboxed e) => Semigroup (Bytes i e) where (<>) = (++)

instance (Index i, Unboxed e) => Monoid (Bytes i e) where mempty = def

instance (Index i) => Default (Bytes i e)
  where
    def = let (l, u) = unsafeBounds 0 in runST $ ST $
      \ s1# -> case newByteArray# 0# s1# of
        (# s2#, marr# #) -> case unsafeFreezeByteArray# marr# s2# of
          (# s3#, bytes# #) -> (# s3#, Bytes l u 0 bytes# #)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Unboxed e, Index i) => Linear (Bytes i e) e
  where
    isNull (Bytes l u n _) = isEmpty (l, u) || n < 1
    
    lzero = def
    
    head Z  = pfailEx "(:>)"
    head es = es .! lower es
    
    tail Z  = pfailEx "(:>)"
    tail es = drop 1 es
    
    last Z  = pfailEx "(:<)"
    last es = es .! upper es
    
    init Z  = pfailEx "(:<)"
    init es = take (sizeOf es - 1) es
    
    {-# INLINE single #-}
    single e = let (l, u) = unsafeBounds 1 in runST $ ST $
      \ s1# -> case newUnboxed' e 1# s1# of
        (# s2#, marr# #) -> case unsafeFreezeByteArray# marr# s2# of
          (# s3#, bytes# #) -> (# s3#, Bytes l u 1 bytes# #)
    
    fromList = fromFoldable
    
    {-# INLINE fromFoldable #-}
    fromFoldable es = runST $ fromFoldableM es >>= done
    
    Z  ++ ys = ys
    xs ++  Z = xs
    xs ++ ys = fromList $ listL xs ++ listL ys
    
    {-# INLINE replicate #-}
    replicate n e = runST $ filled n e >>= done
    
    listL (Bytes _ _ n bytes#) = [ bytes# !# i# | (I# i#) <- [0 .. n - 1] ]
    listR (Bytes _ _ n bytes#) = [ bytes# !# i# | (I# i#) <- [n - 1, n - 2 .. 0] ]
    
    {-# INLINE concatMap #-}
    concatMap f ess = fromList $ foldr (\ a l -> listL (f a) ++ l) [] ess
    
    {-# INLINE concat #-}
    concat ess = fromList $ foldr (\ a l -> listL a ++ l) [] ess

instance (Index i, Unboxed e) => Split (Bytes i e) e
  where
    {-#  INLINE take #-}
    take n es
        | n <= 0 = Z
        | n >= l = es
        |  True  = fromList [ es !^ i | i <- [ 0 .. n - 1 ] ]
      where
        l = sizeOf es
    
    {-# INLINE drop #-}
    drop n es
        | n <= 0 = es
        | n >= l = Z
        |  True  = fromListN (l - n) [ es !^ i | i <- [ n .. l - 1 ] ]
      where
        l = sizeOf es
    
    isPrefixOf xs ys = n1 <= n2 && equals 0
      where
        equals i = i == n1 || (xs !^ i == ys !^ i) && equals (i + 1)
        n1 = sizeOf xs
        n2 = sizeOf ys
    
    isSuffixOf xs ys = n1 <= n2 && equals 0 o
      where
        equals i j = i == n1 || (xs !^ i == ys !^ j) && equals (i + 1) (j + 1)
        o  = max 0 (n2 - n1)
        n1 = sizeOf xs
        n2 = sizeOf ys
    
    {-# INLINE prefix #-}
    -- see SDP.Linear.suffix and SDP.Array.foldr
    prefix p (Bytes _ _ n arr#) = go 0
      where
        go i@(I# i#) = n' == i ? 0 $ p (arr# !# i#) ? (go $ i + 1) + 1 $ 0
        n' = max 0 n
    
    {-# INLINE suffix #-}
    -- see SDP.Linear.suffix and SDP.Array.foldl
    suffix p (Bytes _ _ n arr#) = go $ max 0 n - 1
      where
        go i@(I# i#) = -1 == i ? 0 $ p (arr# !# i#) ? (go $ i - 1) + 1 $ 0

instance (Index i, Unboxed e) => Bordered (Bytes i e) i e
  where
    sizeOf (Bytes _ _ n _) = max 0 n
    bounds (Bytes l u _ _) = (l, u)
    lower  (Bytes l _ _ _) = l
    upper  (Bytes _ u _ _) = u

--------------------------------------------------------------------------------

{- Indexed, Set and Sort instances. -}

instance (Index i, Unboxed e) => Indexed (Bytes i e) i e
  where
    {-# INLINE assoc #-}
    assoc bnds ascs = runST $ fromAssocs bnds ascs >>= done
    
    {-# INLINE assoc' #-}
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    {-# INLINE (//) #-}
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    arr // ascs = runST $ newLinear (listL arr) >>= (`overwrite` ascs) >>= done
    
    fromIndexed es = runST $ do
        copy <- filled_ n (unreachEx "fromIndexed")
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        done copy
      where
        n = sizeOf es
    
    {-# INLINE (!^) #-}
    (Bytes _ _ _ bytes#) !^ (I# i#) = bytes# !# i#
    
    {-# INLINE (!) #-}
    (!) (Bytes l u _ bytes#) i = case offset (l, u) i of (I# i#) -> bytes# !# i#
    
    p .$ es@(Bytes l u _ _) = index (l, u) <$> p .$ listL es
    p *$ es@(Bytes l u _ _) = index (l, u) <$> p *$ listL es

instance (Index i, Unboxed e) => Set (Bytes i e) e
  where
    setWith f es = nubSorted f $ sortBy f es
    
    insertWith _ e Z  = single e
    insertWith f e es = isContainedIn f e es ? es $ res
      where
        res = fromList . insertWith f e $ listL es
    
    deleteWith _ _ Z  = Z
    deleteWith f e es = isContainedIn f e es ? es $ res
      where
        res = fromList . deleteWith f e $ listL es
    
    intersectionWith f xs ys = fromList $ intersection' 0 0
      where
        intersection' i j = i == n1 || j == n2 ? [] $ case x `f` y of
            LT -> intersection' (i + 1) j
            EQ -> x : intersection' (i + 1) (j + 1)
            GT -> intersection' i (j + 1)
          where
            x = xs !^ i; n1 = sizeOf xs
            y = ys !^ j; n2 = sizeOf ys
    
    unionWith f xs ys = fromList $ union' 0 0
      where
        union' i j
          | i == n1 = (ys !^) <$> [j .. n2 - 1]
          | j == n2 = (xs !^) <$> [i .. n1 - 1]
          |  True   = case x `f` y of
            LT -> x : union' (i + 1) j
            EQ -> x : union' (i + 1) (j + 1)
            GT -> y : union' i (j + 1)
          where
            x = xs !^ i; n1 = sizeOf xs
            y = ys !^ j; n2 = sizeOf ys
    
    differenceWith f xs ys = fromList $ difference' 0 0
      where
        difference' i j
            | i == n1 = []
            | j == n2 = (xs !^) <$> [i .. n1 - 1]
            |  True   = case x `f` y of
              LT -> x : difference' (i + 1) j
              EQ -> difference' (i + 1) (j + 1)
              GT -> difference' i (j + 1)
          where
            x = xs !^ i; n1 = sizeOf xs
            y = ys !^ j; n2 = sizeOf ys
    
    symdiffWith f xs ys = fromList $ symdiff' 0 0
      where
        n1 = sizeOf xs; n2 = sizeOf ys
        symdiff' i j
            | i == n1 = (ys !^) <$> [j .. n2 - 1]
            | j == n2 = (xs !^) <$> [i .. n1 - 1]
            |  True   = case x `f` y of
              LT -> x : symdiff' (i + 1) j
              EQ -> symdiff' (i + 1) (j + 1)
              GT -> y : symdiff' i (j + 1)
          where
            x = xs !^ i; y = ys !^ j
    
    isContainedIn _ _ Z  = False
    isContainedIn f e es = contained' 0
      where
        contained' i = i == sizeOf es ? False $ case e `f` (es !^ i) of
          LT -> False
          EQ -> True
          GT -> contained' (i + 1)
    
    isSubsetWith f xs ys = all (\ x -> isContainedIn f x ys) (listL xs)

instance (Index i, Unboxed e) => Sort (Bytes i e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

instance (Index i, Unboxed e) => E.IsList (Bytes i e)
  where
    type Item (Bytes i e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = listL

instance (Index i) => IsString (Bytes i Char) where fromString = fromList

instance (Index i, Unboxed e, Arbitrary e) => Arbitrary (Bytes i e) where arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

instance (Index i, Unboxed e) => Thaw (ST s) (Bytes i e) (STBytes s i e)
  where
    thaw es@(Bytes l u n _) = ST $ \ s1# -> case rep s1# of
        (# s2#, es'@(STBytes _ _ _ marr#) #) -> (# s2#, (STBytes l u n marr#) `asTypeOf` es' #)
      where
        (ST rep) = newLinear (listL es)

instance (Index i, Unboxed e) => Freeze (ST s) (STBytes s i e) (Bytes i e)
  where
    freeze = done

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: (Index i) => STBytes s i e -> ST s (Bytes i e)
done (STBytes l u n mbytes#) = ST $ \ s1# -> case unsafeFreezeByteArray# mbytes# s1# of
  (# s2#, bytes# #) -> (# s2#, Bytes l u n bytes# #)

nubSorted :: (Index i, Unboxed e) => (e -> e -> Ordering) -> Bytes i e -> Bytes i e
nubSorted _ Z  = Z
nubSorted f es = fromList $ foldr fun [last es] ((es !^) <$> [0 .. sizeOf es - 2])
  where
    fun = \ e ls -> e `f` head ls == EQ ? ls $ e : ls

filled_ :: (Index i, Unboxed e) => Int -> e -> ST s (STBytes s i e)
filled_ n@(I# n#) e = ST $ \ s1# -> case newUnboxed e n# s1# of
    (# s2#, marr# #) -> (# s2#, STBytes l u n marr# #)
  where
    (l, u) = unsafeBounds n

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.Bytes." ++ msg

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.Bytes." ++ msg




