{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns #-}
{-# LANGUAGE RoleAnnotations, TypeFamilies #-}

{- |
    Module      :  SDP.Bytes
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    SDP.Bytes provides 'Bytes' - immutable strict unboxed array type.
    This implementation of UArray no much different from @Data.Array.Unboxed@
    (array), but incopatible with it.
-}
module SDP.Bytes
(
  -- * Exports
  module SDP.Unboxed,
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Set,
  
  -- * Bytes
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

import SDP.SortM.Tim
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
    xs == ys =
     let eq' i = i ==. xs || (xs !^ i == ys !^ i) && eq' (i + 1)
     in  bounds xs == bounds ys && xs .==. ys && eq' 0

instance (Index i, Unboxed e, Ord e) => Ord (Bytes i e)
  where
    compare Z  Z  = EQ
    compare xs ys =
      let cmp' i = i ==. xs ? EQ $ (xs !^ i <=> ys !^ i) <> cmp' (i + 1)
      in  (xs <==> ys) <> cmp' 0

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

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance (Index i, Unboxed e) => Semigroup (Bytes i e) where (<>) = (++)
instance (Index i, Unboxed e) => Monoid    (Bytes i e) where mempty = def

instance (Index i) => Default (Bytes i e)
  where
    def = let (l, u) = defaultBounds 0 in runST $ ST $
      \ s1# -> case newByteArray# 0# s1# of
        (# s2#, marr# #) -> case unsafeFreezeByteArray# marr# s2# of
          (# s3#, bytes# #) -> (# s3#, Bytes l u 0 bytes# #)

instance (Index i, Unboxed e, Arbitrary e) => Arbitrary (Bytes i e)
  where
    arbitrary = fromList <$> arbitrary

instance Estimate (Bytes i e)
  where
    (Bytes _ _ n1 _) <==> (Bytes _ _ n2 _) = n1 <=> n2
    (Bytes _ _ n1 _) .>.  (Bytes _ _ n2 _) = n1  >  n2
    (Bytes _ _ n1 _) .<.  (Bytes _ _ n2 _) = n1  <  n2
    (Bytes _ _ n1 _) .<=. (Bytes _ _ n2 _) = n1 <=  n2
    (Bytes _ _ n1 _) .>=. (Bytes _ _ n2 _) = n1 >=  n2
    
    (Bytes _ _ n1 _) <.=> n2 = n1 <=> n2
    (Bytes _ _ n1 _)  .>  n2 = n1  >  n2
    (Bytes _ _ n1 _)  .<  n2 = n1  <  n2
    (Bytes _ _ n1 _) .>=  n2 = n1 >=  n2
    (Bytes _ _ n1 _) .<=  n2 = n1 <=  n2

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Unboxed e, Index i) => Linear (Bytes i e) e
  where
    isNull (Bytes l u n _) = isEmpty (l, u) || n < 1
    
    lzero = def
    
    toHead e es = fromListN (sizeOf es + 1) (e : listL es)
    
    head Z  = pfailEx "(:>)"
    head es = es .! lower es
    
    tail Z  = pfailEx "(:>)"
    tail es = drop 1 es
    
    toLast es e = fromListN (sizeOf es + 1) $ i_foldr (:) [e] es
    
    last Z  = pfailEx "(:<)"
    last es = es .! upper es
    
    init Z  = pfailEx "(:<)"
    init es = take (sizeOf es - 1) es
    
    {-# INLINE single #-}
    single e = let (l, u) = defaultBounds 1 in runST $ ST $
      \ s1# -> case newUnboxed' e 1# s1# of
        (# s2#, marr# #) -> case unsafeFreezeByteArray# marr# s2# of
          (# s3#, bytes# #) -> (# s3#, Bytes l u 1 bytes# #)
    
    fromList = fromFoldable
    
    {-# INLINE fromListN #-}
    fromListN n es = runST $ newLinearN n es >>= done
    
    {-# INLINE fromFoldable #-}
    fromFoldable es = runST $ fromFoldableM es >>= done
    
    Z  ++ ys = ys
    xs ++  Z = xs
    xs ++ ys = fromList $ i_foldr (:) (listL ys) xs
    
    {-# INLINE replicate #-}
    replicate n e = runST $ filled n e >>= done
    
    listL (Bytes _ _ n bytes#) = [ bytes# !# i# | (I# i#) <- [0 .. n - 1] ]
    listR (Bytes _ _ n bytes#) = [ bytes# !# i# | (I# i#) <- [n - 1, n - 2 .. 0] ]
    
    {-# INLINE concatMap #-}
    concatMap f = fromList . foldr (\ a l -> listL (f a) ++ l) []
    
    {-# INLINE concat #-}
    concat = fromList . foldr (\ a l -> listL a ++ l) []
    
    partitions f es = fromList <$> partitions f (listL es)

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
        |  n <= 0  = es
        | n >=. es = Z
        |   True   = fromListN (l - n) [ es !^ i | i <- [ n .. l - 1 ] ]
      where
        l = sizeOf es
    
    splits ns es = fromList <$> splits ns (listL es)
    chunks ns es = fromList <$> chunks ns (listL es)
    parts  ns es = fromList <$> parts  ns (listL es)
    
    isPrefixOf xs ys =
      let equals i = i ==. xs || (xs !^ i) == (ys !^ i) && equals (i + 1)
      in  xs .<=. ys && equals 0
    
    isSuffixOf xs ys =
      let equals i j = i ==. xs || (xs !^ i) == (ys !^ j) && equals (i + 1) (j + 1)
      in  xs .<=. ys && equals 0 (max 0 $ sizeOf ys - sizeOf xs)
    
    {-# INLINE prefix #-}
    prefix p = i_foldr (\ e c -> p e ? c + 1 $ 0) 0
    
    {-# INLINE suffix #-}
    suffix p = i_foldl (\ c e -> p e ? c + 1 $ 0) 0

instance (Index i, Unboxed e) => Bordered (Bytes i e) i e
  where
    {-# INLINE indexIn #-}
    indexIn (Bytes l u _ _) = inRange (l, u)
    
    {-# INLINE indexOf #-}
    indexOf (Bytes l u _ _) = index (l, u)
    
    {-# INLINE offsetOf #-}
    offsetOf (Bytes l u _ _) = offset (l, u)
    
    {-# INLINE sizeOf #-}
    sizeOf  (Bytes _ _ n _) = max 0 n
    
    {-# INLINE bounds #-}
    bounds  (Bytes l u _ _) = (l, u)
    
    {-# INLINE lower #-}
    lower   (Bytes l _ _ _) = l
    
    {-# INLINE upper #-}
    upper   (Bytes _ u _ _) = u

--------------------------------------------------------------------------------

{- Indexed, IFold, Set and Sort instances. -}

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
    
    (*$) p = ifoldr (\ i e is -> p e ? (i : is) $ is) []

instance (Index i, Unboxed e) => IFold (Bytes i e) i e
  where
    {-# INLINE ifoldr #-}
    ifoldr  f base = \ bytes@(Bytes _ _ n _) ->
      let go i = n == i ? base $ f (indexOf bytes i) (bytes !^ i) (go $ i + 1)
      in  go 0
    
    {-# INLINE ifoldl #-}
    ifoldl  f base = \ bytes ->
      let go i = -1 == i ? base $ f (indexOf bytes i) (go $ i - 1) (bytes !^ i)
      in  go (sizeOf bytes - 1)
    
    {-# INLINE i_foldr #-}
    i_foldr f base = \ arr ->
      let go i = sizeOf arr == i ? base $ f (arr !^ i) (go $ i + 1)
      in  go 0
    
    {-# INLINE i_foldl #-}
    i_foldl f base = \ arr ->
      let go i = -1 == i ? base $ f (go $ i - 1) (arr !^ i)
      in  go (sizeOf arr - 1)

instance (Index i, Unboxed e) => Set (Bytes i e) e
  where
    setWith f = nubSorted f . sortBy f
    
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
    
    isContainedIn = binarySearch
    
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

{-# INLINE nubSorted #-}
nubSorted :: (Index i, Unboxed e) => (e -> e -> Ordering) -> Bytes i e -> Bytes i e
nubSorted _ Z  = Z
nubSorted f es = fromList $ foldr fun [last es] ((es !^) <$> [0 .. sizeOf es - 2])
  where
    fun = \ e ls -> e `f` head ls == EQ ? ls $ e : ls

filled_ :: (Index i, Unboxed e) => Int -> e -> ST s (STBytes s i e)
filled_ n@(I# n#) e = ST $ \ s1# -> case newUnboxed e n# s1# of
    (# s2#, marr# #) -> (# s2#, STBytes l u n marr# #)
  where
    (l, u) = defaultBounds n

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.Bytes." ++ msg

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.Bytes." ++ msg


