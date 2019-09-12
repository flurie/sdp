{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

{- |
    Module      :  SDP.Array
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    SDP.Array provides immutable lazy boxed array type.
    This implementation of array no much different from Data.Array (array),
    but incopatible with it.
    The main difference is the Index class instead of Ix.
-}
module SDP.Array
(
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  Array (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed
import SDP.Sort
import SDP.Scan
import SDP.Set

import Test.QuickCheck

import GHC.Base
  (
    Array#, Int (..),
    
    newArray#, thawArray#, unsafeFreezeArray#, writeArray#, indexArray#,
    
    isTrue#, (==#), (+#)
  )

import GHC.Show ( appPrec )
import GHC.ST   ( ST (..), runST )

import Text.Read
import Text.Read.Lex ( expect )

import qualified GHC.Exts as E
import Data.String ( IsString (..) )

import SDP.SortM.Tim
import SDP.Array.ST

import SDP.Simple

default ()

--------------------------------------------------------------------------------

{- |
  This Array type definition is no different from the standard GHC.Arr,
  but I have to redefine it because of the limitation of the Ix class.
-}
data Array i e = Array !i !i {-# UNPACK #-} !Int (Array# e)

type role Array nominal representational

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Index i, Eq e) => Eq (Array i e) where (==) = eq1

instance (Index i) => Eq1 (Array i)
  where
    liftEq _  Z  Z  = True
    liftEq eq xs ys = l1 == l2 && u1 == u2 && n1 == n2 && eq' 0#
      where
        !(Array l1 u1 n1 arr1#) = xs
        !(Array l2 u2 n2 arr2#) = ys
        !(I# n#) = n1
        
        eq' i# = isTrue# (i# ==# n#) || ((e1 `eq` e2) && eq' (i# +# 1#))
          where
            (# e1 #) = indexArray# arr1# i#
            (# e2 #) = indexArray# arr2# i#

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Index i, Ord e) => Ord (Array i e) where compare = compare1

instance (Index i) => Ord1 (Array i)
  where
    liftCompare  _  Z  Z  = EQ
    liftCompare cmp xs ys = (xs <==> ys) <> cmp' 0#
      where
        !(Array _ _ _ arr1#) = xs
        !(Array _ _ _ arr2#) = ys
        !(I# n#) = sizeOf xs
        
        cmp' i# = if isTrue# (i# ==# n#) then EQ else (e1 `cmp` e2) <> cmp' (i# +# 1#)
          where
            (# e1 #) = indexArray# arr1# i#
            (# e2 #) = indexArray# arr2# i#

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Index i, Show i, Show e) => Show (Array i e)
  where
    showsPrec p arr@(Array l u _ _) = showParen (p > appPrec) $ showString "array "
                                                              . shows (l, u)
                                                              . showChar ' '
                                                              . shows (assocs arr)

instance (Index i, Read i, Read e) => Read (Array i e)
  where
    readList = readListDefault
    readPrec = parens $ do
      prec appPrec (lift . expect $ Ident "array")
      liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default and Arbitrary instances. -}

instance (Index i) => Semigroup (Array i e) where (<>) = (++)
instance (Index i) => Monoid    (Array i e) where mempty = Z
instance (Index i) => Default   (Array i e) where def = Z

instance (Index i, Arbitrary e) => Arbitrary (Array i e)
  where
    arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances. -}

instance (Index i) => Functor (Array i)
  where
    fmap f arr@(Array l u n@(I# n#) _) = runST $ ST $ \ s1# ->
      case newArray# n# (unreachEx "fmap") s1# of
        (# s2#, marr# #) ->
          let go i@(I# i#) s3# = if i == n
              then case unsafeFreezeArray# marr# s3# of (# s4#, arr# #) -> (# s4#, Array l u n arr# #)
              else case writeArray# marr# i# (f $ arr !^ i) s3# of s5# -> go (i + 1) s5#
          in go 0 s2#

instance (Index i) => Zip (Array i)
  where
    zipWith f as bs              = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i)
        sz      = eminimum [EL as, EL bs]
    
    zipWith3 f as bs cs          = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i)
        sz      = eminimum [EL as, EL bs, EL cs]
    
    zipWith4 f as bs cs ds       = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i) (ds !^ i)
        sz      = eminimum [EL as, EL bs, EL cs, EL ds]
    
    zipWith5 f as bs cs ds es    = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i) (ds !^ i) (es !^ i)
        sz      = eminimum [EL as, EL bs, EL cs, EL ds, EL es]
    
    zipWith6 f as bs cs ds es fs = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i) (ds !^ i) (es !^ i) (fs !^ i)
        sz      = eminimum [EL as, EL bs, EL cs, EL ds, EL es, EL fs]

instance (Index i) => Applicative (Array i)
  where
    pure = single
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable, Scan and Traversable instances. -}

instance (Index i) => Foldable (Array i)
  where
    {-# INLINE foldr #-}
    foldr  f base = \ arr ->
      let go i = arr ==. i ? base $ f (arr !^ i) (go $ i + 1)
      in  go 0
    
    {-# INLINE foldl #-}
    foldl  f base = \ arr ->
      let go i = -1 == i ? base $ f (go $ i - 1) (arr !^ i)
      in  go $ sizeOf arr - 1
    
    {-# INLINE foldr' #-}
    foldr' f base = \ arr ->
      let go i !a = -1 == i ? a $ go (i - 1) (f (arr !^ i) a)
      in  go (sizeOf arr - 1) base
    
    {-# INLINE foldl' #-}
    foldl' f base = \ arr ->
      let go i !a = arr ==. i ? a $ go (i + 1) (f a $ arr !^ i)
      in  go 0 base
    
    {-# INLINE foldr1 #-}
    foldr1 f = \ arr ->
      let go i = arr ==. (i + 1) ? e $ f e (go $ i + 1) where e = arr !^ i
      in  null arr ? pfailEx "foldr1" $ go 0
    
    {-# INLINE foldl1 #-}
    foldl1 f = \ arr ->
      let go i = 0 == i ? e $ f (go $ i - 1) e where e = arr !^ i
      in  null arr ? pfailEx "foldl1" $ go (sizeOf arr - 1)
    
    {-# INLINE toList #-}
    toList arr = [ arr !^ i | i <- [ 0 .. sizeOf arr - 1 ] ]
    
    {-# INLINE null #-}
    null   (Array l u n _) = isEmpty (l, u) || n < 1
    
    {-# INLINE length #-}
    length (Array _ _ n _) = max 0 n

instance (Index i) => Scan (Array i)
  where
    scanl f w es = null es ? single w $ fromListN l (w : res w 0)
      where
        l = sizeOf es + 1
        res !curr !n = nxt : res nxt (n + 1)
          where
            nxt = f curr (es !^ n)
    
    scanr f w es = null es ? single w $ fromListN l (res w (l - 2) [w])
      where
        l = sizeOf es + 1
        res !curr !n ws = n < 0 ? ws $ res prv (n - 1) (prv : ws)
          where
            prv = f (es !^ n) curr
    
    scanl' f w es = null es ? single w $ fromListN l (w : res w 0)
      where
        l = sizeOf es + 1
        res !curr !n = nxt : res nxt (n + 1)
          where
            nxt = f curr (es !^ n)
    
    scanl1 f es = null es ? pfailEx "scanl1" $ fromListN l (res w 0)
      where
        l = sizeOf es
        w = head es
        res !curr !n = nxt : res nxt (n + 1)
          where
            nxt = f curr (es !^ n)
    
    scanr1 f es = null es ? pfailEx "scanr1" $ fromList (res w (l - 2) [w])
      where
        l = sizeOf es
        w = last es
        res !curr !n ws = n < 0 ? ws $ res prv (n - 1) (prv : ws)
          where
            prv = f (es !^ n) curr

instance (Index i) => Traversable (Array i)
  where
    traverse f = fmap fromList . foldr (\ x ys -> liftA2 (:) (f x) ys) (pure [])

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i) => Linear (Array i e) e
  where
    isNull = null
    
    {-# INLINE lzero #-}
    lzero = runST $ filled 0 (unreachEx "lzero") >>= done
    
    toHead e es = fromListN (sizeOf es + 1) (e : listL es)
    
    head Z  = pfailEx "(:>)"
    head es = es !^ 0
    
    tail Z  = pfailEx "(:>)"
    tail es = fromListN (sizeOf es - 1) . tail $ listL es
    
    toLast es e = fromListN (sizeOf es + 1) $ foldr (:) [e] es
    
    last Z  = pfailEx "(:<)"
    last es = es !^ (sizeOf es - 1)
    
    init Z  = pfailEx "(:<)"
    init es = fromListN (sizeOf es - 1) $ listL es
    
    fromList = fromFoldable
    
    {-# INLINE fromListN #-}
    fromListN n es = runST $ newLinearN n es >>= done
    
    {-# INLINE fromFoldable #-}
    fromFoldable es = runST $ fromFoldableM es >>= done
    
    {-# INLINE single #-}
    single e = runST $ filled 1 e >>= done
    
    xs ++ ys = fromListN (sizeOf xs + sizeOf ys) $ foldr (:) (listL ys) xs
    
    {-# INLINE replicate #-}
    replicate n e = runST $ filled n e >>= done
    
    listL = toList
    
    {-# INLINE listR #-}
    listR = flip (:) `foldl` []
    
    {-# INLINE concatMap #-}
    concatMap f = fromList . foldr (\ a l -> foldr (:) l $ f a) []
    
    {-# INLINE concat #-}
    concat = fromList . foldr (\ a l -> foldr (:) l a) []
    
    partitions f es = fromList <$> partitions f (listL es)

instance (Index i) => Split (Array i e) e
  where
    {-#  INLINE take #-}
    take n es
        | n <= 0 = Z
        | n >= l = es
        |  True  = fromListN n [ es !^ i | i <- [ 0 .. n - 1 ] ]
      where
        l = sizeOf es
    
    {-# INLINE drop #-}
    drop n es
        | n <= 0 = es
        | n >= l = Z
        |  True  = fromListN (l - n) [ es !^ i | i <- [ n .. l - 1 ] ]
      where
        l = sizeOf es
    
    splits ns es = fromList <$> splits ns (listL es)
    chunks ns es = fromList <$> chunks ns (listL es)
    parts  ns es = fromList <$> parts  ns (listL es)
    
    isPrefixOf xs ys = xs .<=. ys && equals 0#
      where
        !(Array _ _ _ arr1#) = xs
        !(Array _ _ _ arr2#) = ys
        !(I# n#) = sizeOf xs
        
        equals i# = isTrue# (i# ==# n#) || (e1 == e2 && equals (i# +# 1#))
          where
            (# e1 #) = indexArray# arr1# i#
            (# e2 #) = indexArray# arr2# i#
    
    isSuffixOf xs ys = xs .<=. ys && equals 0# o#
      where
        !(Array _ _ _ arr1#) = xs
        !(Array _ _ _ arr2#) = ys
        
        !(I# o#) = max 0 (sizeOf ys - lx)
        !lx@(I# n#) = sizeOf xs
        
        equals i# j# = isTrue# (i# ==# n#) || (e1 == e2 && equals (i# +# 1#) (j# +# 1#))
          where
            (# e1 #) = indexArray# arr1# i#
            (# e2 #) = indexArray# arr2# j#

instance (Index i) => Bordered (Array i e) i e
  where
    {-# INLINE indexIn #-}
    indexIn (Array l u _ _) = inRange (l, u)
    
    {-# INLINE indexOf #-}
    indexOf (Array l u _ _) = index (l, u)
    
    {-# INLINE offsetOf #-}
    offsetOf (Array l u _ _) = offset (l, u)
    
    {-# INLINE sizeOf #-}
    sizeOf  (Array _ _ n _) = max 0 n
    
    {-# INLINE bounds #-}
    bounds  (Array l u _ _) = (l, u)
    
    {-# INLINE lower #-}
    lower   (Array l _ _ _) = l
    
    {-# INLINE upper #-}
    upper   (Array _ u _ _) = u

--------------------------------------------------------------------------------

{- Indexed, IFold, Set and Sort instances. -}

instance (Index i) => Indexed (Array i e) i e
  where
    {-# INLINE assoc' #-}
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    fromIndexed es = runST $ do
        copy <- filled n (unreachEx "fromIndexed")
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        done copy
      where
        n = sizeOf es
    
    {-# INLINE (//) #-}
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    arr // ascs = runST $ fromFoldableM arr >>= (`overwrite` ascs) >>= done
    
    {-# INLINE (!^) #-}
    (Array _ _ _ arr#) !^ (I# i#) = case indexArray# arr# i# of (# e #) -> e
    
    {-# INLINE (!) #-}
    (!) arr i = arr !^ offsetOf arr i
    
    (*$) p = ifoldr (\ i e is -> p e ? (i : is) $ is) []

instance (Index i) => IFold (Array i e) i e
  where
    {-# INLINE ifoldr #-}
    ifoldr  f base = \ arr ->
      let go i = arr ==. i ? base $ f (indexOf arr i) (arr !^ i) (go $ i + 1)
      in  go 0
    
    {-# INLINE ifoldl #-}
    ifoldl  f base = \ arr ->
      let go i = -1 == i ? base $ f (indexOf arr i) (go $ i - 1) (arr !^ i)
      in  go (sizeOf arr - 1)
    
    i_foldr = foldr
    i_foldl = foldl

instance (Index i) => Set (Array i e) e
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
    
    {-# INLINE intersectionWith #-}
    intersectionWith f xs ys = fromList $ intersection' 0 0
      where
        n1 = sizeOf xs; n2 = sizeOf ys
        intersection' i j = i == n1 || j == n2 ? [] $ case x `f` y of
            LT -> intersection' (i + 1) j
            EQ -> x : intersection' (i + 1) (j + 1)
            GT -> intersection' i (j + 1)
          where
            x = xs !^ i; y = ys !^ j
    
    {-# INLINE unionWith #-}
    unionWith f xs ys = fromList $ union' 0 0
      where
        n1 = sizeOf xs; n2 = sizeOf ys
        union' i j
          | i == n1 = (ys !^) <$> [j .. n2 - 1]
          | j == n2 = (xs !^) <$> [i .. n1 - 1]
          |  True   = case x `f` y of
            LT -> x : union' (i + 1) j
            EQ -> x : union' (i + 1) (j + 1)
            GT -> y : union' i (j + 1)
          where
            x = xs !^ i; y = ys !^ j
    
    {-# INLINE differenceWith #-}
    differenceWith f xs ys = fromList $ difference' 0 0
      where
        n1 = sizeOf xs; n2 = sizeOf ys
        difference' i j
            | i == n1 = []
            | j == n2 = (xs !^) <$> [i .. n1 - 1]
            |  True   = case x `f` y of
              LT -> x : difference' (i + 1) j
              EQ -> difference' (i + 1) (j + 1)
              GT -> difference' i (j + 1)
          where
            x = xs !^ i; y = ys !^ j
    
    {-# INLINE symdiffWith #-}
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
    
    {-# INLINE isContainedIn #-}
    isContainedIn = binarySearch

instance (Index i) => Sort (Array i e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

instance (Index i) => E.IsList (Array i e)
  where
    type Item (Array i e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = toList

instance (Index i) => IsString (Array i Char) where fromString = fromList

instance (Index i) => Estimate (Array i) where xs <==> ys = sizeOf xs <=> sizeOf ys

--------------------------------------------------------------------------------

{- Thaw and Freeze instances. -}

instance (Index i) => Thaw (ST s) (Array i e) (STArray s i e)
  where
    thaw (Array l u n@(I# n#) arr#) = ST $
      \ s1# -> case thawArray# arr# 0# n# s1# of
        (# s2#, marr# #) -> (# s2#, STArray l u n marr# #)

instance (Index i) => Freeze (ST s) (STArray s i e) (Array i e)
  where
    freeze = done

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: STArray s i e -> ST s (Array i e)
done (STArray l u n marr#) = ST $ \ s1# -> case unsafeFreezeArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, Array l u n arr# #)

{-# INLINE nubSorted #-}
nubSorted :: (Index i) => (e -> e -> Ordering) -> Array i e -> Array i e
nubSorted _ Z  = Z
nubSorted f es = fromList $ foldr fun [last es] ((es !^) <$> [0 .. sizeOf es - 2])
  where
    fun = \ e ls -> e `f` head ls == EQ ? ls $ e : ls

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.Array." ++ msg

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.Array." ++ msg


