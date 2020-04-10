{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.Internal.SArray
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Internal.SArray@ is internal module, that represent lazy boxed
    array pseudo-primitive types 'SArray\#' and 'STArray\#'.
-}
module SDP.Internal.SArray
(
  -- * Pseudo-primitive types
  SArray#, STArray#,
  
  -- ** Safe (copy) unpack
  fromPseudoArray#, fromPseudoMutableArray#,
  
  -- ** Unsafe unpack
  unsafeUnpackPseudoArray#, unsafeUnpackMutableArray#,
  
  -- ** Unsafe pack
  unsafePackPseudoArray#, unsafePackMutableArray#,
  
  -- ** Coerce
  coercePseudoArray#, coerceMutableArray#
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.IndexedM
import SDP.Sort
import SDP.Scan
import SDP.Set

import SDP.SortM.Tim
import SDP.SortM

import Data.Bifunctor
import Data.Coerce

import GHC.Exts
  (
    Array#, MutableArray#, Int (..), State#,
    
    newArray#, indexArray#, readArray#, writeArray#,
    
    thawArray#, unsafeThawArray#, freezeArray#, unsafeFreezeArray#,
    
    copyArray#, copyMutableArray#, cloneArray#, cloneMutableArray#,
    
    isTrue#, sameMutableArray#, (+#), (-#), (==#)
  )
import GHC.ST ( runST, ST (..), STRep )

import SDP.Internal.Commons

default ()

--------------------------------------------------------------------------------

{- |
  SArray\# - pseudo-primitive lazy boxed immutable type.
  
  SArray\# isn't real Haskell primitive (like "GHC.Exts" types) but for
  reliability and stability, I made it inaccessible to direct work.
  
  If you need a primitive type (Array\#), then you can get it only by
  fromPseudoArray\# (copying function).
-}
data SArray# e = SArray#
                        {-# UNPACK #-} !Int -- ^ Element count (not a real size)
                        {-# UNPACK #-} !Int -- ^ Offset
                        !(Array# e)         -- ^ Real primitive array

type role SArray# representational

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Eq e) => Eq (SArray# e) where (==) = eq1

instance Eq1 SArray#
  where
    liftEq eq xs@(SArray# c1 _ _) ys@(SArray# c2 _ _) = c1 == c2 && eq' 0
      where
        eq' i = i == c1 || eq (xs !^ i) (ys !^ i) && eq' (i + 1)

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Ord e) => Ord (SArray# e) where compare = compare1

instance Ord1 SArray#
  where
    liftCompare cmp xs@(SArray# c1 _ _) ys@(SArray# c2 _ _) = cmp' 0
      where
        cmp' i = i == c ? c1 <=> c2 $ (xs!^i) `cmp` (ys!^i) <> cmp' (i + 1)
        c = min c1 c2

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance Semigroup (SArray# e) where (<>) = (++)
instance Monoid    (SArray# e) where mempty = Z
instance Default   (SArray# e) where def = Z

instance (Arbitrary e) => Arbitrary (SArray# e)
  where
    arbitrary = fromList <$> arbitrary

instance Estimate (SArray# e)
  where
    (SArray# c1 _ _) <==> (SArray# c2 _ _) = c1 <=> c2
    (SArray# c1 _ _) .>.  (SArray# c2 _ _) = c1  >  c2
    (SArray# c1 _ _) .<.  (SArray# c2 _ _) = c1  <  c2
    (SArray# c1 _ _) .<=. (SArray# c2 _ _) = c1 <=  c2
    (SArray# c1 _ _) .>=. (SArray# c2 _ _) = c1 >=  c2
    
    (SArray# c1 _ _) <.=> c2 = c1 <=> c2
    (SArray# c1 _ _)  .>  c2 = c1  >  c2
    (SArray# c1 _ _)  .<  c2 = c1  <  c2
    (SArray# c1 _ _) .>=  c2 = c1 >=  c2
    (SArray# c1 _ _) .<=  c2 = c1 <=  c2

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances. -}

instance Functor SArray#
  where
    fmap f arr@(SArray# n@(I# n#) _ _) = runST $ ST $ \ s1# ->
      case newArray# n# (unreachEx "fmap") s1# of
        (# s2#, marr# #) ->
          let go i@(I# i#) s3# = if i == n
              then case unsafeFreezeArray# marr# s3# of (# s4#, arr# #) -> (# s4#, SArray# n 0 arr# #)
              else case writeArray# marr# i# (f $ arr ! i) s3# of s5# -> go (i + 1) s5#
          in go 0 s2#

instance Zip SArray#
  where
    zipWith f as bs = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i)
        sz = minimum [sizeOf as, sizeOf bs]
    
    zipWith3 f as bs cs = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i)
        sz = minimum [sizeOf as, sizeOf bs, sizeOf cs]
    
    zipWith4 f as bs cs ds = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i) (ds !^ i)
        sz = minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds]
    
    zipWith5 f as bs cs ds es = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i) (ds !^ i) (es !^ i)
        sz = minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds, sizeOf es]
    
    zipWith6 f as bs cs ds es fs = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !^ i) (bs !^ i) (cs !^ i) (ds !^ i) (es !^ i) (fs !^ i)
        sz = minimum [sizeOf as, sizeOf bs, sizeOf cs, sizeOf ds, sizeOf es, sizeOf fs]

instance Applicative SArray#
  where
    pure = single
    fs <*> es = concatMap (<$> es) fs

--------------------------------------------------------------------------------

{- Foldable and Traversable instances. -}

instance Foldable SArray#
  where
    foldr  f base = \ arr ->
      let go i = arr .== i ? base $ f (arr !^ i) (go $ i + 1)
      in  go 0
    
    foldl  f base = \ arr ->
      let go i = -1 == i ? base $ f (go $ i - 1) (arr !^ i)
      in  go (sizeOf arr - 1)
    
    foldr' f base = \ arr ->
      let go i !a = -1 == i ? a $ go (i - 1) (f (arr !^ i) a)
      in  go (sizeOf arr - 1) base
    
    foldl' f base = \ arr ->
      let go i !a = arr .== i ? a $ go (i + 1) (f a $ arr !^ i)
      in  go 0 base
    
    foldr1 f = \ arr ->
      let go i = arr .== (i + 1) ? e $ f e (go $ i + 1) where e = arr !^ i
      in  null arr ? pfailEx "foldr1" $ go 0
    
    foldl1 f = \ arr ->
      let go i = 0 == i ? e $ f (go $ i - 1) e where e = arr !^ i
      in  null arr ? pfailEx "foldl1" $ go (sizeOf arr - 1)
    
    toList = foldr (:) []
    
    null   (SArray# c _ _) = c == 0
    length (SArray# c _ _) = c

instance Traversable SArray#
  where
    traverse f = fmap fromList . foldr (\ x ys -> liftA2 (:) (f x) ys) (pure [])

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance Linear (SArray# e) e
  where
    isNull (SArray# c _ _) = c < 1
    
    lzero         = runST $ filled 0 (unreachEx "lzero") >>= done
    single      e = runST $ filled 1 e >>= done
    replicate n e = runST $ filled n e >>= done
    
    toHead e (SArray# (I# c#) (I# o#) arr#) = runST $ ST $
        \ s1# -> case newArray# n# e s1# of
          (# s2#, marr# #) -> case copyArray# arr# o# marr# 1# c# s2# of
            s3# -> case unsafeFreezeArray# marr# s3# of
              (# s4#, res# #) -> (# s4#, SArray# (I# n#) 0 res# #)
      where
        n# = c# +# 1#
    
    toLast (SArray# (I# c#) (I# o#) arr#) e = runST $ ST $
        \ s1# -> case newArray# n# e s1# of
          (# s2#, marr# #) -> case copyArray# arr# o# marr# 0# c# s2# of
            s3# -> case unsafeFreezeArray# marr# s3# of
              (# s4#, res# #) -> (# s4#, SArray# (I# n#) 0 res# #)
      where
        n# = c# +# 1#
    
    head es = es !^ 0
    last es@(SArray# c _ _) = es !^ (c - 1)
    
    tail (SArray# c o arr#) = SArray# (c - 1) (o + 1) arr#
    init (SArray# c o arr#) = SArray# (c - 1) o arr#
    
    fromList = fromFoldable
    
    fromListN  n es = runST $ newLinearN  n es >>= done
    fromFoldable es = runST $ fromFoldableM es >>= done
    
    SArray# (I# n1#) (I# o1#) arr1# ++ SArray# (I# n2#) (I# o2#) arr2# =
      runST $ ST $ \ s1# -> case newArray# n# (unreachEx "(++)") s1# of
        (#s2#, marr# #) -> case copyArray# arr1# o1# marr# 0# n1# s2# of
          s3# -> case copyArray# arr2# o2# marr# n1# n2# s3# of
            s4# -> case unsafeFreezeArray# marr# s4# of
              (# s5#, arr# #) -> (# s5#, SArray# (I# n#) 0 arr# #)
      where
        n# = n1# +# n2#
    
    listL = toList
    listR = flip (:) `foldl` []
    
    reverse es = runST $ fromIndexed' es >>= reversed >>= done
    
    concatMap f = fromList . foldr (flip (foldr (:)) . f) []
    concat      = fromList . foldr (flip $ foldr (:)) []
    
    select  f = foldr (\ o -> case f o of {Just e -> (e :); _ -> id}) []
    
    extract f = second fromList . foldr g ([], [])
      where
        g = \ o -> case f o of {Just e -> first (e :); _ -> second (o :)}
    
    selects fs = second fromList . selects fs . listL

instance Split (SArray# e) e
  where
    -- | O(1) 'take', O(1) memory.
    take n es@(SArray# c o arr#)
      | n <= 0 = Z
      | n >= c = es
      |  True  = SArray# n o arr#
    
    -- | O(1) 'drop', O(1) memory.
    drop n es@(SArray# c o arr#)
      | n <= 0 = es
      | n >= c = Z
      |  True  = SArray# (c - n) (o + n) arr#
    
    -- | O(1) 'keep', O(1) memory.
    keep n es@(SArray# c o arr#)
      | n <= 0 = Z
      | n >= c = es
      |  True  = SArray# n (o + c - n) arr#
    
    -- | O(1) 'sans', O(1) memory.
    sans n es@(SArray# c o arr#)
      | n <= 0 = es
      | n >= c = Z
      |  True  = SArray# (c - n) o arr#
    
    prefix p xs@(SArray# c _ _) =
      let go i = i < c && p (xs !^ i) ? go (i + 1) $ i
      in  go 0
    
    suffix p xs@(SArray# c _ _) =
      let go i = i > 0 && p (xs !^ i) ? go (i - 1) $ i
      in  c - go (c - 1) - 1
    
    isPrefixOf xs@(SArray# c1 _ _) ys@(SArray# c2 _ _) =
      let eq i = i == c1 || (xs !^ i) == (ys !^ i) && eq (i + 1)
      in  c1 <= c2 && eq 0
    
    isSuffixOf xs@(SArray# c1 _ _) ys@(SArray# c2 _ _) =
      let eq i j = i == c1 || (xs !^ i) == (ys !^ j) && eq (i + 1) (j + 1)
      in  c1 <= c2 && eq 0 (c2 - c1)
    
    selectWhile =
      let go i f es = i ==. es ? [] $ maybe [] (: go (i + 1) f es) $ f (es !^ i)
      in  go 0
    
    selectEnd g xs =
      let go i f es = i == 0 ? [] $ maybe [] (: go (i - 1) f es) $ f (es !^ i)
      in  reverse (go (sizeOf xs - 1) g xs)

instance Bordered (SArray# e) Int e
  where
    lower _ = 0
    
    sizeOf   (SArray# c _ _) = c
    upper    (SArray# c _ _) = c - 1
    bounds   (SArray# c _ _) = (0, c - 1)
    indices  (SArray# c _ _) = [0 .. c - 1]
    indexOf  (SArray# c _ _) = index (0, c - 1)
    indexIn  (SArray# c _ _) = \ i -> i >= 0 && i < c
    offsetOf (SArray# c _ _) = offset (0, c - 1)

--------------------------------------------------------------------------------

{- Set, Scan and Sort instances. -}

instance Set (SArray# e) e
  where
    setWith f = nubSorted f . sortBy f
    
    insertWith f e es = case (\ x -> x `f` e /= LT) .$ es of
      Nothing -> es :< e
      Just  i -> e `f` (es!^i) == EQ ? es $ before i e es
    
    deleteWith f e es = isContainedIn f e es ? except (\ x -> f e x == EQ) es $ es
    
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
    
    isContainedIn = binaryContain
    
    lookupLTWith _ _ Z  = Nothing
    lookupLTWith f o es
        | GT <- o `f` last' = Just last'
        | GT <- o `f` head' = look' head' 0 u'
        |       True        = Nothing
      where
        head' = es .! 0
        last' = es .! u'
        u' = upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' r l (j - 1)
            EQ -> Just $ j < 1 ? r $ es !^ (j - 1)
            GT -> look' e (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j
    
    lookupLEWith _ _ Z  = Nothing
    lookupLEWith f o es
        | GT <- o `f` last' = Just last'
        | LT <- o `f` head' = Nothing
        |       True        = look' head' 0 u'
      where
        head' = es .! 0
        last' = es .! u'
        u' = upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' r l (j - 1)
            _  -> look' e (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j
    
    lookupGTWith _ _ Z  = Nothing
    lookupGTWith f o es
        | LT <- o `f` head' = Just head'
        | LT <- o `f` last' = look' last' 0 u'
        |       True        = Nothing
      where
        head' = es .! 0
        last' = es .! u'
        u' = upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> j >= u' ? Nothing $ Just (es !^ (j + 1))
            GT -> look' r (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j
    
    lookupGEWith _ _ Z  = Nothing
    lookupGEWith f o es
        | GT <- o `f` last' = Nothing
        | GT <- o `f` head' = look' last' 0 u'
        |       True        = Just head'
      where
        head' = es .! 0
        last' = es .! u'
        u' = upper es
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> Just e
            GT -> look' r (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j

instance Scan (SArray# e) e

instance Sort (SArray# e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

{- Indexed and IFold instances. -}

instance Indexed (SArray# e) Int e
  where
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    fromIndexed es = runST $ do
        let n = sizeOf es
        copy <- filled n (unreachEx "fromIndexed")
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        done copy
    
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    arr // ascs = runST $ fromFoldableM arr >>= (`overwrite` ascs) >>= done
    
    (!^) (SArray# _ (I# o#) arr#) = \ (I# i#) -> case indexArray# arr# (i# +# o#) of (# e #) -> e
    
    (.!) = (!^)
    (!)  = (!^)
    
    (*$) p = ifoldr (\ i e is -> p e ? (i : is) $ is) []

instance IFold (SArray# e) Int e
  where
    ifoldr f base = \ arr@(SArray# c _ _) ->
      let go i = c == i ? base $ f i (arr !^ i) (go $ i + 1)
      in  go 0
    
    ifoldl f base = \ arr@(SArray# c _ _) ->
      let go i = -1 == i ? base $ f i (go $ i - 1) (arr !^ i)
      in  go (c - 1)
    
    i_foldr = foldr
    i_foldl = foldl

--------------------------------------------------------------------------------

instance Thaw (ST s) (SArray# e) (STArray# s e)
  where
    thaw (SArray# c@(I# c#) (I# o#) arr#) = ST $
      \ s1# -> case thawArray# arr# o# c# s1# of
        (# s2#, marr# #) -> (# s2#, STArray# c 0 marr# #)
    
    unsafeThaw (SArray# c o arr#) = ST $
      \ s1# -> case unsafeThawArray# arr# s1# of
        (# s2#, marr# #) -> (# s2#, STArray# c o marr# #)

instance Freeze (ST s) (STArray# s e) (SArray# e)
  where
    freeze (STArray# c@(I# c#) (I# o#) marr#) = ST $
      \ s1# -> case freezeArray# marr# o# c# s1# of
        (# s2#, arr# #) -> (# s2#, SArray# c 0 arr# #)
    
    unsafeFreeze = done

--------------------------------------------------------------------------------

-- | Primitive mutable array type for internal use.
data STArray# s e = STArray#
                            {-# UNPACK #-} !Int  -- ^ Element count (not a real size)
                            {-# UNPACK #-} !Int  -- ^ Offset
                            !(MutableArray# s e) -- ^ Real primitive array

type role STArray# nominal representational

--------------------------------------------------------------------------------

instance Eq (STArray# s e)
  where
    (STArray# c1 o1 marr1#) == (STArray# c2 o2 marr2#) = res
      where
        same = isTrue# (sameMutableArray# marr1# marr2#)
        res  = c1 == c2 && (c1 == 0 || o1 == o2 && same)

--------------------------------------------------------------------------------

{- BorderedM, LinearM and SplitM instances. -}

instance BorderedM (ST s) (STArray# s e) Int e
  where
    getIndexOf (STArray# c _ _) = return . inRange (0, c - 1)
    getIndices (STArray# c _ _) = return [ 0 .. c - 1 ]
    getBounds  (STArray# c _ _) = return (0, c - 1)
    getUpper   (STArray# c _ _) = return (c - 1)
    getSizeOf  (STArray# c _ _) = return c
    
    getLower _ = return 0

instance LinearM (ST s) (STArray# s e) e
  where
    newNull = ST $ \ s1# -> case newArray# 0# (unreachEx "newNull") s1# of
      (# s2#, marr# #) -> (# s2#, STArray# 0 0 marr# #)
    
    nowNull (STArray# c _ _) = return (c < 1)
    
    getHead es = do s <- getSizeOf es; s < 1 ? empEx "getHead" $ es !#> 0
    getLast es = do s <- getSizeOf es; s < 1 ? empEx "getLast" $ es !#> (s - 1)
    
    newLinear = fromFoldableM
    
    newLinearN c es = ST $ \ s1# -> case newArray# n# err s1# of
      (# s2#, marr# #) ->
        let go y r = \ i# s3# -> case writeArray# marr# i# y s3# of
              s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
        in done' n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
      where
        err = undEx "newLinearN"
        !n@(I# n#) = max 0 c
    
    fromFoldableM es = ST $ \ s1# -> case newArray# n# err s1# of
      (# s2#, marr# #) ->
        let go y r = \ i# s3# -> case writeArray# marr# i# y s3# of
              s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
        in done' n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
      where
        err = unreachEx "fromFoldableM"
        !n@(I# n#) = length es
    
    getLeft  es@(STArray# n _ _) = (es !#>) `mapM` [0 .. n - 1]
    getRight es@(STArray# n _ _) = (es !#>) `mapM` [n - 1, n - 2 .. 0]
    
    copied es@(STArray# n _ _) = do
      copy <- filled n $ unreachEx "copied"
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy
    
    copied' es l n = do
      copy <- n `filled` unreachEx "copied'"
      forM_ [0 .. n - 1] $ \ i -> es !#> (l + i) >>= writeM_ copy i
      return copy
    
    reversed es@(STArray# n _ _) =
      let go i j = when (i < j) $ go (i + 1) (j - 1) >> swapM es i j
      in  go 0 (n - 1) >> return es
    
    filled n e = let !n'@(I# n#) = max 0 n in ST $
      \ s1# -> case newArray# n# e s1# of
        (# s2#, marr# #) -> (# s2#, STArray# n' 0 marr# #)
    
    copyTo src sc trg tc n@(I# n#) = when (n > 0) $ do
        when (sc < 0 || tc < 0) $ underEx "copyTo"
        when (so + n > n1 || to + n > n2) $ overEx "copyTo"
        ST $ \ s1# -> case copyMutableArray# src# so# trg# to# n# s1# of
          s2# -> (# s2#, () #)
      where
        !(STArray# n1 o1 src#) = src; !so@(I# so#) = o1 + sc
        !(STArray# n2 o2 trg#) = trg; !to@(I# to#) = o2 + tc

instance SplitM (ST s) (STArray# s e) e
  where
    takeM n es@(STArray# c o marr#)
      | n <= 0 = newNull
      | n >= c = return es
      |  True  = return (STArray# n o marr#)
    
    dropM n es@(STArray# c o marr#)
      | n >= c = newNull
      | n <= 0 = return es
      |  True  = return (STArray# (c - n) (o + n) marr#)
    
    keepM n es@(STArray# c o marr#)
      | n <= 0 = newNull
      | n >= c = return es
      |  True  = return (STArray# n (c - n + o) marr#)
    
    sansM n es@(STArray# c o marr#)
      | n >= c = newNull
      | n <= 0 = return es
      |  True  = return (STArray# (c - n) o marr#)
    
    splitM n es@(STArray# c o marr#)
      | n <= 0 = do e' <- newNull; return (e', es)
      | n >= c = do e' <- newNull; return (es, e')
      |  True  = return (STArray# n o marr#, STArray# (c - n) (o + n) marr#)
    
    divideM n es@(STArray# c o marr#)
      | n <= 0 = do e' <- newNull; return (es, e')
      | n >= c = do e' <- newNull; return (e', es)
      |  True  = return (STArray# n (c - n + o) marr#, STArray# (c - n) o marr#)
    
    prefixM p es@(STArray# c _ _) =
      let go i = i >= c ? return i $ do e <- es !#> i; p e ? go (i + 1) $ return i
      in  go 0
    
    suffixM p es@(STArray# c _ _) =
      let go i = i == 0 ? return c $ do e <- es !#> i; p e ? go (i - 1) $ return (c - i - 1)
      in  go (max 0 (c - 1))
    
    mprefix p es@(STArray# c _ _) =
      let go i = i >= c ? return i $ do e <- es !#> i; p e ?: go (i + 1) $ return i
      in  go 0
    
    msuffix p es@(STArray# c _ _) =
      let go i = i == 0 ? return c $ do e <- es !#> i; p e ?: go (i - 1) $ return (c - i - 1)
      in  go (max 0 (c - 1))

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance IndexedM (ST s) (STArray# s e) Int e
  where
    fromAssocs' bnds defvalue ascs = size bnds `filled` defvalue >>= (`overwrite` ascs)
    
    {-# INLINE (!#>) #-}
    (!#>) (STArray# _ (I# o#) marr#) = \ (I# i#) -> ST $ readArray# marr# (o# +# i#)
    
    (>!) = (!#>)
    (!>) = (!#>)
    
    writeM_ = writeM
    
    {-# INLINE writeM #-}
    writeM (STArray# _ (I# o#) marr#) = \ (I# i#) e -> ST $
      \ s1# -> case writeArray# marr# (o# +# i#) e s1# of s2# -> (# s2#, () #)
    
    overwrite es@(STArray# c _ _) ascs =
      let ies = filter (inRange (0, c - 1) . fst) ascs
      in  mapM_ (uncurry $ writeM_ es) ies >> return es
    
    fromIndexed' es = do
        let n = sizeOf es
        copy <- filled n (unreachEx "fromIndexed'")
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        return copy
    
    fromIndexedM es = do
      n    <- getSizeOf es
      copy <- filled n (unreachEx "fromIndexedM")
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy

instance IFoldM (ST s) (STArray# s e) Int e
  where
    ifoldrM  f base = \ arr@(STArray# n _ _) ->
      let go i =  n == i ? return base $ bindM2 (arr !#> i) (go $ i + 1) (f i)
      in  go 0
    
    ifoldlM  f base = \ arr@(STArray# n _ _) ->
      let go i = -1 == i ? return base $ bindM2 (go $ i - 1) (arr !#> i) (f i)
      in  go (n - 1)
    
    i_foldrM f base = \ arr@(STArray# n _ _) ->
      let go i = n == i ? return base $ bindM2 (arr !#> i) (go $ i + 1) f
      in  go 0
    
    i_foldlM f base = \ arr@(STArray# n _ _) ->
      let go i = -1 == i ? return base $ bindM2 (go $ i - 1) (arr !#> i) f
      in  go (n - 1)

instance SortM (ST s) (STArray# s e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

{- |
  unsafeUnpackPseudoArray\# returns ByteArray\# field of SArray\# or fails (if
  offset is not 0).
-}
unsafeUnpackPseudoArray# :: SArray# e -> Array# e
unsafeUnpackPseudoArray# = \ (SArray# _ 0 arr#) -> arr#

-- | unsafePackPseudoArray\# creates new SArray\# from sized Array\#.
unsafePackPseudoArray# :: Int -> Array# e -> SArray# e
unsafePackPseudoArray# n arr# = SArray# (max 0 n) 0 arr#

-- | fromPseudoArray\# returns new Array\# (uses cloneArray\#).
fromPseudoArray# :: SArray# e -> Array# e
fromPseudoArray# (SArray# (I# c#) (I# o#) arr#) = cloneArray# arr# o# c#

-- | coercePseudoArray\# is 'coerce' alias.
coercePseudoArray# :: (Coercible a b) => SArray# a -> SArray# b
coercePseudoArray# =  coerce

-- | fromPseudoMutableArray\# returns new MutableArray\#.
fromPseudoMutableArray# :: STArray# s e -> State# s -> (# State# s, MutableArray# s e #)
fromPseudoMutableArray# (STArray# (I# c#) (I# o#) marr#) = cloneMutableArray# marr# o# c#

{- |
  unsafeUnpackMutableArray# returns ByteArray\# field of STArray\# or
  fails (if offset is not 0).
-}
unsafeUnpackMutableArray# :: STArray# s e -> MutableArray# s e
unsafeUnpackMutableArray# =  \ (STArray# _ 0 marr#) -> marr#

-- | unsafePackMutableArray\# creates new STArray\# from sized MutableArray\#.
unsafePackMutableArray# :: Int -> MutableArray# s e -> STArray# s e
unsafePackMutableArray# n marr# = STArray# (max 0 n) 0 marr#

-- | coercePseudoArray\# is 'coerce' alias.
coerceMutableArray# :: (Coercible a b) => STArray# s a -> STArray# s b
coerceMutableArray# =  coerce

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: STArray# s e -> ST s (SArray# e)
done (STArray# n o marr#) = ST $ \ s1# -> case unsafeFreezeArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, SArray# n o arr# #)

{-# INLINE done' #-}
done' :: Int -> MutableArray# s e -> STRep s (STArray# s e)
done' n marr# = \ s1# -> (# s1#, STArray# n 0 marr# #)

{-# INLINE nubSorted #-}
nubSorted :: Compare e -> SArray# e -> SArray# e
nubSorted _ Z  = Z
nubSorted f es = fromList $ foldr fun [last es] ((es !^) <$> [0 .. sizeOf es - 2])
  where
    fun = \ e ls -> e `f` head ls == EQ ? ls $ e : ls

before :: Int -> e -> SArray# e -> SArray# e
before n@(I# n#) e es@(SArray# c@(I# c#) (I# o#) arr#)
  | n >= c = es :< e
  | n <= 0 = e :> es
  |  True  = runST $ ST $ \ s1# -> case newArray# (c# +# 1#) e s1# of
    (# s2#, marr# #) -> case copyArray# arr# o# marr# 0# n# s2# of
      s3# -> case copyArray# arr# (o# +# n#) marr# (n# +# 1#) (c# -# n#) s3# of
        s4# -> case unsafeFreezeArray# marr# s4# of
          (# s5#, res# #) -> (# s5#, SArray# (c + 1) 0 res# #)

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.Internal.SArray."

empEx :: String -> a
empEx =  throw . EmptyRange . showString "in SDP.Internal.SArray."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Internal.SArray."

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Internal.SArray."

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.Internal.SArray."

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Internal.SArray."

