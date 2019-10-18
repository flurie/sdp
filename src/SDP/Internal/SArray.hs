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
    SArray#, STArray#,
    
    fromPseudoArray#, fromPseudoMutableArray#
  )
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.IndexedM
import SDP.SortM
import SDP.Sort
import SDP.Scan
import SDP.Set

import GHC.Exts
  (
    Array#, MutableArray#, Int (..), State#,
    
    newArray#, indexArray#, readArray#, writeArray#,
    
    thawArray#, unsafeThawArray#, freezeArray#, unsafeFreezeArray#,
    
    cloneArray#, cloneMutableArray#,
    
    isTrue#, sameMutableArray#, (+#), (-#), (==#)
  )
import GHC.ST ( runST, ST (..), STRep )

import SDP.SortM.Tim
import SDP.Simple

default ()

--------------------------------------------------------------------------------

{- |
  SArray\# - pseudo-primitive lazy boxed immutable type with hiden constuctors.
  
  SArray\# isn't real Haskell primitive (like "GHC.Exts" types) but for
  reliability and stability, I made it inaccessible to direct work.
  
  If you need a primitive type (Array\#), then you can get it only by copying
  fromPseudoArray\# function.
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

instance (Show e) => Show (SArray# e) where show = show . listL

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Ord e) => Ord (SArray# e) where compare = compare1

instance Ord1 SArray#
  where
    liftCompare cmp xs@(SArray# c1 _ _) ys@(SArray# c2 _ _) = cmp' 0
      where
        cmp' i = i == c ? (c1 <=> c2) $ cmp (xs !^ i) (ys !^ i) <> cmp' (i + 1)
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
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable, Scan and Traversable instances. -}

instance Foldable SArray#
  where
    {-# INLINE foldr #-}
    foldr  f base = \ arr ->
      let go i = arr .== i ? base $ f (arr !^ i) (go $ i + 1)
      in  go 0
    
    {-# INLINE foldl #-}
    foldl  f base = \ arr ->
      let go i = -1 == i ? base $ f (go $ i - 1) (arr !^ i)
      in  go (sizeOf arr - 1)
    
    {-# INLINE foldr' #-}
    foldr' f base = \ arr ->
      let go i !a = -1 == i ? a $ go (i - 1) (f (arr !^ i) a)
      in  go (sizeOf arr - 1) base
    
    {-# INLINE foldl' #-}
    foldl' f base = \ arr ->
      let go i !a = arr .== i ? a $ go (i + 1) (f a $ arr !^ i)
      in  go 0 base
    
    {-# INLINE foldr1 #-}
    foldr1 f = \ arr ->
      let go i = arr .== (i + 1) ? e $ f e (go $ i + 1) where e = arr !^ i
      in  null arr ? pfailEx "foldr1" $ go 0
    
    {-# INLINE foldl1 #-}
    foldl1 f = \ arr ->
      let go i = 0 == i ? e $ f (go $ i - 1) e where e = arr !^ i
      in  null arr ? pfailEx "foldl1" $ go (sizeOf arr - 1)
    
    {-# INLINE toList #-}
    toList = foldr (:) []
    
    {-# INLINE null #-}
    null   (SArray# c _ _) = c == 0
    
    {-# INLINE length #-}
    length (SArray# c _ _) = c

instance Scan SArray#
  where
    scanl _ w Z  = single w
    scanl f w es@(SArray# c _ _) = fromListN (c + 1) $ w : go w 0
      where
        go !curr !n = nxt : go nxt (n + 1)
          where
            nxt = f curr (es !^ n)
    
    scanr _ w Z  = single w
    scanr f w es@(SArray# c _ _) = fromListN (c + 1) $ go w (c - 1) [w]
      where
        go !curr !n ws = n < 0 ? ws $ go prv (n - 1) (prv : ws)
          where
            prv = f (es !^ n) curr
    
    scanl' _ w Z  = single w
    scanl' f w es@(SArray# c _ _) = fromListN (c + 1) $ w : go w 0
      where
        go !curr !n = nxt : go nxt (n + 1)
          where
            nxt = f curr (es !^ n)
    
    scanl1 _ Z  = pfailEx "scanl1"
    scanl1 f es@(SArray# c _ _) = fromListN c $ go (head es) 0
      where
        go !curr !n = nxt : go nxt (n + 1)
          where
            nxt = f curr (es !^ n)
    
    scanr1 _ Z  = pfailEx "scanr1"
    scanr1 f es@(SArray# c _ _) = fromList $ go w (c - 2) [w]
      where
        w = last es
        go !curr !n ws = n < 0 ? ws $ go prv (n - 1) (prv : ws)
          where
            prv = f (es !^ n) curr

instance Traversable SArray#
  where
    traverse f = fmap fromList . foldr (\ x ys -> liftA2 (:) (f x) ys) (pure [])

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance Linear (SArray# e) e
  where
    isNull (SArray# c _ _) = c == 0
    
    {-# INLINE lzero #-}
    lzero = runST $ filled 0 (unreachEx "lzero") >>= done
    
    -- | O(n) 'toHead', O(n) memory.
    toHead e es = fromListN (sizeOf es + 1) (e : listL es)
    
    head es = es !^ 0
    
    -- | O(1) 'tail', O(1) memory.
    tail (SArray# c o arr#) = SArray# (c - 1) (o + 1) arr#
    
    -- | O(n) 'toLast', O(n) memory.
    toLast es e = fromListN (sizeOf es + 1) $ foldr (:) [e] es
    
    last es@(SArray# c _ _) = es !^ (c - 1)
    
    -- | O(1) 'init', O(1) memory.
    init (SArray# c o arr#) = SArray# (c - 1) o arr#
    
    fromList = fromFoldable
    
    {-# INLINE fromListN #-}
    fromListN n es = runST $ newLinearN n es >>= done
    
    {-# INLINE fromFoldable #-}
    fromFoldable es = runST $ fromFoldableM es >>= done
    
    {-# INLINE single #-}
    single e = runST $ filled 1 e >>= done
    
    -- | O (m + n) '++', O(n + m) memory.
    xs ++ ys = fromListN (sizeOf xs + sizeOf ys) $ foldr (:) (listL ys) xs
    
    {-# INLINE replicate #-}
    -- | O(n) 'replicate', O(n) memory.
    replicate n e = runST $ filled n e >>= done
    
    listL = toList
    
    {-# INLINE listR #-}
    listR = flip (:) `foldl` []
    
    {-# INLINE concatMap #-}
    concatMap f = fromList . foldr (\ a l -> foldr (:) l $ f a) []
    
    {-# INLINE concat #-}
    concat = fromList . foldr (\ a l -> foldr (:) l a) []

instance Split (SArray# e) e
  where
    {-# INLINE take #-}
    -- | O(1) 'take', O(1) memory.
    take n es@(SArray# c o arr#)
      | n <= 0 = Z
      | n >= c = es
      | True = SArray# n o arr#
    
    {-# INLINE drop #-}
    -- | O(1) 'drop', O(1) memory.
    drop n es@(SArray# c o arr#)
      | n <= 0 = es
      | n >= c = Z
      |  True  = SArray# (c - n) (o + n) arr#
    
    isPrefixOf xs@(SArray# c1 _ _) ys@(SArray# c2 _ _) = c1 <= c2 && eq 0
      where
        eq i = i == c1 || (xs !^ i) == (ys !^ i) && eq (i + 1)
    
    isSuffixOf xs@(SArray# c1 _ _) ys@(SArray# c2 _ _) = c1 <= c2 && eq 0 (c2 - c1)
      where
        eq i j = i == c1 || (xs !^ i) == (ys !^ j) && eq (i + 1) (j + 1)

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

{- Indexed, IFold, Set and Sort instances. -}

instance Indexed (SArray# e) Int e
  where
    {-# INLINE assoc' #-}
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    fromIndexed es = runST $ do
        let n = sizeOf es
        copy <- filled n (unreachEx "fromIndexed")
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        done copy
    
    {-# INLINE (//) #-}
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    arr // ascs = runST $ fromFoldableM arr >>= (`overwrite` ascs) >>= done
    
    (!^) = (!)
    
    (!) (SArray# _ (I# o#) arr#) = \ (I# i#) -> case indexArray# arr# (i# +# o#) of (# e #) -> e
    
    (*$) p = ifoldr (\ i e is -> p e ? (i : is) $ is) []

instance IFold (SArray# e) Int e
  where
    {-# INLINE ifoldr #-}
    ifoldr f base = \ arr@(SArray# c _ _) ->
      let go i = c == i ? base $ f i (arr !^ i) (go $ i + 1)
      in  go 0
    
    {-# INLINE ifoldl #-}
    ifoldl f base = \ arr@(SArray# c _ _) ->
      let go i = -1 == i ? base $ f i (go $ i - 1) (arr !^ i)
      in  go (c - 1)
    
    i_foldr = foldr
    i_foldl = foldl

instance Set (SArray# e) e
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
    isContainedIn = binaryContain
    
    lookupLTWith _ _ Z  = Nothing
    lookupLTWith f o es
        | GT <- o `f` last' = Just last'
        | GT <- o `f` head' = look' head' 0 (sizeOf es - 1)
        |       True        = Nothing
      where
        head' = es .! lower es
        last' = es .! upper es
        
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
        |       True        = look' head' 0 (sizeOf es - 1)
      where
        head' = es .! lower es
        last' = es .! upper es
        
        look' r l u = if l > u then Just r else case o `f` e of
            LT -> look' r l (j - 1)
            _  -> look' e (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j
    
    lookupGTWith _ _ Z  = Nothing
    lookupGTWith f o es
        | LT <- o `f` head' = Just head'
        | LT <- o `f` last' = look' last' 0 (sizeOf es - 1)
        |       True        = Nothing
      where
        head' = es .! lower es
        last' = es .! upper es
        
        look' r l u = if l > u then Just r else case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> j >= (sizeOf es - 1) ? Nothing $ Just (es !^ (j + 1))
            GT -> look' r (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j
    
    lookupGEWith _ _ Z  = Nothing
    lookupGEWith f o es
        | GT <- o `f` last' = Nothing
        | GT <- o `f` head' = look' last' 0 (sizeOf es - 1)
        |       True        = Just head'
      where
        head' = es .! lower es
        last' = es .! upper es
        
        look' r l u = if l > u then Just r else case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> Just e
            GT -> look' r (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j

instance Sort (SArray# e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

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

{- BorderedM and LinearM instances. -}

instance BorderedM (ST s) (STArray# s e) Int e
  where
    {-# INLINE getLower #-}
    getLower _ = return 0
    
    {-# INLINE getUpper #-}
    getUpper   (STArray# c _ _) = return (c - 1)
    
    {-# INLINE getBounds #-}
    getBounds  (STArray# c _ _) = return (0, c - 1)
    
    {-# INLINE getSizeOf #-}
    getSizeOf  (STArray# c _ _) = return c
    
    {-# INLINE getIndices #-}
    getIndices (STArray# c _ _) = return [ 0 .. c - 1 ]
    
    {-# INLINE getIndexOf #-}
    getIndexOf (STArray# c _ _) = return . inRange (0, c - 1)

instance LinearM (ST s) (STArray# s e) e
  where
    newLinear = fromFoldableM
    
    {-# INLINE newLinearN #-}
    newLinearN c es = ST $ \ s1# -> case newArray# n# err s1# of
      (# s2#, marr# #) ->
        let go y r = \ i# s3# -> case writeArray# marr# i# y s3# of
              s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
        in done' n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
      where
        err = undEx "newLinearN"
        !n@(I# n#) = max 0 c
    
    {-# INLINE fromFoldableM #-}
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
    
    {-# INLINE copied #-}
    copied es@(STArray# n _ _) = do
      copy <- filled n $ unreachEx "copied"
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy
    
    {-# INLINE copied' #-}
    copied' es l n = do
      copy <- n `filled` unreachEx "copied'"
      forM_ [0 .. n - 1] $ \ i -> es !#> (l + i) >>= writeM_ copy i
      return copy
    
    {-# INLINE reversed #-}
    reversed es@(STArray# n _ _) =
      let go i j = when (i < j) $ go (i + 1) (j - 1) >> swapM es i j
      in  go 0 (n - 1) >> return es
    
    {-# INLINE filled #-}
    filled n e = let !n'@(I# n#) = max 0 n in ST $
      \ s1# -> case newArray# n# e s1# of
        (# s2#, marr# #) -> (# s2#, STArray# n' 0 marr# #)

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
    
    {-# INLINE overwrite #-}
    overwrite es@(STArray# c _ _) ascs =
      let ies = filter (inRange (0, c - 1) . fst) ascs
      in  mapM_ (uncurry $ writeM_ es) ies >> return es
    
    {-# INLINE fromIndexed' #-}
    fromIndexed' es = do
        let n = sizeOf es
        copy <- filled n (unreachEx "fromIndexed'")
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        return copy
    
    {-# INLINE fromIndexedM #-}
    fromIndexedM es = do
      n    <- getSizeOf es
      copy <- filled n (unreachEx "fromIndexedM")
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy

instance IFoldM (ST s) (STArray# s e) Int e
  where
    {-# INLINE ifoldrM #-}
    ifoldrM  f base arr@(STArray# n _ _) = go 0
      where
        go i =  n == i ? return base $ bindM2 (arr !#> i) (go $ i + 1) (f i)
    
    {-# INLINE ifoldlM #-}
    ifoldlM  f base arr@(STArray# n _ _) = go (n - 1)
      where
        go i = -1 == i ? return base $ bindM2 (go $ i - 1) (arr !#> i) (f i)
    
    {-# INLINE i_foldrM #-}
    i_foldrM f base arr@(STArray# n _ _) = go 0
      where
        go i = n == i ? return base $ bindM2 (arr !#> i) (go $ i + 1) f
    
    {-# INLINE i_foldlM #-}
    i_foldlM f base arr@(STArray# n _ _) = go (n - 1)
      where
        go i = -1 == i ? return base $ bindM2 (go $ i - 1) (arr !#> i) f

instance SortM (ST s) (STArray# s e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

{-# INLINE fromPseudoArray# #-}
-- | fromPseudoArray\# returns ''new'' Array\# (uses cloneArray\#).
fromPseudoArray# :: SArray# e -> Array# e
fromPseudoArray# (SArray# (I# c#) (I# o#) arr#) = cloneArray# arr# o# c#

{-# INLINE fromPseudoMutableArray# #-}
-- | fromPseudoMutableArray\# return ''new'' MutableArray\#
fromPseudoMutableArray# :: STArray# s e -> State# s -> (# State# s, MutableArray# s e #)
fromPseudoMutableArray# (STArray# (I# c#) (I# o#) marr#) = cloneMutableArray# marr# o# c#

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

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.SArray" ++ msg

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.SArray" ++ msg

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.SArray" ++ msg




