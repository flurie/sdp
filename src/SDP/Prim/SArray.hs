{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RoleAnnotations #-}

{- |
    Module      :  SDP.Prim.SArray
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Prim.SArray@ is internal module, that represent lazy boxed
    array pseudo-primitive types 'SArray\#' and 'STArray\#'.
-}
module SDP.Prim.SArray
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  module SDP.Sort,
  module SDP.Set,
  
  -- * Pseudo-primitive types
  IOArray# ( IOArray# ), STArray#, SArray#,
  
  -- ** Safe (copy) unpack
  fromSArray#, fromSTArray#,
  
  -- ** Unsafe unpack
  unpackSArray#, offsetSArray#, unpackSTArray#, offsetSTArray#,
  
  -- ** Unsafe pack
  packSArray#, packSTArray#,
  
  -- ** Coerce
  coerceSArray#, coerceSTArray#
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.Internal
import SDP.SortM
import SDP.Sort
import SDP.Scan
import SDP.Set

import SDP.SortM.Tim

import GHC.Exts
  (
    Array#, MutableArray#, State#, Int#,
    
    newArray#, indexArray#, readArray#, writeArray#,
    
    thawArray#, unsafeThawArray#, freezeArray#, unsafeFreezeArray#,
    
    copyArray#, copyMutableArray#, cloneArray#, cloneMutableArray#,
    
    sameMutableArray#, (+#), (-#), (==#)
  )
import GHC.ST ( ST (..), STRep )

import qualified GHC.Exts as E

import Data.Typeable
import Data.Proxy

import Text.Read

import Foreign ( Ptr, Storable, callocArray, peekElemOff, pokeElemOff )

import Control.Monad.ST

default ()

--------------------------------------------------------------------------------

{- |
  SArray\# - pseudo-primitive lazy boxed immutable type.
  
  SArray\# isn't real Haskell primitive (like "GHC.Exts" types) but for
  reliability and stability, I made it inaccessible to direct work.
  
  If you need a primitive type (Array\#), then you can get it only by
  fromSArray\# (copying function).
-}
data SArray# e = SArray#
                        {-# UNPACK #-} !Int -- ^ Element count (not a real size)
                        {-# UNPACK #-} !Int -- ^ Offset (is elements)
                        !(Array# e)         -- ^ Real primitive array
  deriving ( Typeable )

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
        cmp' i = i == (c1 `min` c2) ? c1 <=> c2 $ (xs!^i) `cmp` (ys!^i) <> cmp' (i + 1)

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Show e) => Show (SArray# e) where showsPrec p = showsPrec p . listL

instance (Read e) => Read (SArray# e) where readPrec = fromList <$> readPrec

--------------------------------------------------------------------------------

{- Overloaded Lists and Strings support. -}

instance IsString (SArray# Char) where fromString = fromList

instance E.IsList (SArray# e)
  where
    type Item (SArray# e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = toList

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Nullable, Default and Estimate instances. -}

instance Nullable (SArray# e)
  where
    lzero  = runST $ filled 0 (unreachEx "lzero") >>= done
    isNull = \ (SArray# c _ _) -> c < 1

instance Semigroup (SArray# e) where (<>) = (++)
instance Monoid    (SArray# e) where mempty = Z
instance Default   (SArray# e) where def = Z

instance Estimate (SArray# e)
  where
    (<==>) = on (<=>) sizeOf
    (.>.)  = on  (>)  sizeOf
    (.<.)  = on  (<)  sizeOf
    (.<=.) = on  (<=) sizeOf
    (.>=.) = on  (>=) sizeOf
    
    (<.=>) = (<=>) . sizeOf
    (.>)   = (>)   . sizeOf
    (.<)   = (<)   . sizeOf
    (.>=)  = (>=)  . sizeOf
    (.<=)  = (<=)  . sizeOf

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
    
    (!^) (SArray# _ (I# o#) arr#) = \ (I# i#) -> case indexArray# arr# (i# +# o#) of (# e #) -> e
    
    reverse es = runST $ fromIndexed' es >>= reversed >>= done
    
    concat ess = runST $ do
        marr@(STArray# _ _ marr#) <- filled s (unreachEx "concat")
        
        let write# (SArray# c@(I# c#) (I# o#) arr#) i@(I# i#) = ST $
              \ s2# -> case copyArray# arr# o# marr# i# c# s2# of
                s3# -> (# s3#, i + c #)
        
        void $ foldl (\ b a -> write# a =<< b) (return 0) ess
        
        done marr
      where
        s = foldr' ((+) . sizeOf) 0 ess
    
    select  f = foldr (\ o es -> case f o of {Just e -> e : es; _ -> es}) []
    
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
    
    -- | O(1) 'split', O(1) memory.
    split n es@(SArray# c o arr#)
      | n <= 0 = (Z, es)
      | n >= c = (es, Z)
      |  True  = (SArray# n o arr#, SArray# (c - n) (o + n) arr#)
    
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
    
    -- | O(1) 'divide', O(1) memory.
    divide n es@(SArray# c o arr#)
      | n <= 0 = (Z, es)
      | n >= c = (es, Z)
      |  True  = (SArray# n (o + c - n) arr#, SArray# (c - n) o arr#)
    
    splitsBy f es = dropWhile f <$> f *$ es `parts` es
    
    complement n@(I# n#) e es@(SArray# c@(I# c#) (I# o#) src#) = case c <=> n of
      EQ -> es
      GT -> take n es
      LT -> runST $ ST $ \ s1# -> case newArray# n# e s1# of
        (# s2#, marr# #) -> case copyArray# src# o# marr# 0# c# s2# of
          s3# -> case unsafeFreezeArray# marr# s3# of
            (# s4#, arr# #) -> (# s4#, SArray# n 0 arr# #)
    
    combo _  Z = 0
    combo f es = go (head es) 1
      where
        go e i = let e' = es !^ i in i == n || not (f e e') ? i $ go e' (i + 1)
        
        n = sizeOf es
    
    each n es = case n <=> 1 of {LT -> Z; EQ -> es; GT -> fromList $ go (n - 1)}
      where
        go i = i < s ? es!^i : go (i + n) $ []
        
        s = sizeOf es
    
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
    
    selectWhile f es@(SArray# c _ _) =
      let go i = i == c ? [] $ maybe [] (: go (i + 1)) $ f (es !^ i)
      in  go 0
    
    selectEnd g xs@(SArray# c _ _) =
      let go i f es = i == 0 ? [] $ maybe [] (: go (i - 1) f es) $ f (es !^ i)
      in  reverse $ go (c - 1) g xs

instance Bordered (SArray# e) Int
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
      Just  i -> e `f` (es!^i) == EQ ? es $ before i e es
      Nothing -> es :< e
    
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
    
    (.!) = (!^)
    
    (*$) p = ifoldr (\ i e is -> p e ? (i : is) $ is) []

instance IFold (SArray# e) Int e
  where
    ifoldr = ofoldr
    ifoldl = ifoldl
    
    ofoldr f base = \ arr@(SArray# c _ _) ->
      let go i = c == i ? base $ f i (arr !^ i) (go $ i + 1)
      in  go 0
    
    ofoldl f base = \ arr@(SArray# c _ _) ->
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
                            {-# UNPACK #-} !Int  -- ^ Offset (in elements)
                            !(MutableArray# s e) -- ^ Real primitive array
  deriving ( Typeable )

type role STArray# nominal representational

--------------------------------------------------------------------------------

instance Eq (STArray# s e)
  where
    (STArray# c1 o1 marr1#) == (STArray# c2 o2 marr2#) = res
      where
        same = isTrue# (sameMutableArray# marr1# marr2#)
        res  = c1 == c2 && (c1 == 0 || o1 == o2 && same)

--------------------------------------------------------------------------------

{- Estimate, Bordered, BorderedM, LinearM and SplitM instances. -}

instance Estimate (STArray# s e)
  where
    (<==>) = on (<=>) sizeOf
    
    (.>.)  = on (>)  sizeOf
    (.<.)  = on (<)  sizeOf
    (.<=.) = on (<=) sizeOf
    (.>=.) = on (>=) sizeOf
    
    (<.=>) = (<=>) . sizeOf
    (.>)   = (>)   . sizeOf
    (.<)   = (<)   . sizeOf
    (.>=)  = (>=)  . sizeOf
    (.<=)  = (<=)  . sizeOf

instance Bordered (STArray# s e) Int
  where
    bounds (STArray# c _ _) = (0, c - 1)
    sizeOf (STArray# c _ _) = c

instance BorderedM (ST s) (STArray# s e) Int
  where
    nowIndexIn (STArray# c _ _) = return . inRange (0, c - 1)
    getIndices (STArray# c _ _) = return [0 .. c - 1]
    getBounds  (STArray# c _ _) = return (0, c - 1)
    getUpper   (STArray# c _ _) = return (c - 1)
    getSizeOf  (STArray# c _ _) = return c
    
    getLower _ = return 0

instance LinearM (ST s) (STArray# s e) e
  where
    newNull = ST $ \ s1# -> case newArray# 0# (unreachEx "newNull") s1# of
      (# s2#, marr# #) -> (# s2#, STArray# 0 0 marr# #)
    
    nowNull es = return (sizeOf es < 1)
    
    getHead es = es >! 0
    getLast es = es >! upper es
    
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
    
    {-# INLINE (!#>) #-}
    (!#>) (STArray# _ (I# o#) marr#) = \ (I# i#) -> ST $ readArray# marr# (o# +# i#)
    
    copied es@(STArray# n _ _) = do
      copy <- filled n $ unreachEx "copied"
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy
    
    copied' es l n = do
      copy <- n `filled` unreachEx "copied'"
      forM_ [0 .. n - 1] $ \ i -> es !#> (l + i) >>= writeM_ copy i
      return copy
    
    reversed es =
      let go i j = when (i < j) $ go (i + 1) (j - 1) >> swapM es i j
      in  go 0 (sizeOf es - 1) >> return es
    
    filled n e = let !n'@(I# n#) = max 0 n in ST $
      \ s1# -> case newArray# n# e s1# of
        (# s2#, marr# #) -> (# s2#, STArray# n' 0 marr# #)
    
    copyTo src sc trg tc n@(I# n#) = when (n > 0) $ do
        when      (sc < 0 || tc < 0)      $ underEx "copyTo"
        when (sc + n > n1 || tc + n > n2) $ overEx  "copyTo"
        ST $ \ s1# -> case copyMutableArray# src# so# trg# to# n# s1# of
          s2# -> (# s2#, () #)
      where
        !(STArray# n1 o1 src#) = src; !(I# so#) = o1 + sc
        !(STArray# n2 o2 trg#) = trg; !(I# to#) = o2 + tc
    
    merged ess = do
        marr <- filled n (unreachEx "merged")
        
        let writer arr@(STArray# c _ _) i = (i + c) <$ copyTo arr 0 marr i c
        
        void $ foldr ((=<<) . writer) (return 0) ess
        
        return marr
      where
        n = foldr' ((+) . sizeOf) 0 ess

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
      let go i = i >= c ? return i $ do e <- es !#> i; p e ?^ go (i + 1) $ return i
      in  go 0
    
    msuffix p es@(STArray# c _ _) =
      let go i = i == 0 ? return c $ do e <- es !#> i; p e ?^ go (i - 1) $ return (c - i - 1)
      in  go (max 0 (c - 1))

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance IndexedM (ST s) (STArray# s e) Int e
  where
    fromAssocs' bnds defvalue ascs = size bnds `filled` defvalue >>= (`overwrite` ascs)
    
    (>!) = (!#>)
    
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
    ifoldrM = ofoldrM
    ifoldlM = ofoldlM
    
    ofoldrM  f base = \ arr@(STArray# n _ _) ->
      let go i =  n == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f i
      in  go 0
    
    ofoldlM  f base = \ arr@(STArray# n _ _) ->
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f i
      in  go (n - 1)
    
    i_foldrM f base = \ arr@(STArray# n _ _) ->
      let go i = n == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f
      in  go 0
    
    i_foldlM f base = \ arr@(STArray# n _ _) ->
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f
      in  go (n - 1)

instance SortM (ST s) (STArray# s e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

-- | Primitive int-indexed unboxed array in monad 'IO'.
newtype IOArray# e = IOArray# (STArray# RealWorld e) deriving ( Eq )

unpack :: IOArray# e -> STArray# RealWorld e
unpack =  \ (IOArray# arr#) -> arr#

--------------------------------------------------------------------------------

{- Estimate, Bordered and BorderedM instances. -}

instance Estimate (IOArray# e)
  where
    (<==>) = on (<=>) sizeOf
    
    (.>.)  = on (>)  sizeOf
    (.<.)  = on (<)  sizeOf
    (.<=.) = on (<=) sizeOf
    (.>=.) = on (>=) sizeOf
    
    (<.=>) = (<=>) . sizeOf
    (.>)   = (>)   . sizeOf
    (.<)   = (<)   . sizeOf
    (.>=)  = (>=)  . sizeOf
    (.<=)  = (<=)  . sizeOf

instance Bordered (IOArray# e) Int
  where
    sizeOf (IOArray# es) = sizeOf es
    bounds (IOArray# es) = bounds es

instance BorderedM IO (IOArray# e) Int
  where
    getIndexOf = stToIO ... getIndexOf . unpack
    getIndices = stToIO . getIndices . unpack
    getBounds  = stToIO . getBounds . unpack
    getSizeOf  = stToIO . getSizeOf . unpack
    getUpper   = stToIO . getUpper . unpack
    getLower _ = return 0

--------------------------------------------------------------------------------

{- LinearM and SplitM instances. -}

instance LinearM IO (IOArray# e) e
  where
    newNull = pack' newNull
    singleM = pack' . singleM
    nowNull = stToIO . nowNull . unpack
    getHead = stToIO . getHead . unpack
    getLast = stToIO . getLast . unpack
    
    prepend e = pack' . prepend e . unpack
    append es = pack' . append (unpack es)
    
    newLinear     = pack' . newLinear
    newLinearN    = pack' ... newLinearN
    fromFoldableM = pack' . fromFoldableM
    
    (!#>) = stToIO ... (!#>) . unpack
    
    copied   = pack'  . copied   . unpack
    getLeft  = stToIO . getLeft  . unpack
    getRight = stToIO . getRight . unpack
    reversed = pack'  . reversed . unpack
    
    copied' es = pack' ... copied' (unpack es)
    
    merged = pack' . merged . foldr ((:) . unpack) []
    filled = pack' ... filled
    
    copyTo src so trg to = stToIO . copyTo (unpack src) so (unpack trg) to

instance SplitM IO (IOArray# e) e
  where
    takeM n = pack' . takeM n . unpack
    dropM n = pack' . dropM n . unpack
    keepM n = pack' . keepM n . unpack
    sansM n = pack' . sansM n . unpack
    
    prefixM f = stToIO . prefixM f . unpack
    suffixM f = stToIO . suffixM f . unpack
    
    mprefix p es = go 0
      where
        go i = i >= c ? return i $ do e <- es !#> i; p e ?^ go (i + 1) $ return i
        
        c = sizeOf es
    
    msuffix p es = go (max 0 (c - 1))
      where
        go i = i == 0 ? return c $ do e <- es !#> i; p e ?^ go (i - 1) $ return (c - i - 1)
        
        c = sizeOf es

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance IndexedM IO (IOArray# e) Int e
  where
    fromAssocs  bnds = pack'  .  fromAssocs  bnds
    fromAssocs' bnds = pack' ... fromAssocs' bnds
    
    (>!) = (!#>)
    
    writeM_ = writeM
    
    writeM es = stToIO ... writeM (unpack es)
    overwrite = pack' ... overwrite . unpack
    
    fromIndexed' = pack' . fromIndexed'
    
    fromIndexedM es = do
      n    <- getSizeOf es
      copy <- filled n (unreachEx "fromIndexedM")
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy

instance IFoldM IO (IOArray# e) Int e
  where
    ifoldrM f base arr =
      let go i = sizeOf arr == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f i
      in  go 0
    
    ifoldlM f base arr =
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f i
      in  go (sizeOf arr - 1)
    
    i_foldrM f base arr =
      let go i = sizeOf arr == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f
      in  go 0
    
    i_foldlM f base arr =
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f
      in  go (sizeOf arr - 1)

instance SortM IO (IOArray# e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

instance Thaw IO (SArray# e) (IOArray# e)
  where
    unsafeThaw = pack' . unsafeThaw
    thaw       = pack' . thaw

instance Freeze IO (IOArray# e) (SArray# e)
  where
    unsafeFreeze = stToIO . unsafeFreeze . unpack
    freeze       = stToIO . freeze . unpack

--------------------------------------------------------------------------------

instance (Storable e) => Freeze IO (Int, Ptr e) (SArray# e)
  where
    freeze (n, ptr) = do
        let !n'@(I# n#) = max 0 n
        es' <- stToIO . ST $ \ s1# -> case newArray# n# err s1# of
          (# s2#, marr# #) -> (# s2#, IOArray# (STArray# n' 0 marr#) #)
        forM_ [0 .. n' - 1] $ \ i -> peekElemOff ptr i >>= writeM_ es' i
        freeze es'
      where
        err = undEx "freeze {(Int, Ptr e) => SArray#}" `asProxyTypeOf` ptr

instance (Storable e) => Thaw IO (SArray# e) (Int, Ptr e)
  where
    thaw (SArray# n o arr#) = do
      ptr <- callocArray n
      forM_ [o .. n + o - 1] $
        \ i@(I# i#) -> let (# e #) = indexArray# arr# i# in pokeElemOff ptr i e
      return (n, ptr)

--------------------------------------------------------------------------------

-- | unpackSArray\# returns ByteArray\# field of SArray\#.
unpackSArray# :: SArray# e -> Array# e
unpackSArray# = \ (SArray# _ _ arr#) -> arr#

-- | offsetSArray\# returns SArray\# offset in elements.
offsetSArray# :: SArray# e -> Int#
offsetSArray# =  \ (SArray# _ (I# o#) _) -> o#

-- | packSArray\# creates new SArray\# from sized Array\#.
packSArray# :: Int -> Array# e -> SArray# e
packSArray# n arr# = SArray# (max 0 n) 0 arr#

-- | fromSArray\# returns new Array\# (uses cloneArray\#).
fromSArray# :: SArray# e -> Array# e
fromSArray# (SArray# (I# c#) (I# o#) arr#) = cloneArray# arr# o# c#

-- | coerceSArray\# is 'coerce' alias.
coerceSArray# :: (Coercible a b) => SArray# a -> SArray# b
coerceSArray# =  coerce

-- | unpackSTArray# returns ByteArray\# field of STArray\# or fails.
unpackSTArray# :: STArray# s e -> MutableArray# s e
unpackSTArray# =  \ (STArray# _ _ marr#) -> marr#

-- | offsetSTArray\# returns STArray\# offset in elements.
offsetSTArray# :: STArray# s e -> Int#
offsetSTArray# =  \ (STArray# _ (I# o#) _) -> o#

-- | packSTArray\# creates new STArray\# from sized MutableArray\#.
packSTArray# :: Int -> MutableArray# s e -> STArray# s e
packSTArray# n marr# = STArray# (max 0 n) 0 marr#

-- | fromSTArray\# returns new MutableArray\#.
fromSTArray# :: STArray# s e -> State# s -> (# State# s, MutableArray# s e #)
fromSTArray# (STArray# (I# c#) (I# o#) marr#) = cloneMutableArray# marr# o# c#

-- | coerceSTArray\# is 'coerce' alias.
coerceSTArray# :: (Coercible a b) => STArray# s a -> STArray# s b
coerceSTArray# =  coerce

--------------------------------------------------------------------------------

{-# INLINE pack' #-}
pack' :: ST RealWorld (STArray# RealWorld e) -> IO (IOArray# e)
pack' =  stToIO . coerce

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
undEx =  throw . UndefinedValue . showString "in SDP.Prim.SArray."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Prim.SArray."

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Prim.SArray."

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.Prim.SArray."

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Prim.SArray."




