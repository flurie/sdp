{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RoleAnnotations #-}

{- |
    Module      :  SDP.Prim.SBytes
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Prim.SBytes@ is internal module, that represent lazy boxed
    array pseudo-primitive types 'SBytes\#' and 'STBytes\#'.
-}
module SDP.Prim.SBytes
(
  -- * Preudo-primitive types
  SBytes#, STBytes#,
  
  -- ** Safe (copy) unpack
  fromPseudoBytes#, fromPseudoMutableBytes#,
  
  -- ** Unsafe unpack
  unsafeUnpackPseudoBytes#, unsafeUnpackMutableBytes#,
  
  -- ** Unsafe pack
  unsafePackPseudoBytes#, unsafePackMutableBytes#,
  
  -- ** Unsafe coerce
  unsafeCoercePseudoBytes#, unsafeCoerceMutableBytes#,
  
  -- ** Safe memory allocation
  filled_
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.Internal
import SDP.Unboxed

import SDP.SortM.Tim
import SDP.SortM
import SDP.Sort
import SDP.Scan
import SDP.Set

import GHC.Exts
  (
    ByteArray#, MutableByteArray#, State#,
    
    newByteArray#, unsafeFreezeByteArray#,
    
    sameMutableByteArray#, (+#), (-#), (==#)
  )
import GHC.ST ( runST, ST (..), STRep )

import qualified GHC.Exts as E

import Data.Typeable

import Text.Read

default ()

--------------------------------------------------------------------------------

{- |
  SBytes\# - pseudo-primitive strict unboxed immutable type.
  
  SBytes\# isn't real Haskell primitive (like "GHC.Exts" types) but for
  reliability and stability, I made it inaccessible to direct work.
  
  If you need a primitive type (ByteArray\#), then you can get it only by
  copying fromPseudoBytes\# function.
-}
data SBytes# e = SBytes#
                        {-# UNPACK #-} !Int -- ^ Element count (not a real size)
                        {-# UNPACK #-} !Int -- ^ Offset (in elements)
                        !(ByteArray#)       -- ^ Real primitive byte array
  deriving ( Typeable )

type role SBytes# representational

--------------------------------------------------------------------------------

{- Eq instance. -}

instance (Unboxed e) => Eq (SBytes# e)
  where
    xs@(SBytes# c1 _ _) == ys@(SBytes# c2 _ _) = c1 == c2 && eq' 0
      where
        eq' i = i == c1 || (xs!^i) == (ys!^i) && eq' (i + 1)

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Unboxed e, Ord e) => Ord (SBytes# e)
  where
    compare xs@(SBytes# c1 _ _) ys@(SBytes# c2 _ _) = cmp' 0
      where
        cmp' i = i == c ? c1 <=> c2 $ (xs!^i <=> ys!^i) <> cmp' (i + 1)
        c = min c1 c2

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Unboxed e, Show e) => Show (SBytes# e) where showsPrec p = showsPrec p . listL

instance (Unboxed e, Read e) => Read (SBytes# e) where readPrec = fromList <$> readPrec

--------------------------------------------------------------------------------

{- Overloaded Lists and Strings support. -}

instance IsString (SBytes# Char) where fromString = fromList

instance (Unboxed e) => E.IsList (SBytes# e)
  where
    type Item (SBytes# e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = listL

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Nullable, Default and Estimate instances. -}

instance Nullable (SBytes# e)
  where
    lzero = runST $ zero >>= done
      where
        zero :: ST s (STBytes# s e)
        zero =  ST $ \ s1# -> case newByteArray# 0# s1# of
          (# s2#, marr# #) -> (# s2#, STBytes# 0 0 marr# #)
    
    isNull = \ (SBytes# c _ _) -> c < 1

instance (Unboxed e) => Semigroup (SBytes# e) where (<>) = (++)
instance (Unboxed e) => Monoid    (SBytes# e) where mempty = Z

instance Default (SBytes# e)
  where
    def = runST $ empty' >>= done
      where
        empty' = ST $ \ s1# -> case newByteArray# 0# s1# of
          (# s2#, marr# #) -> (# s2#, STBytes# 0 0 marr# #)

instance Estimate (SBytes# e)
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

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Unboxed e) => Linear (SBytes# e) e
  where
    toHead e (SBytes# (I# c#) (I# o#) bytes#) = runST $ ST $
        \ s1# -> case newUnboxed' e n# s1# of
          (# s2#, marr# #) -> case copyUnboxed# e bytes# o# marr# 1# c# s2# of
            s3# -> case unsafeFreezeByteArray# marr# s3# of
              (# s4#, res# #) -> (# s4#, SBytes# (I# n#) 0 res# #)
      where
        n# = c# +# 1#
    
    toLast (SBytes# (I# c#) (I# o#) arr#) e = runST $ ST $
        \ s1# -> case newUnboxed' e n# s1# of
          (# s2#, marr# #) -> case copyUnboxed# e arr# o# marr# 0# c# s2# of
            s3# -> case unsafeFreezeByteArray# marr# s3# of
              (# s4#, res# #) -> (# s4#, SBytes# (I# n#) 0 res# #)
      where
        n# = c# +# 1#
    
    head es = es !^ 0
    last es@(SBytes# c _ _) = es !^ (c - 1)
    
    tail (SBytes# c o arr#) = SBytes# (c - 1) (o + 1) arr#
    init (SBytes# c o arr#) = SBytes# (c - 1) o arr#
    
    fromList = fromFoldable
    
    fromListN  n es = runST $ newLinearN  n es >>= done
    fromFoldable es = runST $ fromFoldableM es >>= done
    
    single e = runST $ filled 1 e >>= done
    
    SBytes# (I# n1#) (I# o1#) arr1# ++ SBytes# (I# n2#) (I# o2#) arr2# = go undefined
      where
        go :: (Unboxed e) => e -> SBytes# e
        go e = runST $ ST $ \ s1# -> case newUnboxed e n# s1# of
          (# s2#, marr# #) -> case copyUnboxed# e arr1# o1# marr# 0# n1# s2# of
            s3# -> case copyUnboxed# e arr2# o2# marr# n1# n2# s3# of
              s4# -> case unsafeFreezeByteArray# marr# s4# of
                (# s5#, arr# #) -> (# s5#, SBytes# (I# n#) 0 arr# #)
          where
            n# = n1# +# n2#
    
    {-# INLINE (!^) #-}
    (!^) (SBytes# _ (I# o#) arr#) = \ (I# i#) -> arr# !# (i# +# o#)
    
    replicate n e = runST $ filled n e >>= done
    
    listL = i_foldr (:) []
    listR = flip (:) `i_foldl` []
    
    concat ess = runST $ do
        marr@(STBytes# _ _ marr#) <- filled_ s
        
        let write# (SBytes# c@(I# c#) (I# o#) arr#) i@(I# i#) = ST $
              \ s2# -> case copyUnboxed# e arr# o# marr# i# c# s2# of
                s3# -> (# s3#, i + c #)
        
        void $ foldl (\ b a -> write# a =<< b) (return 0) ess
        
        done marr
      where
        s = foldr' ((+) . sizeOf) 0 ess
        e = (const undefined :: f (SBytes# e) -> e) ess
    
    reverse es = runST $ fromIndexed' es >>= reversed >>= done
    
    select  f = i_foldr (\ o es -> case f o of {Just e -> e : es; _ -> es}) []
    
    extract f = second fromList . i_foldr g ([], [])
      where
        g = \ o -> case f o of {Just e -> first (e :); _ -> second (o :)}
    
    selects fs = second fromList . selects fs . listL

instance (Unboxed e) => Split (SBytes# e) e
  where
    -- | O(1) 'take', O(1) memory.
    take n es@(SBytes# c o arr#)
      | n <= 0 = Z
      | n >= c = es
      | True = SBytes# n o arr#
    
    -- | O(1) 'drop', O(1) memory.
    drop n es@(SBytes# c o arr#)
      | n <= 0 = es
      | n >= c = Z
      |  True  = SBytes# (c - n) (o + n) arr#
    
    -- | O(1) 'split', O(1) memory.
    split n es@(SBytes# c o arr#)
      | n <= 0 = (Z, es)
      | n >= c = (es, Z)
      |  True  = (SBytes# n o arr#, SBytes# (c - n) (o + n) arr#)
    
    -- | O(1) 'keep', O(1) memory.
    keep n es@(SBytes# c o arr#)
      | n <= 0 = Z
      | n >= c = es
      |  True  = SBytes# n (o + c - n) arr#
    
    -- | O(1) 'sans', O(1) memory.
    sans n es@(SBytes# c o arr#)
      | n <= 0 = es
      | n >= c = Z
      |  True  = SBytes# (c - n) o arr#
    
    -- | O(1) 'divide', O(1) memory.
    divide n es@(SBytes# c o arr#)
      | n <= 0 = (Z, es)
      | n >= c = (es, Z)
      |  True  = (SBytes# n (o + c - n) arr#, SBytes# (c - n) o arr#)
    
    splitsBy f es = dropWhile f <$> f *$ es `parts` es
    
    combo _  Z = 0
    combo f es = go (head es) 1
      where
        go e i = let e' = es !^ i in i == n || not (f e e') ? i $ go e' (i + 1)
        
        n = sizeOf es
    
    complement n@(I# n#) e es@(SBytes# c@(I# c#) (I# o#) src#) = case c <=> n of
      EQ -> es
      GT -> take n es
      LT -> runST $ ST $ \ s1# -> case newUnboxed' e n# s1# of
        (# s2#, mbytes# #) -> case copyUnboxed# e src# o# mbytes# 0# c# s2# of
          s3# -> case unsafeFreezeByteArray# mbytes# s3# of
            (# s4#, bytes# #) -> (# s4#, SBytes# n 0 bytes# #)
    
    each n es = case n <=> 1 of {LT -> Z; EQ -> es; GT -> fromList $ go (n - 1)}
      where
        go i = i < s ? es!^i : go (i + n) $ []
        
        s = sizeOf es
    
    prefix p xs@(SBytes# c _ _) =
      let go i = i < c && p (xs !^ i) ? go (i + 1) $ i
      in  go 0
    
    suffix p xs@(SBytes# c _ _) =
      let go i = i > 0 && p (xs !^ i) ? go (i - 1) $ i
      in  c - go (c - 1) - 1
    
    isPrefixOf xs@(SBytes# c1 _ _) ys@(SBytes# c2 _ _) =
      let eq i = i == c1 || (xs !^ i) == (ys !^ i) && eq (i + 1)
      in  c1 <= c2 && eq 0
    
    isSuffixOf xs@(SBytes# c1 _ _) ys@(SBytes# c2 _ _) =
      let eq i j = i == c1 || (xs !^ i) == (ys !^ j) && eq (i + 1) (j + 1)
      in  c1 <= c2 && eq 0 (c2 - c1)
    
    selectWhile f es@(SBytes# c _ _) =
      let go i = i == c ? [] $ maybe [] (: go (i + 1)) $ f (es !^ i)
      in  go 0
    
    selectEnd g xs@(SBytes# c _ _) =
      let go i f es = i == 0 ? [] $ maybe [] (: go (i - 1) f es) $ f (es !^ i)
      in  reverse $ go (c - 1) g xs

instance Bordered (SBytes# e) Int
  where
    lower _ = 0
    
    sizeOf   (SBytes# c _ _) = c
    upper    (SBytes# c _ _) = c - 1
    bounds   (SBytes# c _ _) = (0, c - 1)
    indices  (SBytes# c _ _) = [0 .. c - 1]
    indexOf  (SBytes# c _ _) = index (0, c - 1)
    offsetOf (SBytes# c _ _) = offset (0, c - 1)
    indexIn  (SBytes# c _ _) = \ i -> i >= 0 && i < c

--------------------------------------------------------------------------------

{- Set and Sort instances. -}

instance (Unboxed e) => Set (SBytes# e) e
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
        
        look' r l u = l > u ? Just r $ case o `f` e of
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
        
        look' r l u = l > u ? Just r $ case o `f` e of
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
        
        look' r l u = l > u ? Just r $ case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> Just e
            GT -> look' r (j + 1) u
          where
            j = l + (u - l) `div` 2
            e = es !^ j
    
    isSubsetWith f xs ys = i_foldr (\ x b -> b && isContainedIn f x ys) True xs

instance (Unboxed e) => Scan (SBytes# e) e

instance (Unboxed e) => Sort (SBytes# e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

{- Indexed and IFold instances. -}

instance (Unboxed e) => Indexed (SBytes# e) Int e
  where
    assoc bnds ascs = runST $ fromAssocs bnds ascs >>= done
    
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    fromIndexed es = runST $ do
        let n = sizeOf es
        copy <- filled_ n
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        done copy
    
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    arr // ascs = runST $ thaw arr >>= (`overwrite` ascs) >>= done
    
    (.!) = (!^)
    
    (*$) p = ifoldr (\ i e is -> p e ? (i : is) $ is) []

instance (Unboxed e) => IFold (SBytes# e) Int e
  where
    ifoldr = ofoldr
    ifoldl = ofoldl
    
    ofoldr f base = \ arr@(SBytes# c _ _) ->
      let go i = c == i ? base $ f i (arr !^ i) (go $ i + 1)
      in  go 0
    
    ofoldl f base = \ arr@(SBytes# c _ _) ->
      let go i = -1 == i ? base $ f i (go $ i - 1) (arr !^ i)
      in  go (c - 1)
    
    i_foldr f base = \ arr@(SBytes# c _ _) ->
      let go i = c == i ? base $ f (arr !^ i) (go $ i + 1)
      in  go 0
    
    i_foldl f base = \ arr@(SBytes# c _ _) ->
      let go i = -1 == i ? base $ f (go $ i - 1) (arr !^ i)
      in  go (c - 1)

--------------------------------------------------------------------------------

instance (Unboxed e) => Thaw (ST s) (SBytes# e) (STBytes# s e)
  where
    thaw = thaw' $ unreachEx "thaw"
      where
        thaw' :: (Unboxed e) => e -> SBytes# e -> ST s (STBytes# s e)
        thaw' e (SBytes# c@(I# c#) (I# o#) arr#) = do
          marr@(STBytes# _ _ marr#) <- filled_ c
          ST $ \ s1# -> case copyUnboxed# e arr# o# marr# 0# c# s1# of
            s2# -> (# s2#, () #)
          return marr

instance (Unboxed e) => Freeze (ST s) (STBytes# s e) (SBytes# e)
  where
    freeze es = cloneSTBytes# es >>= done
    
    unsafeFreeze = done

--------------------------------------------------------------------------------

-- | Primitive mutable byte array type for internal use.
data STBytes# s e = STBytes#
                            {-# UNPACK #-} !Int    -- ^ Element count (not a real size)
                            {-# UNPACK #-} !Int    -- ^ Offset
                            !(MutableByteArray# s) -- ^ Real primitive byte array
  deriving ( Typeable )

type role STBytes# nominal representational

--------------------------------------------------------------------------------

instance Eq (STBytes# s e)
  where
    (STBytes# c1 o1 marr1#) == (STBytes# c2 o2 marr2#) = res
      where
        same = isTrue# (sameMutableByteArray# marr1# marr2#)
        res  = c1 == c2 && (c1 == 0 || o1 == o2 && same)

--------------------------------------------------------------------------------

{- Estimate, Bordered, BorderedM, LinearM and SplitM instances. -}

instance Estimate (STBytes# s e)
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

instance Bordered (STBytes# s e) Int
  where
    sizeOf (STBytes# c _ _) = c
    bounds (STBytes# c _ _) = (0, c - 1)

instance BorderedM (ST s) (STBytes# s e) Int
  where
    getLower _ = return 0
    
    nowIndexIn (STBytes# c _ _) = return . inRange (0, c - 1)
    getIndices (STBytes# c _ _) = return [0 .. c - 1]
    getBounds  (STBytes# c _ _) = return (0, c - 1)
    getUpper   (STBytes# c _ _) = return (c - 1)
    getSizeOf  (STBytes# c _ _) = return c

instance (Unboxed e) => LinearM (ST s) (STBytes# s e) e
  where
    newNull = ST $ \ s1# -> case newByteArray# 0# s1# of
      (# s2#, marr# #) -> (# s2#, STBytes# 0 0 marr# #)
    
    nowNull es = return (sizeOf es < 1)
    
    getHead es = es >! 0
    getLast es = es >! upper es
    
    newLinear = fromFoldableM
    
    newLinearN c es = newLinearN' (undEx "newLinearN")
      where
        newLinearN' :: (Unboxed e) => e -> ST s (STBytes# s e)
        newLinearN' e = ST $ \ s1# -> case newUnboxed e n# s1# of
          (# s2#, marr# #) ->
            let go y r = \ i# s3# -> case writeByteArray# marr# i# y s3# of
                  s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
            in done' n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
        !n@(I# n#) = max 0 c
    
    fromFoldableM es = fromFoldable' (unreachEx "fromFoldableM")
      where
        fromFoldable' :: (Unboxed e) => e -> ST s (STBytes# s e)
        fromFoldable' e = ST $ \ s1# -> case newUnboxed e n# s1# of
          (# s2#, marr# #) ->
            let go y r = \ i# s3# -> case writeByteArray# marr# i# y s3# of
                  s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
            in done' n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
        !n@(I# n#) = length es
    
    getLeft  es@(STBytes# n _ _) = (es !#>) `mapM` [0 .. n - 1]
    getRight es@(STBytes# n _ _) = (es !#>) `mapM` [n - 1, n - 2 .. 0]
    
    copied es@(STBytes# n _ _) = do
      copy <- filled_ n
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy
    
    copied' es l n = do
      copy <- filled_ n
      forM_ [0 .. n - 1] $ \ i -> es !#> (l + i) >>= writeM_ copy i
      return copy
    
    reversed es@(STBytes# n _ _) =
      let go i j = when (i < j) $ go (i + 1) (j - 1) >> swapM es i j
      in  go 0 (n - 1) >> return es
    
    filled n e = ST $ \ s1# -> case newUnboxed' e n# s1# of
        (# s2#, marr# #) -> (# s2#, STBytes# n' 0 marr# #)
      where
        !n'@(I# n#) = max 0 n
    
    copyTo src sc trg tc n@(I# n#) = when (n > 0) $ do
        when      (sc < 0 || tc < 0)      $ underEx "copyTo"
        when (sc + n > n1 || tc + n > n2) $ overEx  "copyTo"
        
        ST $ \ s1# -> case copyUnboxedM# (elem' src) src# so# trg# to# n# s1# of
          s2# -> (# s2#, () #)
      where
        elem' = const undefined :: STBytes# s e -> e
        
        !(STBytes# n1 o1 src#) = src; !(I# so#) = o1 + sc
        !(STBytes# n2 o2 trg#) = trg; !(I# to#) = o2 + tc
    
    merged ess = do
        marr <- filled n (unreachEx "merged")
        
        let writer arr@(STBytes# c _ _) i = (i + c) <$ copyTo arr 0 marr i c
        
        void $ foldr ((=<<) . writer) (return 0) ess
        
        return marr
      where
        n = foldr' ((+) . sizeOf) 0 ess

instance (Unboxed e) => SplitM (ST s) (STBytes# s e) e
  where
    takeM n es@(STBytes# c o marr#)
      | n <= 0 = newNull
      | n >= c = return es
      |  True  = return (STBytes# n o marr#)
    
    dropM n es@(STBytes# c o marr#)
      | n >= c = newNull
      | n <= 0 = return es
      |  True  = return (STBytes# (c - n) (o + n) marr#)
    
    keepM n es@(STBytes# c o marr#)
      | n <= 0 = newNull
      | n >= c = return es
      |  True  = return (STBytes# n (c - n + o) marr#)
    
    sansM n es@(STBytes# c o marr#)
      | n >= c = newNull
      | n <= 0 = return es
      |  True  = return (STBytes# (c - n) o marr#)
    
    splitM n es@(STBytes# c o marr#)
      | n <= 0 = do e' <- newNull; return (e', es)
      | n >= c = do e' <- newNull; return (es, e')
      |  True  = return (STBytes# n o marr#, STBytes# (c - n) (o + n) marr#)
    
    divideM n es@(STBytes# c o marr#)
      | n <= 0 = do e' <- newNull; return (es, e')
      | n >= c = do e' <- newNull; return (e', es)
      |  True  = return (STBytes# n (c - n + o) marr#, STBytes# (c - n) o marr#)
    
    prefixM p es@(STBytes# c _ _) =
      let go i = i >= c ? return i $ do e <- es !#> i; p e ? go (i + 1) $ return i
      in  go 0
    
    suffixM p es@(STBytes# c _ _) =
      let go i = i == 0 ? return c $ do e <- es !#> i; p e ? go (i - 1) $ return (c - i - 1)
      in  go (max 0 (c - 1))
    
    mprefix p es@(STBytes# c _ _) =
      let go i = i >= c ? return i $ do e <- es !#> i; p e ?^ go (i + 1) $ return i
      in  go 0
    
    msuffix p es@(STBytes# c _ _) =
      let go i = i == 0 ? return c $ do e <- es !#> i; p e ?^ go (i - 1) $ return (c - i - 1)
      in  go (max 0 (c - 1))

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Unboxed e) => IndexedM (ST s) (STBytes# s e) Int e
  where
    fromAssocs  bnds ascs = filled_ (size bnds) >>= (`overwrite` ascs)
    fromAssocs' bnds defvalue ascs = size bnds `filled` defvalue >>= (`overwrite` ascs)
    
    {-# INLINE (!#>) #-}
    (!#>) (STBytes# _ (I# o#) marr#) = \ (I# i#) -> ST $ marr# !># (o# +# i#)
    
    (>!) = (!#>)
    
    writeM_ = writeM
    
    {-# INLINE writeM #-}
    writeM (STBytes# _ (I# o#) marr#) = \ (I# i#) e -> ST $
      \ s1# -> case writeByteArray# marr# (o# +# i#) e s1# of
        s2# -> (# s2#, () #)
    
    overwrite es@(STBytes# c _ _) ascs =
      let ies = filter (inRange (0, c - 1) . fst) ascs
      in  mapM_ (uncurry $ writeM_ es) ies >> return es
    
    fromIndexed' es = do
        let n = sizeOf es
        copy <- filled_ n
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        return copy
    
    fromIndexedM es = do
      n    <- getSizeOf es
      copy <- filled_ n
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy

instance (Unboxed e) => IFoldM (ST s) (STBytes# s e) Int e
  where
    ifoldrM = ofoldrM
    ifoldlM = ofoldlM
    
    ofoldrM  f base = \ arr@(STBytes# n _ _) ->
      let go i =  n == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f i
      in  go 0
    
    ofoldlM  f base = \ arr@(STBytes# n _ _) ->
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ (f i)
      in  go (n - 1)
    
    i_foldrM f base = \ arr@(STBytes# n _ _) ->
      let go i = n == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f
      in  go 0
    
    i_foldlM f base = \ arr@(STBytes# n _ _) ->
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f
      in  go (n - 1)

instance (Unboxed e) => SortM (ST s) (STBytes# s e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

{- |
  unsafeUnpackPseudoBytes\# returns ByteArray\# field of SBytes\# or fails (if
  offset is not 0).
-}
unsafeUnpackPseudoBytes# :: (Unboxed e) => SBytes# e -> ByteArray#
unsafeUnpackPseudoBytes# = \ (SBytes# _ 0 marr#) -> marr#

-- | unsafePackPseudoBytes\# creates new SBytes\# from sized ByteArray\#.
unsafePackPseudoBytes# :: (Unboxed e) => Int -> ByteArray# -> SBytes# e
unsafePackPseudoBytes# n marr# = SBytes# (max 0 n) 0 marr#

-- | fromPseudoBytes\# returns new ByteArray\#.
fromPseudoBytes# :: (Unboxed e) => SBytes# e -> ByteArray#
fromPseudoBytes# es = case clone err es of (SBytes# _ _ arr#) -> arr#
  where
    clone :: (Unboxed e) => e -> SBytes# e -> SBytes# e
    clone e (SBytes# c@(I# c#) o@(I# o#) arr#) = runST $ ST $
      \ s1# -> case newUnboxed e c# s1# of
        (# s2#, marr# #) -> case copyUnboxed# e arr# o# marr# 0# c# s2# of
          s3# -> case unsafeFreezeByteArray# marr# s3# of
            (# s4#, arr'# #) -> (# s4#, SBytes# c o arr'# #)
    err = unreachEx "fromPseudoBytes#"

{- |
  Unsafe low-lowel coerce of an array with recounting the number of elements and
  offset (with possible rounding).
-}
unsafeCoercePseudoBytes# :: (Unboxed a, Unboxed b) => SBytes# a -> SBytes# b
unsafeCoercePseudoBytes# =  go Proxy Proxy
  where
    go :: (Unboxed a, Unboxed b) => Proxy a -> Proxy b -> SBytes# a -> SBytes# b
    go pa pb (SBytes# n o arr#) = SBytes# n' o' arr#
      where
        n' = n * s1 `div` s2; s1 = psizeof pa
        o' = o * s1 `div` s2; s2 = psizeof pb

{- |
  unsafeUnpackMutableBytes# returns MutableByteArray\# field of STBytes\# or
  fails (if offset is not 0).
-}
unsafeUnpackMutableBytes# :: (Unboxed e) => STBytes# s e -> MutableByteArray# s
unsafeUnpackMutableBytes# =  \ (STBytes# _ 0 marr#) -> marr#

-- | unsafePackMutableBytes\# creates new STBytes\# from sized MutableByteArray\#.
unsafePackMutableBytes# :: (Unboxed e) => Int -> MutableByteArray# s -> STBytes# s e
unsafePackMutableBytes# n marr# = STBytes# (max 0 n) 0 marr#

-- | fromPseudoMutableBytes\# returns new MutableByteArray\#
fromPseudoMutableBytes# :: (Unboxed e) => STBytes# s e -> State# s -> (# State# s, MutableByteArray# s #)
fromPseudoMutableBytes# es = \ s1# -> case clone es s1# of
    (# s2#, (STBytes# _ _ marr#) #) -> (# s2#, marr# #)
  where
    clone :: (Unboxed e) => STBytes# s e -> State# s -> (# State# s, STBytes# s e #)
    clone es' = \ s1# -> case cloneSTBytes# es' of ST rep -> rep s1#

{- |
  Unsafe low-lowel coerce of an mutable array with recounting the number of
  elements and offset (with possible rounding).
-}
unsafeCoerceMutableBytes# :: (Unboxed a, Unboxed b) => STBytes# s a -> STBytes# s b
unsafeCoerceMutableBytes# =  go Proxy Proxy
  where
    go :: (Unboxed a, Unboxed b) => Proxy a -> Proxy b -> STBytes# s a -> STBytes# s b
    go pa pb (STBytes# n o arr#) = STBytes# n' o' arr#
      where
        n' = n * s1 `div` s2; s1 = psizeof pa
        o' = o * s1 `div` s2; s2 = psizeof pb

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: STBytes# s e -> ST s (SBytes# e)
done (STBytes# n o marr#) = ST $ \ s1# -> case unsafeFreezeByteArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, SBytes# n o arr# #)

{-# INLINE done' #-}
done' :: Int -> MutableByteArray# s -> STRep s (STBytes# s e)
done' n marr# = \ s1# -> (# s1#, STBytes# n 0 marr# #)

-- | filled_ creates filled by default value pseudo-primitive.
filled_ :: (Unboxed e) => Int -> ST s (STBytes# s e)
filled_ c@(I# c#) = fill' (unreachEx "filled_")
  where
    fill' :: (Unboxed e) => e -> ST s (STBytes# s e)
    fill' e = ST $ \ s1# -> case newUnboxed e c# s1# of
      (# s2#, marr# #) -> (# s2#, (STBytes# c 0 marr#) #)

cloneSTBytes# :: (Unboxed e) => STBytes# s e -> ST s (STBytes# s e)
cloneSTBytes# = clone' (unreachEx "cloneSTBytes#")
  where
    clone' :: (Unboxed e) => e -> STBytes# s e -> ST s (STBytes# s e)
    clone' e (STBytes# c@(I# c#) (I# o#) marr#) = do
      marr@(STBytes# _ _ marr'#) <- filled_ c
      ST $ \ s1# -> case copyUnboxedM# e marr# o# marr'# 0# c# s1# of
        s2# -> (# s2#, () #)
      return marr

before :: (Unboxed e) => Int -> e -> SBytes# e -> SBytes# e
before n@(I# n#) e es@(SBytes# c@(I# c#) (I# o#) arr#)
  | n >= c = es :< e
  | n <= 0 = e :> es
  |  True  = runST $ ST $ \ s1# -> case newUnboxed' e (c# +# 1#) s1# of
    (# s2#, marr# #) -> case copyUnboxed# e arr# o# marr# 0# n# s2# of
      s3# -> case copyUnboxed# e arr# (o# +# n#) marr# (n# +# 1#) (c# -# n#) s3# of
        s4# -> case unsafeFreezeByteArray# marr# s4# of
          (# s5#, res# #) -> (# s5#, SBytes# (c + 1) 0 res# #)

{-# INLINE nubSorted #-}
nubSorted :: (Unboxed e) => Compare e -> SBytes# e -> SBytes# e
nubSorted _ Z  = Z
nubSorted f es = fromList $ foldr fun [last es] ((es !^) <$> [0 .. sizeOf es - 2])
  where
    fun = \ e ls -> e `f` head ls == EQ ? ls $ e : ls

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.Prim.SBytes."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Prim.SBytes."

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Prim.SBytes."

unreachEx :: String -> a
unreachEx =  throw . UnreachableException . showString "in SDP.Prim.SBytes."



