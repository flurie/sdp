{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.Internal.SBytes
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Internal.SBytes@ is internal module, that represent lazy boxed
    array pseudo-primitive types 'SBytes\#' and 'STBytes\#'.
-}
module SDP.Internal.SBytes
  (
    SBytes#, STBytes#,
    
    fromPseudoBytes#, fromPseudoMutableBytes#, filled_
  )
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.IndexedM
import SDP.Unboxed
import SDP.SortM
import SDP.Sort
import SDP.Set

import GHC.Exts
  (
    ByteArray#, MutableByteArray#, Int (..), State#,
    
    newByteArray#, unsafeFreezeByteArray#,
    
    isTrue#, sameMutableByteArray#, (+#), (-#), (==#)
  )
import GHC.ST ( runST, ST (..), STRep )

import SDP.SortM.Tim
import SDP.Simple

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
                        {-# UNPACK #-} !Int -- ^ Offset
                        !(ByteArray#)       -- ^ Real primitive byte array

type role SBytes# representational

instance (Unboxed e, Show e) => Show (SBytes# e) where showsPrec p = showsPrec p . listL

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Unboxed e) => Eq (SBytes# e)
  where
    xs@(SBytes# c1 _ _) == ys@(SBytes# c2 _ _) = c1 == c2 && eq' 0
      where
        eq' i = i == c1 || (xs !^ i) == (ys !^ i) && eq' (i + 1)

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Unboxed e, Ord e) => Ord (SBytes# e)
  where
    compare xs@(SBytes# c1 _ _) ys@(SBytes# c2 _ _) = cmp' 0
      where
        cmp' i = i == c ? (c1 <=> c2) $ ((xs !^ i) <=> (ys !^ i)) <> cmp' (i + 1)
        c = min c1 c2

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance (Unboxed e) => Semigroup (SBytes# e) where (<>) = (++)
instance (Unboxed e) => Monoid    (SBytes# e) where mempty = Z

instance Default (SBytes# e)
  where
    def = runST $ empty' >>= done
      where
        empty' = ST $ \ s1# -> case newByteArray# 0# s1# of
          (# s2#, marr# #) -> (# s2#, STBytes# 0 0 marr# #)

instance (Unboxed e, Arbitrary e) => Arbitrary (SBytes# e)
  where
    arbitrary = fromList <$> arbitrary

instance Estimate (SBytes# e)
  where
    (SBytes# c1 _ _) <==> (SBytes# c2 _ _) = c1 <=> c2
    (SBytes# c1 _ _) .>.  (SBytes# c2 _ _) = c1  >  c2
    (SBytes# c1 _ _) .<.  (SBytes# c2 _ _) = c1  <  c2
    (SBytes# c1 _ _) .<=. (SBytes# c2 _ _) = c1 <=  c2
    (SBytes# c1 _ _) .>=. (SBytes# c2 _ _) = c1 >=  c2
    
    (SBytes# c1 _ _) <.=> c2 = c1 <=> c2
    (SBytes# c1 _ _)  .>  c2 = c1  >  c2
    (SBytes# c1 _ _)  .<  c2 = c1  <  c2
    (SBytes# c1 _ _) .>=  c2 = c1 >=  c2
    (SBytes# c1 _ _) .<=  c2 = c1 <=  c2

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Unboxed e) => Linear (SBytes# e) e
  where
    isNull (SBytes# c _ _) = c == 0
    
    {-# INLINE lzero #-}
    lzero = runST $ filled 0 (unreachEx "lzero") >>= done
    
    -- | O(n) 'toHead', O(n) memory.
    toHead e es = fromListN (sizeOf es + 1) (e : listL es)
    
    head es = es !^ 0
    
    -- | O(1) 'tail', O(1) memory.
    tail (SBytes# c o arr#) = SBytes# (c - 1) (o + 1) arr#
    
    -- | O(n) 'toLast', O(n) memory.
    toLast es e = fromListN (sizeOf es + 1) $ i_foldr (:) [e] es
    
    last es@(SBytes# c _ _) = es !^ (c - 1)
    
    -- | O(1) 'init', O(1) memory.
    init (SBytes# c o arr#) = SBytes# (c - 1) o arr#
    
    fromList = fromFoldable
    
    {-# INLINE fromListN #-}
    fromListN n es = runST $ newLinearN n es >>= done
    
    {-# INLINE fromFoldable #-}
    fromFoldable es = runST $ fromFoldableM es >>= done
    
    {-# INLINE single #-}
    single e = runST $ filled 1 e >>= done
    
    -- | O (m + n) '++', O(n + m) memory.
    xs ++ ys = fromListN (sizeOf xs + sizeOf ys) $ i_foldr (:) (listL ys) xs
    
    {-# INLINE replicate #-}
    -- | O(n) 'replicate', O(n) memory.
    replicate n e = runST $ filled n e >>= done
    
    listL = i_foldr (:) []
    
    {-# INLINE listR #-}
    listR = flip (:) `i_foldl` []
    
    {-# INLINE concatMap #-}
    concatMap f = fromList . foldr (\ a l -> i_foldr (:) l $ f a) []
    
    {-# INLINE concat #-}
    concat = fromList . foldr (\ a l -> i_foldr (:) l a) []

instance (Unboxed e) => Split (SBytes# e) e
  where
    {-# INLINE take #-}
    -- | O(1) 'take', O(1) memory.
    take n es@(SBytes# c o arr#)
      | n <= 0 = Z
      | n >= c = es
      | True = SBytes# n o arr#
    
    {-# INLINE drop #-}
    -- | O(1) 'drop', O(1) memory.
    drop n es@(SBytes# c o arr#)
      | n <= 0 = es
      | n >= c = Z
      |  True  = SBytes# (c - n) (o + n) arr#
    
    isPrefixOf xs@(SBytes# c1 _ _) ys@(SBytes# c2 _ _) = c1 <= c2 && eq 0
      where
        eq i = i == c1 || (xs !^ i) == (ys !^ i) && eq (i + 1)
    
    isSuffixOf xs@(SBytes# c1 _ _) ys@(SBytes# c2 _ _) = c1 <= c2 && eq 0 (c2 - c1)
      where
        eq i j = i == c1 || (xs !^ i) == (ys !^ j) && eq (i + 1) (j + 1)
    
    prefix p = i_foldr (\ e c -> p e ? c + 1 $ 0) 0
    suffix p = i_foldl (\ c e -> p e ? c + 1 $ 0) 0

instance (Unboxed e) => Bordered (SBytes# e) Int e
  where
    lower _ = 0
    
    sizeOf   (SBytes# c _ _) = c
    upper    (SBytes# c _ _) = c - 1
    bounds   (SBytes# c _ _) = (0, c - 1)
    
    indices  (SBytes# c _ _) = [0 .. c - 1]
    indexOf  (SBytes# c _ _) = index (0, c - 1)
    indexIn  (SBytes# c _ _) = \ i -> i >= 0 && i < c
    
    offsetOf (SBytes# c _ _) = offset (0, c - 1)

--------------------------------------------------------------------------------

{- Indexed, IFold, Set and Sort instances. -}

instance (Unboxed e) => Indexed (SBytes# e) Int e
  where
    {-# INLINE assoc #-}
    assoc bnds ascs = runST $ fromAssocs bnds ascs >>= done
    
    {-# INLINE assoc' #-}
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    fromIndexed es = runST $ do
        let n = sizeOf es
        copy <- filled_ n
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        done copy
    
    {-# INLINE (//) #-}
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    arr // ascs = runST $ thaw arr >>= (`overwrite` ascs) >>= done
    
    (!^) = (!)
    (.!) = (!)
    
    (!) (SBytes# _ (I# o#) arr#) = \ (I# i#) -> arr# !# (i# +# o#)
    
    (*$) p = ifoldr (\ i e is -> p e ? (i : is) $ is) []

instance (Unboxed e) => IFold (SBytes# e) Int e
  where
    {-# INLINE ifoldr #-}
    ifoldr f base = \ arr@(SBytes# c _ _) ->
      let go i = c == i ? base $ f i (arr !^ i) (go $ i + 1)
      in  go 0
    
    {-# INLINE ifoldl #-}
    ifoldl f base = \ arr@(SBytes# c _ _) ->
      let go i = -1 == i ? base $ f i (go $ i - 1) (arr !^ i)
      in  go (c - 1)
    
    {-# INLINE i_foldr #-}
    i_foldr f base = \ arr@(SBytes# c _ _) ->
      let go i = c == i ? base $ f (arr !^ i) (go $ i + 1)
      in  go 0
    
    {-# INLINE i_foldl #-}
    i_foldl f base = \ arr@(SBytes# c _ _) ->
      let go i = -1 == i ? base $ f (go $ i - 1) (arr !^ i)
      in  go (c - 1)

instance (Unboxed e) => Set (SBytes# e) e
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
    
    isSubsetWith f xs ys = i_foldr (\ x b -> b && isContainedIn f x ys) True xs

instance (Unboxed e) => Sort (SBytes# e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

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

type role STBytes# nominal representational

--------------------------------------------------------------------------------

instance Eq (STBytes# s e)
  where
    (STBytes# c1 o1 marr1#) == (STBytes# c2 o2 marr2#) = res
      where
        same = isTrue# (sameMutableByteArray# marr1# marr2#)
        res  = c1 == c2 && (c1 == 0 || o1 == o2 && same)

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance (Unboxed e) => BorderedM (ST s) (STBytes# s e) Int e
  where
    {-# INLINE getLower #-}
    getLower _ = return 0
    
    {-# INLINE getUpper #-}
    getUpper   (STBytes# c _ _) = return (c - 1)
    
    {-# INLINE getBounds #-}
    getBounds  (STBytes# c _ _) = return (0, c - 1)
    
    {-# INLINE getSizeOf #-}
    getSizeOf  (STBytes# c _ _) = return c
    
    {-# INLINE getIndices #-}
    getIndices (STBytes# c _ _) = return [ 0 .. c - 1 ]
    
    {-# INLINE getIndexOf #-}
    getIndexOf (STBytes# c _ _) = return . inRange (0, c - 1)

instance (Unboxed e) => LinearM (ST s) (STBytes# s e) e
  where
    newLinear = fromFoldableM
    
    {-# INLINE newLinearN #-}
    newLinearN c es = newLinearN' (undEx "newLinearN")
      where
        newLinearN' :: (Unboxed e) => e -> ST s (STBytes# s e)
        newLinearN' e = ST $ \ s1# -> case newUnboxed e n# s1# of
          (# s2#, marr# #) ->
            let go y r = \ i# s3# -> case writeByteArray# marr# i# y s3# of
                  s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
            in done' n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
        !n@(I# n#) = max 0 c
    
    {-# INLINE fromFoldableM #-}
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
    
    {-# INLINE copied #-}
    copied es@(STBytes# n _ _) = do
      copy <- filled_ n
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy
    
    {-# INLINE copied' #-}
    copied' es l n = do
      copy <- filled_ n
      forM_ [0 .. n - 1] $ \ i -> es !#> (l + i) >>= writeM_ copy i
      return copy
    
    {-# INLINE reversed #-}
    reversed es@(STBytes# n _ _) =
      let go i j = when (i < j) $ go (i + 1) (j - 1) >> swapM es i j
      in  go 0 (n - 1) >> return es
    
    {-# INLINE filled #-}
    filled n e = ST $ \ s1# -> case newUnboxed' e n# s1# of
        (# s2#, marr# #) -> (# s2#, STBytes# n' 0 marr# #)
      where
        !n'@(I# n#) = max 0 n

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Unboxed e) => IndexedM (ST s) (STBytes# s e) Int e
  where
    fromAssocs  bnds ascs = filled_ (size bnds) >>= (`overwrite` ascs)
    fromAssocs' bnds defvalue ascs = size bnds `filled` defvalue >>= (`overwrite` ascs)
    
    {-# INLINE (!#>) #-}
    (!#>) (STBytes# _ (I# o#) marr#) = \ (I# i#) -> ST $ marr# !># (o# +# i#)
    
    (>!) = (!#>)
    (!>) = (!#>)
    
    writeM_ = writeM
    
    {-# INLINE writeM #-}
    writeM (STBytes# _ (I# o#) marr#) = \ (I# i#) e -> ST $
      \ s1# -> case writeByteArray# marr# (o# +# i#) e s1# of
        s2# -> (# s2#, () #)
    
    {-# INLINE overwrite #-}
    overwrite es@(STBytes# c _ _) ascs =
      let ies = filter (inRange (0, c - 1) . fst) ascs
      in  mapM_ (uncurry $ writeM_ es) ies >> return es
    
    {-# INLINE fromIndexed' #-}
    fromIndexed' es = do
        let n = sizeOf es
        copy <- filled_ n
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        return copy
    
    {-# INLINE fromIndexedM #-}
    fromIndexedM es = do
      n    <- getSizeOf es
      copy <- filled_ n
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy

instance (Unboxed e) => IFoldM (ST s) (STBytes# s e) Int e
  where
    {-# INLINE ifoldrM #-}
    ifoldrM  f base arr@(STBytes# n _ _) = go 0
      where
        go i =  n == i ? return base $ bindM2 (arr !#> i) (go $ i + 1) (f i)
    
    {-# INLINE ifoldlM #-}
    ifoldlM  f base arr@(STBytes# n _ _) = go (n - 1)
      where
        go i = -1 == i ? return base $ bindM2 (go $ i - 1) (arr !#> i) (f i)
    
    {-# INLINE i_foldrM #-}
    i_foldrM f base arr@(STBytes# n _ _) = go 0
      where
        go i = n == i ? return base $ bindM2 (arr !#> i) (go $ i + 1) f
    
    {-# INLINE i_foldlM #-}
    i_foldlM f base arr@(STBytes# n _ _) = go (n - 1)
      where
        go i = -1 == i ? return base $ bindM2 (go $ i - 1) (arr !#> i) f

instance (Unboxed e) => SortM (ST s) (STBytes# s e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

{-# INLINE fromPseudoBytes# #-}
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

{-# INLINE fromPseudoMutableBytes# #-}
-- | fromPseudoMutableBytes\# return new MutableByteArray\#
fromPseudoMutableBytes# :: (Unboxed e) => STBytes# s e -> State# s -> (# State# s, MutableByteArray# s #)
fromPseudoMutableBytes# es = \ s1# -> case clone es s1# of
    (# s2#, (STBytes# _ _ marr#) #) -> (# s2#, marr# #)
  where
    clone :: (Unboxed e) => STBytes# s e -> State# s -> (# State# s, STBytes# s e #)
    clone es' = \ s1# -> case cloneSTBytes# es' of ST rep -> rep s1#

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: STBytes# s e -> ST s (SBytes# e)
done (STBytes# n o marr#) = ST $ \ s1# -> case unsafeFreezeByteArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, SBytes# n o arr# #)

{-# INLINE done' #-}
done' :: Int -> MutableByteArray# s -> STRep s (STBytes# s e)
done' n marr# = \ s1# -> (# s1#, STBytes# n 0 marr# #)

{-
  filled_ is an internal function that must never be exported.
  
  It creates filled by default value pseudo-primitive and is used only once - in
  SDP.ByteList.Ublist.fromAssocs.
-}
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

{-# INLINE nubSorted #-}
nubSorted :: (Unboxed e) => Compare e -> SBytes# e -> SBytes# e
nubSorted _ Z  = Z
nubSorted f es = fromList $ foldr fun [last es] ((es !^) <$> [0 .. sizeOf es - 2])
  where
    fun = \ e ls -> e `f` head ls == EQ ? ls $ e : ls

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.SBytes." ++ msg

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.SBytes." ++ msg


