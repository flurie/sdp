{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.Unrolled.Unlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.Unrolled.Unlist@ provides service type 'Unlist' - lazy boxed unrolled
    linked list for @Unrolled@.
-}
module SDP.Unrolled.Unlist
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Unlist
  Unlist (..)
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.Indexed
import SDP.Sort
import SDP.Scan
import SDP.Set

import GHC.Show ( appPrec )
import GHC.Base
  (
    Array#, MutableArray#, Int (..), Int#,
    
    newArray#, unsafeFreezeArray#, writeArray#, indexArray#, (-#)
  )

import GHC.ST ( ST (..), STRep, runST )

import Data.String ( IsString (..) )

import SDP.Unrolled.STUnlist
import SDP.SortM.Tim

import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | Unlist is internal data representation for Unrolled.
data Unlist e = UNEmpty | Unlist {-# UNPACK #-} !Int (Array# e) (Unlist e)

type role Unlist representational

{-# COMPLETE Z, Unlist #-}

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Eq e) => Eq (Unlist e)
  where
    (==) = go 0
      where
        go o xs@(Unlist c1 _ xss) ys@(Unlist n2 _ yss) = if n1 > n2
            then and [ xs !^ (o + i) == ys !^ i | i <- [0 .. n2 - 1] ] && go (o + n2) xs yss
            else and [ xs !^ (o + i) == ys !^ i | i <- [0 .. n1 - 1] ] && go    n1    ys xss
          where
            n1 = c1 - o
        go o xs ys = sizeOf xs == o && isNull ys

instance Eq1 Unlist
  where
    liftEq f = go 0 0
      where
        go o1 o2 xs@(Unlist c1 _ xss) ys@(Unlist c2 _ yss) = if c1 > c2 - d
            then all (\ i -> xs !^ (d + i) `f` (ys !^ i)) [o2 .. c2 - 1] && go (d + c2) 0 xs yss
            else all (\ i -> xs !^ i `f` (ys !^ (i - d))) [o1 .. c1 - 1] && go 0 (c1 - d) xss ys
          where
            d = o1 - o2 -- count of elements between xs and ys positions
        go o1 o2 xs ys = sizeOf xs == o1 && sizeOf ys == o2

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Ord e) => Ord (Unlist e) where compare = compare1

instance Ord1 Unlist
  where
    liftCompare cmp = go 0 0
      where
        go o1 o2 xs@(Unlist c1 _ xss) ys@(Unlist c2 _ yss) = if c1 > c2 - d
            then fold [ xs !^ (d + i) `cmp` (ys !^ i) | i <- [o2 .. c2 - 1] ] <> go (d + c2) 0 xs yss
            else fold [ xs !^ i `cmp` (ys !^ (i - d)) | i <- [o1 .. c1 - 1] ] <> go 0 (c1 - d) xss ys
          where
            d = o1 - o2 -- count of elements between xs and ys positions
        go o1 o2 xs ys = (sizeOf xs - o1) <=> (sizeOf ys - o2)

--------------------------------------------------------------------------------

{- Show instance. -}

instance (Show e) => Show (Unlist e)
  where
    showsPrec p unl = showParen (p > appPrec) $ showString "unlist "
                                              . shows (assocs unl)

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance Semigroup (Unlist e) where (<>) = (++)

instance Monoid    (Unlist e) where mempty = def

instance Default   (Unlist e) where def = UNEmpty

instance (Arbitrary e) => Arbitrary (Unlist e)
  where
    arbitrary = fromList <$> arbitrary

instance Estimate (Unlist e)
  where
    (<==>) = go 0
      where
        go d Z    Z = d <=> 0
        go d Z unls = d <=.> unls
        go d unls Z = d > 0 ? GT $ unls <.=> (-d)
        go d (Unlist c1 _ unls1) (Unlist c2 _ unls2) = go (d + c1 - c2) unls1 unls2
    
    es <.=> n2 = n2 < 0 ? GT $ go es n2
      where
        go          Z          m2 = 0  <=> m2
        go (Unlist m1 _ unls') m2 = m1  >  m2 ? GT $ go unls' (m2 - m1)

--------------------------------------------------------------------------------

{- Functor and Applicative instances. -}

instance Functor Unlist
  where
    fmap _ UNEmpty = UNEmpty
    fmap f (Unlist n@(I# n#) arr# arrs) = runST $ ST $ \ s1# ->
      case newArray# n# (unreachEx "fmap") s1# of
        (# s2#, marr# #) ->
          let go i@(I# i#) s# = if i == n
              then done' n (f <$> arrs) marr# s#
              else fill marr# (i, f $ arr# !# i#) (go $ i + 1) s#
          in go 0 s2#

instance Applicative Unlist
  where
    pure = single
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable and Traversable instances. -}

instance Foldable Unlist
  where
    {-# INLINE foldr #-}
    foldr f base = \ es -> case es of
      Z -> base
      (Unlist c arr# arrs) ->
        let go b i@(I# i#) = c == i ? b $ f (arr# !# i#) (go b $ i + 1)
        in  go (foldr f base arrs) 0
    
    {-# INLINE foldr' #-}
    foldr' f base = \ es -> case es of
      Z -> base
      (Unlist c arr# arrs) ->
        let go b i@(I# i#) = c == i ? b $ f (arr# !# i#) (go b $ i + 1)
        in  go (foldr' f base arrs) 0
    
    {-# INLINE foldl #-}
    foldl f base = \ es -> case es of
      Z -> base
      (Unlist c arr# arrs) ->
        let go b i@(I# i#) = -1 == i ? b $ f (go b $ i - 1) (arr# !# i#)
        in  foldl f (go base $ c - 1) arrs
    
    {-# INLINE foldl' #-}
    foldl' f base = \ es -> case es of
      Z -> base
      (Unlist c arr# arrs) ->
        let go b i@(I# i#) = -1 == i ? b $ f (go b $ i - 1) (arr# !# i#)
        in  foldl' f (go base c) arrs
    
    length es = case es of {Unlist n _ arrs -> max 0 n + length arrs; _ -> 0}
    
    toList = listL
    
    null = \ es -> case es of {Unlist c _ _ -> c < 1; _ -> True}

instance Traversable Unlist
  where
    traverse f = fmap fromList . foldr (\ x ys -> liftA2 (:) (f x) ys) (pure [])

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance Linear (Unlist e) e
  where
    isNull = null
    
    lzero = def
    
    toHead e Z = single e
    toHead e (Unlist c unl# unls)
        | c < lim = (Unlist c1  unl1#  unls)
        |   True  = (Unlist 1  single# unls)
      where
        !(Unlist c1  unl1#  Z) = fromList $ e : listL (Unlist c unl# Z)
        !(Unlist 1  single# Z) = single e
    
    head Z  = pfailEx "(:>)"
    head es = es !^ 0
    
    tail Z  = pfailEx "(:<)"
    tail es@(Unlist _ _ Z)    = fromList . tail $ toList es
    tail (Unlist c unl# unls) = Unlist c' new# unls
      where
        !(Unlist c' new# _) = tail (Unlist c unl# Z)
    
    toLast Z e = single e
    toLast es@(Unlist c unl# unls) e
      | isNull unls && c < lim = fromList $ listL es :< e
      |          True          = Unlist c unl# (unls :< e)
    
    last Z = pfailEx "(:<)"
    last (Unlist (I# c#) unl# Z) = unl# !# (c# -# 1#)
    last    (Unlist _ _ unls)    = last unls
    
    init Z = pfailEx "(:>)"
    init es@(Unlist _ _ Z)    = fromList . init $ toList es
    init (Unlist c unl# unls) = Unlist c unl# (init unls)
    
    init Z                    = pfailEx "(:>)"
    init es@(Unlist _ _ Z)    = fromList . init $ listL es
    init (Unlist c unl# unls) = Unlist c unl# (init unls)
    
    {-# INLINE single #-}
    single e = runST $ filled 1 e >>= done
    
    listL = foldr (:) []
    
    {-# INLINE fromList #-}
    fromList es = runST $ newLinear es >>= done
    
    Z ++ ys = ys
    xs ++ Z = xs
    (Unlist c arr# arrs) ++ ys = Unlist c arr# (arrs ++ ys)
    
    -- | Deduplicated Unlist.
    {-# INLINE replicate #-}
    replicate n e = copy count
      where
        chunk  = runST $ ST $ \ s1# -> case newArray# l# e s1# of (# s2#, marr# #) -> done' lim Z marr# s2#
        rest   = runST $ ST $ \ s1# -> case newArray# r# e s1# of (# s2#, marr# #) -> done' restSize Z marr# s2#
        copy c = case c <=> 0 of {LT -> Z; EQ -> rest; GT -> chunk ++ copy (c - 1)}
        
        !(count, restSize@(I# r#)) = n `divMod` lim
        
        !(I# l#) = lim
    
    {-# INLINE reverse #-}
    reverse = fromList . i_foldl (flip (:)) []
    
    partition  p  es = let (x, y) = partition p (toList es) in (fromList x, fromList y)
    partitions ps es = fromList <$> partitions ps (toList es)

instance Split (Unlist e) e
  where
    {-# INLINE take #-}
    take n es
        |   n < 1  = Z
        | es .<= n = es
        |   True   = take' n es
      where
        take' _ Z = Z
        take' n' (Unlist c arr# arrs) = n' >= c ? Unlist c arr# other $ fromListN n' rest
          where
            rest  = [ arr# !# i# | (I# i#) <- [0 .. n' - 1] ]
            other = take' (n' - c) arrs
    
    {-# INLINE drop #-}
    drop n es
        |   n < 1  = es
        | es .<= n = Z
        |   True   = drop' n es
      where
        drop' _ Z = Z
        drop' n' (Unlist c arr# arrs) = n' >= c ? rest $ other ++ arrs
          where
            rest  = drop' (n' - c) arrs
            other = fromListN (c - n') [ arr# !# i# | (I# i#) <- [n' .. c - 1] ]
    
    isPrefixOf xs ys = toList xs `isPrefixOf` toList ys
    isInfixOf  xs ys = toList xs `isInfixOf`  toList ys
    isSuffixOf xs ys = toList xs `isSuffixOf` toList ys

instance Bordered (Unlist e) Int e
  where
    sizeOf  es = case es of {Unlist n _ unls -> max 0 n + sizeOf unls; _ -> 0}
    indexIn es = \ i -> i >= 0 && i <. es
    
    lower  _  = 0
    upper  es = sizeOf es - 1
    bounds es = (0, sizeOf es - 1)

--------------------------------------------------------------------------------

{- Indexed, IFold, Set and Sort instances. -}

instance Indexed (Unlist e) Int e
  where
    {-# INLINE assoc' #-}
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    {-# INLINE (//) #-}
    Z // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    es // ascs = isNull ascs ? es $ runST $ fromFoldableM es >>= flip overwrite ascs >>= done
    
    fromIndexed es = runST $ do
        copy <- filled n (unreachEx "fromIndexed")
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        done copy
      where
        n = sizeOf es
    
    Z !^ _ = error "in SDP.Unrolled.Unlist.(!^)"
    (Unlist c arr# arrs) !^ i@(I# i#) = i < c ? e $ arrs !^ (i - c)
      where
        (# e #) = indexArray# arr# i#
    
    {-# INLINE (.!) #-}
    es .! n = es !^ (n - lower es)
    
    (!) es n = case inBounds bs n of
        ER -> throw $ EmptyRange     msg
        UR -> throw $ IndexOverflow  msg
        OR -> throw $ IndexUnderflow msg
        IN -> es !^ offset bs n
      where
        msg = "in SDP.Unrolled.(!)"
        bs  = bounds es
    
    p .$ es = p .$ toList es
    p *$ es = p *$ toList es

instance IFold (Unlist e) Int e
  where
    {-# INLINE ifoldr #-}
    ifoldr  f base = \ unl -> case unl of
      Z                    -> base
      (Unlist c unl# unls) ->
        let go b i@(I# i#) = c == i ? b $ f i (unl# !# i#) (go b $ i + 1)
        in  go (ifoldr f base unls) 0
    
    {-# INLINE ifoldl #-}
    ifoldl  f base = \ unl -> case unl of
      Z                    -> base
      (Unlist c unl# unls) ->
        let go b i@(I# i#) = -1 == i ? b $ f i (go b $ i - 1) (unl# !# i#)
        in  ifoldl f (go base $ c - 1) unls
    
    i_foldr = foldr
    i_foldl = foldl

instance Set (Unlist e) e
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
    
    lookupLTWith _ _ Z  = Nothing
    lookupLTWith f o es
        | GT <- o `f` last' = Just last'
        | GT <- o `f` head' = look' head' 0 (sizeOf es - 1)
        |       True        = Nothing
      where
        head' = es .! lower es
        last' = es .! upper es
        
        look' r l u = if l > u then Just r else case o `f` e of
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
    
    {-# INLINE isContainedIn #-}
    isContainedIn f e = contain
      where
        contain Z = False
        contain arr@(Unlist n _ arrs)
            | LT <- f e    (arr !^ 0)    = False
            | GT <- f e (arr !^ (n - 1)) = contain arrs
            |            True            = search 0 (n - 1)
          where
            search l u = l > u ? contain arrs $ case f e (arr !^ j) of
                LT -> search l (j - 1)
                EQ -> True
                GT -> search (j + 1) u
              where
                j = l + (u - l `div` 2)

instance Sort (Unlist e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

instance IsString (Unlist Char) where fromString = fromList

--------------------------------------------------------------------------------

instance Thaw (ST s) (Unlist e) (STUnlist s e)
  where
    thaw Z = return STUNEmpty
    thaw (Unlist n unl# unls) = liftA2 cat list (thaw unls)
      where
        cat  = \ (STUnlist _ stunl# _) stunls -> STUnlist n stunl# stunls
        list = newLinear [ unl# !# i# | (I# i#) <- [0 .. n - 1] ]

instance Freeze (ST s) (STUnlist s e) (Unlist e)
  where
    freeze es = copied es >>= done

--------------------------------------------------------------------------------

{-# INLINE (!#) #-}
(!#) :: Array# e -> Int# -> e
arr# !# i# = case indexArray# arr# i# of (# e #) -> e

{-# INLINE fill #-}
fill :: MutableArray# s e -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) nxt = \ s1# -> case writeArray# marr# i# e s1# of s2# -> nxt s2#

{-# INLINE done' #-}
done' :: Int -> Unlist e -> MutableArray# s e -> STRep s (Unlist e)
done' c rest marr# = \ s1# -> case unsafeFreezeArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, Unlist c arr# rest #)

{-# INLINE done #-}
done :: STUnlist s e -> ST s (Unlist e)
done        STUNEmpty        = return UNEmpty
done (STUnlist n marr# marr) = done marr >>= \ arr -> ST $
  \ s1# -> case unsafeFreezeArray# marr# s1# of
    (# s2#, arr# #) -> (# s2#, Unlist n arr# arr #)

{-# INLINE nubSorted #-}
nubSorted :: (e -> e -> Ordering) -> Unlist e -> Unlist e
nubSorted f (xs :< x) = fromList $ foldr (\ e ls@(l : _) -> e `f` l == EQ ? ls $ e : ls) [x] xs
nubSorted _ _ = Z

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.Unrolled.Unlist." ++ msg

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.Unrolled.Unlist." ++ msg

lim :: Int
lim =  1024




