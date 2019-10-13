{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns #-}

{- |
    Module      :  SDP.ByteList.Ublist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.ByteList.Ublist@ provides service type 'Ublist' - strict boxed unrolled
    linked list for @ByteList@.
-}
module SDP.ByteList.Ublist
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Unboxed,
  module SDP.Sort,
  module SDP.Set,
  
  -- * Ublist
  Ublist (..)
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Set

import GHC.Base
  (
    ByteArray#, MutableByteArray#, Int (..),
    
    unsafeFreezeByteArray#, (-#)
  )

import GHC.ST   ( ST (..), STRep, runST )
import GHC.Show ( appPrec )

import Data.String ( IsString (..) )

import SDP.ByteList.STUblist
import SDP.SortM.Tim
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | Ublist is internal data representation for ByteList.
data Ublist e = UBEmpty | Ublist {-# UNPACK #-} !Int (ByteArray#) (Ublist e)

{-# COMPLETE Z, Ublist #-}

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Unboxed e) => Eq (Ublist e)
  where
    {-# INLINE (==) #-}
    (==) = go 0
      where
        go o xs@(Ublist c1 _ xss) ys@(Ublist n2 _ yss) = if n1 > n2
            then and [ xs !^ (o + i) == ys !^ i | i <- [0 .. n2 - 1] ] && go (o + n2) xs yss
            else and [ xs !^ (o + i) == ys !^ i | i <- [0 .. n1 - 1] ] && go    n1    ys xss
          where
            n1 = c1 - o
        go o xs ys = sizeOf xs == o && isNull ys

instance (Unboxed e, Ord e) => Ord (Ublist e)
  where
    {-# INLINE compare #-}
    compare = go 0 0
      where
        go o1 o2 xs@(Ublist c1 _ xss) ys@(Ublist c2 _ yss) = if c1 > c2 - d
            then fold [ xs !^ (d + i) <=> (ys !^ i) | i <- [o2 .. c2 - 1] ] <> go (d + c2) 0 xs yss
            else fold [ xs !^ i <=> (ys !^ (i - d)) | i <- [o1 .. c1 - 1] ] <> go 0 (c1 - d) xss ys
          where
            d = o1 - o2 -- count of elements between xs and ys positions
        go o1 o2 xs ys = (sizeOf xs - o1) <=> (sizeOf ys - o2)

--------------------------------------------------------------------------------

{- Show instances. -}

instance (Unboxed e, Show e) => Show (Ublist e)
  where
    showsPrec p ubl = showParen (p > appPrec) $ showString "ublist "
                                              . shows (assocs ubl)

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance (Unboxed e) => Semigroup (Ublist e) where (<>) = (++)

instance (Unboxed e) => Monoid (Ublist e) where mempty = UBEmpty

instance Default (Ublist e) where def = UBEmpty

instance (Unboxed e, Arbitrary e) => Arbitrary (Ublist e)
  where
    arbitrary = fromList <$> arbitrary

instance (Unboxed e) => Estimate (Ublist e)
  where
    (<==>) = go 0
      where
        go d Z    Z = d <=> 0
        go d Z ubls = d <=.> ubls
        go d ubls Z = d > 0 ? GT $ ubls <.=> (-d)
        go d (Ublist c1 _ ubls1') (Ublist c2 _ ubls2') = go (d + c1 - c2) ubls1' ubls2'
    
    es <.=> n2 = n2 < 0 ? GT $ go es n2
      where
        go          Z          m2 = 0  <=> m2
        go (Ublist m1 _ ubls') m2 = m1  >  m2 ? GT $ go ubls' (m2 - m1)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Unboxed e) => Linear (Ublist e) e
  where
    isNull es = case es of {Ublist c _ _ -> c < 1; _ -> True}
    
    lzero = def
    
    head Z  = pfailEx ".(:>)"
    head es = es .! 0
    
    tail Z = pfailEx "(:<)"
    tail es@(Ublist _ _ Z)       = fromList . tail $ listL es
    tail es@(Ublist c ubl# ubls) = Ublist c' new# ubls
      where
        !(Ublist c' new# _) = (`asTypeOf` es) $ tail (Ublist c ubl# Z)
    
    toHead e Z = single e
    toHead e (Ublist c ubl# ubls)
        | c < lim = (Ublist c1  ubl1#  ubls)
        |   True  = (Ublist 1  single# ubls)
      where
        !(Ublist c1  ubl1#  Z) = fromList $ e : listL (Ublist c ubl# Z)
        !(Ublist 1  single# Z) = single e
    
    last Z = pfailEx "(:<)"
    last (Ublist (I# c#) ubl# Z) = ubl# !# (c# -# 1#)
    last    (Ublist _ _ ubls)    = last ubls
    
    init Z                    = pfailEx "(:>)"
    init es@(Ublist c _ Z)    = fromListN (c - 1) . init $ listL es
    init (Ublist c arr# arrs) = Ublist c arr# (init arrs)
    
    toLast Z e = single e
    toLast es@(Ublist c ubl# ubls) e
      | isNull ubls && c < lim = fromList $ listL es :< e
      |          True          = Ublist c ubl# (ubls :< e)
    
    {-# INLINE single #-}
    single e = runST $ filled 1 e >>= done
    
    {-# INLINE fromList #-}
    fromList es = runST $ newLinear es >>= done
    
    {-# INLINE listL #-}
    
    listL = i_foldr (:) []
    
    Z  ++ ys = ys
    xs ++  Z = xs
    (Ublist c ubl# ubls) ++ ys = Ublist c ubl# (ubls ++ ys)
    
    {-# INLINE replicate #-}
    replicate n e = copy count
      where
        chunk  = runST $ ST $ \ s1# -> case newUnboxed' e l# s1# of (# s2#, mubl# #) -> done'    lim   mubl# Z s2#
        rest   = runST $ ST $ \ s1# -> case newUnboxed' e r# s1# of (# s2#, mubl# #) -> done' restSize mubl# Z s2#
        copy c = case c <=> 0 of {LT -> Z; EQ -> rest; GT -> chunk ++ copy (c - 1)}
        
        !(count, restSize@(I# r#)) = n `divMod` lim
        !(I# l#) = lim
    
    {-# INLINE reverse #-}
    reverse = fromList . i_foldl (flip (:)) []
    
    partitions ps es = fromList <$> partitions ps (listL es)

instance (Unboxed e) => Split (Ublist e) e
  where
    {-# INLINE take #-}
    take n es
        |   n < 1  = Z
        | es .<= n = es
        |   True   = take' n es
      where
        take' _ Z = Z
        take' n' (Ublist c ubl# ubls) = n' >= c ? Ublist c ubl# other $ fromListN n' rest
          where
            rest  = [ ubl# !# i# | (I# i#) <- [0 .. n' - 1] ]
            other = take' (n' - c) ubls
    
    {-# INLINE drop #-}
    drop n es
        |   n < 1  = es
        | es .<= n = Z
        |   True   = drop' n es
      where
        drop' _ Z = Z
        drop' n' (Ublist c ubl# ubls) = n' >= c ? rest $ other ++ ubls
          where
            rest  = drop' (n' - c) ubls
            other = fromListN (c - n') [ ubl# !# i# | (I# i#) <- [n' .. c - 1] ]
    
    isPrefixOf xs ys = listL xs `isPrefixOf` listL ys
    isInfixOf  xs ys = listL xs `isInfixOf`  listL ys
    isSuffixOf xs ys = listL xs `isSuffixOf` listL ys
    
    prefix p = i_foldr (\ e c -> p e ? c + 1 $ 0) 0
    suffix p = i_foldl (\ c e -> p e ? c + 1 $ 0) 0

instance (Unboxed e) => Bordered (Ublist e) Int e
  where
    sizeOf  es = case es of {Ublist n _ ubls -> max 0 n + sizeOf ubls; _ -> 0}
    indexIn es = \ i -> i >= 0 && i < sizeOf es
    
    lower   _  = 0
    upper   es = sizeOf es - 1
    bounds  es = (0, sizeOf es - 1)

--------------------------------------------------------------------------------

{- Indexed, IFold, Set and Sort instances. -}

instance (Unboxed e) => Indexed (Ublist e) Int e
  where
    {-# INLINE assoc  #-}
    assoc  bnds ascs = runST $ fromAssocs bnds ascs >>= done
    
    {-# INLINE assoc' #-}
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    {-# INLINE (//) #-}
    Z // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    es // ascs = isNull ascs ? es $ runST $ newLinear (listL es) >>= flip overwrite ascs >>= done
    
    fromIndexed es = runST $ do
        copy <- filled_ n (unreachEx "fromIndexed")
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        done copy
      where
        n = sizeOf es
    
    (!^) = (.!)
    
    {-# INLINE (.!) #-}
    es .! i@(I# i#)
        | isNull es = error "in SDP.ByteList.Ublist.(.!)"
        |   i < n   = bytes# !# i#
        |    True   = arrs .! (n - i)
      where
        !(Ublist n bytes# arrs) = es
    
    (!) es i = case inBounds (bounds es) i of
        ER -> throw $ EmptyRange     msg
        UR -> throw $ IndexUnderflow msg
        IN -> es .! i
        OR -> throw $ IndexOverflow  msg
      where
        msg = "in SDP.ByteList.Ublist.(!)"
    
    p .$ es = p .$ listL es
    p *$ es = p *$ listL es

instance (Unboxed e) => IFold (Ublist e) Int e
  where
    ifoldr  f base = \ ubl -> case ubl of
      Z                    -> base
      (Ublist c ubl# ubls) ->
        let go b i@(I# i#) = c == i ? b $ f i (ubl# !# i#) (go b $ i + 1)
        in  go (ifoldr f base ubls) 0
    
    ifoldl  f base = \ ubl -> case ubl of
      Z                    -> base
      (Ublist c ubl# ubls) ->
        let go b i@(I# i#) = -1 == i ? b $ f i (go b $ i - 1) (ubl# !# i#)
        in  ifoldl f (go base $ c - 1) ubls
    
    i_foldr f base = \ ubl -> case ubl of
      Z                    -> base
      (Ublist c ubl# ubls) ->
        let go b i@(I# i#) = c == i ? b $ f (ubl# !# i#) (go b $ i + 1)
        in  go (i_foldr f base ubls) 0
    
    i_foldl f base = \ ubl -> case ubl of
      Z                    -> base
      (Ublist c arr# arrs) ->
        let go b i@(I# i#) = -1 == i ? b $ f (go b $ i - 1) (arr# !# i#)
        in  i_foldl f (go base $ c - 1) arrs

instance (Unboxed e) => Set (Ublist e) e
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
        contain arr@(Ublist n _ arrs)
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
    
    isSubsetWith f xs ys = all (\ x -> isContainedIn f x ys) (listL xs)

instance (Unboxed e) => Sort (Ublist e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

instance IsString (Ublist Char) where fromString = fromList

--------------------------------------------------------------------------------

instance (Unboxed e) => Thaw (ST s) (Ublist e) (STUblist s e)
  where
    thaw Z = return STUBEmpty
    thaw (Ublist n ubl# ubls) = liftA2 cat list (thaw ubls)
      where
        cat  :: (Unboxed e) => STUblist s e -> STUblist s e -> STUblist s e
        cat  =  \ (STUblist _ stubl# _) stubls -> STUblist n stubl# stubls
        
        list = newLinear [ ubl# !# i# | (I# i#) <- [0 .. n - 1] ]

instance (Unboxed e) => Freeze (ST s) (STUblist s e) (Ublist e)
  where
    freeze es = copied es >>= done

--------------------------------------------------------------------------------

{-# INLINE done' #-}
done' :: (Unboxed e) => Int -> MutableByteArray# s -> Ublist e -> STRep s (Ublist e)
done' c marr# ubls = \ s1# -> case unsafeFreezeByteArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, Ublist c arr# ubls #)

{-# INLINE done #-}
done :: STUblist s e -> ST s (Ublist e)
done (STUblist n marr# marr) = done marr >>= \ arr -> ST $
  \ s1# -> case unsafeFreezeByteArray# marr# s1# of
    (# s2#, arr# #) -> (# s2#, Ublist n arr# arr #)
done _ = return UBEmpty

filled_ :: (Unboxed e) => Int -> e -> ST s (STUblist s e)
filled_ n e
    | n <  1  = return STUBEmpty
    | n < lim = ST $ \ s1# -> case newUnboxed e l# s1# of
      (# s2#, mubl# #) -> (# s2#, STUblist n mubl# STUBEmpty #)
    |  True   = filled_ (n - lim) e >>= \ mubls -> ST $
      \ s1# -> case newUnboxed e l# s1# of
        (# s2#, mubl# #) -> (# s2#, STUblist n mubl# mubls #)
  where
    !(I# l#) = lim

nubSorted :: (Unboxed e) => (e -> e -> Ordering) -> Ublist e -> Ublist e
nubSorted f (xs :< x) = fromList $ i_foldr (\ e ls@(l : _) -> e `f` l == EQ ? ls $ e : ls) [x] xs
nubSorted _ _ = Z

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.ByteList.Ublist." ++ msg

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.ByteList.Ublist." ++ msg

lim :: Int
lim =  1024

