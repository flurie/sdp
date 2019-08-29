{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns #-}

{- |
    Module      :  SDP.Unrolled.Ublist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    This module provides service type Ublist - strict boxed unrolled linked list
    for SDP.ByteList.
-}
module SDP.ByteList.Ublist
(
  module SDP.Indexed,
  module SDP.Unboxed,
  module SDP.Sort,
  module SDP.Set,
  
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
    
    unsafeFreezeByteArray#, isTrue#, (+#), (-#), (==#), (<#)
  )

import GHC.ST   ( ST (..), STRep, runST )
import GHC.Show ( appPrec )

import Data.String ( IsString (..) )

import SDP.ByteList.STUblist
import SDP.SortM.Stuff
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | Ublist is internal data representation.
data Ublist e = UBEmpty | Ublist {-# UNPACK #-} !Int (ByteArray#) (Ublist e)

{-# COMPLETE Z, Ublist #-}

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Eq e,  Unboxed e) => Eq  (Ublist e) where xs == ys = listL xs == listL ys

instance (Ord e, Unboxed e) => Ord (Ublist e) where compare xs ys = listL xs <=> listL ys

--------------------------------------------------------------------------------

{- Show instances. -}

instance (Unboxed e, Show e) => Show (Ublist e)
  where
    showsPrec p ubl = showParen (p > appPrec) $ showString "ublist "
                                              . shows (assocs ubl)

--------------------------------------------------------------------------------

{- Semigroup, Monoid and Default instances. -}

instance (Unboxed e) => Semigroup (Ublist e) where (<>) = (++)

instance (Unboxed e) => Monoid (Ublist e) where mempty = UBEmpty

instance Default (Ublist e) where def = UBEmpty

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Unboxed e) => Linear (Ublist e) e
  where
    isNull es = case es of {Ublist c _ _ -> c < 1; _ -> True}
    
    lzero = def
    
    head Z  = pfailEx ".(:>)"
    head es = es .! 0
    
    tail Z = pfailEx "(:<)"
    tail es@(Ublist _ _ Z) = fromList . tail $ listL es
    tail es@(Ublist c ubl# ubls) = Ublist c' new# ubls
      where
        !(Ublist c' new# _) = (`asTypeOf` es) $ tail (Ublist c ubl# Z)
    
    toHead e Z = single e
    toHead e (Ublist c ubl# ubls) = c < lim ? res $ (Ublist 1 single# ubls)
      where
        res = fromListN (c + 1) $ e : listL (Ublist c ubl# Z)
        !(Ublist 1 single# Z) = single e
    
    last Z = pfailEx "(:<)"
    last (Ublist (I# c#) ubl# Z) = ubl# !# (c# -# 1#)
    last (Ublist _ _ ubls) = last ubls
    
    init Z                    = pfailEx "(:>)"
    init es@(Ublist c _ Z)    = fromListN (c - 1) . init $ listL es
    init (Ublist c arr# arrs) = Ublist c arr# (init arrs)
    
    toLast Z e = single e
    toLast es@(Ublist c _ Z) e = c < lim ? res $ (Ublist 1 single# Z)
      where
        res = fromList (listL es :< e)
        !(Ublist 1 single# Z) = single e
    toLast (Ublist c ubl# ubls) e = Ublist c ubl# (ubls :< e)
    
    {-# INLINE single #-}
    single e = runST $ filled 1 e >>= done
    
    fromList = fromFoldable
    
    {-# INLINE fromFoldable #-}
    fromFoldable es = runST $ fromFoldableM es >>= done
    
    {-# INLINE listL #-}
    listL = list' 0#
      where
        list' _ Z = []
        list' i# es@(Ublist (I# n#) ubl# ubls) = if isTrue# (i# <# n#)
          then (ubl# !# i#) : list' (i# +# 1#) es
          else list' 0# ubls
    
    Z  ++ ys = ys
    xs ++  Z = xs
    (Ublist c ubl# ubls) ++ ys = Ublist c ubl# (ubls ++ ys)
    
    {-# INLINE replicate #-}
    replicate n e = copy count
      where
        chunk  = runST $ ST $ \ s1# -> case newUnboxed' e l# s1# of (# s2#, mubl# #) -> done'    lim   Z mubl# s2#
        rest   = runST $ ST $ \ s1# -> case newUnboxed' e r# s1# of (# s2#, mubl# #) -> done' restSize Z mubl# s2#
        copy c = case c <=> 0 of {LT -> Z; EQ -> rest; GT -> chunk ++ copy (c - 1)}
        
        !(count, restSize@(I# r#)) = n `divMod` lim
        !(I# l#) = lim
    
    {-# INLINE reverse #-}
    reverse es' = reverse' Z es'
      where
        reverse' tail' Z = tail'
        reverse' tail' (Ublist n@(I# n#) bytes# bytes) = reverse' (Ublist n rev# tail') bytes
          where
            !(Ublist _ rev# _) = toChunk err chunk `asTypeOf` bytes
            
            toChunk :: (Unboxed e) => e -> [e] -> Ublist e
            toChunk e es = runST $ ST $ \ s1# -> case newUnboxed e n# s1# of
              (# s2#, marr# #) ->
                let go x r = \ i# s3# -> case writeByteArray# marr# i# x s3# of
                      s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
                in done' n Z marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
            
            chunk = [ bytes# !# i# | (I# i#) <- [ n - 1, n - 2 .. 0 ] ]
            err   = unreachEx "reverse"
    
    partitions ps es = fromList <$> partitions ps (listL es)

instance (Unboxed e) => Split (Ublist e) e
  where
    {-# INLINE take #-}
    take n es
        | n <= 0 = Z
        | l <= n = es
        |  True  = take' n es
      where
        take' _ Z = Z
        take' n' (Ublist c ubl# ubls) = n' >= c ? Ublist c ubl# other $ fromListN n' rest
          where
            rest  = [ ubl# !# i# | (I# i#) <- [0 .. n' - 1] ]
            other = take' (n' - c) ubls
        l = sizeOf es
    
    {-# INLINE drop #-}
    drop n es
        | n <= 0 = es
        | l <= n = Z
        |  True  = drop' n es
      where
        drop' _ Z = Z
        drop' n' (Ublist c ubl# ubls) = n' >= c ? rest $ other ++ ubls
          where
            rest  = drop' (n' - c) ubls
            other = fromListN (c - n') [ ubl# !# i# | (I# i#) <- [n' .. c - 1] ]
        l = sizeOf es
    
    isPrefixOf xs ys = listL xs `isPrefixOf` listL ys
    isInfixOf  xs ys = listL xs `isInfixOf`  listL ys
    isSuffixOf xs ys = listL xs `isSuffixOf` listL ys
    
    prefix p es = prefix p (listL es)
    suffix p es = suffix p (listL es)

instance (Unboxed e) => Bordered (Ublist e) Int e
  where
    lower   _  = 0
    upper   es = sizeOf es - 1
    indexOf es i = i >= 0 && i < sizeOf es
    sizeOf  es = case es of {Ublist n _ ubls -> max 0 n + sizeOf ubls; _ -> 0}

--------------------------------------------------------------------------------

{- Indexed, Set and Sort instances. -}

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

instance (Unboxed e) => Set (Ublist e) e
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
    
    {-# INLINE isContainedIn #-}
    isContainedIn f e es = contain es
      where
        contain Z = False
        contain (Ublist n arr# arrs) = contain' 0 || contain arrs
          where
            contain' i@(I# i#) = i == n ? False $ case e `f` (arr# !# i#) of
              LT -> False
              EQ -> True
              GT -> contain' (i + 1)
    
    isSubsetWith f xs ys = all (\ x -> isContainedIn f x ys) (listL xs)

instance (Unboxed e) => Sort (Ublist e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

instance IsString (Ublist Char) where fromString = fromList

instance (Unboxed e, Arbitrary e) => Arbitrary (Ublist e) where arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

instance (Unboxed e) => Thaw (ST s) (Ublist e) (STUblist s e)
  where
    thaw Z = return (STUBEmpty)
    thaw (Ublist n ubl# ubls) = liftA2 thaw' list (thaw ubls)
      where
        thaw' :: (Unboxed e) => STUblist s e -> STUblist s e -> STUblist s e
        thaw' = \ (STUblist _ stubl# _) stubls -> STUblist n stubl# stubls
        
        list = newLinear [ ubl# !# i# | (I# i#) <- [0 .. n - 1] ]

instance (Unboxed e) => Freeze (ST s) (STUblist s e) (Ublist e)
  where
    freeze = done

--------------------------------------------------------------------------------

{-# INLINE done' #-}
done' :: (Unboxed e) => Int -> Ublist e -> MutableByteArray# s -> STRep s (Ublist e)
done' c arrs marr# = \ s1# -> case unsafeFreezeByteArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, Ublist c arr# arrs #)

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
nubSorted f (xs :< x) = fromList $ i_foldr [x] xs
  where
    i_foldr base Z = base
    i_foldr base (Ublist c arr# arrs) = go (i_foldr base arrs) 0
      where
        go b i@(I# i#) = c == i ? b $ (e `f` l == EQ ? ls $ e : ls)
          where
            ls@(l : _) = (go b $ i + 1)
            e = arr# !# i#
nubSorted _ _ = Z

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.ByteList.Ublist." ++ msg

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.ByteList.Ublist." ++ msg

lim :: Int
lim =  1024

