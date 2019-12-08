{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
    Module      :  SDP.Unrolled.Unlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.Unrolled.Unlist@ provides 'Unlist' - lazy boxed unrolled linked list.
-}
module SDP.Unrolled.Unlist
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  -- * Unlist
  Unlist (..), SArray#, fromPseudoArray#
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed
import SDP.Sort
import SDP.Scan
import SDP.Set

import Test.QuickCheck

import GHC.Generics ( Generic (..) )

import GHC.Show ( appPrec  )
import GHC.Base ( Int (..) )
import GHC.ST   ( ST (..), runST )

import Data.String ( IsString (..) )

import SDP.Unrolled.STUnlist
import SDP.SortM.Tim

import SDP.Internal.SArray
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | Unlist is internal data representation for Unrolled.
data Unlist e = UNEmpty | Unlist !(SArray# e) (Unlist e) deriving ( Generic )

type role Unlist representational

{-# COMPLETE Z, Unlist #-}

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Eq e) => Eq (Unlist e)
  where
    Z == Z = True
    xs@(Unlist arr1# arr1) == ys@(Unlist arr2# arr2) = if n1 > n2
        then take n2 arr1# == arr2# && drop n2 xs == arr2
        else take n1 arr2# == arr1# && drop n1 ys == arr1
      where
        n1 = sizeOf arr1#
        n2 = sizeOf arr2#
    _ == _ = False

instance Eq1 Unlist
  where
    liftEq f = go
      where
        go xs@(Unlist arr1# arr1) ys@(Unlist arr2# arr2) = if n1 > n2
            then liftEq f (take n2 arr1#) arr2# && go (drop n2 xs) arr2
            else liftEq f arr1# (take n1 arr2#) && go arr1 (drop n1 ys)
          where
            n1 = sizeOf arr1#; n2 = sizeOf arr2#
        go Z Z = True
        go _ _ = False

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Ord e) => Ord (Unlist e) where compare = compare1

instance Ord1 Unlist
  where
    liftCompare cmp = go
      where
        go xs@(Unlist arr1# arr1) ys@(Unlist arr2# arr2) = if n1 > n2
            then liftCompare cmp (take n2 arr1#) arr2# <> go (drop n2 xs) arr2
            else liftCompare cmp arr1# (take n2 arr2#) <> go arr1 (drop n2 ys)
          where
            n1 = sizeOf arr1#; n2 = sizeOf arr2#
        go Z Z = EQ
        go Z _ = LT
        go _ Z = GT

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
        go o Z   Z = o <=> 0
        go o xs  Z = xs <.=> (-o)
        go o Z  ys = o <=.> ys
        go o (Unlist arr1# arr1) (Unlist arr2# arr2) =
          let n1 = sizeOf arr1#; n2 = sizeOf arr2#
          in  go (o + n1 - n2) arr1 arr2
    
    Z <.=> n = 0 <=> n
    (Unlist arr# arr) <.=> m = arr# .> m ? GT $ arr <.=> (m - sizeOf arr#)

--------------------------------------------------------------------------------

{- Functor and Applicative instances. -}

instance Functor Unlist
  where
    fmap f es = case es of {Unlist arr# arr -> Unlist (f <$> arr#) (f <$> arr); _ -> Z}

instance Applicative Unlist
  where
    pure = single
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable and Traversable instances. -}

instance Foldable Unlist
  where
    foldr  f base = \ es -> case es of {Z -> base; Unlist arr# arr -> foldr  f (foldr  f base arr) arr#}
    foldr' f base = \ es -> case es of {Z -> base; Unlist arr# arr -> foldr' f (foldr' f base arr) arr#}
    
    foldl  f base = \ es -> case es of {Z -> base; Unlist arr# arr -> foldl  f (foldl  f base arr#) arr}
    foldl' f base = \ es -> case es of {Z -> base; Unlist arr# arr -> foldl' f (foldl' f base arr#) arr}
    
    length es = case es of {Unlist arr# arr -> sizeOf arr# + sizeOf arr; _ -> 0}
    null es = case es of {UNEmpty -> True; Unlist Z UNEmpty -> True; _ -> False}

instance Traversable Unlist
  where
    traverse f = fmap fromList . foldr (\ x ys -> liftA2 (:) (f x) ys) (pure [])

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance Linear (Unlist e) e
  where
    isNull = null
    
    lzero = UNEmpty
    
    toHead e Z = single e
    toHead e es@(Unlist arr# arr) = arr# .< lim ? Unlist (e :> arr#) arr $ Unlist (single e) es
    
    head Z  = pfailEx "(:>)"
    head es = es !^ 0
    
    tail Z = pfailEx "(:<)"
    tail (Unlist arr# arr) = isNull arr# ? tail arr $ Unlist (tail arr#) arr
    
    toLast Z e = single e
    toLast (Unlist arr# arr) e = isNull arr# ? (arr :< e) $ Unlist arr# (arr :< e)
    
    last Z = pfailEx "(:<)"
    last (Unlist arr# arr) = isNull arr ? last arr# $ last arr
    
    init Z = pfailEx "(:>)"
    init (Unlist arr# arr) = isNull arr ? Unlist (init arr#) Z $ Unlist arr# (init arr)
    
    single = replicate 1
    
    listL = foldr (:) []
    
    fromList = foldr (\ list -> Unlist $ fromList list) Z . chunks lim
    
    Z ++ ys = ys
    xs ++ Z = xs
    (Unlist arr# arr) ++ ys = Unlist arr# (arr ++ ys)
    
    -- | Deduplicated Unlist: O(1), O(1) memory (limited by a constant on top).
    replicate n e = copy count
      where
        (count, rst) = n `divMod` lim
        copy c = case c <=> 0 of
          LT -> Z
          EQ -> Unlist rest Z
          GT -> Unlist chunk (copy $ c - 1)
        
        chunk = replicate lim e
        rest  = replicate rst e
    
    reverse = fromList . foldl (flip (:)) []
    
    partition p es = (fromList x, fromList y)
      where
        (x, y) = partition p $ toList es
    
    partitions ps = fmap fromList . partitions ps . toList

instance Split (Unlist e) e
  where
    take n es | n < 1 = Z | es .<= n = es | True = take' n es
      where
        take' _  Z = Z
        take' n' (Unlist arr# arr) = case n <=.> arr# of
          LT -> Unlist (take n' arr#) Z
          EQ -> arr
          GT -> Unlist arr# (take (n' - sizeOf arr#) arr)
    
    drop n es | n < 1 = es | es .<= n = Z | True = drop' n es
      where
        drop' _ Z = Z
        drop' n' (Unlist arr# arr) = case n' <=.> arr# of
          LT -> Unlist (drop n' arr#) arr
          EQ -> arr
          GT -> drop' (n' - sizeOf arr#) arr
    
    isPrefixOf xs ys = toList xs `isPrefixOf` toList ys
    isInfixOf  xs ys = toList xs `isInfixOf`  toList ys
    isSuffixOf xs ys = toList xs `isSuffixOf` toList ys

instance Bordered (Unlist e) Int e
  where
    sizeOf es = case es of {Unlist arr# arr -> sizeOf arr# + sizeOf arr; _ -> 0}
    
    indexIn es = \ i -> i >= 0 && i <. es
    
    -- | Quick unchecked offset.
    offsetOf = const id
    
    -- | Quick unchecked index.
    indexOf = const id
    
    lower  _  = 0
    upper  es = sizeOf es - 1
    bounds es = (0, sizeOf es - 1)

--------------------------------------------------------------------------------

{- Indexed, IFold, Set and Sort instances. -}

instance Indexed (Unlist e) Int e
  where
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
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
    
    {-# INLINE (!^) #-}
    Z !^ _ = error "in SDP.Unrolled.Unlist.(!^)"
    (Unlist arr# arr) !^ i = i < c ? arr# !^ i $ arr !^ (i - c)
      where
        c = sizeOf arr#
    
    {-# INLINE (.!) #-}
    (.!) = (!^)
    
    (!) Z _ = throw $ EmptyRange "in SDP.Unrolled.(!)"
    (!) (Unlist arr# arr) i
        | isNull arr# = throw $ IndexOverflow  "in SDP.Unrolled.(!)"
        |    i < 0    = throw $ IndexUnderflow "in SDP.Unrolled.(!)"
        |    i < c    = arr# !^ i
        |    True     = arr  !^ (i - c)
      where
        c = sizeOf arr#
    
    p .$ es = go es 0
      where
        go Z _ = Nothing
        go (Unlist arr# arr) o = case p .$ arr# of
          Just  i -> Just (i + o)
          Nothing -> go arr $! o + sizeOf arr#
    
    (*$) p es = go es 0
      where
        go Z _ = []
        go (Unlist arr# arr) o = (p *$ arr#) ++ (go arr $! o + sizeOf arr#)

instance IFold (Unlist e) Int e
  where
    ifoldr f base = \ es -> case es of {Z -> base; (Unlist arr# arr) -> ifoldr f (ifoldr f base arr) arr#}
    ifoldl f base = \ es -> case es of {Z -> base; (Unlist arr# arr) -> ifoldl f (ifoldl f base arr#) arr}
    
    i_foldr = foldr
    i_foldl = foldl

instance Set (Unlist e) e
  where
    setWith f = nubSorted f . sortBy f
    
    insertWith _ e Z = single e
    insertWith f e (Unlist arr# arr) = isContainedIn f e arr# ?
      Unlist (insertWith f e arr#) arr $ Unlist arr# (insertWith f e arr)
    
    deleteWith _ _ Z = Z
    deleteWith f e (Unlist arr# arr) = isContainedIn f e arr# ?
      Unlist (deleteWith f e arr#) arr $ Unlist arr# (deleteWith f e arr)
    
    intersectionWith f xs ys = fromList $ on (intersectionWith f) listL xs ys
    
    unionWith f xs ys = fromList $ on (unionWith f) listL xs ys
    
    differenceWith f xs ys = fromList $ on (differenceWith f) listL xs ys
    
    symdiffWith f xs ys = fromList $ on (symdiffWith f) listL xs ys
    
    lookupLTWith _ _ Z  = Nothing
    lookupLTWith f o es
        | GT <- o `f` last' = Just last'
        | GT <- o `f` head' = look' head' 0 (upper es)
        |       True        = Nothing
      where
        head' = es !^ 0
        last' = es !^ upper es
        
        look' r l u = if l > u then Just r else case o `f` e of
            LT -> look' r l (j - 1)
            EQ -> Just (j < 1 ? r $ es !^ (j - 1))
            GT -> look' e (j + 1) u
          where
            j = center l u; e = es !^ j
    
    lookupLEWith _ _ Z  = Nothing
    lookupLEWith f o es
        | GT <- o `f` last' = Just last'
        | LT <- o `f` head' = Nothing
        |       True        = look' head' 0 (upper es)
      where
        head' = es !^ 0
        last' = es !^ upper es
        
        look' r l u = if l > u then Just r else case o `f` e of
            LT -> look' r l (j - 1)
            _  -> look' e (j + 1) u
          where
            j = center l u; e = es !^ j
    
    lookupGTWith _ _ Z  = Nothing
    lookupGTWith f o es
        | LT <- o `f` head' = Just head'
        | LT <- o `f` last' = look' last' 0 (upper es)
        |       True        = Nothing
      where
        head' = es !^ 0
        last' = es !^ upper es
        
        look' r l u = if l > u then Just r else case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> (j + 1) >=. es ? Nothing $ Just (es !^ (j + 1))
            GT -> look' r (j + 1) u
          where
            j = center l u; e = es !^ j
    
    lookupGEWith _ _ Z  = Nothing
    lookupGEWith f o es
        | GT <- o `f` last' = Nothing
        | GT <- o `f` head' = look' last' 0 (upper es)
        |       True        = Just head'
      where
        head' = es !^ 0
        last' = es !^ upper es
        
        look' r l u = if l > u then Just r else case o `f` e of
            LT -> look' e l (j - 1)
            EQ -> Just e
            GT -> look' r (j + 1) u
          where
            j = center l u; e = es !^ j
    
    isContainedIn = binaryContain

instance Sort (Unlist e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

instance IsString (Unlist Char) where fromString = fromList

--------------------------------------------------------------------------------

instance Thaw (ST s) (Unlist e) (STUnlist s e)
  where
    thaw Z = return STUNEmpty
    thaw (Unlist arr# arr) = liftA2 STUnlist (thaw arr#) (thaw arr)
    
    unsafeThaw Z = return STUNEmpty
    unsafeThaw (Unlist arr# arr) = liftA2 STUnlist (unsafeThaw arr#) (unsafeThaw arr)

instance Freeze (ST s) (STUnlist s e) (Unlist e)
  where
    freeze STUNEmpty = return Z
    freeze (STUnlist marr# marr) = liftA2 Unlist (freeze marr#) (freeze marr)
    
    unsafeFreeze STUNEmpty = return Z
    unsafeFreeze (STUnlist marr# marr) = liftA2 Unlist (unsafeFreeze marr#) (unsafeFreeze marr)

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: STUnlist s e -> ST s (Unlist e)
done = unsafeFreeze

{-# INLINE center #-}
center :: Int -> Int -> Int
center l u = l + (u - l) `div` 2

{-# INLINE nubSorted #-}
nubSorted :: Compare e -> Unlist e -> Unlist e
nubSorted f (xs :< x) = fromList $ foldr (\ e ls@(l : _) -> e `f` l == EQ ? ls $ e : ls) [x] xs
nubSorted _ _ = Z

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.Unrolled.Unlist." ++ msg

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.Unrolled.Unlist." ++ msg

lim :: Int
lim =  1024


