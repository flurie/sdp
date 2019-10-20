{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, RoleAnnotations #-}

{- |
    Module      :  SDP.ByteList.Ublist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.ByteList.Ublist@ provides 'Ublist' - strict boxed unrolled linked list.
-}
module SDP.ByteList.Ublist
(
  -- * Exports
  module SDP.Indexed,
  module SDP.Unboxed,
  module SDP.Sort,
  module SDP.Set,
  
  -- * Ublist
  Ublist (..), fromPseudoBytes#
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Set

import GHC.Base ( Int  (..) )
import GHC.Show (  appPrec  )

import GHC.ST   ( runST, ST (..) )

import Data.String ( IsString (..) )

import SDP.ByteList.STUblist
import SDP.SortM.Tim

import SDP.Internal.SBytes
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | Ublist is internal data representation for ByteList.
data Ublist e = UBEmpty | Ublist !(SBytes# e) (Ublist e)

type role Ublist representational

{-# COMPLETE Z, Ublist #-}

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Unboxed e) => Eq (Ublist e)
  where
    (==) = go
      where
        go xs@(Ublist arr1# arr1) ys@(Ublist arr2# arr2) = if n1 > n2
            then take n2 arr1# == arr2# && go (drop n2 xs) arr2
            else take n1 arr2# == arr1# && go (drop n1 ys) arr1
          where
            n1 = sizeOf arr1#; n2 = sizeOf arr2#
        go Z Z = True
        go _ _ = False

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Ord e, Unboxed e) => Ord (Ublist e)
  where
    compare Z Z = EQ
    compare Z _ = LT
    compare _ Z = GT
    compare xs@(Ublist arr1# arr1) ys@(Ublist arr2# arr2) = if n1 > n2
        then (take n2 arr1# <=> arr2#) <> compare (drop n2 xs) arr2
        else (arr1# <=> take n2 arr2#) <> compare arr1 (drop n2 ys)
      where
        n1 = sizeOf arr1#; n2 = sizeOf arr2#

--------------------------------------------------------------------------------

{- Show instance. -}

instance (Show e, Unboxed e) => Show (Ublist e)
  where
    showsPrec p ubl = showParen (p > appPrec) $ showString "ublist "
                                              . shows (assocs ubl)

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Default, Arbitrary and Estimate instances. -}

instance (Unboxed e) => Semigroup (Ublist e) where (<>) = (++)
instance (Unboxed e) => Monoid    (Ublist e) where mempty = def

instance Default (Ublist e) where def = UBEmpty

instance (Arbitrary e, Unboxed e) => Arbitrary (Ublist e)
  where
    arbitrary = fromList <$> arbitrary

instance (Unboxed e) => Estimate (Ublist e)
  where
    (<==>) = go 0
      where
        go o Z   Z = o <=> 0
        go o xs  Z = xs <.=> (-o)
        go o Z  ys = o <=.> ys
        go o (Ublist arr1# arr1) (Ublist arr2# arr2) =
          let n1 = sizeOf arr1#; n2 = sizeOf arr2#
          in  go (o + n1 - n2) arr1 arr2
    
    Z <.=> n = 0 <=> n
    (Ublist arr# arr) <.=> m = arr# .> m ? GT $ arr <.=> (m - sizeOf arr#)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Unboxed e) => Linear (Ublist e) e
  where
    isNull es = case es of {UBEmpty -> True; Ublist Z UBEmpty -> True; _ -> False}
    
    lzero = UBEmpty
    
    toHead e Z = single e
    toHead e es@(Ublist arr# arr) = arr# .< lim ? Ublist (e :> arr#) arr $ Ublist (single e) es
    
    head Z  = pfailEx "(:>)"
    head es = es !^ 0
    
    tail Z = pfailEx "(:<)"
    tail (Ublist arr# arr) = isNull arr# ? tail arr $ Ublist (tail arr#) arr
    
    toLast Z e = single e
    toLast (Ublist arr# arr) e = isNull arr# ? (arr :< e) $ Ublist arr# (arr :< e)
    
    last Z = pfailEx "(:<)"
    last (Ublist arr# arr) = isNull arr ? last arr# $ last arr
    
    init Z = pfailEx "(:>)"
    init (Ublist arr# arr) = isNull arr ? Ublist (init arr#) Z $ Ublist arr# (init arr)
    
    {-# INLINE single #-}
    single = replicate 1
    
    listL = i_foldr (:) []
    
    {-# INLINE fromList #-}
    fromList = i_foldr (\ list -> Ublist $ fromList list) Z . chunks lim
    
    Z ++ ys = ys
    xs ++ Z = xs
    (Ublist arr# arr) ++ ys = Ublist arr# (arr ++ ys)
    
    -- | Deduplicated Unlist: O(1), O(1) memory (limited by a constant on top).
    {-# INLINE replicate #-}
    
    replicate n e = copy count
      where
        (count, rst) = n `divMod` lim
        copy c = case c <=> 0 of
          LT -> Z
          EQ -> Ublist rest Z
          GT -> Ublist chunk (copy $ c - 1)
        
        chunk = replicate lim e
        rest  = replicate rst e
    
    {-# INLINE reverse #-}
    reverse = fromList . i_foldl (flip (:)) []
    
    partition p es = (fromList x, fromList y)
      where
        (x, y) = partition p $ listL es
    
    partitions ps = fmap fromList . partitions ps . listL

instance (Unboxed e) => Split (Ublist e) e
  where
    {-# INLINE take #-}
    take n es | n < 1 = Z | es .<= n = es | True = take' n es
      where
        take' _  Z = Z
        take' n' (Ublist arr# arr) = case n <=.> arr# of
          LT -> Ublist (take n' arr#) Z
          EQ -> arr
          GT -> Ublist arr# (take (n' - sizeOf arr#) arr)
    
    {-# INLINE drop #-}
    drop n es | n < 1 = es | es .<= n = Z | True = drop' n es
      where
        drop' _ Z = Z
        drop' n' (Ublist arr# arr) = case n' <=.> arr# of
          LT -> Ublist (drop n' arr#) arr
          EQ -> arr
          GT -> drop' (n' - sizeOf arr#) arr
    
    isPrefixOf xs ys = listL xs `isPrefixOf` listL ys
    isInfixOf  xs ys = listL xs `isInfixOf`  listL ys
    isSuffixOf xs ys = listL xs `isSuffixOf` listL ys
    
    prefix p = i_foldr (\ e c -> p e ? c + 1 $ 0) 0
    suffix p = i_foldl (\ c e -> p e ? c + 1 $ 0) 0

instance (Unboxed e) => Bordered (Ublist e) Int e
  where
    sizeOf es = case es of {Ublist arr# arr -> sizeOf arr# + sizeOf arr; _ -> 0}
    
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

instance (Unboxed e) => Indexed (Ublist e) Int e
  where
    {-# INLINE assoc #-}
    assoc bnds ascs = runST $ fromAssocs bnds ascs >>= done
    
    {-# INLINE assoc' #-}
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    {-# INLINE (//) #-}
    Z // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    es // ascs = isNull ascs ? es $ runST $ thaw es >>= (`overwrite` ascs) >>= done
    
    fromIndexed es = runST $ do
        copy <- filled n (unreachEx "fromIndexed")
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        done copy
      where
        n = sizeOf es
    
    Z !^ _ = error "in SDP.ByteList.Ublist.(!^)"
    (Ublist arr# arr) !^ i = i < c ? arr# !^ i $ arr !^ (i - c)
      where
        c = sizeOf arr#
    
    {-# INLINE (.!) #-}
    (.!) = (!^)
    
    (!) Z _ = throw $ EmptyRange "in SDP.ByteList.Ublist.(!)"
    (!) (Ublist arr# arr) i
        | isNull arr# = throw $ IndexOverflow  "in SDP.ByteList.Ublist.(!)"
        |    i < 0    = throw $ IndexUnderflow "in SDP.ByteList.Ublist.(!)"
        |    i < c    = arr# !^ i
        |    True     = arr  !^ (i - c)
      where
        c = sizeOf arr#
    
    {-# INLINE (.$) #-}
    p .$ es = go es 0
      where
        go Z _ = Nothing
        go (Ublist arr# arr) o = case p .$ arr# of
          Just  i -> Just (i + o)
          Nothing -> go arr $! o + sizeOf arr#
    
    {-# INLINE (*$) #-}
    (*$) p es = go es 0
      where
        go Z _ = []
        go (Ublist arr# arr) o = (p *$ arr#) ++ (go arr $! o + sizeOf arr#)

instance (Unboxed e) => IFold (Ublist e) Int e
  where
    {-# INLINE ifoldr #-}
    ifoldr  f = \ base es -> case es of {Z -> base; (Ublist arr# arr) -> ifoldr  f (ifoldr  f base arr) arr#}
    
    {-# INLINE ifoldl #-}
    ifoldl  f = \ base es -> case es of {Z -> base; (Ublist arr# arr) -> ifoldl  f (ifoldl  f base arr#) arr}
    
    {-# INLINE i_foldr #-}
    i_foldr f = \ base es -> case es of {Z -> base; (Ublist arr# arr) -> i_foldr f (i_foldr f base arr) arr#}
    
    {-# INLINE i_foldl #-}
    i_foldl f = \ base es -> case es of {Z -> base; (Ublist arr# arr) -> i_foldl f (i_foldl f base arr#) arr}

instance (Unboxed e) => Set (Ublist e) e
  where
    setWith f = nubSorted f . sortBy f
    
    insertWith _ e Z = single e
    insertWith f e (Ublist arr# arr) = isContainedIn f e arr# ?
      Ublist (insertWith f e arr#) arr $ Ublist arr# (insertWith f e arr)
    
    deleteWith _ _ Z = Z
    deleteWith f e (Ublist arr# arr) = isContainedIn f e arr# ?
      Ublist (deleteWith f e arr#) arr $ Ublist arr# (deleteWith f e arr)
    
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
    
    {-# INLINE isContainedIn #-}
    isContainedIn = binaryContain
    
    isSubsetWith f xs ys = i_foldr (\ e b -> b && isContainedIn f e ys) True xs

instance (Unboxed e) => Sort (Ublist e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

instance IsString (Ublist Char) where fromString = fromList

--------------------------------------------------------------------------------

instance (Unboxed e) => Thaw (ST s) (Ublist e) (STUblist s e)
  where
    thaw Z = return STUBEmpty
    thaw (Ublist arr# arr) = liftA2 STUblist (thaw arr#) (thaw arr)
    
    unsafeThaw Z = return STUBEmpty
    unsafeThaw (Ublist arr# arr) = liftA2 STUblist (unsafeThaw arr#) (unsafeThaw arr)

instance (Unboxed e) => Freeze (ST s) (STUblist s e) (Ublist e)
  where
    freeze STUBEmpty = return Z
    freeze (STUblist marr# marr) = liftA2 Ublist (freeze marr#) (freeze marr)
    
    unsafeFreeze STUBEmpty = return Z
    unsafeFreeze (STUblist marr# marr) = liftA2 Ublist (unsafeFreeze marr#) (unsafeFreeze marr)

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: (Unboxed e) => STUblist s e -> ST s (Ublist e)
done = unsafeFreeze

{-# INLINE center #-}
center :: Int -> Int -> Int
center l u = l + (u - l) `div` 2

{-# INLINE nubSorted #-}
nubSorted :: (Unboxed e) => Compare e -> Ublist e -> Ublist e
nubSorted f (xs :< x) = fromList $ i_foldr (\ e ls@(l : _) -> e `f` l == EQ ? ls $ e : ls) [x] xs
nubSorted _ _ = Z

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.ByteList.Ublist." ++ msg

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.ByteList.Ublist." ++ msg

lim :: Int
lim =  1024





