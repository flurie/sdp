{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.Unrolled
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    This module provides Unrolled - unrolled linked list.
-}

module SDP.Unrolled
(
  Unrolled (..),
  Unlist, -- type Unlist is abstract.
  
  module SDP.Indexed,
  module SDP.Scan,
  module SDP.Set
)
where

import Prelude ()
import SDP.SafePrelude
import Test.QuickCheck

import GHC.Base
  (
    Array#, MutableArray#, Int (..),
    
    newArray#, unsafeFreezeArray#, writeArray#, indexArray#,
    
    isTrue#, (+#), (-#), (==#)
  )
import GHC.ST   ( ST (..), STRep, runST )
import GHC.Show ( appPrec )

import Text.Read
import Text.Read.Lex ( expect )

import SDP.Array.Mutable
import SDP.Indexed
import SDP.Simple
import SDP.Scan
import SDP.Set

default ()

--------------------------------------------------------------------------------

-- | Bordered unrolled linked list.
data Unrolled i e = Unrolled !i !i (Unlist e)

type role Unrolled nominal representational

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Eq e, Index i) => Eq (Unrolled i e) where (==) = eq1

instance (Index i) => Eq1 (Unrolled i)
  where
    liftEq f unr1@(Unrolled l1 u1 xs) unr2@(Unrolled l2 u2 ys) = res
      where
        res = null xs && null ys || l1 == l2 && u1 == u2 && liftEq f xs' ys'
        xs' = toList unr1
        ys' = toList unr2

--------------------------------------------------------------------------------

{- Ord and Ord1 innstances. -}

instance (Ord e, Index i) => Ord (Unrolled i e) where compare = compare1

instance (Index i) => Ord1 (Unrolled i)
  where
    liftCompare cmp unr1 unr2 = liftCompare cmp' (assocs unr1) (assocs unr2)
      where
        cmp' (ix, x) (iy, y) = (ix <=> iy) <> (cmp x y)

--------------------------------------------------------------------------------

{- Show and Read instances -}

instance (Index i, Show i, Show e) => Show (Unrolled i e)
  where
    showsPrec p unr@(Unrolled l u _) = showParen (p > appPrec) shows'
      where
        shows' = showString "unrolled " . shows (l, u) . showChar ' ' . shows (assocs unr)

instance (Index i, Read i, Read e) => Read (Unrolled i e)
  where
    readList = readListDefault
    readPrec = parens $ prec appPrec (lift . expect $ Ident "unrolled") >> liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances -}

instance (Index i) => Functor (Unrolled i)
  where
    fmap f (Unrolled l u es) = Unrolled l u (f <$> es)

instance (Index i) => Zip (Unrolled i)
  where
    zipWith  f as bs             = fromList $ zipWith  f (toList as) (toList bs)
    zipWith3 f as bs cs          = fromList $ zipWith3 f (toList as) (toList bs) (toList cs)
    zipWith4 f as bs cs ds       = fromList $ zipWith4 f (toList as) (toList bs) (toList cs) (toList ds)
    zipWith5 f as bs cs ds es    = fromList $ zipWith5 f (toList as) (toList bs) (toList cs) (toList ds) (toList es)
    zipWith6 f as bs cs ds es fs = fromList $ zipWith6 f (toList as) (toList bs) (toList cs) (toList ds) (toList es) (toList fs)

instance (Index i) => Applicative (Unrolled i)
  where
    pure = single
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable, Scan and Traversable instances -}

instance (Index i) => Foldable (Unrolled i)
  where
    foldr  f base (Unrolled l u es) = foldr  f base $ take (size (l, u)) es
    foldl  f base (Unrolled l u es) = foldl  f base $ take (size (l, u)) es
    
    foldr' f base (Unrolled l u es) = foldr' f base $ take (size (l, u)) es
    foldl' f base (Unrolled l u es) = foldl' f base $ take (size (l, u)) es
    
    foldr1 f (Unrolled l u es) = foldr1 f $ take (size (l, u)) es
    foldl1 f (Unrolled l u es) = foldl1 f $ take (size (l, u)) es
    
    toList   (Unrolled l u es) = take n $ toList es where n = size (l, u)
    elem e   (Unrolled l u es) = elem e $ take (size (l, u)) es
    null     (Unrolled l u es) = null es || isEmpty (l, u)
    length   (Unrolled l u  _) = size (l, u)

-- instance (Index i) => Scan (Unrolled i)

instance (Index i) => Traversable (Unrolled i)
  where
    traverse f arr = fromList <$> traverse f (toList arr)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i) => Linear (Unrolled i e) e
  where
    isNull = null
    
    listL  = toList
    
    fromListN n es = Unrolled l u $ fromListN n es
      where
        l = unsafeIndex 0
        u = unsafeIndex $ max 0 n - 1
    
    uncons Z = throw $ EmptyRange "in SDP.Unrolled.(:>)"
    uncons (Unrolled l u es) = (x, es <. 2 ? Z $ Unrolled l1 u xs)
      where
        (x, xs) = uncons es
        l1 = next (l, u) l
    
    unsnoc Z = throw $ EmptyRange "in SDP.Unrolled.(:<)"
    unsnoc (Unrolled l u es) = (es <. 2 ? Z $ Unrolled l u1 xs, x)
      where
        (xs, x) = unsnoc es
        u1 = prev (l, u) u
    
    concat xss = Unrolled l u res
      where
        (n', res) = foldr f (0, Z) xss
        
        f = \ (Unrolled _ _ xs) (len, ys) -> (len + length xs, xs ++ ys)
        
        l = unsafeIndex 0
        u = unsafeIndex $ max 0 n' - 1
    
    intersperse e (Unrolled _ _ es) = Unrolled l1 u1 (intersperse e es)
      where
        n1 = case n <=> 0 of {LT -> -2; EQ -> 0; GT -> 2 * n - 2}; n = length es
        u1 = unsafeIndex n1
        l1 = unsafeIndex 0

instance (Index i) => Split (Unrolled i e) e
  where
    take n unr@(Unrolled l u es)
        | n <= 0 = Z
        | n >= c = unr
        |  True  = Unrolled l u' (take n es)
      where
        u' = index (l, u) $ n - 1
        c  = size  (l, u)
    
    drop n unr@(Unrolled l u es)
        | n <= 0 = unr
        | n >= c = Z
        |  True  = Unrolled l' u (drop n es)
      where
        l' = index (l, u) n
        c  = size  (l, u)
    
    split n unr@(Unrolled l u es)
        | n <= 0 = (Z, unr)
        | n >= c = (unr, Z)
        |  True  = (Unrolled l u' take', Unrolled l' u drop')
      where
        u' = index (l, u) $ n - 1
        l' = index (l, u)   n
        c  = size  (l, u)
        
        (take', drop') = split n es
    
    prefix f = prefix f . toList
    suffix f = suffix f . toList
    
    isPrefixOf = isPrefixOf `on` toList
    isInfixOf  = isInfixOf  `on` toList
    isSuffixOf = isSuffixOf `on` toList

instance (Index i) => Bordered (Unrolled i e) i e
  where
    indices (Unrolled l u _) = range (l, u)
    bounds  (Unrolled l u _) = (l, u)
    lower   (Unrolled l _ _) = l
    upper   (Unrolled _ u _) = u
    
    sizeOf  (Unrolled l u _) = size (l, u)

--------------------------------------------------------------------------------

instance (Index i) => Indexed (Unrolled i e) i e
  where
    -- [internal]: it's correct, but completly inneficient (Set []). Rewrite.
    assoc' bnds e ies = fromListN n sorted
      where
        sorted = snds $ unionWith cmpfst (setWith cmpfst ies) filler
        filler = zip (range bnds) (replicate n e)
        n = size bnds
    
    Z  // []   = Z
    Z  // ascs = assoc (minimum ixs, maximum ixs) ascs where ixs = fsts ascs
    
    (Unrolled l u es) // ascs = Unrolled l' u' es'
      where
        es' = es // [ (offset (l, u) i, e) | (i, e) <- ascs ]
        u'  = unsafeIndex $ upper es'
        l'  = unsafeIndex 0
    
    (Unrolled l u es)   .! i = es !# (offset (l, u) i)
    
    (!) (Unrolled l u es)  i = es !# (offset (l, u) i)
    
    (Unrolled l u arrs) !? i = inRange (l, u) i ? Just (arrs !# offset (l, u) i) $ Nothing
    
    predicate .$ (Unrolled l u es) = index (l, u) <$> (predicate .$ es)
    predicate *$ (Unrolled l u es) = index (l, u) <$> (predicate *$ es)

--------------------------------------------------------------------------------

instance (Index i, Arbitrary e) => Arbitrary (Unrolled i e)
  where
    arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

instance (Index i) => Estimate (Unrolled i)
  where
    (Unrolled l1 u1 _) <==> (Unrolled l2 u2 _) = compare s1 s2
      where
        s1 = size (l1, u1)
        s2 = size (l2, u2)

instance (Index i) => LineS (Unrolled i e) e
  where
    stream = stream . toList

instance (Index i) => Default (Unrolled i e) where def = Z

--------------------------------------------------------------------------------

{--------------------}
{- INTERNAL SECTION -}
{--------------------}

-- | Unlist is internal (abstract) type.
data Unlist e = UNEmpty | Unlist {-# UNPACK #-} !Int (Array# e) (Unlist e)

--------------------------------------------------------------------------------

{- Eq1 and Ord1 instances. -}

instance Eq1  Unlist where liftEq f xs ys = liftEq f (toList xs) (toList ys)

instance Ord1 Unlist where liftCompare f xs ys = liftCompare f (toList xs) (toList ys)

--------------------------------------------------------------------------------

{- Functor instance. -}

instance Functor Unlist
  where
    fmap _ UNEmpty = UNEmpty
    fmap f arr@(Unlist n@(I# n#) _ arrs) = runST
        (
          ST $ \ s1# ->
            case newArray# n# (undEx "fmap") s1# of
              (# s2#, marr# #) ->
                let go i s3# = if i == n
                    then case unsafeFreezeArray# marr# s3# of
                      (# s4#, arr# #) -> (# s4#, Unlist n arr# (f <$> arrs) #)
                    else fill marr# (i, f $ arr !# i) (go $ i + 1) s3#
                in go 0 s2#
        )

--------------------------------------------------------------------------------

{- Foldable instance. -}

instance Foldable Unlist
  where
    foldr _ base Z = base
    foldr f base (Unlist c arr# arrs) = go (foldr f base arrs) 0
      where
        go b i@(I# i#) = i == c ? b $ f e (go b $ i + 1)
          where
            (# e #) = indexArray# arr# i#
    
    foldr' _ base Z = base
    foldr' f base (Unlist c arr# arrs) = go (foldr' f base arrs) 0
      where
        go b i@(I# i#) = i == c ? b $ f e (go b $ i + 1)
          where
            (# e #) = indexArray# arr# i#
    
    foldl _ base Z = base
    foldl f base (Unlist c arr# arrs) = foldl f (go base $ c - 1) arrs
      where
        go b i@(I# i#) = -1 == i ? b $ f (go b $ i - 1) e
          where
            (# e #) = indexArray# arr# i#
    
    foldl' _ base Z = base
    foldl' f base (Unlist c arr# arrs) = foldl' f (go base c) arrs
      where
        go b i@(I# i#) = -1 == i ? b $ f (go b $ i - 1) e
          where
            (# e #) = indexArray# arr# i#
    
    length Z = 0
    length (Unlist c _ arrs) = c + (length arrs)
    
    toList Z = []
    toList (Unlist c arr# arrs) = foldr addIxElem (toList arrs) [0 .. c - 1]
      where
        addIxElem (I# i#) = let (# e #) = indexArray# arr# i# in (e :)
    
    null UNEmpty = True
    null (Unlist c _ _) = c < 0

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance Linear (Unlist e) e
  where
    isNull = null
    
    listL = toList
    
    fromList [] = UNEmpty
    fromList es = runST $ ST
        (
          \ s1# -> case newArray# n# (undEx "fromList") s1# of
              (# s2#, marr# #) ->
                let go e r = \ i# s3# -> case writeArray# marr# i# e s3# of
                      s4# -> if isTrue# (i# ==# n# -# 1#)
                                then s4#
                                else r (i# +# 1#) s4#
                in done n' (fromList others) marr#
                (
                  if n' == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2#
                )
        )
      where
        (curr, others) = split _UNLIST_CHUNK_MAX_SIZE_ es
        !n'@(I# n#)    = length curr
    
    head Z  = throw $ EmptyRange "in SDP.Unrolled.(:>)"
    head es = es !# 0
    
    last Z  = throw $ EmptyRange "in SDP.Unrolled.(:<)"
    last es = es !# (length es - 1)
    
    tail Z  = throw $ EmptyRange "in SDP.Unrolled.(:<)"
    tail es@(Unlist _ _ Z) = fromList . tail $ toList es
    tail (Unlist c arr# arrs) = Unlist c' new# arrs
      where
        !(Unlist c' new# _) = tail (Unlist c arr# Z)
    
    init Z = throw $ EmptyRange "in SDP.Unrolled.(:>)"
    init es@(Unlist _ _ Z) = fromList . init $ toList es
    init (Unlist c arr# arrs) = Unlist c arr# (init arrs)
    
    Z ++ ys = ys
    xs ++ Z = xs
    (Unlist c arr# arrs) ++ ys = Unlist c arr# (arrs ++ ys)
    
    replicate n e = copy count chunk
      where
        copy c ch@(Unlist _ chunk# _) = case c <=> 0 of
          LT -> Z
          EQ -> fromListN restsize (repeat e)
          GT -> Unlist lim chunk# (copy (c - 1) ch)
        
        (count, restsize) = n `divMod` lim
        
        chunk = fromListN lim (repeat e)
        lim   = _UNLIST_CHUNK_MAX_SIZE_
    
    toHead e Z = single e
    toHead e (Unlist c arr# arrs) = c < lim ? res1 $ (Unlist 1 single# arrs)
      where
        res1 = fromListN (max 0 c + 1) $ e : toList (Unlist c arr# Z)
        !(Unlist 1 single# Z) = single e
        
        lim  = _UNLIST_CHUNK_MAX_SIZE_
    
    toLast Z e = single e
    toLast es@(Unlist c _ Z) e = c < lim ? res1 $ (Unlist 1 single# Z)
      where
        res1 = fromListN (max 0 c + 1) $ foldr (:) [e] es
        !(Unlist 1 single# Z) = single e
        
        lim  = _UNLIST_CHUNK_MAX_SIZE_
    toLast (Unlist c arr# arrs) e = Unlist c arr# (toLast arrs e)
    
    partition predicate es = (fromList x, fromList y)
      where
        (x, y) = partition predicate $ toList es

instance Split (Unlist e) e
  where
    take n es
        |  n <= 0  = Z
        | es <=. n = es
        |   True   = take' n es
      where
        take' _ Z = Z
        take' n' (Unlist c arr# arrs) = n' > c ? take' (n' - c) arrs $ Unlist c1 arr1# arrs
          where
            !(Unlist c1 arr1# _) = fromList . take n' . toList $ Unlist c arr# Z
    
    drop n es
        |  n <=  0 = es
        | es <=. n = Z
        |   True   = drop' n es
      where
        drop' _ Z = Z
        drop' n' (Unlist c arr# arrs) = n' > c ? drop' (n' - c) arrs $ Unlist c1 arr1# arrs
          where
            !(Unlist c1 arr1# _) = fromList . drop n' . toList $ Unlist c arr# Z
    
    split n es
        |  n <= 0  = (Z, es)
        | es <=. n = (es, Z)
        |   True   = split' n es
      where
        split' _  Z = (Z, Z)
        split' n' (Unlist c arr# arrs) = n' > c ? split' (n' - c) arrs $ (Unlist c1 arr1# arrs, Unlist c2 arr2# arrs)
          where
            (take', drop')       = split n' . toList $ Unlist c arr# Z
            !(Unlist c1 arr1# _) = fromList take'
            !(Unlist c2 arr2# _) = fromList drop'
    
    isPrefixOf = isPrefixOf `on` toList
    isInfixOf  = isInfixOf  `on` toList
    isSuffixOf = isSuffixOf `on` toList
    
    prefix f = prefix f . toList
    suffix f = suffix f . toList

instance Bordered (Unlist e) Int e
  where
    lower  _  = 0
    upper  es = length es - 1
    bounds es = (0, length es - 1)
    
    sizeOf es = length es

--------------------------------------------------------------------------------

{- Indexed instance. -}

instance Indexed (Unlist e) Int e
  where
    assoc' bnds@(l, u) defvalue ascs = isEmpty bnds ? UNEmpty $ runST
        (
          ST $ \ s1# -> case newArray# n# defvalue s1# of
            (# s2#, marr# #) -> foldr (fill marr#) (done n rest marr#) ies s2#
        )
      where
        !n@(I# n#)     = min lim $ size bnds
        (curr, others) = partition (inRange (0, n - 1) . fst) ascs
        
        rest = assoc' (l + n, u) defvalue others
        
        ies = [ (offset bnds i, e) | (i, e) <- curr ]
        lim = _UNLIST_CHUNK_MAX_SIZE_
    
    Z // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = minimum $ fsts ascs
        u = maximum $ fsts ascs
    
    es@(Unlist c@(I# c#) _ arrs) // ascs = runST $ thaw >>= (`writes` (arrs // others))
      where
        writes (STArray l' u' n' marr#) rest = ST $ foldr (fill marr#) (done n' rest marr#) ies
          where
            ies = [ (offset (l', u') i, e) | (i, e) <- curr ]
        
        thaw = ST $ \ s1# -> case newArray# c# (undEx "(//)") s1# of
          (# s2#, marr# #) ->
            let copy i@(I# i#) s3# = if i == c
                  then s3#
                  else copy (i + 1) (writeArray# marr# i# (es !# i) s3#)
            in case copy 0 s2# of s3# -> (# s3#, STArray 0 (c - 1) c marr# #)
        
        (curr, others) = partition (\ (i, _) -> inRange (0, c - 1) i) ascs
    
    es .! n = es !# (n + lower es)
    
    (!) es n = case inBounds bs n of
        ER -> throw $ EmptyRange     "in SDP.Unrolled.(!)"
        UR -> throw $ IndexOverflow  "in SDP.Unrolled.(!)"
        OR -> throw $ IndexUnderflow "in SDP.Unrolled.(!)"
        IN -> es !# offset bs n
      where
        bs = bounds es
    
    es !? n = inRange bs n ? Just (es !# offset bs n) $ Nothing where bs = bounds es
    
    predicate .$ es = predicate .$ toList es
    predicate *$ es = predicate *$ toList es

--------------------------------------------------------------------------------

instance Estimate Unlist
  where
    UNEmpty         <==>         UNEmpty = EQ
    (Unlist c1 _ _) <==>         UNEmpty = c1 <=>  0
    UNEmpty         <==> (Unlist c2 _ _) = 0  <=> c2
    (Unlist c1 _ _) <==> (Unlist c2 _ _) = c1 <=> c2

instance (Arbitrary e) => Arbitrary (Unlist e)
  where
    arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

{-# INLINE (!#) #-}
(!#) :: Unlist e -> Int -> e
(Unlist c arr# arrs) !# i@(I# i#) = i < c ? (case indexArray# arr# i# of (# e #) -> e) $ arrs !# (i - c)
_ !# _ = error "SDP.Unrolled.(!#) tried to find element in empty Unlist"

{-# INLINE done #-}
done :: Int -> Unlist e -> MutableArray# s e -> STRep s (Unlist e)
done c rest marr# = \ s1# -> case unsafeFreezeArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, Unlist c arr# rest #)

_UNLIST_CHUNK_MAX_SIZE_ :: Int
_UNLIST_CHUNK_MAX_SIZE_ =  1024

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.Unrolled." ++ msg

