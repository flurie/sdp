{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module SDP.Unrolled
(
  Unrolled (..),
  
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

{- Unrolled type section. Free for public use. -}

data Unrolled i e = Unrolled
                  !i                  {- lower  bound -}
                  !i                  {- upper  bound -}
                  {-# UNPACK #-} !Int {-     size     -}
                  (UNList e)          {-  container   -}

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Eq e, Index i) => Eq (Unrolled i e) where (==) = eq1

instance (Index i) => Eq1 (Unrolled i)
  where
    liftEq f unr1 unr2 = (null unr1 && null unr2) || (l1 == l2 && u1 == u2 && n1 == n2 && liftEq f xs ys)
      where
        (Unrolled l1 u1 n1 xs) = unr1
        (Unrolled l2 u2 n2 ys) = unr2

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
    showsPrec p arr@(Unrolled l u _ _) = showParen (p > appPrec) shows'
      where
        shows' = showString "unrolled " . shows (l, u) . showChar ' ' . shows (assocs arr)

instance (Index i, Read i, Read e) => Read (Unrolled i e)
  where
    readList = readListDefault
    readPrec = parens $ prec appPrec (lift . expect $ Ident "unrolled") >> liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances -}

instance (Index i) => Functor (Unrolled i)
  where
    fmap f (Unrolled l u n es) = Unrolled l u n (f <$> es)

-- instance (Index i) => Zip (Unrolled i)

instance (Index i) => Applicative (Unrolled i)
  where
    pure = single
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable, Scan and Traversable instances -}

instance (Index i) => Foldable (Unrolled i)
  where
    foldr  f base (Unrolled _ _ _ es) = foldr  f base es
    foldl  f base (Unrolled _ _ _ es) = foldl  f base es
    
    foldr' f base (Unrolled _ _ _ es) = foldr' f base es
    foldl' f base (Unrolled _ _ _ es) = foldl' f base es
    
    foldr1 f (Unrolled _ _ _ es) = foldr1 f es
    foldl1 f (Unrolled _ _ _ es) = foldl1 f es
    
    length (Unrolled _ _ n  _) = n
    toList (Unrolled _ _ _ es) = toList es
    elem e (Unrolled _ _ _ es) = e `elem` es
    null   (Unrolled l u n es) = null es || n < 1 || isEmpty (l, u)

-- instance (Index i) => Scan (Unrolled i)

instance (Index i) => Traversable (Unrolled i)
  where
    traverse f arr = fromList <$> traverse f (toList arr)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i) => Linear (Unrolled i)
  where
    fromListN n es = Unrolled l u n $ fromListN n es
      where
        l = unsafeIndex 0
        u = unsafeIndex $ max 0 n - 1
    
    uncons Z = throw $ EmptyRange "in SDP.Unrolled.(:>)"
    uncons (Unrolled l u n es) = (x, n < 2 ? Z $ Unrolled l1 u n1 xs)
      where
        (x, xs) = uncons es
        l1 = next (l, u) l
        n1 = n - 1
    
    unsnoc Z = throw $ EmptyRange "in SDP.Unrolled.(:<)"
    unsnoc (Unrolled l u n es) = (n < 2 ? Z $ Unrolled l u1 n1 xs, x)
      where
        (xs, x) = unsnoc es
        u1 = prev (l, u) u
        n1 = n - 1
    
    concat xss = Unrolled l u n' res
      where
        (n', res) = foldr (\ (Unrolled _ _ n xs) (len, ys) -> (len + n, xs ++ ys)) (0, Z) xss
        
        l = unsafeIndex 0
        u = unsafeIndex $ max 0 n' - 1
    
    -- filter, partition
    
    -- isSubseqOf
    
    intersperse e (Unrolled _ _ n es) = Unrolled l1 u1 n1 (intersperse e es)
      where
        n1 = case n <=> 0 of {LT -> -1; EQ -> 1; GT -> 2 * n - 1}
        u1 = unsafeIndex (n1 - 1)
        l1 = unsafeIndex 0

instance (Index i) => Split (Unrolled i)
  where
    take n (Unrolled l u c es) = Unrolled l u' n' (take n' es)
      where
        u' = index (l, u) n'
        n' = min c n
    
    drop n (Unrolled l u c es) = Unrolled l' u n' (drop n' es)
      where
        l' = index (l, u) n'
        n' = min c n

instance (Index i) => Bordered (Unrolled i) i
  where
    indices (Unrolled l u _  _) = range (l, u)
    bounds  (Unrolled l u _  _) = (l, u)
    lower   (Unrolled l _ _  _) = l
    upper   (Unrolled _ u _  _) = u

--------------------------------------------------------------------------------

instance (Index i) => Indexed (Unrolled i) i
  where
    -- [internal]: it's correct, but completly inneficient (Set []). Rewrite.
    assoc' bnds e ies = fromListN n $ snds ixset
      where
        ixset  = unionWith cmpfst (setWith cmpfst ies) filler
        filler = zip (range bnds) (replicate n e)
        n = size bnds
    
    Z  // []   = Z
    Z  // ascs = assoc (l, u) ascs
      where
        l = minimum $ fsts ascs
        u = maximum $ fsts ascs
    
    (Unrolled l u _ es) // ascs = Unrolled l' u' n es'
      where
        ascs' = (\ (i, e) -> (offset (l, u) i, e)) <$> ascs
        l'    = unsafeIndex $ lower es'
        u'    = unsafeIndex $ upper es'
        n     = size (l', u')
        es'   = es // ascs'
    
    (Unrolled l u _ es)   .! i = es !# (offset (l, u) i)
    
    (!) (Unrolled l u _ es)  i = es !# (offset (l, u) i)
    
    (Unrolled l u _ arrs) !? i = inRange (l, u) i ? Just e $ Nothing
      where
        e = arrs !# offset (l, u) i
    
    predicate .$ (Unrolled l u _ es) = index (l, u) <$> (predicate .$ es)
    predicate *$ (Unrolled l u _ es) = Unrolled l' u' n es'
      where
        es' = index (l, u) <$> (predicate *$ es)
        u'  = index (l, u) (n - 1)
        l'  = index (l, u) 0
        n   = length es'

--------------------------------------------------------------------------------

instance (Index i, Arbitrary e) => Arbitrary (Unrolled i e)
  where
    arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

instance (Index i) => Estimate (Unrolled i)
  where
    (Unrolled _ _ n1 _) <==> (Unrolled _ _ n2 _) = n1 <=> n2

instance (Index i) => LineS (Unrolled i)

--------------------------------------------------------------------------------

{--------------------}
{- INTERNAL SECTION -}
{--------------------}

data UNList e = UNEmpty | UNList {-# UNPACK #-} !Int (Array# e) (UNList e)

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Eq e) => Eq (UNList e) where (==) = eq1

instance Eq1 UNList where liftEq f xs ys = liftEq f (toList xs) (toList ys)

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Ord e) => Ord (UNList e) where compare = compare1

instance Ord1 UNList where liftCompare f xs ys = liftCompare f (toList xs) (toList ys)

--------------------------------------------------------------------------------

{- [internal]: Show instance. -}

instance (Show e) => Show (UNList e)
  where
    showsPrec p UNEmpty = showParen (p > appPrec) $ showString "UNEmpty"
    showsPrec p es = showParen (p > appPrec) shows'
      where
        shows' = showString "unlist " . shows (toList es)

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances. -}

instance Functor UNList
  where
    fmap _ UNEmpty = UNEmpty
    fmap f arr@(UNList n@(I# n#) _ arrs) = runST
        (
          ST $ \ s1# ->
            case newArray# n# (undEx "fmap") s1# of
              (# s2#, marr# #) ->
                let go i s3# = if i == n
                    then case unsafeFreezeArray# marr# s3# of
                      (# s4#, arr# #) -> (# s4#, UNList n arr# (f <$> arrs) #)
                    else fill marr# (i, f $ arr !# i) (go $ i + 1) s3#
                in go 0 s2#
        )

instance Zip UNList
  where
    zipWith  f as bs             = fromList $ zipWith  f (toList as) (toList bs)
    zipWith3 f as bs cs          = fromList $ zipWith3 f (toList as) (toList bs) (toList cs)
    zipWith4 f as bs cs ds       = fromList $ zipWith4 f (toList as) (toList bs) (toList cs) (toList ds)
    zipWith5 f as bs cs ds es    = fromList $ zipWith5 f (toList as) (toList bs) (toList cs) (toList ds) (toList es)
    zipWith6 f as bs cs ds es fs = fromList $ zipWith6 f (toList as) (toList bs) (toList cs) (toList ds) (toList es) (toList fs)

instance Applicative UNList
  where
    pure = single
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable, Zip and Traversable instances. -}

instance Foldable UNList
  where
    foldr _ base Z = base
    foldr f base (UNList c arr# arrs) = go (foldr f base arrs) 0
      where
        go b i@(I# i#) = i == c ? b $ f e (go b $ i + 1)
          where
            (# e #) = indexArray# arr# i#
    
    foldr' _ base Z = base
    foldr' f base (UNList c arr# arrs) = go (foldr' f base arrs) 0
      where
        go b i@(I# i#) = i == c ? b $ f e (go b $ i + 1)
          where
            (# e #) = indexArray# arr# i#
    
    foldl _ base Z = base
    foldl f base (UNList c arr# arrs) = foldl f (go base $ c - 1) arrs
      where
        go b i@(I# i#) = -1 == i ? b $ f (go b $ i - 1) e
          where
            (# e #) = indexArray# arr# i#
    
    foldl' _ base Z = base
    foldl' f base (UNList c arr# arrs) = foldl' f (go base c) arrs
      where
        go b i@(I# i#) = -1 == i ? b $ f (go b $ i - 1) e
          where
            (# e #) = indexArray# arr# i#
    
    length Z = 0
    length (UNList c _ arrs) = c + (length arrs)
    
    toList Z = []
    toList (UNList c arr# arrs) = foldr addIxElem (toList arrs) [0 .. c - 1]
      where
        addIxElem (I# i#) = let (# e #) = indexArray# arr# i# in (e :)
    
    null UNEmpty = True
    null (UNList c _ _) = c < 0

-- instance Scan UNList

instance Traversable UNList
  where
    traverse f arr = fromList <$> traverse f (toList arr)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance Linear UNList
  where
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
    tail es@(UNList _ _ Z) = fromList . tail $ toList es
    tail (UNList c arr# arrs) = UNList c' new# arrs
      where
        !(UNList c' new# _) = tail (UNList c arr# Z)
    
    init Z = throw $ EmptyRange "in SDP.Unrolled.(:>)"
    init es@(UNList _ _ Z) = fromList . init $ toList es
    init (UNList c arr# arrs) = UNList c arr# (init arrs)
    
    Z ++ ys = ys
    xs ++ Z = xs
    (UNList c arr# arrs) ++ ys = UNList c arr# (arrs ++ ys)
    
    replicate n e = copy count chunk
      where
        copy c ch@(UNList _ chunk# _) = case c <=> 0 of
          LT -> Z
          EQ -> fromListN restsize (repeat e)
          GT -> UNList lim chunk# (copy (c - 1) ch)
        
        (count, restsize) = n `divMod` lim
        
        chunk = fromListN lim (repeat e)
        lim   = _UNLIST_CHUNK_MAX_SIZE_
    
    toHead e Z = single e
    toHead e (UNList c arr# arrs) = c < lim ? res1 $ (UNList 1 single# arrs)
      where
        res1 = fromListN (max 0 c + 1) $ e : toList (UNList c arr# Z)
        !(UNList 1 single# Z) = single e
        
        lim  = _UNLIST_CHUNK_MAX_SIZE_
    
    toLast Z e = single e
    toLast es@(UNList c _ Z) e = c < lim ? res1 $ (UNList 1 single# Z)
      where
        res1 = fromListN (max 0 c + 1) $ foldr (:) [e] es
        !(UNList 1 single# Z) = single e
        
        lim  = _UNLIST_CHUNK_MAX_SIZE_
    toLast (UNList c arr# arrs) e = UNList c arr# (toLast arrs e)
    
    filter predicate = fromList . filter predicate . toList
    
    partition predicate es = (fromList x, fromList y)
      where
        (x, y) = partition predicate $ toList es

instance Split UNList
  where
    take n es
        |  n <= 0  = Z
        | es <=. n = es
        |   True   = take' n es
      where
        take' _ Z = Z
        take' n' (UNList c arr# arrs) = n' > c ? take' (n' - c) arrs $ UNList c1 arr1# arrs
          where
            !(UNList c1 arr1# _) = fromList . take n' . toList $ UNList c arr# Z
    
    drop n es
        |  n <=  0 = es
        | es <=. n = Z
        |   True   = drop' n es
      where
        drop' _ Z = Z
        drop' n' (UNList c arr# arrs) = n' > c ? drop' (n' - c) arrs $ UNList c1 arr1# arrs
          where
            !(UNList c1 arr1# _) = fromList . drop n' . toList $ UNList c arr# Z
    
    split n es
        |  n  <  0  = (Z, es)
        | es >=. n  = (es, Z)
        |    True   = split' n es
      where
        split' _ Z = (Z, Z)
        split' n' (UNList c arr# arrs) = n' > c ? (UNList c arr# ts, ds) $ (fromList tpart, UNList c1 arr1# arrs)
          where
            (tpart, dpart) = split n' . toList $ UNList c arr# Z
            !(UNList _ arr1# _) = fromList dpart
            (ts, ds) = split' c1 arrs
            c1 = n' - c
    
    isPrefixOf xs ys = and $ zipWith (==) xs ys
    isSuffixOf xs ys = and $ zipWith (==) xs (take n ys)
      where
        n = length xs - length ys

instance Bordered UNList Int
  where
    lower  _  = 0
    upper  es = length es - 1
    bounds es = (0, length es - 1)

--------------------------------------------------------------------------------

instance Indexed UNList Int
  where
    assoc' bnds@(l, u) defvalue ascs = isEmpty bnds ? UNEmpty $ runST
        (
          ST $ \ s1# -> case newArray# n# defvalue s1# of
            (# s2#, marr# #) -> foldr (fill marr#) (done n rest marr#) ies s2#
        )
      where
        !n@(I# n#)     = min lim $ size bnds
        (curr, others) = partition (\ (i, _) -> inRange (0, n - 1) i) ascs
        
        rest = assoc' (l + n, u) defvalue others
        
        ies = [ (offset bnds i, e) | (i, e) <- curr ]
        lim = _UNLIST_CHUNK_MAX_SIZE_
    
    Z // []   = Z
    Z // ascs = assoc (l, u) ascs
      where
        l = minimum $ fsts ascs
        u = maximum $ fsts ascs
    
    es@(UNList c@(I# c#) _ arrs) // ascs = runST $ thaw >>= (`writes` (arrs // others))
      where
        writes (STArray l' u' n' marr#) rest = ST $ foldr (fill marr#) (done n' rest marr#) ies
          where
            ies = [ (offset (l', u') i, e) | (i, e) <- curr ]
        
        thaw = ST $ \s1# -> case newArray# c# (undEx "(//)") s1# of
          (# s2#, marr# #) ->
            let copy i@(I# i#) s3# = if i == c
                  then s3#
                  else copy (i + 1) (writeArray# marr# i# (es !# i) s3#)
            in case copy 0 s2# of s3# -> (# s3#, STArray 0 (c - 1) c marr# #)
        
        (curr, others) = partition (\ (i, _) -> inRange (0, c - 1) i) ascs
    
    es .! n = es !# offset (bounds es) n
    
    (!) es n
        |     null  es     = throw $ EmptyRange     "in SDP.Unrolled.(!)"
        | isOverflow  bs n = throw $ IndexOverflow  "in SDP.Unrolled.(!)"
        | isUnderflow bs n = throw $ IndexUnderflow "in SDP.Unrolled.(!)"
        |       True       = es !# offset bs n
      where
        bs = bounds es
    
    es !? n = inRange bs n ? Just (es !# offset bs n) $ Nothing
      where
        bs = bounds es
    
    predicate .$ es = predicate .$ (toList es)
    predicate *$ es = fromList $ predicate *$ (toList es)

--------------------------------------------------------------------------------

instance Estimate UNList
  where
    UNEmpty         <==>         UNEmpty = EQ
    (UNList c1 _ _) <==>         UNEmpty = c1 <=>  0
    UNEmpty         <==> (UNList c2 _ _) = 0  <=> c2
    (UNList c1 _ _) <==> (UNList c2 _ _) = c1 <=> c2

instance (Arbitrary e) => Arbitrary (UNList e)
  where
    arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

{-# INLINE (!#) #-}
(!#) :: UNList e -> Int -> e
(UNList c arr# arrs) !# i@(I# i#) = i < c ? e $ arrs !# (i - c)
  where
    (# e #) = indexArray# arr# i#

{-# INLINE done #-}
done :: Int -> UNList e -> MutableArray# s e -> STRep s (UNList e)
done c rest marr# = \s1# -> case unsafeFreezeArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, UNList c arr# rest #)

_UNLIST_CHUNK_MAX_SIZE_ :: Int
_UNLIST_CHUNK_MAX_SIZE_ =  1024

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.Unrolled." ++ msg

