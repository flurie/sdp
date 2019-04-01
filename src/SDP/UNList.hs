{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module SDP.UNList where

import Prelude ()
import SDP.SafePrelude
import Test.QuickCheck

import GHC.Base
  (
    Array#, MutableArray#, Int (..),
    newArray#, unsafeFreezeArray#, writeArray#, indexArray#,
    isTrue#, (+#), (-#), (==#)
  )
import GHC.ST   ( ST (..), STRep (..), runST )

import GHC.Show ( appPrec )

import Text.Read
import Text.Read.Lex ( expect )

import SDP.Array.Mutable
import SDP.Indexed
import SDP.Scan
import SDP.Set

import SDP.Simple

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
                      (# s4#, arr# #) -> (# s2#, UNList n arr# (f <$> arrs) #)
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
        (curr, others) = splitAt _UNLIST_CHUNK_MAX_SIZE_ es
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
    
    splitAt n es
        |  n  <  0  = (Z, es)
        | es >=. n  = (es, Z)
        |    True   = split' n es
      where
        split' _ Z = (Z, Z)
        split' n' (UNList c arr# arrs) = n' > c ? (UNList c arr# ts, ds) $ (fromList tpart, UNList c1 arr1# arrs)
          where
            (tpart, dpart) = splitAt n' . toList $ UNList c arr# Z
            !(UNList _ arr1# _) = fromList dpart
            (ts, ds) = split' c1 arrs
            c1 = n' - c
    
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
        (UNList 1 single# Z) = single e
        
        lim  = _UNLIST_CHUNK_MAX_SIZE_
    
    toLast Z e = single e
    toLast es@(UNList c arr# Z) e = c < lim ? res1 $ (UNList 1 single# Z)
      where
        res1 = fromListN (max 0 c + 1) $ foldr (:) [e] es
        (UNList 1 single# Z) = single e
        
        lim  = _UNLIST_CHUNK_MAX_SIZE_
    toLast (UNList c arr# arrs) e = UNList c arr# (toLast arrs e)
    
    -- default: takeWhile, dropWhile, takeEnd, dropEnd, span, break
    
    filter predicate = fromList . filter predicate . toList
    
    partition predicate es = (fromList x, fromList y)
      where
        (x, y) = partition predicate $ toList es
    
    isPrefixOf xs ys = and $ zipWith (==) xs ys
    isSuffixOf xs ys = and $ zipWith (==) xs (take n ys)
      where
        n = length xs - length ys
    
    isInfixOf  = isInfixOf `on` toList
    isSubseqOf = isSubseqOf `on` toList
    
    intersperse e = fromList . intersperse e . toList

_UNLIST_CHUNK_MAX_SIZE_ :: Int
_UNLIST_CHUNK_MAX_SIZE_ =  1024

instance Bordered UNList Int
  where
    lower  _  = 0
    upper  es = length es - 1
    bounds es = (0, length es - 1)

--------------------------------------------------------------------------------

{-
  Unchecked instance (assoc, assoc', (//))
-}

instance Indexed UNList Int
  where
    assoc' bnds@(l, u) defvalue ascs = runST
        (
          ST $ \ s1# -> case newArray# n# defvalue s1# of
            (# s2#, marr# #) -> foldr (fill marr#) (done n (undefined) marr#) ies s2#
        )
      where
        ies = [ (offset bnds i, e) | (i, e) <- curr ]
        
        (count, !n@(I# n#)) = (size bnds) `divMod` lim
        
        (curr, others) = partition (\ (i, e) -> inRange (0, lim - 1) i) ies
        lim = _UNLIST_CHUNK_MAX_SIZE_
    
    Z // ascs = assoc (l, u) ascs
      where
        l = minimum $ fsts ascs
        u = maximum $ fsts ascs
    
    es@(UNList c@(I# c#) arr# arrs) // ascs = runST $ thaw >>= (`writes` (arrs // others))
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
        
        (curr, others) = partition (\ (i, e) -> inRange (0, c - 1) i) ascs
    
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

(!#) :: UNList e -> Int -> e
(UNList c arr# arrs) !# i@(I# i#) = i < c ? e $ arrs !# (i - c)
  where
    (# e #) = indexArray# arr# i#

done :: Int -> UNList e -> MutableArray# s e -> STRep s (UNList e)
done c rest marr# = \s1# -> case unsafeFreezeArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, UNList c arr# rest #)

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.Unrolled." ++ msg

