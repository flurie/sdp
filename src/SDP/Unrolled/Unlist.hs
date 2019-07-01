{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns #-}

{- |
    Module      :  SDP.Unrolled.Unlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    Stability   :  stable
    
    This module provides service type Unlist - lazy boxed unrolled linked list for SDP.Unrolled.
-}

module SDP.Unrolled.Unlist
(
  module SDP.Indexed,
  module SDP.Scan,
  module SDP.Set,
  
  Unlist (..)
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.Indexed
import SDP.Scan
import SDP.Set

import GHC.Show ( appPrec )
import GHC.Base
  (
    Array#, MutableArray#, Int (..), Int#,
    
    newArray#, unsafeFreezeArray#, writeArray#, indexArray#,
    
    isTrue#, (+#), (-#), (==#)
  )
import GHC.ST   ( ST (..), STRep, runST )

import SDP.Internal.MutableArrays ( STArray (..) )
import SDP.Simple

--------------------------------------------------------------------------------

-- | Unlist is internal (abstract) type.
data Unlist e = UNEmpty | Unlist {-# UNPACK #-} !Int (Array# e) (Unlist e)

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Eq e) => Eq (Unlist e) where (==) = eq1

instance Eq1 Unlist where liftEq f xs ys = liftEq f (toList xs) (toList ys)

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Ord e) => Ord (Unlist e) where compare = compare1

instance Ord1 Unlist where liftCompare f xs ys = liftCompare f (toList xs) (toList ys)

--------------------------------------------------------------------------------

{- Show instance. -}

instance (Show e) => Show (Unlist e)
  where
    showsPrec p unl = showParen (p > appPrec) $ showString "unlist "
                                              . shows (assocs unl)

--------------------------------------------------------------------------------

{- Functor instance. -}

instance Functor Unlist
  where
    fmap _ UNEmpty = UNEmpty
    fmap f arr@(Unlist n@(I# n#) _ arrs) = runST $ ST $ \ s1# ->
      case newArray# n# (unreachEx "fmap") s1# of
        (# s2#, marr# #) ->
          let go i s# = if i == n
              then done n (f <$> arrs) marr# s#
              else fill marr# (i, f $ arr !# i) (go $ i + 1) s#
          in go 0 s2#

--------------------------------------------------------------------------------

{- Foldable instance. -}

instance Foldable Unlist
  where
    foldr _ base Z = base
    foldr f base (Unlist c arr# arrs) = go (foldr f base arrs) 0
      where
        go b i@(I# i#) = c == i ? b $ f (arr# !^ i#) (go b $ i + 1)
    
    foldr' _ base Z = base
    foldr' f base (Unlist c arr# arrs) = go (foldr' f base arrs) 0
      where
        go b i@(I# i#) = c == i ? b $ f (arr# !^ i#) (go b $ i + 1)
    
    foldl _ base Z = base
    foldl f base (Unlist c arr# arrs) = foldl f (go base $ c - 1) arrs
      where
        go b i@(I# i#) = -1 == i ? b $ f (go b $ i - 1) (arr# !^ i#)
    
    foldl' _ base Z = base
    foldl' f base (Unlist c arr# arrs) = foldl' f (go base c) arrs
      where
        go b i@(I# i#) = -1 == i ? b $ f (go b $ i - 1) (arr# !^ i#)
    
    length es = case es of {Unlist n _ arrs -> max 0 n + length arrs; _ -> 0}
    
    toList UNEmpty = []
    toList (Unlist c arr# arrs) = [ arr# !^ i# | (I# i#) <- [0 .. c - 1] ] ++ toList arrs
    
    {-# INLINE null #-}
    null es = case es of {Unlist c _ _ -> c < 1; _ -> True}

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance Linear (Unlist e) e
  where
    {-# INLINE isNull #-}
    isNull es = null es
    
    {-# INLINE lzero #-}
    lzero = UNEmpty
    
    {-# INLINE toHead #-}
    toHead e Z = single e
    toHead e (Unlist c arr# arrs) = c < lim ? res1 $ (Unlist 1 single# arrs)
      where
        res1 = fromListN (max 0 c + 1) $ e : toList (Unlist c arr# Z)
        !(Unlist 1 single# Z) = single e
    
    {-# INLINE head #-}
    head Z  = pfailEx "(:>)"
    head es = es !# 0
    
    {-# INLINE tail #-}
    tail Z  = pfailEx "(:<)"
    tail es@(Unlist _ _ Z)    = fromList . tail $ toList es
    tail (Unlist c arr# arrs) = Unlist c' new# arrs
      where
        !(Unlist c' new# _) = tail (Unlist c arr# Z)
    
    {-# INLINE toLast #-}
    toLast Z e = single e
    toLast es@(Unlist c _ Z) e = c < lim ? res1 $ (Unlist 1 single# Z)
      where
        res1 = fromListN (max 0 c + 1) $ foldr (:) [e] es
        !(Unlist 1 single# Z) = single e
    toLast (Unlist c arr# arrs) e = Unlist c arr# (toLast arrs e)
    
    {-# INLINE last #-}
    last Z  = pfailEx "(:<)"
    last es = es !# (length es - 1)
    
    {-# INLINE init #-}
    init Z = pfailEx "(:>)"
    init es@(Unlist _ _ Z)    = fromList . init $ toList es
    init (Unlist c arr# arrs) = Unlist c arr# (init arrs)
    
    {-# INLINE single #-}
    single e = runST $ ST $ \ s1# -> case newArray# 1# e s1# of (# s2#, marr# #) -> done 1 Z marr# s2#
    
    {-# INLINE listL #-}
    listL es = toList es
    
    {-# INLINE fromList #-}
    fromList es = fromFoldable es
    
    {-# INLINE fromFoldable #-}
    fromFoldable es = foldr (\ x y -> toChunk' err lim x ++ y) rest' chunks
      where
        (chunks :< rest)  = take count [ lim, lim .. ] `splits` toList es
        (count, restSize) = length es `divMod` lim
        
        rest' = toChunk' err restSize rest
        err   = unreachEx "fromFoldable"
    
    Z ++ ys = ys
    xs ++ Z = xs
    (Unlist c arr# arrs) ++ ys = Unlist c arr# (arrs ++ ys)
    
    {-# INLINE replicate #-}
    replicate n e = copy count
      where
        chunk  = runST $ ST $ \ s1# -> case newArray# l# e s1# of (# s2#, marr# #) -> done lim Z marr# s2#
        rest   = runST $ ST $ \ s1# -> case newArray# r# e s1# of (# s2#, marr# #) -> done restSize Z marr# s2#
        copy c = case c <=> 0 of {LT -> Z; EQ -> rest; GT -> chunk ++ copy (c - 1)}
        
        !(count, restSize@(I# r#)) = n `divMod` lim
        
        !(I# l#) = lim
    
    reverse es' = reverse' Z es'
      where
        reverse' :: Unlist e -> Unlist e -> Unlist e
        reverse' tail' Z = tail'
        reverse' tail' (Unlist c bytes# bytes) = reverse' (Unlist c rev# tail') bytes
          where
            !(Unlist _ rev# _) = toChunk' err c listR'
            
            listR' = [ bytes# !^ i# | (I# i#) <- [ c - 1, c - 2 .. 0 ] ]
            err    = unreachEx "reverse"
    
    {-# INLINe partition #-}
    partition p es = let (x, y) = partition p $ toList es in (fromList x, fromList y)
    
    {-# INLINE partitions #-}
    partitions ps es = fromList <$> (partitions ps $ toList es)

instance Split (Unlist e) e
  where
    take n es
        |  n <= 0  = Z
        | es <=. n = es
        |   True   = take' n es
      where
        take' _ Z = Z
        take' n' (Unlist c arr# arrs) = n' >= c ? Unlist c arr# other $ fromListN n' rest
          where
            rest  = [ arr# !^ i# | (I# i#) <- [0 .. n' - 1] ]
            other = take' (n' - c) arrs
    
    drop n es
        |  n <=  0 = es
        | es <=. n = Z
        |   True   = drop' n es
      where
        drop' _ Z = Z
        drop' n' (Unlist c arr# arrs) = n' >= c ? rest $ other ++ arrs
          where
            rest  = drop' (n' - c) arrs
            other = fromListN (c - n') [ arr# !^ i# | (I# i#) <- [n' .. c - 1] ]
    
    isPrefixOf xs ys = toList xs `isPrefixOf` toList ys
    isInfixOf  xs ys = toList xs `isInfixOf`  toList ys
    isSuffixOf xs ys = toList xs `isSuffixOf` toList ys
    
    prefix p es = prefix p $ toList es
    suffix p es = suffix p $ toList es

instance Bordered (Unlist e) Int e
  where
    {-# INLINE lower #-}
    lower  _  = 0
    
    {-# INLINE upper #-}
    upper  es = length es - 1
    
    {-# INLINE sizeOf #-}
    sizeOf es = length es

--------------------------------------------------------------------------------

{- Indexed instance. -}

instance Indexed (Unlist e) Int e
  where
    assoc' bnds defvalue ascs = isEmpty bnds ? UNEmpty $ res
      where
        res = foldr (\ x y -> toChunk lim defvalue x ++ y) rest' chunks
        
        (count, restSize) = size bnds `divMod` lim
        (chunks :< rest)  = partitions funs ies
        
        funs  = [ \ (i, _) -> i < l' | l' <- [ lim, 2 * lim .. count * lim ] ]
        rest' = toChunk restSize defvalue rest
        ies   = filter (inRange bnds . fst) ascs
    
    Z // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    es@(Unlist c@(I# c#) _ arrs) // ascs = runST $ thaw >>= writes (arrs // others)
      where
        writes rest (STArray l' u' n' marr#) = ST $ foldr (fill marr#) (done n' rest marr#) ies
          where ies = [ (offset (l', u') i, e) | (i, e) <- curr ]
        
        thaw = ST $ \ s1# -> case newArray# c# (unreachEx "(//)") s1# of
          (# s2#, marr# #) ->
            let copy i@(I# i#) s3# = if i == c then s3# else copy (i + 1) (writeArray# marr# i# (es !# i) s3#)
            in case copy 0 s2# of s3# -> (# s3#, STArray 0 (c - 1) c marr# #)
        
        (curr, others) = partition (\ (i, _) -> inRange (0, c - 1) i) ascs
    
    es .! n = es !# (n + lower es)
    
    (!) es n = case inBounds bs n of
        ER -> throw $ EmptyRange     msg
        UR -> throw $ IndexOverflow  msg
        OR -> throw $ IndexUnderflow msg
        IN -> es !# offset bs n
      where
        msg = "in SDP.Unrolled.(!)"
        bs  = bounds es
    
    p .$ es = p .$ toList es
    p *$ es = p *$ toList es

--------------------------------------------------------------------------------

instance Estimate Unlist
  where
    UNEmpty         <==> ys = case ys of {Unlist c2 _ _ ->  0 <=> c2; _ -> EQ}
    (Unlist c1 _ _) <==> ys = case ys of {Unlist c2 _ _ -> c1 <=> c2; _ -> c1 <=> 0}

instance (Arbitrary e) => Arbitrary (Unlist e) where arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

{-# INLINE (!#) #-}
(!#) :: Unlist e -> Int -> e
(Unlist c arr# arrs) !# i@(I# i#) = i < c ? (case indexArray# arr# i# of (# e #) -> e) $ arrs !# (i - c)
_ !# _ = error "SDP.Unrolled.(!#) tried to find element in empty Unlist"

{-# INLINE (!^) #-}
(!^) :: Array# e -> Int# -> e
arr# !^ i# = case indexArray# arr# i# of (# e #) -> e

{-# INLINE fill #-}
fill :: MutableArray# s e -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) nxt = \ s1# -> case writeArray# marr# i# e s1# of s2# -> nxt s2#

{-# INLINE done #-}
done :: Int -> Unlist e -> MutableArray# s e -> STRep s (Unlist e)
done c rest marr# = \ s1# -> case unsafeFreezeArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, Unlist c arr# rest #)

toChunk :: Int -> e -> [(Int, e)] -> Unlist e
toChunk n@(I# n#) e ies' = runST $ ST $ \ s1# -> case newArray# n# e s1# of
  (# s2#, marr# #) -> foldr (fill marr#) (done n Z marr#) ies' s2#

toChunk' :: (Foldable f) => e -> Int -> f e -> Unlist e
toChunk' err n@(I# n#) chunk = runST $ ST $ \ s1# -> case newArray# n# err s1# of
  (# s2#, marr# #) ->
    let go x r = \ i# s3# -> case writeArray# marr# i# x s3# of
          s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
    in done n Z marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) chunk 0# s2# )

lim :: Int
lim =  1024

pfailEx       :: String -> a
pfailEx   msg =  throw . PatternMatchFail $ "in SDP.Unrolled.Unlist." ++ msg

unreachEx     :: String -> a
unreachEx msg =  throw . UnreachableException $ "in SDP.Unrolled.Unlist." ++ msg




