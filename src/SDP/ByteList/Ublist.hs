{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns #-}

{- |
    Module      :  SDP.Unrolled.Ublist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    Stability   :  stable
    
    This module provides service type Ublist - strict boxed unrolled linked list
    for SDP.Unrolled.
-}

module SDP.ByteList.Ublist
(
  module SDP.Indexed,
  module SDP.Unboxed,
  module SDP.Set,
  
  Ublist (..)
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.Indexed
import SDP.Unboxed
import SDP.Set

import GHC.Base
  (
    ByteArray#, MutableByteArray#, Int (..),
    
    unsafeFreezeByteArray#,
    
    isTrue#, (+#), (-#), (==#), (<#)
  )
import GHC.ST   ( ST (..), STRep, runST )
import GHC.Show ( appPrec )

import SDP.Internal.MutableArrays ( STUArray (..) )
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

instance (Unboxed e) => Semigroup (Ublist e) where xs <> ys = xs ++ ys

instance (Unboxed e) => Monoid    (Ublist e) where mempty = UBEmpty

instance Default (Ublist e) where def = UBEmpty

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Unboxed e) => Linear (Ublist e) e
  where
    {-# INLINE isNull #-}
    isNull es = case es of {Ublist c _ _ -> c < 1; _ -> True}
    
    {-# INLINE lzero #-}
    lzero = UBEmpty
    
    {-# INLINE head #-}
    head Z  = pfailEx ".(:>)"
    head es = es .! 0
    
    {-# INLINE tail #-}
    tail Z                          = pfailEx "(:<)"
    tail es@(Ublist c _ Z)          = fromListN (c - 1) . tail $ listL es
    tail es@(Ublist c bytes# bytes) = Ublist c' new# bytes
      where
        !(Ublist c' new# _) = (`asTypeOf` es) $ tail (Ublist c bytes# Z)
    
    toHead e Z = single e
    toHead e (Ublist c arr# arrs) = c < lim ? res $ (Ublist 1 single# arrs)
      where
        res = fromListN (c + 1) $ e : listL (Ublist c arr# Z)
        !(Ublist 1 single# Z) = single e
    
    {-# INLINE last #-}
    last Z  = pfailEx "(:<)"
    last (Ublist (I# c#) bytes# bytes) = case bytes of {Z -> bytes# !# (c# -# 1#); _ -> last bytes}
    
    {-# INLINE init #-}
    init Z                    = pfailEx "(:>)"
    init es@(Ublist c _ Z)    = fromListN (c - 1) . init $ listL es
    init (Ublist c arr# arrs) = Ublist c arr# (init arrs)
    
    toLast Z e = single e
    toLast es@(Ublist c _ Z) e = c < lim ? res $ (Ublist 1 single# Z)
      where
        res = fromListN (max 0 c + 1) $ toLast (listL es) e
        !(Ublist 1 single# Z) = single e
    toLast (Ublist c arr# arrs) e = Ublist c arr# (toLast arrs e)
    
    single e = runST $ ST $ \ s1# -> case newUnboxed' e 1# s1# of (# s2#, marr# #) -> done 1 Z marr# s2#
    
    {-# INLINE fromList #-}
    fromList es = fromFoldable es
    
    {-# INLINE fromFoldable #-}
    fromFoldable es = foldr (\ x y -> toChunk' lim err x ++ y) rest' chunks
      where
        (chunks :< rest)  = take count [ lim, lim .. ] `splits` toList es
        (count, restSize) = length es `divMod` lim
        
        rest' = toChunk' restSize err rest
        err   = unreachEx "fromFoldable"
    
    {-# INLINE listL #-}
    listL es' = list' 0# es'
      where
        list' _ Z = []
        list' i# es@(Ublist (I# n#) bytes# bytes) = isTrue# (i# <# n#) ? begin $ rest
          where
            begin = (bytes# !# i#) : list' (i# +# 1#) es
            rest  = list' 0# bytes
    
    Z  ++ ys = ys
    xs ++  Z = xs
    (Ublist c arr# arrs) ++ ys = Ublist c arr# (arrs ++ ys)
    
    {-# INLINE replicate #-}
    replicate n e = copy count
      where
        chunk  = runST $ ST $ \ s1# -> case newUnboxed' e l# s1# of (# s2#, marr# #) -> done lim Z marr# s2#
        rest   = runST $ ST $ \ s1# -> case newUnboxed' e r# s1# of (# s2#, marr# #) -> done restSize Z marr# s2#
        copy c = case c <=> 0 of {LT -> Z; EQ -> rest; GT -> chunk ++ copy (c - 1)}
        
        !(count, restSize@(I# r#)) = n `divMod` lim
        !(I# l#) = lim
    
    reverse es' = reverse' Z es'
      where
        reverse' tail' Z = tail'
        reverse' tail' (Ublist c bytes# bytes) = reverse' (Ublist c rev# tail') bytes
          where
            !(Ublist _ rev# _) = toChunk' c err listR' `asTypeOf` bytes
            
            listR' = [ bytes# !# i# | (I# i#) <- [ c - 1, c - 2 .. 0 ] ]
            err    = unreachEx "reverse"
    
    {-# INLINE partitions #-}
    partitions ps es = fromList <$> (partitions ps $ listL es)

instance (Unboxed e) => Split (Ublist e) e
  where
    take n es
        |     n <= 0     = Z
        | sizeOf es <= n = es
        |      True      = take' n es
      where
        take' _ Z = Z
        take' n' (Ublist c arr# arrs) = n' >= c ? Ublist c arr# other $ fromListN n' rest
          where
            rest  = [ arr# !# i# | (I# i#) <- [0 .. n' - 1] ]
            other = take' (n' - c) arrs
    
    drop n es
        |     n <=  0    = es
        | sizeOf es <= n = Z
        |      True      = drop' n es
      where
        drop' _ Z = Z
        drop' n' (Ublist c arr# arrs) = n' >= c ? rest $ other ++ arrs
          where
            rest  = drop' (n' - c) arrs
            other = fromListN (c - n') [ arr# !# i# | (I# i#) <- [n' .. c - 1] ]
    
    isPrefixOf xs ys = listL xs `isPrefixOf` listL ys
    isInfixOf  xs ys = listL xs `isInfixOf`  listL ys
    isSuffixOf xs ys = listL xs `isSuffixOf` listL ys
    
    prefix p es = prefix p $ listL es
    suffix p es = suffix p $ listL es

instance (Unboxed e) => Bordered (Ublist e) Int e
  where
    {-# INLINE lower #-}
    lower  _  = 0
    {-# INLINE upper #-}
    upper  es = sizeOf es - 1
    
    {-# INLINE sizeOf #-}
    sizeOf es = case es of {Ublist n _ arrs -> max 0 n + sizeOf arrs; _ -> 0}
    
    {-# INLINE indexOf #-}
    indexOf es i = i >= 0 && i < sizeOf es

--------------------------------------------------------------------------------

instance (Unboxed e) => Indexed (Ublist e) Int e
  where
    Z // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    es@(Ublist c@(I# c#) _ arrs) // ascs = runST $ thaw es err >>= writes (arrs // others)
      where
        writes rest (STUArray l' u' n' marr#) = ST $ foldr (fill marr#) (done n' rest marr#) ies
          where ies = [ (offset (l', u') i, e) | (i, e) <- curr ]
        
        -- [internal]: thaw uses (.!), but may be faster.
        thaw :: (Unboxed e) => Ublist e -> e -> ST s (STUArray s Int e)
        thaw es' e = ST $ \ s1# -> case newUnboxed e c# s1# of
          (# s2#, marr# #) ->
            let copy i@(I# i#) s3# = if i == c
                  then s3#
                  else copy (i + 1) (writeByteArray# marr# i# (es' .! i) s3#)
            in case copy 0 s2# of s3# -> (# s3#, STUArray 0 (c - 1) c marr# #)
        
        (curr, others) = partition (\ (i, _) -> inRange (0, c - 1) i) ascs
        
        err = unreachEx "(//)"
    
    assoc bnds ascs = isEmpty bnds ? UBEmpty $ res
      where
        -- This fold is the default concatMap with rest' as last element.
        res = foldr (\ x y -> toChunk lim err x ++ y) rest' chunks
        
        -- count of full (size of lim) chunks.
        (count, restSize) = size bnds `divMod` lim
        (chunks :< rest)  = partitions funs ies
        
        funs  = [ \ (i, _) -> i < l' | l' <- [ lim, 2 * lim .. count * lim ] ]
        rest' = toChunk restSize err rest
        
        ies = filter (inRange bnds . fst) ascs
        err = unreachEx "assoc"
    
    assoc' bnds defvalue ascs = isEmpty bnds ? UBEmpty $ res
      where
        res = foldr (\ x y -> toChunk lim defvalue x ++ y) rest' chunks
        
        (count, restSize) = size bnds `divMod` lim
        (chunks :< rest)  = partitions funs ies
        
        funs  = [ \ (i, _) -> i < l' | l' <- [ lim, 2 * lim .. count * lim ] ]
        rest' = toChunk restSize defvalue rest
        
        ies = filter (inRange bnds . fst) ascs
    
    -- | Note: Ublist allows reading by negative offset.
    (Ublist n bytes# arrs) .! i@(I# i#) = i < n ? bytes# !# i# $ arrs .! (n - i)
    Z .! _ = error "wrong using of (.!) from SDP.ByteList.Ublist"
    
    (!) Z _ = throw $ EmptyRange "in SDP.ByteList.Ublist.(!)"
    (!) (Ublist n bytes# arrs) i@(I# i#)
      |    i < 0    = throw $ IndexUnderflow "in SDP.ByteList.Ublist.(!)"
      |    i < n    = bytes# !# i#
      | isNull arrs = throw $ IndexOverflow  "in SDP.ByteList.Ublist.(!)"
      |     True    = arrs ! (n - 1)
    
    p .$ es = p .$ listL es
    p *$ es = p *$ listL es

--------------------------------------------------------------------------------

instance (Unboxed e, Arbitrary e) => Arbitrary (Ublist e) where arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

pfailEx       :: String -> a
pfailEx msg   =  throw . PatternMatchFail $ "in SDP.ByteList.Ublist." ++ msg

unreachEx     :: String -> a
unreachEx msg =  throw . UnreachableException $ "in SDP.ByteList.Ublist." ++ msg

{-# INLINE fill #-}
-- | internal mutable byte array filler.
fill :: (Unboxed e) => MutableByteArray# s -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) nxt = \ s1# -> case writeByteArray# marr# i# e s1# of s2# -> nxt s2#

-- | internal Ublist creator.
done :: (Unboxed e) => Int -> Ublist e -> MutableByteArray# s -> STRep s (Ublist e)
done c arrs marr# = \ s1# -> case unsafeFreezeByteArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, Ublist c arr# arrs #)

toChunk :: (Unboxed e) => Int -> e -> [(Int, e)] -> Ublist e
toChunk n@(I# n#) e ies' = runST $ ST $ \ s1# -> case newUnboxed e n# s1# of
  (# s2#, marr# #) -> foldr (fill marr#) (done n Z marr#) ies' s2#

toChunk' :: (Unboxed e, Foldable f) => Int -> e -> f e -> Ublist e
toChunk' n@(I# n#) e chunk = runST $ ST $ \ s1# -> case newUnboxed e n# s1# of
  (# s2#, marr# #) ->
    let go x r = \ i# s3# -> case writeByteArray# marr# i# x s3# of
          s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
    in done n Z marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) chunk 0# s2# )

-- | lim is internal constant - maximal size of chunk.
lim :: Int
lim =  1024




