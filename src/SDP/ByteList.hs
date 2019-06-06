{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.Unrolled
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    This module provides ByteList - strict boxed unrolled linked list.
-}

module SDP.ByteList
  (
    ByteList (..),
    Ublist, -- type Ublist is abstract.
    
    module SDP.Indexed,
    module SDP.Set
  )
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import GHC.Base
  (
    ByteArray#, MutableByteArray#, Int (..),
    
    unsafeFreezeByteArray#,
    
    isTrue#, (+#), (-#), (==#)
  )
import GHC.ST   ( ST (..), STRep, runST )
import GHC.Show ( appPrec )

import Text.Read
import Text.Read.Lex ( expect )

import SDP.Indexed
import SDP.Set

import SDP.Unboxed

import SDP.Array.Mutable ( STUArray (..) )
import SDP.Simple

--------------------------------------------------------------------------------

-- | Bordered strict unboxed unrolled linked list.
data ByteList i e = ByteList !i !i (Ublist e)

type role ByteList nominal representational

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Eq e,  Unboxed e, Index i) => Eq  (ByteList i e)
  where
    xs == ys = (assocs xs) == (assocs ys)

instance (Ord e, Unboxed e, Index i) => Ord (ByteList i e)
  where
    compare xs ys = (assocs xs) <=> (assocs ys)

--------------------------------------------------------------------------------

{- Show and Read instances -}

instance (Index i, Show i, Unboxed e, Show e) => Show (ByteList i e)
  where
    showsPrec p unr@(ByteList l u _) = showParen (p > appPrec) shows'
      where
        shows' = showString "bytelist " . shows (l, u) . showChar ' ' . shows (assocs unr)

instance (Index i, Read i, Unboxed e, Read e) => Read (ByteList i e)
  where
    readList = readListDefault
    readPrec = parens $ prec appPrec (lift . expect $ Ident "bytelist") >> liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i, Unboxed e) => Linear (ByteList i e) e
  where
    isNull (ByteList l u bytes) = isEmpty (l, u) || isNull bytes
    
    uncons Z = throw $ EmptyRange "in SDP.ByteList.(:>)"
    uncons (ByteList l u es) = (x, sizeOf es < 2 ? Z $ ByteList l1 u xs)
      where
        (x, xs) = uncons es
        l1 = next (l, u) l
    
    unsnoc Z = throw $ EmptyRange "in SDP.ByteList.(:<)"
    unsnoc (ByteList l u es) = (sizeOf es < 2 ? Z $ ByteList l u1 xs, x)
      where
        (xs, x) = unsnoc es
        u1 = prev (l, u) u
    
    fromListN n es = ByteList l u $ fromListN n es
      where
        l = unsafeIndex 0
        u = unsafeIndex $ max 0 n - 1
    
    replicate n e = ByteList l u $ replicate n e
      where
        l = unsafeIndex 0
        u = unsafeIndex $ max 0 n - 1
    
    concat xss = ByteList l u res
      where
        (n', res) = foldr f (0, Z) xss
        
        f = \ (ByteList _ _ xs) (len, ys) -> (len + sizeOf xs, xs ++ ys)
        
        l = unsafeIndex 0
        u = unsafeIndex $ max 0 n' - 1
    
    intersperse e (ByteList _ _ es) = ByteList l1 u1 $ intersperse e es
      where
        n1 = case n <=> 0 of {LT -> -2; EQ -> 0; GT -> 2 * n - 2}; n = sizeOf es
        u1 = unsafeIndex n1
        l1 = unsafeIndex 0
    
    listL  (ByteList _ _ bytes) = listL bytes
    listR  (ByteList _ _ bytes) = listR bytes

instance (Index i, Unboxed e) => Split (ByteList i e) e
  where
    prefix p (ByteList l u es) = min (prefix p es) $ size (l, u)
    suffix p (ByteList l u es) = min (prefix p es) $ size (l, u)
    
    take n list@(ByteList l u es)
        |      n <= 0      = Z
        | n >= size (l, u) = list
        |       True       = ByteList l u' $ take n es
      where
        u' = index (l, u) $ n - 1
    
    drop n list@(ByteList l u es)
        | n <= 0 = list
        | n >= size (l, u) = list
        | True = ByteList l' u $ take n es
      where
        l' = index (l, u) $ n

instance (Index i, Unboxed e) => Bordered (ByteList i e) i e
  where
    indices (ByteList l u _) = range (l, u)
    sizeOf  (ByteList l u _) = size (l, u)
    bounds  (ByteList l u _) = (l, u)
    lower   (ByteList l _ _) = l
    upper   (ByteList _ u _) = u

--------------------------------------------------------------------------------

instance (Index i, Unboxed e) => Indexed (ByteList i e) i e
  where
    -- [internal]: it's correct, but completly inneficient (Set []), rewrite.
    assoc' bnds e ies = fromListN n sorted
      where
        sorted = snds $ unionWith cmpfst (setWith cmpfst ies) filler
        filler = zip (range bnds) (replicate n e)
        n = size bnds
    
    Z  // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    (ByteList l u es) // ascs = ByteList l' u' es'
      where
        es' = es // [ (offset (l, u) i, e) | (i, e) <- ascs ]
        u'  = unsafeIndex $ upper es'
        l'  = unsafeIndex 0
    
    (!)  (ByteList l u es) i = es ! (offset (l, u) i)
    p .$ (ByteList l u es)   = index (l, u) <$> (p .$ es)
    p *$ (ByteList l u es)   = index (l, u) <$> (p *$ es)

--------------------------------------------------------------------------------

{--------------------}
{- INTERNAL SECTION -}
{--------------------}

-- | Ublist is internal data representation.
data Ublist e = UBEmpty | Ublist {-# UNPACK #-} !Int (ByteArray#) (Ublist e)

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Eq e,  Unboxed e) => Eq  (Ublist e) where xs == ys = (listL xs) == (listL ys)

instance (Ord e, Unboxed e) => Ord (Ublist e) where compare xs ys = compare (listL xs) (listL ys)

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Unboxed e, Show e) => Show (Ublist e)
  where
    showsPrec p arr = showParen (p > appPrec) shows'
      where
        shows' = showString "ublist " . shows (bounds arr) . showChar ' ' . shows (assocs arr)

instance (Unboxed e, Read e) => Read (Ublist e)
  where
    readList = readListDefault
    readPrec = parens $ prec appPrec (lift . expect $ Ident "ublist") >> liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Unboxed e) => Linear (Ublist e) e
  where
    {-# INLINE isNull #-}
    isNull es = case es of {UBEmpty -> True; _ -> False}
    
    {-# INLINE fromList #-}
    fromList []  = UBEmpty
    fromList es' = fromList' err es'
      where
        err = throw $ UndefinedValue "in SDP.ByteList.fromList"
        
        fromList' :: (Unboxed e) => e -> [e] -> Ublist e
        fromList' e es = runST $ ST $
          \ s1# -> case newUnboxed e n# s1# of
              (# s2#, marr# #) ->
                let go x r = \ i# s3# -> case writeByteArray# marr# i# x s3# of
                      s4# -> if isTrue# (i# ==# n# -# 1#)
                                then s4#
                                else r (i# +# 1#) s4#
                in done n' (fromList others) marr#
                (
                  if n' == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2#
                )
          where
            (curr, others) = split _UBLIST_CHUNK_MAX_SIZE_ es
            !n'@(I# n#)    = length curr
    
    {-# INLINE listL #-}
    listL es' = list' 0 es'
      where
        list' _ Z = []
        list' i@(I# i#) es@(Ublist n bytes# arrs) = i < n ? bytes# !# i# : list' (i + 1) es $ list' 0 arrs
    
    {-# INLINE head #-}
    head Z  = throw $ EmptyRange "in SDP.ByteList.(:>)"
    head es = es .! 0
    
    {-# INLINE last #-}
    last Z  = throw $ EmptyRange "in SDP.ByteList.(:<)"
    last es = es .! (sizeOf es - 1)
    
    {-# INLINE tail #-}
    tail Z                       = throw $ EmptyRange "in SDP.ByteList.(:<)"
    tail es@(Ublist _ _ Z)       = fromList . tail $ listL es
    tail es@(Ublist c arr# arrs) = Ublist c' new# arrs
      where
        !(Ublist c' new# _) = (`asTypeOf` es) $ tail (Ublist c arr# Z)
    
    {-# INLINE init #-}
    init Z                    = throw $ EmptyRange "in SDP.ByteList.(:>)"
    init es@(Ublist _ _ Z)    = fromList . init $ listL es
    init (Ublist c arr# arrs) = Ublist c arr# (init arrs)
    
    Z ++ ys = ys
    xs ++ Z = xs
    (Ublist c arr# arrs) ++ ys = Ublist c arr# (arrs ++ ys)
    
    {-# INLINE replicate #-}
    replicate n e = copy count
      where
        copy c = case c <=> 0 of
          LT -> Z
          EQ -> fromListN restsize $ repeat e
          GT -> Ublist lim chunk# (copy $ c - 1)
        
        (count, restsize) = n `divMod` lim
        
        !(Ublist _ chunk# _) = fromListN lim (repeat e)
        lim   = _UBLIST_CHUNK_MAX_SIZE_
    
    toHead e Z = single e
    toHead e (Ublist c arr# arrs) = c < lim ? res1 $ (Ublist 1 single# arrs)
      where
        res1 = fromListN (max 0 c + 1) $ e : listL (Ublist c arr# Z)
        !(Ublist 1 single# Z) = single e
        
        lim  = _UBLIST_CHUNK_MAX_SIZE_
    
    toLast Z e = single e
    toLast es@(Ublist c _ Z) e = c < lim ? res1 $ (Ublist 1 single# Z)
      where
        res1 = fromListN (max 0 c + 1) $ toLast (listL es) e
        !(Ublist 1 single# Z) = single e
        
        lim  = _UBLIST_CHUNK_MAX_SIZE_
    toLast (Ublist c arr# arrs) e = Ublist c arr# (toLast arrs e)

instance (Unboxed e) => Split (Ublist e) e
  where
    take n es
        |     n <= 0     = Z
        | sizeOf es <= n = es
        |      True      = take' n es
      where
        take' _ Z = Z
        take' n' (Ublist c arr# arrs) = n' > c ? take' (n' - c) arrs $ res
          where
            res = fromList [ arr# !# i# | (I# i#) <- [0 .. n - 1] ]
    
    drop n es
        |     n <=  0    = es
        | sizeOf es <= n = Z
        |      True      = drop' n es
      where
        drop' _ Z = Z
        drop' n' (Ublist c arr# arrs) = n' > c ? drop' (n' - c) arrs $ res
          where
            res = (`asTypeOf` arrs) $ fromList [ arr# !# i# | (I# i#) <- [n' .. c - 1] ]
    
    isPrefixOf = isPrefixOf `on` listL
    isInfixOf  = isInfixOf  `on` listL
    isSuffixOf = isSuffixOf `on` listL
    
    prefix f = prefix f . listL
    suffix f = suffix f . listL

instance (Unboxed e) => Bordered (Ublist e) Int e
  where
    lower  _  = 0
    upper  es = sizeOf es - 1
    bounds es = (0, sizeOf es - 1)
    
    {-# INLINE sizeOf #-}
    sizeOf es = case es of {Z -> 0; Ublist n _ arrs -> n + sizeOf arrs}
    
    indexOf es i = i >= 0 && i < sizeOf es

--------------------------------------------------------------------------------

instance (Unboxed e) => Indexed (Ublist e) Int e
  where
    Z // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = minimum $ fsts ascs
        u = maximum $ fsts ascs
    
    es@(Ublist c@(I# c#) _ arrs) // ascs = runST $ do
        stuarray <- thaw es (throw $ UndefinedValue "in SDP.ByteList.(//)")
        writes stuarray $ arrs // others
      where
        writes (STUArray l' u' n' marr#) rest = ST $ foldr (fill marr#) (done n' rest marr#) ies
          where
            ies = [ (offset (l', u') i, e) | (i, e) <- curr ]
        
        thaw :: (Unboxed e) => Ublist e -> e -> ST s (STUArray s Int e)
        thaw es' e = ST $ \ s1# -> case newUnboxed e c# s1# of
          (# s2#, marr# #) ->
            let copy i@(I# i#) s3# = if i == c
                  then s3#
                  else copy (i + 1) (writeByteArray# marr# i# (es' .! i) s3#)
            in case copy 0 s2# of s3# -> (# s3#, STUArray 0 (c - 1) c marr# #)
        
        (curr, others) = partition (\ (i, _) -> inRange (0, c - 1) i) ascs
    
    assoc' bnds@(l, u) defvalue ascs = isEmpty bnds ? UBEmpty $ runST $
        ST $ \ s1# -> case newUnboxed defvalue n# s1# of
          (# s2#, marr# #) -> foldr (fill marr#) (done n rest marr#) ies s2#
      where
        !n@(I# n#)     = min lim $ size bnds
        (curr, others) = partition (inRange (0, n - 1) . fst) ascs
        
        rest = assoc' (l + n, u) defvalue others
        
        ies  = [ (offset bnds i, e) | (i, e) <- curr ]
        lim  = _UBLIST_CHUNK_MAX_SIZE_
    
    -- | Note: Ublist allows reading by negative offset.
    (.!) (Ublist n bytes# arrs) i@(I# i#) = i < n ? bytes# !# i# $ arrs .! (n - i)
    
    (!) Z _ = throw $ EmptyRange "in SDP.ByteList.(!)"
    (!) (Ublist n bytes# arrs) i@(I# i#)
      |    i < 0    = throw $ IndexUnderflow "in SDP.ByteList.(!)"
      |    i < n    = bytes# !# i#
      | isNull arrs = throw $ IndexOverflow  "in SDP.ByteList.(!)"
      |     True    = arrs ! (n - 1)
    
    (.$) p es = p .$ (listL es)
    (*$) p es = p *$ (listL es)

--------------------------------------------------------------------------------

instance (Unboxed e, Arbitrary e) => Arbitrary (Ublist e)
  where
    arbitrary = fromList <$> arbitrary

instance (Index i, Unboxed e, Arbitrary e) => Arbitrary (ByteList i e)
  where
    arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

fill :: (Unboxed e) => MutableByteArray# s -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) nxt = \ s1# -> case writeByteArray# marr# i# e s1# of s2# -> nxt s2#

done :: (Unboxed e) => Int -> Ublist e -> MutableByteArray# s -> STRep s (Ublist e)
done c arrs marr# = \ s1# -> case unsafeFreezeByteArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, Ublist c arr# arrs #)

_UBLIST_CHUNK_MAX_SIZE_ :: Int
_UBLIST_CHUNK_MAX_SIZE_ =  1024