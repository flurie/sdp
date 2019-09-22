{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.Bytes.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    @SDP.Bytes.ST@ provides 'STBytes' - mutable lazy boxed array type.
    This implementation of array no much different from @Data.Array.ST@ (array),
    but incopatible with it.
-}
module SDP.Bytes.ST
(
  -- * Exports
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * STBytes
  STBytes (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Unboxed

import GHC.Base
  (
    MutableByteArray#, Int (..),
    
    isTrue#, sameMutableByteArray#, (+#), (-#), (==#)
  )

import GHC.ST ( ST (..), STRep )

import SDP.SortM
import SDP.SortM.Tim

import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | STBytes is mutable version of Bytes.
data STBytes s i e = STBytes !i !i {-# UNPACK #-} !Int (MutableByteArray# s)

type role STBytes nominal nominal representational

--------------------------------------------------------------------------------

{- Eq instance. -}

instance (Index i, Unboxed e) => Eq (STBytes s i e)
  where
    (STBytes l1 u1 n1 marr1#) == (STBytes l2 u2 n2 marr2#) = res
      where
        res  = n1 == n2 && (n1 == 0 || l1 == l2 && u1 == u2 && same)
        same = isTrue# (sameMutableByteArray# marr1# marr2#)

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance (Index i, Unboxed e) => BorderedM (ST s) (STBytes s i e) i e
  where
    {-# INLINE getLower #-}
    getLower   (STBytes l _ _ _) = return l
    
    {-# INLINE getUpper #-}
    getUpper   (STBytes _ u _ _) = return u
    
    {-# INLINE getSizeOf #-}
    getSizeOf  (STBytes _ _ n _) = return $ max 0 n
    
    {-# INLINE getIndices #-}
    getIndices (STBytes l u _ _) = return $ range   (l, u)
    
    {-# INLINE getIndexOf #-}
    getIndexOf (STBytes l u _ _) = return . inRange (l, u)

instance (Index i, Unboxed e) => LinearM (ST s) (STBytes s i e) e
  where
    newLinear = fromFoldableM
    
    {-# INLINE newLinearN #-}
    newLinearN c es = newLinearN' err
      where
        newLinearN' :: (Index i, Unboxed e) => e -> ST s (STBytes s i e)
        newLinearN' e = ST $ \ s1# -> case newUnboxed e n# s1# of
          (# s2#, marr# #) ->
            let go y r = \ i# s3# -> case writeByteArray# marr# i# y s3# of
                  s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
            in done (defaultBounds n) n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
        
        err = undEx "newLinearN"
        !n@(I# n#) = max 0 c
    
    {-# INLINE fromFoldableM #-}
    fromFoldableM es = fromFoldable' $ unreachEx "fromFoldableM"
      where
        fromFoldable' :: (Index i, Unboxed e) => e -> ST s (STBytes s i e)
        fromFoldable' e = ST $ \ s1# -> case newUnboxed e n# s1# of
          (# s2#, marr# #) ->
            let go y r = \ i# s3# -> case writeByteArray# marr# i# y s3# of
                  s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
            in done (defaultBounds n) n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
        
        !n@(I# n#) = length es
    
    getLeft  es@(STBytes _ _ n _) = (es !#>) `mapM` [0 .. n - 1]
    getRight es@(STBytes _ _ n _) = (es !#>) `mapM` [n - 1, n - 2 .. 0]
    
    copied (STBytes _ _ n _) = do
      copy <- filled_ n (unreachEx "copied")
      forM_ [0 .. n - 1] $ \ i -> copy !#> i >>= writeM_ copy i
      return copy
    
    copied' es l n = do
      copy <- filled_ n (unreachEx "copied'")
      forM_ [0 .. n - 1] $ \ i -> es !#> (i + l) >>= writeM_ copy i
      return copy
    
    {-# INLINE reversed #-}
    reversed es = liftA2 zip (getIndices es) (getRight es) >>= overwrite es
    
    {-# INLINE filled #-}
    filled n e = ST $ \ s1# -> case newUnboxed' e n# s1# of
        (# s2#, marr# #) -> (# s2#, STBytes l u n' marr# #)
      where
        (l, u) = defaultBounds n'
        !n'@(I# n#) = max 0 n

--------------------------------------------------------------------------------

{- IndexedM, IFoldM and SortM instances. -}

instance (Index i, Unboxed e) => IndexedM (ST s) (STBytes s i e) i e
  where
    {-# INLINE fromAssocs #-}
    fromAssocs bnds ascs = filledB_ bnds err >>= (`overwrite` ascs)
      where
        filledB_ :: (Index i, Unboxed e) => (i, i) -> e -> ST s (STBytes s i e)
        filledB_ (l, u) e = ST $ \ s1# -> case newUnboxed e n# s1# of
          (# s2#, marr# #) -> (# s2#, STBytes l u n marr# #)
        !n@(I# n#) = size bnds
        err = unreachEx "fromAssocs"
    
    {-# INLINE fromAssocs' #-}
    fromAssocs' bnds@(l, u) defvalue ascs = do
        arr <- ST $ \ s1# -> case newUnboxed' defvalue n# s1# of
          (# s2#, marr# #) -> (# s2#, STBytes l u n marr# #)
        overwrite arr ascs
      where
        !n@(I# n#) = size bnds
    
    {-# INLINE (!#>) #-}
    (STBytes _ _ _ marr#) !#> (I# i#) = ST $ marr# !># i#
    
    {-# INLINE (>!) #-}
    (STBytes l u _ marr#) >! i = case offset (l, u) i of (I# i#) -> ST $ marr# !># i#
    es@(STBytes l u _ _)  !> i = case inBounds (l, u) i of
      ER -> throw $ EmptyRange     msg
      UR -> throw $ IndexUnderflow msg
      IN -> es >! i
      OR -> throw $ IndexOverflow  msg
      where
        msg = "in SDP.Bytes.ST.(!>)"
    
    {-# INLINE writeM_ #-}
    writeM_ (STBytes _ _ _ mbytes#) (I# i#) e = ST $
      \ s1# -> case writeByteArray# mbytes# i# e s1# of s2# -> (# s2#, () #)
    
    {-# INLINE writeM #-}
    writeM (STBytes l u _ mbytes#) i e = let !(I# i#) = offset (l, u) i in ST $
      \ s1# -> case writeByteArray# mbytes# i# e s1# of s2# -> (# s2#, () #)
    
    {-# INLINE overwrite #-}
    overwrite (STBytes l u n marr#) ascs = ST $ foldr (fill marr#) (done (l, u) n marr#) ies
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
    
    fromIndexed' es = do
        copy <- filled_ n (unreachEx "fromIndexed'")
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        return copy
      where
        n = sizeOf es
    
    fromIndexedM es = do
      n    <- getSizeOf es
      copy <- filled_ n (unreachEx "fromIndexedM")
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy

instance (Index i, Unboxed e) => IFoldM (ST s) (STBytes s i e) i e
  where
    ifoldrM  f base bytes@(STBytes l u n _) = go 0
      where
        go i =  n == i ? return base $ bindM2 (bytes !#> i) (go $ i + 1) (f $ index (l, u) i)
    
    ifoldlM  f base bytes@(STBytes l u n _) = go $ n - 1
      where
        go i = -1 == i ? return base $ bindM2 (go $ i - 1) (bytes !#> i) (f $ index (l, u) i)
    
    i_foldrM f base bytes@(STBytes _ _ n _) = go 0
      where
        go i =  n == i ? return base $ bindM2 (bytes !#> i) (go $ i + 1) f
    
    i_foldlM f base bytes@(STBytes _ _ n _) = go $ n - 1
      where
        go i = -1 == i ? return base $ bindM2 (go $ i - 1) (bytes !#> i) f

instance (Index i, Unboxed e) => SortM (ST s) (STBytes s i e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

filled_ :: (Index i, Unboxed e) => Int -> e -> ST s (STBytes s i e)
filled_ n@(I# n#) e = ST $ \ s1# -> case newUnboxed e n# s1# of
    (# s2#, marr# #) -> (# s2#, STBytes l u n marr# #)
  where
    (l, u) = defaultBounds n

{-# INLINE done #-}
done :: (Unboxed e) => (i, i) -> Int -> MutableByteArray# s -> STRep s (STBytes s i e)
done (l, u) n marr# = \ s1# -> (# s1#, STBytes l u n marr# #)

{-# INLINE fill #-}
fill :: (Unboxed e) => MutableByteArray# s -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) nxt = \ s1# -> case writeByteArray# marr# i# e s1# of s2# -> nxt s2#

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.Bytes.ST." ++ msg

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.Bytes.ST." ++ msg




