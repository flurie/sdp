{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.Array.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    SDP.Array.ST provides mutable lazy boxed array type.
    This implementation of array no much different from Data.Array.ST (array),
    but incopatible with it.
    The main difference is the Index class instead of Ix.
-}
module SDP.Array.ST
(
  module SDP.IndexedM,
  module SDP.SortM,
  
  STArray (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.IndexedM

import GHC.Base
  (
    MutableArray#, Int (..),
    
    newArray#, writeArray#, readArray#,
    
    isTrue#, sameMutableArray#, (+#), (-#), (==#)
  )

import GHC.ST ( ST (..), STRep )

import SDP.SortM
import SDP.SortM.Stuff

import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | STArray is mutable version of Array.
data STArray s i e = STArray !i !i {-# UNPACK #-} !Int (MutableArray# s e)

type role STArray nominal nominal representational

--------------------------------------------------------------------------------

{- Eq instance. -}

instance (Index i, Eq e) => Eq (STArray s i e)
  where
    (STArray l1 u1 n1 marr1#) == (STArray l2 u2 n2 marr2#) = res
      where
        res  = n1 == n2 && (n1 == 0 || l1 == l2 && u1 == u2 && same)
        same = isTrue# (sameMutableArray# marr1# marr2#)

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance (Index i) => BorderedM (ST s) (STArray s i e) i e
  where
    getLower  (STArray l _ _ _) = return l
    getUpper  (STArray _ u _ _) = return u
    getBounds (STArray l u _ _) = return (l, u)
    getSizeOf (STArray _ _ n _) = return $ max 0 n

instance (Index i) => LinearM (ST s) (STArray s i e) e
  where
    newLinear = fromFoldableM
    
    {-# INLINE fromFoldableM #-}
    fromFoldableM es = ST $ \ s1# -> case newArray# n# err s1# of
      (# s2#, marr# #) ->
        let go y r = \ i# s3# -> case writeArray# marr# i# y s3# of
              s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
        in done n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
      where
        err = unreachEx "fromFoldableM"
        !n@(I# n#) = length es
    
    getLeft  es@(STArray _ _ n _) = (es !#>) `mapM` [0 .. n - 1]
    getRight es@(STArray _ _ n _) = (es !#>) `mapM` [n - 1, n - 2 .. 0]
    
    copied es@(STArray _ _ n _) = do
      copy <- filled n $ unreachEx "copied"
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy
    
    copied' es l u = let n' = size (l, u) in do
      copy <- n' `filled` unreachEx "copied'"
      forM_ [0 .. n' - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy
    
    {-# INLINE reversed #-}
    reversed es = liftA2 zip (getIndices es) (getRight es) >>= overwrite es
    
    {-# INLINE filled #-}
    filled n e = let !n'@(I# n#) = max 0 n in ST $
      \ s1# -> case newArray# n# e s1# of (# s2#, marr# #) -> done n' marr# s2#

--------------------------------------------------------------------------------

{- IndexedM and SortM instances. -}

instance (Index i) => IndexedM (ST s) (STArray s i e) i e
  where
    {-# INLINE fromAssocs' #-}
    fromAssocs' bnds defvalue ascs = size bnds `filled` defvalue >>= (`overwrite` ascs)
    
    (STArray _ _ _ marr#) !#> (I# i#) = ST $ readArray# marr# i#
    
    (STArray l u _ marr#) >! i = case offset (l, u) i of (I# i#) -> ST $ readArray# marr# i#
    es@(STArray l u _ _)  !> i = case inBounds (l, u) i of
      ER -> throw $ EmptyRange     msg
      UR -> throw $ IndexUnderflow msg
      IN -> es >! i
      OR -> throw $ IndexOverflow  msg
      where
        msg = "in SDP.Array.ST.(!>)"
    
    writeM_ (STArray _ _ _ marr#) (I# i#) e = ST $
      \ s1# -> case writeArray# marr# i# e s1# of s2# -> (# s2#, () #)
    
    writeM (STArray l u _ marr#) i e = let !(I# i#) = offset (l, u) i in ST $
      \ s1# -> case writeArray# marr# i# e s1# of s2# -> (# s2#, () #)
    
    {-# INLINE overwrite #-}
    overwrite es@(STArray l u _ _) ascs = mapM_ (uncurry $ writeM_ es) ies >> return es
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
    
    fromIndexed' es = do
        copy <- filled n (unreachEx "fromIndexed'")
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        return copy
      where
        n = sizeOf es
    
    fromIndexedM es = do
      n    <- getSizeOf es
      copy <- filled n (unreachEx "fromIndexedM")
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM_ copy i
      return copy

instance (Index i) => SortM (ST s) (STArray s i e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: (Index i) => Int -> MutableArray# s e -> STRep s (STArray s i e)
done n marr# = \ s1# -> let (l, u) = unsafeBounds n in (# s1#, STArray l u n marr# #)

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.Array.ST." ++ msg

