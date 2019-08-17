{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.Array.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    Stability   :  stable
    
    SDP.Array.ST provides mutable lazy boxed array type.
    This implementation of array no much different from Data.Array.ST (array),
    but incopatible with it.
    The main difference is the Index class instead of Ix.
-}

module SDP.Array.ST
(
  module SDP.IndexedM,
  
  STArray (..)
)
where

import Prelude ( (++), zip, filter, reverse )
import SDP.SafePrelude

import SDP.IndexedM

import GHC.Base
  (
    MutableArray#, Int (..),
    
    newArray#, writeArray#, readArray#,
    
    isTrue#, sameMutableArray#, (+#), (-#), (==#)
  )

import GHC.ST ( ST (..), STRep )

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
    (STArray l1 u1 n1 marr1#) == (STArray l2 u2 n2 marr2#) = n1 == n2 && (n1 == 0 || l1 == l2 && u1 == u2 && isTrue# (sameMutableArray# marr1# marr2#))

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance (Index i) => BorderedM (ST s) (STArray s i e) i e
  where
    getLower  (STArray l _ _ _) = return l
    getUpper  (STArray _ u _ _) = return u
    getSizeOf (STArray _ _ n _) = return $ max 0 n
    
    getIndices (STArray l u _ _)   = return $ range   (l, u)
    getIndexOf (STArray l u _ _) i = return $ inRange (l, u) i

instance (Index i) => LinearM (ST s) (STArray s i e) e
  where
    newLinear es = fromFoldableM es
    
    {-# INLINE fromFoldableM #-}
    fromFoldableM es = ST $ \ s1# -> case newArray# n# err s1# of
      (# s2#, marr# #) ->
        let go y r = \ i# s3# -> case writeArray# marr# i# y s3# of
              s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
        in done (l, u) n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
      where
        !n@(I# n#) = length es
        (l, u) = unsafeBounds n
        err    = unreachEx "fromFoldableM"
    
    getLeft  es@(STArray l u _ _) = sequence [ es >! i | i <- range (l, u) ]
    getRight es@(STArray l u _ _) = sequence [ es >! i | i <- reverse $ range (l, u) ]
    
    copied (STArray _ _ n marr#) = do
      copy@(STArray _ _ _ marr1#) <- filled n $ unreachEx "copied"
      forM_ [0 .. n - 1] $ \ (I# i#) -> ST $ \ s1# -> case readArray# marr# i# s1# of
        (# s2#, e #) -> case writeArray# marr1# i# e s2# of s3# -> (# s3#, () #)
      return copy
    
    {-# INLINE reversed #-}
    reversed es = liftA2 (\ bnds -> zip $ range bnds) (getBounds es) (getRight es) >>= overwrite es
    
    {-# INLINE filled #-}
    filled n e = let !n'@(I# n#) = max 0 n in ST $ \ s1# -> case newArray# n# e s1# of
      (# s2#, marr# #) -> done (unsafeBounds n') n' marr# s2#

--------------------------------------------------------------------------------

{- IndexedM instance. -}

instance (Index i) => IndexedM (ST s) (STArray s i e) i e
  where
    {-# INLINE fromAssocs' #-}
    fromAssocs' bnds defvalue ascs = newArr >>= (`overwrite` ascs)
      where
        newArr = ST $ \ s1# -> case newArray# n# defvalue s1# of (# s2#, marr# #) -> done bnds n marr# s2#
        !n@(I# n#) = size bnds
    
    (STArray _ _ _ marr#) !#> (I# i#) = ST $ readArray# marr# i#
    
    {-# INLINE (>!) #-}
    (STArray l u _ marr#) >! i = case offset (l, u) i of (I# i#) -> ST $ readArray# marr# i#
    
    writeM_ (STArray _ _ _ marr#) (I# i#) e = ST $
      \ s1# -> case writeArray# marr# i# e s1# of s2# -> (# s2#, () #)
    
    writeM (STArray l u _ marr#) i e = let !(I# i#) = offset (l, u) i in ST $
      \ s1# -> case writeArray# marr# i# e s1# of s2# -> (# s2#, () #)
    
    es@(STArray l u _ _) !> i = case inBounds (l, u) i of
      ER -> throw $ EmptyRange     msg
      UR -> throw $ IndexUnderflow msg
      IN -> es >! i
      OR -> throw $ IndexOverflow  msg
      where
        msg = "in SDP.Array.ST.(!>)"
    
    {-# INLINE overwrite #-}
    overwrite es@(STArray l u _ _) ascs = mapM_ (uncurry $ writeM_ es) ies >> return es
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
    
    p *? marr = fsts . filter (p . snd) <$> getAssocs marr

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: (i, i) -> Int -> MutableArray# s e -> STRep s (STArray s i e)
done (l, u) n marr# = \ s1# -> (# s1#, STArray l u n marr# #)

unreachEx     :: String -> a
unreachEx msg =  throw . UnreachableException $ "in SDP.Array.ST." ++ msg

