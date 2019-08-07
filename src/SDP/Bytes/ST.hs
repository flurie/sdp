{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.Bytes.ST
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    Stability   :  stable
    
    SDP.Bytes.ST provides mutable lazy boxed array type.
    This implementation of array no much different from Data.Array.ST (array),
    but incopatible with it.
    The main difference is the Index class instead of Ix.
-}

module SDP.Bytes.ST
(
  module SDP.IndexedM,
  
  STBytes (..)
)
where

import Prelude ( (++), zip, filter, reverse )
import SDP.SafePrelude

import SDP.IndexedM
import SDP.Unboxed

import GHC.Base
  (
    MutableByteArray#, Int (..),
    
    isTrue#, sameMutableByteArray#, (+#), (-#), (==#)
  )

import GHC.ST ( ST (..), STRep )

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
    (STBytes l1 u1 n1 marr1#) == (STBytes l2 u2 n2 marr2#) = n1 == n2 && (n1 == 0 || l1 == l2 && u1 == u2 && isTrue# (sameMutableByteArray# marr1# marr2#))

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance (Index i, Unboxed e) => BorderedM (ST s) (STBytes s i e) i e
  where
    getLower  (STBytes l _ _ _) = return l
    getUpper  (STBytes _ u _ _) = return u
    getSizeOf (STBytes _ _ n _) = return $ max 0 n
    
    getIndices (STBytes l u _ _)   = return $ range   (l, u)
    getIndexOf (STBytes l u _ _) i = return $ inRange (l, u) i

instance (Index i, Unboxed e) => LinearM (ST s) (STBytes s i e) e
  where
    newLinear es = fromFoldableM es
    
    {-# INLINE fromFoldableM #-}
    fromFoldableM es = fromFoldable' $ unreachEx "fromFoldableM"
      where
        fromFoldable' :: (Index i, Unboxed e) => e -> ST s (STBytes s i e)
        fromFoldable' e = ST $ \ s1# -> case newUnboxed e n# s1# of
          (# s2#, marr# #) ->
            let go y r = \ i# s3# -> case writeByteArray# marr# i# y s3# of
                  s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
            in done (unsafeBounds n) n marr# ( if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
        
        !n@(I# n#) = length es
    
    getLeft  es@(STBytes l u _ _) = sequence [ es >! i | i <- reverse $ range (l, u) ]
    getRight es@(STBytes l u _ _) = sequence [ es >! i | i <- range (l, u) ]
    
    {-# INLINE reversed #-}
    reversed es = liftA2 (\ bnds -> zip $ range bnds) (getBounds es) (getRight es) >>= overwrite es
    
    {-# INLINE filled #-}
    filled n e = ST $ \ s1# -> case newUnboxed' e n# s1# of (# s2#, marr# #) -> (# s2#, STBytes l u n' marr# #)
      where
        (l, u) = unsafeBounds n'
        !n'@(I# n#) = max 0 n

--------------------------------------------------------------------------------

{- IndexedM instance. -}

instance (Index i, Unboxed e) => IndexedM (ST s) (STBytes s i e) i e
  where
    {-# INLINE fromAssocs #-}
    fromAssocs bnds ascs = filled_ bnds err >>= (`overwrite` ascs)
      where
        filled_ :: (Index i, Unboxed e) => (i, i) -> e -> ST s (STBytes s i e)
        filled_ (l, u) e = ST $ \ s1# -> case newUnboxed e n# s1# of
          (# s2#, marr# #) -> (# s2#, STBytes l u n marr# #)
        
        err = unreachEx "fromAssocs"
        !n@(I# n#) = size bnds
    
    {-# INLINE fromAssocs' #-}
    fromAssocs' bnds defvalue ascs = size bnds `filled` defvalue >>= (`overwrite` ascs)
    
    {-# INLINE (>!) #-}
    (STBytes l u _ marr#) >! i = case offset (l, u) i of (I# i#) -> ST $ marr# !># i#
    
    es@(STBytes l u _ _) !> i = case inBounds (l, u) i of
      ER -> throw $ EmptyRange     msg
      UR -> throw $ IndexUnderflow msg
      IN -> es >! i
      OR -> throw $ IndexOverflow  msg
      where
        msg = "in SDP.Bytes.ST.(!>)"
    
    writeM (STBytes l u _ mbytes#) i e = let !(I# i#) = offset (l, u) i in ST $
      \ s1# -> case writeByteArray# mbytes# i# e s1# of s2# -> (# s2#, () #)
    
    {-# INLINE overwrite #-}
    overwrite (STBytes l u n marr#) ascs = ST $ foldr (fill marr#) (done (l, u) n marr#) ies
      where
        ies = [ (offset (l, u) i, e) | (i, e) <- ascs, inRange (l, u) i ]
    
    p *? marr = fsts . filter (p . snd) <$> getAssocs marr

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: (Unboxed e) => (i, i) -> Int -> MutableByteArray# s -> STRep s (STBytes s i e)
done (l, u) n marr# = \ s1# -> (# s1#, STBytes l u n marr# #)

{-# INLINE fill #-}
fill :: (Unboxed e) => MutableByteArray# s -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) nxt = \ s1# -> case writeByteArray# marr# i# e s1# of s2# -> nxt s2#

unreachEx     :: String -> a
unreachEx msg =  throw . UnreachableException $ "in SDP.Bytes.ST." ++ msg

