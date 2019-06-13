{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.Bytes
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    SDP.Bytes provides immutable strict unboxed array type.
    This implementation of UArray no much different from Data.Array.Unboxed (array),
    but incopatible with it.
    The main difference is the Index class instead of Ix.
-}

module SDP.Bytes
(
  module SDP.Unboxed,
  module SDP.Indexed,
  module SDP.Set,
  
  Bytes (..)
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.Internal.MutableArrays ( STUArray (..) )

import SDP.Indexed
import SDP.Unboxed
import SDP.Set

import Text.Read
import Text.Read.Lex ( expect )

import GHC.Exts
  (
    ByteArray#, newByteArray#, unsafeFreezeByteArray#,
    
    isTrue#, (+#), (-#), (==#)
  )
import GHC.Show ( appPrec )
import GHC.Int  ( Int (..) )
import GHC.ST   ( ST(..), STRep, runST )

import SDP.Simple

--------------------------------------------------------------------------------

{- |
  This UArray type definition is no different from the standard Data.Array.Base,
  but I have to redefine it because of the limitation of the Ix class.
-}

data Bytes i e = Bytes !i !i {-# UNPACK #-} !Int ByteArray#

type role Bytes nominal nominal

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Index i, Unboxed e) => Eq (Bytes i e)
  where
    xs@(Bytes l1 u1 n1 _) == ys@(Bytes l2 u2 n2 _) = n1 == n2 && (n1 == 0 || l1 == l2 && u1 == u2 && elemEq)
      where
        elemEq = (listL xs) == (listL ys)

instance (Index i, Unboxed e, Ord e) => Ord (Bytes i e)
  where
    compare xs@(Bytes _ _ n1 _) ys@(Bytes _ _ n2 _) = n1 <=> n2 <> elemCmp
      where
        elemCmp = (listL xs) <=> (listL ys)

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Index i, Show i, Unboxed e, Show e) => Show (Bytes i e)
  where
    show es@(Bytes l u _ _) = showString "bytes " . shows (l, u) . showChar ' ' $ show (assocs es)

instance (Index i, Read i, Unboxed e, Read e) => Read (Bytes i e)
  where
    readList = readListDefault
    readPrec = parens $ prec appPrec (lift . expect $ Ident "bytes") >> liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Unboxed e, Index i) => Linear (Bytes i e) e
  where
    isNull es = sizeOf es == 0
    
    uncons Z  = throw $ EmptyRange "in SDP.Bytes.uncons"
    uncons es = (es ! l, drop 1 es) where l = lower es
    
    unsnoc Z  = throw $ EmptyRange "in SDP.Bytes.unsnoc"
    unsnoc es = (take n es, es ! u)
      where
        n = sizeOf es - 1
        u = upper es
    
    fromListN = fromListN' (throw $ UndefinedValue "in SDP.Bytes.fromListN")
      where
        fromListN' :: (Index i, Unboxed e) => e -> Int -> [e] -> Bytes i e
        fromListN' e n es = runST $ ST
            (
              \ s1# -> case newUnboxed e n# s1# of
                  (# s2#, marr# #) ->
                    let go y r = \ i# s3# -> case writeByteArray# marr# i# y s3# of
                          s4# -> if isTrue# (i# ==# n# -# 1#)
                                    then s4#
                                    else r (i# +# 1#) s4#
                    in done (STUArray l u n' marr#)
                    (
                      if n' == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2#
                    )
            )
          where
            !n'@(I# n#) = max 0 $ (es <. n) ? length es $ n
            l = unsafeIndex 0
            u = unsafeIndex (n' - 1)
    
    xs  ++  ys = fromListN (sizeOf xs + sizeOf ys) $ (listL xs) ++ (listL ys)
    
    filter  f  = fromList . filter f . listL
    
    reverse es = fromListN (sizeOf es) (listR es)
    
    listL (Bytes _ _ n bytes#) = [ bytes# !# i# | (I# i#) <- [0 .. n - 1] ]
    listR (Bytes _ _ n bytes#) = [ bytes# !# i# | (I# i#) <- [n - 1, n - 2 .. 0] ]
    
    concatMap f = concat . map f . toList

instance (Index i, Unboxed e) => Split (Bytes i e) e
  where
    take n = fromListN n . listL
    drop n = fromList . drop n . listL
    
    split n es = (fromListN n take', fromList drop')
      where
        (take', drop') = split n (listL es)
    
    prefix p = prefix p . listL
    suffix p = suffix p . listL

instance (Index i, Unboxed e) => Bordered (Bytes i e) i e
  where
    bounds (Bytes l u _ _) = (l, u)
    lower  (Bytes l _ _ _) = l
    upper  (Bytes _ u _ _) = u
    sizeOf (Bytes _ _ n _) = n

--------------------------------------------------------------------------------

{- Indexed instance. -}

instance (Index i, Unboxed e) => Indexed (Bytes i e) i e
  where
    assoc' (l, u) defvalue ascs = runST $ ST $
      \ s1# -> case newUnboxed defvalue n# s1# of
        (# s2#, marr# #) ->
          let gowrite (i, y) r = \ i# s3# -> case writeByteArray# marr# (ix i) y s3# of
                s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
          in case foldr gowrite (\ _ s# -> s#) ies 0# s2# of
            s5# -> case unsafeFreezeByteArray# marr# s5# of
              (# s6#, arr# #) -> (# s6#, Bytes l u n arr# #)
        where
          ies = filter (inRange (l, u) . fst) ascs
          ix i = case offset (l, u) i of (I# i#) -> i#
          !n@(I# n#) = size (l, u)
    
    -- I spent half a day on this code. It was a fun time...
    -- Now I need to understand what I wrote.
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    es'@(Bytes l' u' _ _) // ascs = writecopy' es' err ies'
      where
        writecopy' :: (Index i, Unboxed e) => Bytes i e -> e -> [(i, e)] -> Bytes i e
        writecopy' es@(Bytes l u n@(I# n#) _) err' ies = runST $ ST $
          \ s1# -> case newUnboxed err' n# s1# of
            -- new mutable array created
            (# s2#, marr# #) ->
              -- [begin] copying
              let gocopy y r = \ i# s3# -> case writeByteArray# marr# i# y s3# of
                    s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
              in case foldr gocopy (\ _ s# -> s#) (listL es) 0# s2# of
              -- [end] copyng
                s5# ->
                  -- [begin] writing
                  let gowrite (i, y) r = \ i# s6# -> case writeByteArray# marr# (ix i) y s6# of
                        s7# -> if isTrue# (i# ==# n# -# 1#) then s7# else r (i# +# 1#) s7#
                  in case foldr gowrite (\ _ s# -> s#) ies 0# s5# of
                  -- [end] writing
                    s8# -> case unsafeFreezeByteArray# marr# s8# of
                      (# s9#, arr# #) -> (# s9#, Bytes l u n arr# #)
                      -- array freezed
          where
            ix i = case offset (l, u) i of (I# i#) -> i#
        
        ies' = filter (inRange (l', u') . fst) ascs
        err  = throw $ UndefinedValue "in SDP.Bytes.(//)"
    
    (!)  (Bytes l u _ bytes#) i = case offset (l, u) i of (I# i#) -> bytes# !# i#
    
    (.$) p es@(Bytes l u _ _)   = index (l, u) <$> p .$ (listL es)
    (*$) p es@(Bytes l u _ _)   = index (l, u) <$> p *$ (listL es)

--------------------------------------------------------------------------------

-- instance (Index i, Unboxed e) => Set (Bytes i e) e

instance (Index i, Unboxed e) => LineS (Bytes i e) e
  where
    stream = stream . toList

instance (Index i) => Default (Bytes i e)
  where
    def = runST $ ST $ \ s1# -> case newByteArray# 0# s1# of
      (# s2#, marr# #) -> case unsafeFreezeByteArray# marr# s2# of
        (# s3#, arr# #) -> (# s3#, Bytes l u 0 arr# #)
      where
        l = unsafeIndex 0
        u = unsafeIndex $ -1

instance (Index i, Unboxed e, Arbitrary e) => Arbitrary (Bytes i e)
  where
    arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

done :: (Index i, Unboxed e) => STUArray s i e -> STRep s (Bytes i e)
done (STUArray l u n marr#) = \ s1# -> case unsafeFreezeByteArray# marr# s1# of
  (# s2#, bytes# #) -> (# s2#, Bytes l u n bytes# #)

