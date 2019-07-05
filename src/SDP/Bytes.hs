{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{-# LANGUAGE TypeFamilies #-}

{- |
    Module      :  SDP.Bytes
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    Stability   :  experimental
    
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

import SDP.Indexed
import SDP.Unboxed
import SDP.Set

import Text.Read
import Text.Read.Lex ( expect )

import GHC.Exts
  (
    ByteArray#, MutableByteArray#, newByteArray#, unsafeFreezeByteArray#,
    
    isTrue#, (+#), (-#), (==#)
  )
import GHC.Show ( appPrec )
import GHC.Int  ( Int (..) )
import GHC.ST   ( ST(..), STRep, runST )

import qualified GHC.Exts as E
import Data.String ( IsString (..) )

import SDP.Simple

default ()

--------------------------------------------------------------------------------

{- |
  This UArray type definition is no different from the standard Data.Array.Base,
  but I have to redefine it because of the limitation of the Ix class.
-}

data Bytes i e = Bytes !i !i {-# UNPACK #-} !Int ByteArray#

type role Bytes nominal nominal

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Index i, Unboxed e) => Eq (Bytes i e)         where xs == ys = listL xs == listL ys

instance (Index i, Unboxed e, Ord e) => Ord (Bytes i e) where compare xs ys = listL xs <=> listL ys

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Index i, Show i, Unboxed e, Show e) => Show (Bytes i e)
  where
    showsPrec p es@(Bytes l u _ _) = showParen (p > appPrec) $ showString "bytes "
                                                             . shows (l, u)
                                                             . showChar ' '
                                                             . shows (assocs es)

instance (Index i, Read i, Unboxed e, Read e) => Read (Bytes i e)
  where
    readList = readListDefault
    readPrec = parens $ prec appPrec (lift . expect $ Ident "bytes") >> liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Semigroup, Monoid and Default instances. -}

instance (Index i, Unboxed e) => Semigroup (Bytes i e) where xs <> ys = xs ++ ys

instance (Index i, Unboxed e) => Monoid    (Bytes i e) where mempty = Z

instance (Index i) => Default (Bytes i e)
  where
    def = runST $ ST $ \ s1# -> case newByteArray# 0# s1# of
      (# s2#, marr# #) -> done (unsafeBounds 0) 0 marr# s2#

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Unboxed e, Index i) => Linear (Bytes i e) e
  where
    {-# INLINE isNull #-}
    isNull (Bytes l u n _) = isEmpty (l, u) || n < 1
    
    {-# INLINE lzero #-}
    lzero = def
    
    {-# INLINE head #-}
    head Z  = pfailEx "(:>)"
    head es = es .! lower es
    
    tail Z  = pfailEx "(:>)"
    tail es = drop 1 es
    
    {-# INLINE last #-}
    last Z  = pfailEx "(:<)"
    last es = es .! upper es
    
    {-# INLINE init #-}
    init Z  = pfailEx "(:<)"
    init es = take (sizeOf es - 1) es
    
    {-# INLINE single #-}
    single e = runST $ ST $ \ s1# -> case newUnboxed' e 1# s1# of (# s2#, marr# #) -> done (unsafeBounds 1) 1 marr# s2#
    
    {-# INLINE fromListN #-}
    fromListN n es = fromListN' (unreachEx "fromListN")
      where
        fromListN' :: (Index i, Unboxed e) => e -> Bytes i e
        fromListN' e = runST $ ST $ \ s1# -> case newUnboxed e n# s1# of
          (# s2#, marr# #) ->
            let go y r = \ i# s3# -> case writeByteArray# marr# i# y s3# of
                  s4# -> if isTrue# (i# ==# n# -# 1#) then s4# else r (i# +# 1#) s4#
            in done (unsafeBounds n') n' marr# ( if n' == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2# )
          where
            !n'@(I# n#) = max 0 $ (es <. n) ? length es $ n
    
    {-# INLINE (++) #-}
    Z  ++ ys = ys
    xs ++  Z = xs
    xs ++ ys = fromList $ listL xs ++ listL ys
    
    {-# INLINE reverse #-}
    reverse es = fromListN (sizeOf es) (listR es)
    
    {-# INLINE replicate #-}
    replicate n e = runST $ ST $ \ s1# -> case newUnboxed' e n# s1# of
        (# s2#, marr# #) -> done (unsafeBounds n') n' marr# s2#
      where
        !n'@(I# n#) = max 0 n
    
    {-# INLINE listL #-}
    listL (Bytes _ _ n bytes#) = [ bytes# !# i# | (I# i#) <- [0 .. n - 1] ]
    
    {-# INLINE listR #-}
    listR (Bytes _ _ n bytes#) = [ bytes# !# i# | (I# i#) <- [n - 1, n - 2 .. 0] ]
    
    {-# INLINE concatMap #-}
    concatMap f ess = fromList $ foldr (\ a l -> listL (f a) ++ l) [] ess
    
    {-# INLINE concat #-}
    concat ess = fromList $ foldr (\ a l -> listL a ++ l) [] ess

instance (Index i, Unboxed e) => Split (Bytes i e) e
  where
    take n es = fromList . take n $ listL es
    drop n es = fromList . drop n $ listL es
    
    isPrefixOf xs ys = n1 <= n2 && take n1 (listL xs) == take n2 (listL ys)        where n1 = sizeOf xs; n2 = sizeOf ys
    isSuffixOf xs ys = n1 <= n2 && take n2 (listL xs) == take (n1 - n2) (listL ys) where n1 = sizeOf xs; n2 = sizeOf ys
    
    -- see SDP.Linear.suffix and SDP.Array.foldr
    prefix p (Bytes _ _ n arr#) = go 0
      where
        go i@(I# i#) = n' == i ? 0 $ p (arr# !# i#) ? (go $ i + 1) + 1 $ 0
        n' = max 0 n
    
    -- see SDP.Linear.suffix and SDP.Array.foldl
    suffix p (Bytes _ _ n arr#) = go $ max 0 n - 1
      where
        go i@(I# i#) = -1 == i ? 0 $ p (arr# !# i#) ? (go $ i - 1) + 1 $ 0

instance (Index i, Unboxed e) => Bordered (Bytes i e) i e
  where
    lower  (Bytes l _ _ _) = l
    upper  (Bytes _ u _ _) = u
    sizeOf (Bytes _ _ n _) = max 0 n

--------------------------------------------------------------------------------

{- Indexed instance. -}

instance (Index i, Unboxed e) => Indexed (Bytes i e) i e
  where
    assoc bnds ascs = writes (undEx "assoc") bnds $ filter (inRange bnds . fst) ascs
      where
        writes :: (Unboxed e, Index i) => e -> (i, i) -> [(i, e)] -> Bytes i e
        writes err bs ies = runST $ ST $ \ s1# -> case newUnboxed err n# s1# of
          (# s2#, marr# #) -> foldr (fill marr#) (done bs n marr#) [ (offset bs i, e) | (i, e) <- ies ] s2#
        
        !n@(I# n#) = size bnds
    
    assoc' bnds defvalue ascs = writes defvalue bnds $ filter (inRange bnds . fst) ascs
      where
        writes :: (Unboxed e, Index i) => e -> (i, i) -> [(i, e)] -> Bytes i e
        writes dv bs ies = runST $ ST $ \ s1# -> case newUnboxed' dv n# s1# of
          (# s2#, marr# #) -> foldr (fill marr#) (done bs n marr#) [ (offset bs i, e) | (i, e) <- ies ] s2#
        
        !n@(I# n#) = size bnds
    
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    es'@(Bytes l' u' _ _) // ascs = writecopy' es' err ies'
      where
        writecopy' :: (Index i, Unboxed e) => Bytes i e -> e -> [(i, e)] -> Bytes i e
        writecopy' es@(Bytes l u n@(I# n#) _) err' ies = runST $ ST $ \ s1# -> case newUnboxed err' n# s1# of
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
                in case foldr gowrite (\ _ s# -> s#) ies 0# s5# of s8# -> done (l, u) n marr# s8#
                -- [end] writing
          where
            ix i = case offset (l, u) i of (I# i#) -> i#
        
        ies' = filter (inRange (l', u') . fst) ascs
        err  = undEx "(//)"
    
    (!) (Bytes l u _ bytes#) i = case offset (l, u) i of (I# i#) -> bytes# !# i#
    
    p .$ es@(Bytes l u _ _) = index (l, u) <$> p .$ listL es
    p *$ es@(Bytes l u _ _) = index (l, u) <$> p *$ listL es

--------------------------------------------------------------------------------

instance (Index i, Unboxed e) => E.IsList (Bytes i e)
  where
    type Item (Bytes i e) = e
    
    fromList    es = fromList    es
    fromListN n es = fromListN n es
    toList      es = listL       es

instance (Index i) => IsString (Bytes i Char) where fromString es = fromList es

instance (Index i, Unboxed e, Arbitrary e) => Arbitrary (Bytes i e) where arbitrary = fromList <$> arbitrary

-- instance (Index i, Unboxed e) => Set (Bytes i e) e

--------------------------------------------------------------------------------

{-# INLINE done #-}
done :: (Index i) => (i, i) -> Int -> MutableByteArray# s -> STRep s (Bytes i e)
done (l, u) n marr# = \ s1# -> case unsafeFreezeByteArray# marr# s1# of (# s2#, bytes# #) -> (# s2#, Bytes l u n bytes# #)

{-# INLINE fill #-}
fill :: (Unboxed e) => MutableByteArray# s -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) nxt = \ s1# -> case writeByteArray# marr# i# e s1# of s2# -> nxt s2#

unreachEx     :: String -> a
unreachEx msg =  throw . UnreachableException $ "in SDP.Array." ++ msg

pfailEx       :: String -> a
pfailEx   msg =  throw . PatternMatchFail $ "in SDP.Bytes." ++ msg

undEx         :: String -> a
undEx     msg =  throw . UndefinedValue $ "in SDP.Bytes." ++ msg

