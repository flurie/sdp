{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{-# LANGUAGE TypeFamilies #-}

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

import SDP.Indexed
import SDP.Unboxed
import SDP.Sort
import SDP.Set

import Text.Read
import Text.Read.Lex ( expect )

import GHC.Base ( Int (..), ByteArray#, newByteArray#, unsafeFreezeByteArray# )
import GHC.Show ( appPrec )

import GHC.ST   ( runST, ST (..) )

import qualified GHC.Exts as E
import Data.String ( IsString (..) )

import SDP.SortM.Stuff
import SDP.Bytes.ST

import SDP.Simple

default ()

--------------------------------------------------------------------------------

{- |
  This Bytes type definition is no different from the standard Data.Array.Unboxed,
  but I have to redefine it because of the limitation of the Ix class.
-}

data Bytes i e = Bytes !i !i {-# UNPACK #-} !Int ByteArray#

type role Bytes nominal representational

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
    def = let (l, u) = unsafeBounds 0 in runST $ ST $
      \ s1# -> case newByteArray# 0# s1# of
        (# s2#, marr# #) -> case unsafeFreezeByteArray# marr# s2# of
          (# s3#, bytes# #) -> (# s3#, Bytes l u 0 bytes# #)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Unboxed e, Index i) => Linear (Bytes i e) e
  where
    isNull (Bytes l u n _) = isEmpty (l, u) || n < 1
    
    lzero = def
    
    head Z  = pfailEx "(:>)"
    head es = es .! lower es
    
    tail Z  = pfailEx "(:>)"
    tail es = drop 1 es
    
    last Z  = pfailEx "(:<)"
    last es = es .! upper es
    
    init Z  = pfailEx "(:<)"
    init es = take (sizeOf es - 1) es
    
    {-# INLINE single #-}
    single e = let (l, u) = unsafeBounds 1 in runST $ ST $
      \ s1# -> case newUnboxed' e 1# s1# of
        (# s2#, marr# #) -> case unsafeFreezeByteArray# marr# s2# of
          (# s3#, bytes# #) -> (# s3#, Bytes l u 1 bytes# #)
    
    fromList es = fromFoldable es
    
    {-# INLINE fromFoldable #-}
    fromFoldable es = runST $ fromFoldableM es >>= done
    
    Z  ++ ys = ys
    xs ++  Z = xs
    xs ++ ys = fromList $ listL xs ++ listL ys
    
    {-# INLINE reverse #-}
    reverse es = fromListN (sizeOf es) (listR es)
    
    {-# INLINE replicate #-}
    replicate n e = runST $ filled n e >>= done
    
    listL (Bytes _ _ n bytes#) = [ bytes# !# i# | (I# i#) <- [0 .. n - 1] ]
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
    
    {-# INLINE prefix #-}
    -- see SDP.Linear.suffix and SDP.Array.foldr
    prefix p (Bytes _ _ n arr#) = go 0
      where
        go i@(I# i#) = n' == i ? 0 $ p (arr# !# i#) ? (go $ i + 1) + 1 $ 0
        n' = max 0 n
    
    {-# INLINE suffix #-}
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
    {-# INLINE assoc #-}
    assoc bnds ascs = runST $ fromAssocs bnds ascs >>= done
    
    {-# INLINE assoc' #-}
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    {-# INLINE (//) #-}
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    arr // ascs = runST $ newLinear (listL arr) >>= (`overwrite` ascs) >>= done
    
    {-# INLINE (!^) #-}
    (Bytes _ _ _ bytes#) !^ (I# i#) = bytes# !# i#
    
    {-# INLINE (!) #-}
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

instance (Index i, Unboxed e) => Sort (Bytes i e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

thaw :: (Index i, Unboxed e) => Bytes i e -> ST s (STBytes s i e)
thaw es@(Bytes l u n _) = ST $ \ s1# -> case rep s1# of
    (# s2#, es'@(STBytes _ _ _ marr#) #) -> (# s2#, (STBytes l u n marr#) `asTypeOf` es' #)
  where
    (ST rep) = newLinear $ listL es

{-# INLINE done #-}
done :: (Index i) => STBytes s i e -> ST s (Bytes i e)
done (STBytes l u n mbytes#) = ST $ \ s1# -> case unsafeFreezeByteArray# mbytes# s1# of
  (# s2#, bytes# #) -> (# s2#, Bytes l u n bytes# #)

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.Bytes." ++ msg




