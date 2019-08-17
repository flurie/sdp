{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{-# LANGUAGE TypeFamilies #-}

{- |
    Module      :  SDP.Array
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    Stability   :  stable
    
    SDP.Array provides immutable lazy boxed array type.
    This implementation of array no much different from Data.Array (array),
    but incopatible with it.
    The main difference is the Index class instead of Ix.
-}

module SDP.Array
(
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  Array (..)
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Indexed
import SDP.Sort
import SDP.Scan
import SDP.Set

import Test.QuickCheck

import GHC.Base
  (
    Array#, Int (..),
    
    newArray#, thawArray#, unsafeFreezeArray#, writeArray#, indexArray#
  )

import GHC.Show ( appPrec )
import GHC.ST   ( ST (..), runST )

import Text.Read
import Text.Read.Lex ( expect )

import qualified GHC.Exts as E
import Data.String ( IsString (..) )

import SDP.SortM.Stuff
import SDP.Array.ST

import SDP.Simple

default ()

--------------------------------------------------------------------------------

{- |
  This Array type definition is no different from the standard GHC.Arr,
  but I have to redefine it because of the limitation of the Ix class.
-}
data Array i e = Array !i !i {-# UNPACK #-} !Int (Array# e)

type role Array nominal representational

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Index i, Eq e) => Eq (Array i e) where (==) = eq1

instance (Index i) => Eq1 (Array i)
  where
    liftEq eq xs ys = liftEq eq' (assocs xs) (assocs ys)
      where
        eq' (i1, x) (i2, y) = i1 == i2 && eq x y

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Index i, Ord e) => Ord (Array i e) where compare = compare1

instance (Index i) => Ord1 (Array i)
  where
    liftCompare cmp xs ys = liftCompare cmp' (assocs xs) (assocs ys)
      where
        cmp' (ix, x) (iy, y) = (ix <=> iy) <> (cmp x y)

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Index i, Show i, Show e) => Show (Array i e)
  where
    showsPrec p arr@(Array l u _ _) = showParen (p > appPrec) $ showString "array "
                                                              . shows (l, u)
                                                              . showChar ' '
                                                              . shows (assocs arr)

instance (Index i, Read i, Read e) => Read (Array i e)
  where
    readList = readListDefault
    readPrec = parens $ prec appPrec (lift . expect $ Ident "array") >> liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Semigroup, Monoid and Default instances. -}

instance (Index i) => Semigroup (Array i e) where xs <> ys = xs ++ ys

instance (Index i) => Monoid    (Array i e) where mempty = Z

instance (Index i) => Default   (Array i e) where def = Z

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances. -}

instance (Index i) => Functor (Array i)
  where
    fmap f arr@(Array l u n@(I# n#) _) = runST $ ST $ \ s1# ->
      case newArray# n# (unreachEx "fmap") s1# of
        (# s2#, marr# #) ->
          let go i@(I# i#) s3# = if i == n
              then case unsafeFreezeArray# marr# s3# of (# s4#, arr# #) -> (# s4#, Array l u n arr# #)
              else case writeArray# marr# i# (f $ arr !# i) s3# of s5# -> go (i + 1) s5#
          in go 0 s2#

instance (Index i) => Zip (Array i)
  where
    zipWith f as bs              = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !# i) (bs !# i)
        sz      = eminimum [EL as, EL bs]
    
    zipWith3 f as bs cs          = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !# i) (bs !# i) (cs !# i)
        sz      = eminimum [EL as, EL bs, EL cs]
    
    zipWith4 f as bs cs ds       = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !# i) (bs !# i) (cs !# i) (ds !# i)
        sz      = eminimum [EL as, EL bs, EL cs, EL ds]
    
    zipWith5 f as bs cs ds es    = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !# i) (bs !# i) (cs !# i) (ds !# i) (es !# i)
        sz      = eminimum [EL as, EL bs, EL cs, EL ds, EL es]
    
    zipWith6 f as bs cs ds es fs = fromListN sz $ apply <$> range (0, sz - 1)
      where
        apply i = f (as !# i) (bs !# i) (cs !# i) (ds !# i) (es !# i) (fs !# i)
        sz      = eminimum [EL as, EL bs, EL cs, EL ds, EL es, EL fs]

instance (Index i) => Applicative (Array i)
  where
    pure = single
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable, Scan and Traversable instances. -}

instance (Index i) => Foldable (Array i)
  where
    {-# INLINE foldr #-}
    foldr  f base arr = go 0
      where
        go i = arr ==. i ? base $ f (arr !# i) (go $ i + 1)
    
    {-# INLINE foldl #-}
    foldl  f base arr = go $ length arr - 1
      where
        go i = -1 == i ? base $ f (go $ i - 1) (arr !# i)
    
    {-# INLINE foldr' #-}
    foldr' f base arr = go (length arr - 1) base
      where
        go i !a = -1 == i ? a $ go (i - 1) (f (arr !# i) a)
    
    {-# INLINE foldl' #-}
    foldl' f base arr = go 0 base
      where
        go i !a = arr ==. i ? a $ go (i + 1) (f a $ arr !# i)
    
    {-# INLINE foldr1 #-}
    foldr1 f arr = null arr ? pfailEx "foldr1" $ go 0
      where
        go i = arr ==. (i + 1) ? e $ f e (go $ i + 1) where e = arr !# i
    
    {-# INLINE foldl1 #-}
    foldl1 f arr = null arr ? pfailEx "foldl1" $ go (length arr - 1)
      where
        go i = 0 == i ? e $ f (go $ i - 1) e where e = arr !# i
    
    {-# INLINE toList #-}
    toList arr@(Array _ _ n _) = [ arr !# i | i <- [ 0 .. n - 1 ] ]
    
    {-# INLINE null #-}
    null       (Array l u n _) = isEmpty (l, u) || n < 1
    
    {-# INLINE length #-}
    length     (Array _ _ n _) = max 0 n

instance (Index i) => Scan (Array i)
  where
    scanl f w es = null es ? single w $ fromListN l (w : res w 0)
      where
        l = length es + 1
        -- res generates infinite list, but fromListN catches it.
        res !curr !n = nxt : res nxt (n + 1)
          where
            nxt = f curr (es !# n)
    
    scanr f w es = null es ? single w $ fromListN l (res w (l - 2) [w])
      where
        l = length es + 1
        res !curr !n ws = n < 0 ? ws $ res prv (n - 1) (prv : ws)
          where
            prv = f (es !# n) curr
    
    scanl' f w es = null es ? single w $ fromListN l (w : res w 0)
      where
        l = length es + 1
        res !curr !n = nxt : res nxt (n + 1)
          where
            nxt = f curr (es !# n)
    
    scanl1 f es = null es ? pfailEx "scanl1" $ fromListN l (res w 0)
      where
        l = length es
        w = head es
        res !curr !n = nxt : res nxt (n + 1)
          where
            nxt = f curr (es !# n)
    
    scanr1 f es = null es ? pfailEx "scanr1" $ fromList (res w (l - 2) [w])
      where
        l = length es
        w = last es
        res !curr !n ws = n < 0 ? ws $ res prv (n - 1) (prv : ws)
          where
            prv = f (es !# n) curr

instance (Index i) => Traversable (Array i) where traverse f arr = fromList <$> traverse f (toList arr)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance (Index i) => Linear (Array i e) e
  where
    isNull es = null es
    
    {-# INLINE lzero #-}
    lzero = runST $ filled 0 (unreachEx "lzero") >>= done
    
    toHead e es = fromListN (n + 1) (e : toList es)    where n = length es
    
    head Z  = pfailEx "(:>)"
    head es = es !# 0
    
    tail Z  = pfailEx "(:>)"
    tail es = fromListN (length es - 1) . tail $ toList es
    
    toLast es e = fromListN (length es + 1) $ foldr (:) [e] es
    
    last Z  = pfailEx "(:<)"
    last arr = arr !# (length arr - 1)
    
    init Z  = pfailEx "(:<)"
    init es = fromListN (length es - 1) $ toList es
    
    fromList es = fromFoldable es
    
    {-# INLINE fromFoldable #-}
    fromFoldable es = runST $ fromFoldableM es >>= done
    
    {-# INLINE single #-}
    single e = runST $ filled 1 e >>= done
    
    xs ++ ys = fromListN (sizeOf xs + sizeOf ys) $ listL xs ++ listL ys
    
    {-# INLINE replicate #-}
    replicate n e = runST $ filled n e >>= done
    
    {-# INLINE reverse #-}
    reverse es = length es `fromListN` listR es
    
    listL es = toList es
    
    {-# INLINE listR #-}
    listR es = [ es !# i | i <- [ n - 1, n - 2 .. 0 ] ] where n = length es
    
    {-# INLINE concatMap #-}
    concatMap f ess = fromList $ foldr (\ a l -> toList (f a) ++ l) [] ess
    
    {-# INLINE concat #-}
    concat ess = fromList $ foldr (\ a l -> toList a ++ l) [] ess

instance (Index i) => Split (Array i e) e
  where
    {-#  INLINE take #-}
    take n es
        | n <= 0 = Z
        | n >= l = es
        |  True  = fromList [ es !# i | i <- [ 0 .. n - 1 ] ]
      where
        l = length es
    
    {-# INLINE drop #-}
    drop n es
        | n <= 0 = es
        | n >= l = Z
        |  True  = fromListN (l - n) [ es !# i | i <- [ n .. l - 1 ] ]
      where
        l = length es
    
    {-# INLINE split #-}
    split n es
        | n <= 0 = (Z, es)
        | n >= l = (es, Z)
        |  True  = (fromListN n take', fromListN (l - n) drop')
      where
        (take', drop') = split n $ toList es
        l = length es
    
    isPrefixOf xs ys = xs .<=. ys && equals
      where
        equals = and [ xs !# i == ys !# i | i <- [ 0 .. length xs - 1 ] ]
    
    isSuffixOf xs ys = xs .<=. ys && and equals
      where
        equals  = [ xs !# i == xs !# (i + offset') | i <- [ 0 .. length xs - 1 ] ]
        offset' = length ys - length xs

instance (Index i) => Bordered (Array i e) i e
  where
    lower   (Array l _ _ _) = l
    upper   (Array _ u _ _) = u
    sizeOf  (Array _ _ n _) = n

--------------------------------------------------------------------------------

{- Indexed instance. -}

instance (Index i) => Indexed (Array i e) i e
  where
    {-# INLINE assoc' #-}
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    {-# INLINE (//) #-}
    Z // ascs = null ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    
    arr // ascs = runST $ fromFoldableM arr >>= (`overwrite` ascs) >>= done
    
    {-# INLINE (!) #-}
    (!) arr@(Array l u _ _) i = arr !# offset (l, u) i
    
    p .$ es = (\ i -> p $ es ! i)  `find`  indices es
    p *$ es = (\ i -> p $ es ! i) `filter` indices es

--------------------------------------------------------------------------------

instance (Index i) => E.IsList (Array i e)
  where
    type Item (Array i e) = e
    fromList    es = fromList    es
    fromListN n es = fromListN n es
    toList      es = toList      es

instance (Index i) => IsString (Array i Char) where fromString es = fromList es

instance (Index i) => Estimate (Array i) where xs <==> ys = length xs <=> length ys

-- instance (Index i) => Set (Array i e) e

instance (Index i, Arbitrary e) => Arbitrary (Array i e) where arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

instance (Index i) => Sort (Array i e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

{-# INLINE (!#) #-}
(!#) :: Array i e -> Int -> e
(!#) (Array _ _ _ arr#) (I# i#) = case indexArray# arr# i# of (# e #) -> e

{-# INLINE done #-}
done :: STArray s i e -> ST s (Array i e)
done (STArray l u n marr#) = ST $ \ s1# -> case unsafeFreezeArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, Array l u n arr# #)

thaw :: Array i e -> ST s (STArray s i e)
thaw (Array l u n arr#) = ST $ \ s1# -> case thawArray# arr# 0# n# s1# of
    (# s2#, marr# #) -> (# s2#, STArray l u n marr# #)
  where
    !(I# n#) = max 0 n

pfailEx       :: String -> a
pfailEx   msg =  throw . PatternMatchFail $ "in SDP.Array." ++ msg

unreachEx     :: String -> a
unreachEx msg =  throw . UnreachableException $ "in SDP.Array." ++ msg





