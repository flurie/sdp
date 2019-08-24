{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}

{- |
    Module      :  SDP.Unrolled.Unlist
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    
    This module provides service type Unlist - lazy boxed unrolled linked list
    for SDP.Unrolled.
-}

module SDP.Unrolled.Unlist
(
  module SDP.Indexed,
  module SDP.Sort,
  module SDP.Scan,
  module SDP.Set,
  
  Unlist (..)
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.Indexed
import SDP.Sort
import SDP.Scan
import SDP.Set

import GHC.Show ( appPrec )
import GHC.Base
  (
    Array#, MutableArray#, Int (..), Int#,
    
    newArray#, unsafeFreezeArray#, writeArray#, indexArray#
  )

import GHC.ST ( ST (..), STRep, runST )

import Data.String ( IsString (..) )

import SDP.Unrolled.STUnlist
import SDP.SortM.Stuff
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | Unlist is internal data representation.
data Unlist e = UNEmpty | Unlist {-# UNPACK #-} !Int (Array# e) (Unlist e)

type role Unlist representational

{-# COMPLETE Z, Unlist #-}

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Eq e) => Eq (Unlist e) where (==) = eq1

instance Eq1 Unlist where liftEq f xs ys = liftEq f (toList xs) (toList ys)

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Ord e) => Ord (Unlist e) where compare = compare1

instance Ord1 Unlist where liftCompare f xs ys = liftCompare f (toList xs) (toList ys)

--------------------------------------------------------------------------------

{- Show instance. -}

instance (Show e) => Show (Unlist e)
  where
    showsPrec p unl = showParen (p > appPrec) $ showString "unlist "
                                              . shows (assocs unl)

--------------------------------------------------------------------------------

{- Semigroup, Monoid and Default instances. -}

instance Semigroup (Unlist e) where (<>) = (++)

instance Monoid (Unlist e) where mempty = def

instance Default (Unlist e) where def = UNEmpty

--------------------------------------------------------------------------------

{- Functor instance. -}

instance Functor Unlist
  where
    fmap _ UNEmpty = UNEmpty
    fmap f (Unlist n@(I# n#) arr# arrs) = runST $ ST $ \ s1# ->
      case newArray# n# (unreachEx "fmap") s1# of
        (# s2#, marr# #) ->
          let go i@(I# i#) s# = if i == n
              then done' n (f <$> arrs) marr# s#
              else fill marr# (i, f $ arr# !# i#) (go $ i + 1) s#
          in go 0 s2#

--------------------------------------------------------------------------------

{- Foldable instance. -}

instance Foldable Unlist
  where
    foldr _ base Z = base
    foldr f base (Unlist c arr# arrs) = go (foldr f base arrs) 0
      where
        go b i@(I# i#) = c == i ? b $ f (arr# !# i#) (go b $ i + 1)
    
    foldr' _ base Z = base
    foldr' f base (Unlist c arr# arrs) = go (foldr' f base arrs) 0
      where
        go b i@(I# i#) = c == i ? b $ f (arr# !# i#) (go b $ i + 1)
    
    foldl _ base Z = base
    foldl f base (Unlist c arr# arrs) = foldl f (go base $ c - 1) arrs
      where
        go b i@(I# i#) = -1 == i ? b $ f (go b $ i - 1) (arr# !# i#)
    
    foldl' _ base Z = base
    foldl' f base (Unlist c arr# arrs) = foldl' f (go base c) arrs
      where
        go b i@(I# i#) = -1 == i ? b $ f (go b $ i - 1) (arr# !# i#)
    
    length es = case es of {Unlist n _ arrs -> max 0 n + length arrs; _ -> 0}
    
    toList UNEmpty = []
    toList (Unlist c arr# arrs) = [ arr# !# i# | (I# i#) <- [0 .. c - 1] ] ++ toList arrs
    
    null es = case es of {Unlist c _ _ -> c < 1; _ -> True}

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance Linear (Unlist e) e
  where
    isNull es = null es
    
    lzero = def
    
    toHead e Z = single e
    toHead e (Unlist c arr# arrs) = c < lim ? res1 $ (Unlist 1 single# arrs)
      where
        res1 = fromListN (max 0 c + 1) $ e : toList (Unlist c arr# Z)
        !(Unlist 1 single# Z) = single e
    
    head Z  = pfailEx "(:>)"
    head es = es !^ 0
    
    tail Z  = pfailEx "(:<)"
    tail es@(Unlist _ _ Z)    = fromList . tail $ toList es
    tail (Unlist c arr# arrs) = Unlist c' new# arrs
      where
        !(Unlist c' new# _) = tail (Unlist c arr# Z)
    
    toLast Z e = single e
    toLast es@(Unlist c _ Z) e = c < lim ? res1 $ (Unlist 1 single# Z)
      where
        res1 = fromListN (max 0 c + 1) $ foldr (:) [e] es
        !(Unlist 1 single# Z) = single e
    toLast (Unlist c arr# arrs) e = Unlist c arr# (toLast arrs e)
    
    last Z  = pfailEx "(:<)"
    last es = es !^ (length es - 1)
    
    init Z = pfailEx "(:>)"
    init es@(Unlist _ _ Z)    = fromList . init $ toList es
    init (Unlist c arr# arrs) = Unlist c arr# (init arrs)
    
    {-# INLINE single #-}
    single e = runST $ filled 1 e >>= done
    
    listL = toList
    
    fromList = fromFoldable
    
    {-# INLINE fromFoldable #-}
    fromFoldable es = runST $ fromFoldableM es >>= done
    
    Z ++ ys = ys
    xs ++ Z = xs
    (Unlist c arr# arrs) ++ ys = Unlist c arr# (arrs ++ ys)
    
    -- | Deduplicated Unlist.
    {-# INLINE replicate #-}
    replicate n e = copy count
      where
        chunk  = runST $ ST $ \ s1# -> case newArray# l# e s1# of (# s2#, marr# #) -> done' lim Z marr# s2#
        rest   = runST $ ST $ \ s1# -> case newArray# r# e s1# of (# s2#, marr# #) -> done' restSize Z marr# s2#
        copy c = case c <=> 0 of {LT -> Z; EQ -> rest; GT -> chunk ++ copy (c - 1)}
        
        !(count, restSize@(I# r#)) = n `divMod` lim
        
        !(I# l#) = lim
    
    {-# INLINE reverse #-}
    reverse es' = reverse' Z es'
      where
        reverse' :: Unlist e -> Unlist e -> Unlist e
        reverse' tail' Z = tail'
        reverse' tail' (Unlist n bytes# bytes) = reverse' (Unlist n rev# tail') bytes
          where
            !(Unlist _ rev# _) = runST $ newLinear chunk >>= done
            
            chunk = [ bytes# !# i# | (I# i#) <- [ n - 1, n - 2 .. 0 ] ]
    
    partition  p  es = let (x, y) = partition p $ toList es in (fromList x, fromList y)
    partitions ps es = fromList <$> (partitions ps $ toList es)

instance Split (Unlist e) e
  where
    {-# INLINE take #-}
    take n es
        |  n <= 0  = Z
        | es <=. n = es
        |   True   = take' n es
      where
        take' _ Z = Z
        take' n' (Unlist c arr# arrs) = n' >= c ? Unlist c arr# other $ fromListN n' rest
          where
            rest  = [ arr# !# i# | (I# i#) <- [0 .. n' - 1] ]
            other = take' (n' - c) arrs
    
    {-# INLINE drop #-}
    drop n es
        |  n <=  0 = es
        | es <=. n = Z
        |   True   = drop' n es
      where
        drop' _ Z = Z
        drop' n' (Unlist c arr# arrs) = n' >= c ? rest $ other ++ arrs
          where
            rest  = drop' (n' - c) arrs
            other = fromListN (c - n') [ arr# !# i# | (I# i#) <- [n' .. c - 1] ]
    
    isPrefixOf xs ys = toList xs `isPrefixOf` toList ys
    isInfixOf  xs ys = toList xs `isInfixOf`  toList ys
    isSuffixOf xs ys = toList xs `isSuffixOf` toList ys

instance Bordered (Unlist e) Int e
  where
    lower  _  = 0
    upper  es = length es - 1
    sizeOf es = length es

--------------------------------------------------------------------------------

{- Indexed and Sort instances. -}

instance Indexed (Unlist e) Int e
  where
    {-# INLINE assoc' #-}
    assoc' bnds defvalue ascs = runST $ fromAssocs' bnds defvalue ascs >>= done
    
    {-# INLINE (//) #-}
    Z // ascs = isNull ascs ? Z $ assoc (l, u) ascs
      where
        l = fst $ minimumBy cmpfst ascs
        u = fst $ maximumBy cmpfst ascs
    es // ascs = isNull ascs ? es $ runST $ fromFoldableM es >>= flip overwrite ascs >>= done
    
    fromIndexed es = runST $ do
        copy <- filled n (unreachEx "fromIndexed")
        forM_ [0 .. n - 1] $ \ i -> writeM_ copy i (es !^ i)
        done copy
      where
        n = sizeOf es
    
    Z !^ _ = error "in SDP.Unrolled.Unlist.(!^)"
    (Unlist c arr# arrs) !^ i@(I# i#) = i < c ? e $ arrs !^ (i - c)
      where
        (# e #) = indexArray# arr# i#
    
    {-# INLINE (.!) #-}
    es .! n = es !^ (n - lower es)
    
    (!) es n = case inBounds bs n of
        ER -> throw $ EmptyRange     msg
        UR -> throw $ IndexOverflow  msg
        OR -> throw $ IndexUnderflow msg
        IN -> es !^ offset bs n
      where
        msg = "in SDP.Unrolled.(!)"
        bs  = bounds es
    
    p .$ es = p .$ toList es
    p *$ es = p *$ toList es

instance Sort (Unlist e) e
  where
    sortBy cmp es = runST $ do es' <- thaw es; timSortBy cmp es'; done es'

--------------------------------------------------------------------------------

instance IsString (Unlist Char) where fromString = fromList

instance Estimate Unlist
  where
    UNEmpty         <==> ys = case ys of {Unlist c2 _ _ ->  0 <=> c2; _ -> EQ}
    (Unlist c1 _ _) <==> ys = case ys of {Unlist c2 _ _ -> c1 <=> c2; _ -> c1 <=> 0}

instance (Arbitrary e) => Arbitrary (Unlist e) where arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

instance Thaw (ST s) (Unlist e) (STUnlist s e)
  where
    thaw Z = return (STUNEmpty)
    thaw (Unlist n unl# unls) = liftA2 thaw' list (thaw unls)
      where
        thaw' = \ (STUnlist _ stunl# _) stunls -> STUnlist n stunl# stunls
        list  = newLinear [ unl# !# i# | (I# i#) <- [0 .. n - 1] ]

instance Freeze (ST s) (STUnlist s e) (Unlist e)
  where
    freeze = done

--------------------------------------------------------------------------------

{-# INLINE (!#) #-}
(!#) :: Array# e -> Int# -> e
arr# !# i# = case indexArray# arr# i# of (# e #) -> e

{-# INLINE fill #-}
fill :: MutableArray# s e -> (Int, e) -> STRep s a -> STRep s a
fill marr# (I# i#, e) nxt = \ s1# -> case writeArray# marr# i# e s1# of s2# -> nxt s2#

{-# INLINE done' #-}
done' :: Int -> Unlist e -> MutableArray# s e -> STRep s (Unlist e)
done' c rest marr# = \ s1# -> case unsafeFreezeArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, Unlist c arr# rest #)

{-# INLINE done #-}
done :: STUnlist s e -> ST s (Unlist e)
done        STUNEmpty        = return UNEmpty
done (STUnlist n marr# marr) = done marr >>= \ arr -> ST $
  \ s1# -> case unsafeFreezeArray# marr# s1# of
    (# s2#, arr# #) -> (# s2#, Unlist n arr# arr #)

pfailEx :: String -> a
pfailEx msg = throw . PatternMatchFail $ "in SDP.Unrolled.Unlist." ++ msg

unreachEx :: String -> a
unreachEx msg = throw . UnreachableException $ "in SDP.Unrolled.Unlist." ++ msg

lim :: Int
lim =  1024



