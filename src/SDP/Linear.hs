{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
-- {-# LANGUAGE CPP #-} -- for future use

module SDP.Linear
(
  module SDP.Index,
  module SDP.Zip,
  
  Bordered (..),
  Linear (..),
  
  pattern (:>), pattern (:<), pattern  Z,
  
  tails, inits, nub
)

where

--------------------------------------------------------------------------------

import Prelude ()
import qualified Data.List as L hiding ( concat )

import SDP.SafePrelude
import SDP.Simple
import SDP.Index
import SDP.Zip

--------------------------------------------------------------------------------

infixr 5 ++, :>
infixl 5 :<

--------------------------------------------------------------------------------

{-
  Class of linear data structures that can be created from list.
  
  This is a preliminary version of the Linear class.
  Later several functions will be transferred and renamed.
-}

class (Traversable l) => Linear l
  where
    {-# MINIMAL (fromList|fromListN), (head, tail|uncons), (init, last|unsnoc) #-}
    
    {- Service functions -}
    
    -- Empty line. Service constant, synonym for Z.
    lzero :: l e
    lzero =  fromList []
    
    -- Separates head and tail. Service function, synonym for (x :> xs).
    uncons      :: l e -> (e, l e)
    uncons xs   =  (head xs, tail xs)
    
    -- Adds element to head of line. Service function, synonym for (x :> xs).
    toHead      :: e -> l e -> l e
    toHead e es =  single e ++ es
    
    head :: l e -> e
    head =  fst . uncons
    
    tail :: l e -> l e
    tail =  snd . uncons
    
    -- Separates init and last. Service function, synonym for (xs :< x).
    unsnoc      :: l e -> (l e, e)
    unsnoc xs   =  (init xs, last xs)
    
    -- Adds element to end of line. Service function, synonym for (xs :< x).
    toLast      :: l e ->   e -> l e
    toLast es e =  es ++ single e
    
    init :: l e -> l e
    init =  fst . unsnoc
    
    last :: l e -> e
    last =  snd . unsnoc
    
    {- Construction -}
    
    -- Singleton.
    single      :: e -> l e
    single x    =  fromList [x]
    
    -- Line of n equal elements.
    replicate   :: Int -> e -> l e
    replicate n =  fromListN n . replicate n
    
    -- Line of list elements.
    fromList  :: [e] -> l e
    fromList es = fromListN (length es) es
    
    {-
      Makes it safer to work with endless lists.
      May doesn't evaluate list twice times (default, does).
    -}
    fromListN   :: Int -> [e] -> l e
    fromListN n =  fromList . take n
    
    -- Generalisation of fromList.
    fromFoldable  :: (Foldable f) => f e -> l e
    fromFoldable  =  fromList . toList
    
    -- Generalisation of concatenation.
    (++)        :: l e -> l e -> l e
    xs ++ ys    =  fromList $ (toList xs) ++ (toList ys)
    
    concat      :: (Foldable f) => f (l e) -> l e
    concat      =  foldr (++) lzero
    
    {- Deconstruction -}
    
    take      :: Int -> l e -> l e
    take n es =  fromList . (take n) $ toList es
    
    drop      :: Int -> l e -> l e
    drop n es =  fromList . (drop n) $ toList es
    
    -- takes and drops the longest init
    takeWhile    :: (e -> Bool) -> l e -> l e
    takeWhile p (e :> es) = p e ? (e :> takeWhile p es) $ Z
    takeWhile _     _     = Z
    
    dropWhile    :: (e -> Bool) -> l e -> l e
    dropWhile p es@(e :> tail) = p e ? (dropWhile p tail) $ es
    dropWhile _        _       = Z
    
    -- takes and drops the longest tail
    takeEnd      :: (e -> Bool) -> l e -> l e
    takeEnd p list = t' list list
      where
        t'     Z     tail = tail
        t' (e :> es) tail = t' es (p e ? tail $ es)
    
    dropEnd      :: (e -> Bool) -> l e -> l e
    dropEnd p = foldr (\ e es -> (p e && null es) ? Z $ (e :> es)) Z
    
    span         :: (e -> Bool) -> l e -> (l e, l e)
    span   f  es =  (takeWhile f es, dropWhile f es)
    
    break        :: (e -> Bool) -> l e -> (l e, l e)
    break  f  es =  (takeWhile (not . f) es, dropWhile (not . f) es)
    
    {- Filtering functions -}
    
    filter         :: (e -> Bool) -> l e -> l e
    filter p       =  fromList . filter p . toList
    
    partition      :: (e -> Bool) -> l e -> (l e, l e)
    partition p es = (filter p es, filter (not . p) es)
    
    {- Comparing lines. -}
    
    -- Compares lines as sets.
    isSubseqOf    :: (Eq e) => l e -> l e -> Bool
    isSubseqOf Z _ = True
    isSubseqOf _ Z = False
    isSubseqOf xs@(x :> tail) (y :> ys) = x == y && tail `isSubseqOf` ys || xs `isSubseqOf` ys
    
    isPrefixOf    :: (Eq e) => l e -> l e -> Bool
    isPrefixOf Z _ = True
    isPrefixOf _ Z = False
    isPrefixOf (x :> xs) (y :> ys) = (x == y) && (xs `isPrefixOf` ys)
    
    isSuffixOf    :: (Eq e) => l e -> l e -> Bool
    isSuffixOf Z _ = True
    isSuffixOf _ Z = False
    isSuffixOf (xs :< x) (ys :< y) = (x == y) && (xs `isSuffixOf` ys)
    
    isInfixOf     :: (Eq e) => l e -> l e -> Bool
    isInfixOf Z _   = True
    isInfixOf _ Z   = False
    isInfixOf xs ys = xs `isPrefixOf` ys || xs `isInfixOf` (tail ys)
    
    {- Special functions -}
    
    reverse       :: l e -> l e
    reverse       =  fromList . reverse . toList
    
    intersperse   :: e -> l e -> l e
    intersperse e =  fromList . intersperse e . toList
    
    concatMap     :: (Foldable f) => (e -> l r) -> f e -> l r
    concatMap   f =  foldr' (\ x y -> f x ++ y) Z
    
    subsequences  :: (Linear l) => l e -> [l e]
    subsequences xs =  Z : subsequences' xs
      where
        subsequences'     Z     = Z
        subsequences' (x :> xs) = single x : foldr f [] (subsequences' xs)
          where
            f ys rest = ys : (x :> ys) : rest
    
    listR         :: l e -> [e]
    listR         =  reverse . toList
    
    nubBy         :: (e -> e -> Bool) -> l e -> l e
    nubBy       f = fromList . nubBy f . toList

--------------------------------------------------------------------------------

class (Linear b, Index i) => Bordered (b) i | b -> i
  where
    {-# MINIMAL bounds|(lower, upper) #-}
    
    bounds    :: b e ->  (i, i)
    bounds es =  (lower es, upper es)
    assocs    :: b e -> [(i, e)]
    assocs xs =  zip (indices xs) (toList xs)
    indices   :: b e -> [i]
    indices   =  range . bounds
    lower     :: b e -> i
    lower     =  fst  . bounds
    upper     :: b e -> i
    upper     =  snd  . bounds

--------------------------------------------------------------------------------

instance Bordered [] Int
  where
    bounds  es = (0,   length es - 1)
    assocs  es = zip  [0 .. ] es
    indices es = [0 .. length es - 1]
    lower   es = 0
    upper   es = length es - 1

--------------------------------------------------------------------------------

pattern   Z   :: (Linear l) => l e
pattern Z <- (null -> True)                           where  Z   = lzero

pattern  (:>) :: (Linear l) =>   e -> l e -> l e
pattern x :> xs <- ((null ?: uncons) -> Just (x, xs)) where (:>) = toHead

pattern  (:<) :: (Linear l) => l e ->   e -> l e
pattern xs :< x <- ((null ?: unsnoc) -> Just (xs, x)) where (:<) = toLast

--------------------------------------------------------------------------------

instance Linear []
  where
    fromList    = id
    fromListN n = take n
    lzero       = [ ]
    single x    = [x]
    
    (++)        = (L.++)
    toHead      = (:)
    toLast xs x = foldr' (:) [x] xs
    
    uncons    Z       = throw $ PatternMatchFail "in SDP.Linear.(:>)"
    uncons (e : es)   = (e, es)
    
    unsnoc     Z      = throw $ PatternMatchFail "in SDP.Linear.(:<)"
    unsnoc    [e]     = ([], e)
    unsnoc  (e : es)  = (e : es', e') where (es', e') = unsnoc es
    
    take = L.take
    drop = L.drop
    
    reverse      = L.reverse
    replicate    = L.replicate
    intersperse  = L.intersperse
    
    filter       = L.filter
    partition    = L.partition
    concatMap    = L.concatMap
    fromFoldable = toList
    
    isSubseqOf   = L.isSubsequenceOf
    isPrefixOf   = L.isPrefixOf
    isSuffixOf   = L.isSuffixOf
    isInfixOf    = L.isInfixOf
    
    span         = L.span
    break        = L.break
    listR        = L.reverse
    
    takeWhile    = L.takeWhile
    dropWhile    = L.dropWhile
    
    dropEnd p    = L.foldr (\ x xs -> (p x && null xs) ? [] $ (x : xs)) []
    nubBy        = L.nubBy

--------------------------------------------------------------------------------

nub   :: (Linear l, Eq e) => l e -> l e
nub   =  nubBy (==)

tails :: (Linear l) => l e -> [l e]
tails      Z        = Z
tails all@(_ :> es) = all : tails es

inits :: (Linear l) => l e -> [l e]
inits      Z        = Z
inits all@(es :< _) = all : inits es
