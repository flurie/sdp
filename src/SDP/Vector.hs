{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures, BangPatterns #-}

module SDP.Vector
(
  module SDP.Linear,
  module SDP.Index
)
where

import SDP.SafePrelude
import Prelude ()
import Data.List ( findIndex, findIndices )

import SDP.Simple
import SDP.Linear
import SDP.Index

--------------------------------------------------------------------------------

{- Class of one-indexed data structures. -}

class (Linear v, Index i, Enum i) => Vector (v) i | v -> i
  where
    {-# MINIMAL assoc', (//), ((!)|(!?)), ((.$) | (*$)) #-}
    
    {- Create functions -}
    
    assoc           :: (i, i) -> [(i, e)] -> v e
    assoc bounds    =  assoc' bounds undefEx
    
    assoc'          :: (i, i) -> e -> [(i, e)] -> v e
    
    {- Read functions -}
    
    -- Use (.!) only if you are sure that you will not go beyond the bounds.
    (.!)        :: v e -> i -> e
    dat .! ix   =  fromMaybe (bndEx "(.!)") $ dat !? ix
    
    -- (!) is pretty safe function that is slightly slower than (.!)
    (!)          :: v e -> i -> e
    (!) dat ix   =  fromMaybe (bndEx "(!)") $ dat !? ix
    
    default (!?) :: (Bordered v i) => v e -> i -> Maybe e
    
    -- (!?) is completely safe function
    (!?)         :: v e -> i -> Maybe e
    (!?) dat     =  (inRange $ bounds dat) ?: (dat !)
    
    {- Write functions -}
    
    (//)         :: v e -> [(i, e)] -> v e
    
    -- NOTE: (/>) uses (!) and may throw IndexException.
    (/>)         :: v e -> [i] -> (i -> e -> e) -> v e
    (/>) es is f = es // (assocOf <$> is)
      where
        assocOf i = (i, f i (es ! i))
    
    {- search functions -}
    
    -- searches the index of first matching element
    (.$) :: (e -> Bool) -> v e -> Maybe i
    (.$) f = (null ?: head) . (f *$)
    
    default (*$) :: (Enum i) => (e -> Bool) -> v e -> v i
    
    -- searches the indices of all matching elements
    (*$) :: (e -> Bool) -> v e -> v i
    (*$) f = fsts . filter (f . snd) . enumerate

write        :: (Vector v i) => v e -> i -> e -> v e
write es i e = es // [(i, e)]

(>/>)        :: (Vector v i) => v e -> [i] -> (e -> e) -> v e
(>/>) es  is = (es /> is) . const

instance Vector [] Int
  where
    assoc  = undefined
    assoc' = undefined
    
    (x : xs) .! (!n) = (n == 0) ? x $ xs .! (n - 1)
    
    (!) [] n = throw $ (n < 0 ? IndexUnderflow $ IndexOverflow) "in SDP.Vector.(!) (List)"
    (!) (x : xs) !n = case n <=> 0 of
      GT -> xs ! (n - 1)
      EQ -> x
      LT -> throw $ IndexUnderflow "in SDP.Vector.(!) (List)"
    
    [] !? _ = Nothing
    (x : xs) !? n = case n <=> 0 of
      GT -> xs !? (n - 1)
      EQ -> Just x
      LT -> Nothing
    
    -- [internal]: O (n ^ 2), obviously inefficient implementation. Rewrite after adding Set and Array to O(n * log n).
    xs // es = merge xs (brush es) 0
      where
        merge [] ys _ = snds $ brush ys
        merge xs [] _ = xs
        merge (x : xs) ((i, y) : ys) n = i == n ? y : merge xs ys (n + 1) $ x : merge xs ((i, y) : ys) (n + 1)
        --   not EQ and not LT (neither underflow nor duplications) => GT ^
        brush = sort' . nub' . filter (\ (i, _) -> i >= 0)
    
    (.$) = findIndex
    
    (*$) = findIndices

--------------------------------------------------------------------------------

class (Linear b, Index i) => Bordered (b) i | b -> i
  where
    {-# MINIMAL bounds|(lower, upper) #-}
    
    bounds    :: b e ->  (i, i)
    assocs    :: b e -> [(i, e)]
    indices   :: b e -> [i]
    lower     :: b e -> i
    upper     :: b e -> i
    
    bounds es =  (lower es, upper es)
    assocs xs =  zip (indices xs) (toList xs)
    indices   =  range . bounds
    lower     =   fst  . bounds
    upper     =   snd  . bounds

--------------------------------------------------------------------------------

instance Bordered [] Int
  where
    bounds  es = (0,   length es - 1)
    assocs  es = zip  [0 .. ] es
    indices es = [0 .. length es - 1]
    lower   es = 0
    upper   es = length es - 1

--------------------------------------------------------------------------------

-- Shortcut for IndexException
bndEx s = throw . UndefinedValue $ "SDP.Vector." ++ s

-- Exception for assoc default element definition
undefEx = throw $ IndexOverflow "in SDP.Vector.assoc"

enumerate :: (Linear z, Index i, Enum i) => z e -> z (i, e)
enumerate = enum zero
  where
    zero = toEnum 0
    enum  _     Z     = Z
    enum !c (e :> es) = (c, e) :> enum (succ c) es

