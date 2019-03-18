{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}

module SDP.Indexed
(
  module SDP.Linear,
  
  Indexed (..),
  
  write,
  (>/>)
)
where

import SDP.SafePrelude
import Prelude ()

import Data.List ( findIndex, findIndices )

import SDP.Linear
import SDP.Set

import SDP.Simple

--------------------------------------------------------------------------------

{- Class of indexed data structures. -}

class (Linear v, Index i) => Indexed (v) i | v -> i
  where
    {-# MINIMAL assoc', (//), ((!)|(!?)), ((.$) | (*$)) #-}
    
    {- Create functions -}
    
    assoc           :: (i, i) -> [(i, e)] -> v e
    assoc bounds    =  assoc' bounds undefEx
      where
        undefEx = throw $ IndexOverflow "in SDP.Indexed.assoc"
    
    assoc'          :: (i, i) -> e -> [(i, e)] -> v e
    
    {- Read functions -}
    
    -- Use (.!) only if you are really sure that you will not go beyond the bounds.
    (.!)        :: v e -> i -> e
    dat .! ix   =  fromMaybe (bndEx "(.!)") $ dat !? ix
    
    -- (!) is pretty safe function, throws IndexException.
    (!)          :: v e -> i -> e
    (!) dat ix   =  fromMaybe (bndEx "(!)") $ dat !? ix
    
    default (!?) :: (Bordered v i) => v e -> i -> Maybe e
    
    -- (!?) is completely safe, but so boring function.
    (!?)         :: v e -> i -> Maybe e
    (!?) dat     =  (inRange $ bounds dat) ?: (dat !)
    
    {- Write functions -}
    
    -- Write elements to (immutable) structure.
    (//)         :: v e -> [(i, e)] -> v e
    
    -- Update function. Uses (!) and may throw IndexException.
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
    f *$ es = fsts . filter (f . snd) $ enum es (unsafeIndex 0)
      where
        enum     Z     _ = Z
        enum (e :> es) c = (c, e) :> (enum es $! succ c)

write        :: (Indexed v i) => v e -> i -> e -> v e
write es i e = es // [(i, e)]

(>/>)        :: (Indexed v i) => v e -> [i] -> (e -> e) -> v e
(>/>) es  is = (es /> is) . const

instance Indexed [] Int
  where
    assoc' bounds e = toResultList . normalAssocs
      where
        fill (ie1@(i1, _) : ie2@(i2, _) : xs) = ie1 : fill rest
          where
            rest = nx /= i2 ? (nx, e) : ie2 : xs $ ie2 : xs
            nx   = next bounds i1
        fill xs  = xs
        
        normalAssocs = fill . setWith cmpfst . filter (inRange bounds . fst)
        toResultList = fromListN (size bounds) . snds
    
    (x : xs) .! n = (n == 0) ? x $ xs .! (n - 1)
    
    (!) [] n = throw $ (n < 0 ? IndexUnderflow $ IndexOverflow) "in SDP.Indexed.(!) (List)"
    (x : xs) ! n = case n <=> 0 of
      GT -> xs .! (n - 1)
      EQ -> x
      LT -> throw $ IndexUnderflow "in SDP.Indexed.(!) (List)"
    
    [] !? _ = Nothing
    (x : xs) !? n = case n <=> 0 of
      GT -> xs !? (n - 1)
      EQ -> Just x
      LT -> Nothing
    
    -- [internal]: O (n ^ 2), obviously inefficient implementation. Rewrite after adding Set and Array to O(n * log n).
    xs // es = merge xs (clean es) 0
      where
        merge [] ys _ = snds $ clean ys
        merge xs [] _ = xs
        merge (x : xs) ((i, y) : ys) n = i == n ? y : merge xs ys (n + 1) $ x : merge xs ((i, y) : ys) (n + 1)
        --   not EQ and not LT (neither underflow nor duplications) => GT ^
        clean = sort' . nub' . filter (\ (i, _) -> i >= 0)
    
    (.$) = findIndex
    (*$) = findIndices

--------------------------------------------------------------------------------

bndEx s = throw . UndefinedValue $ "SDP.Indexed." ++ s

