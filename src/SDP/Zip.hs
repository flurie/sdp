module SDP.Zip where

import Prelude ()
import SDP.SafePrelude

--------------------------------------------------------------------------------

{-
  Zip is typeclass with vector semantics (like ZipList).
  zipWith{3 .. 6} may be can be expressed in zipWith{ , 3 .. 5},
  but in many cases it would be extremely memory inefficient.
-}

class (Functor z) => Zip z
  where
    {-# MINIMAL zipWith, zipWith3, zipWith4, zipWith5, zipWith6 #-}
    
    zip  :: z a -> z b                             -> z (a, b)
    zip3 :: z a -> z b -> z c                      -> z (a, b, c)
    zip4 :: z a -> z b -> z c -> z d               -> z (a, b, c, d)
    zip5 :: z a -> z b -> z c -> z d -> z e        -> z (a, b, c, d, e)
    zip6 :: z a -> z b -> z c -> z d -> z e -> z f -> z (a, b, c, d, e, f)
    
    zip  = zipWith  (,)
    zip3 = zipWith3 (,,)
    zip4 = zipWith4 (,,,)
    zip5 = zipWith5 (,,,,)
    zip6 = zipWith6 (,,,,,)
    
    zipWith  :: (a -> b -> c)                     -> z a -> z b -> z c
    zipWith3 :: (a -> b -> c -> d)                -> z a -> z b -> z c -> z d
    zipWith4 :: (a -> b -> c -> d -> e)           -> z a -> z b -> z c -> z d -> z e
    zipWith5 :: (a -> b -> c -> d -> e -> f)      -> z a -> z b -> z c -> z d -> z e -> z f
    zipWith6 :: (a -> b -> c -> d -> e -> f -> g) -> z a -> z b -> z c -> z d -> z e -> z f -> z g

--------------------------------------------------------------------------------

instance Zip []
  where
    zip (a : as) (b : bs) = (a, b) : zip as bs
    zip _ _ = []
    
    zip3 (a : as) (b : bs) (c : cs) = (a, b, c) : zip3 as bs cs
    zip3 _ _ _ = []
    
    zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
    zip4 _ _ _ _ = []
    
    zip5 (a : as) (b : bs) (c :cs) (d : ds) (e : es) = (a, b, c, d, e) : zip5 as bs cs ds es
    zip5 _ _ _ _ _ = []
    
    zipWith  f' = go
      where
        go (a : as) (b : bs) = f' a b : go as bs
        go _ _ = []
    
    zipWith3 f' = go
      where
        go (a : as) (b : bs) (c : cs) = f' a b c : go as bs cs
        go _ _ _ = []
    
    zipWith4 f' = go
      where
        go (a : as) (b : bs) (c : cs) (d : ds) = f' a b c d : go as bs cs ds
        go _ _ _ _ = []
    
    zipWith5 f' = go
      where
        go (a : as) (b : bs) (c : cs) (d : ds) (e : es) = f' a b c d e : go as bs cs ds es
        go _ _ _ _ _ = []
    
    zipWith6 f' = go
      where
        go (a : as) (b : bs) (c : cs) (d : ds) (e : es) (f : fs) = f' a b c d e f : go as bs cs ds es fs
        go _ _ _ _ _ _ = []

--------------------------------------------------------------------------------
