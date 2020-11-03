{- |
    Module      :  SDP.Zip
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Zip" provides 'Zip' - class of 'Control.Applicative.ZipList"-like
    structures.
-}
module SDP.Zip ( Zip (..) ) where

import Prelude ( Functor, ($) )

default ()

--------------------------------------------------------------------------------

-- | Zip is generalization of ZipList applicative semantics (but without pure).
class (Functor z) => Zip z
  where
    {-# MINIMAL zipWith #-}
    
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
    
    zipWith3 f as bs          = zipWith ($) $ zipWith  f as bs
    zipWith4 f as bs cs       = zipWith ($) $ zipWith3 f as bs cs
    zipWith5 f as bs cs ds    = zipWith ($) $ zipWith4 f as bs cs ds
    zipWith6 f as bs cs ds es = zipWith ($) $ zipWith5 f as bs cs ds es

--------------------------------------------------------------------------------

instance Zip []
  where
    zipWith f (a : as) (b : bs) = f a b : zipWith f as bs
    zipWith _ _ _ = []
    
    zipWith3 f (a : as) (b : bs) (c : cs) = f a b c : zipWith3 f as bs cs
    zipWith3 _ _ _ _ = []
    
    zipWith4 f (a : as) (b : bs) (c : cs) (d : ds) = f a b c d : zipWith4 f as bs cs ds
    zipWith4 _ _ _ _ _ = []
    
    zipWith5 f (a : as) (b : bs) (c : cs) (d : ds) (e : es) = f a b c d e : zipWith5 f as bs cs ds es
    zipWith5 _ _ _ _ _ _ = []
    
    zipWith6 f (a : as) (b : bs) (c : cs) (d : ds) (e : es) (g : gs) = f a b c d e g : zipWith6 f as bs cs ds es gs
    zipWith6 _ _ _ _ _ _ _ = []

