{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.Zip
    Copyright   :  (c) Andrey Mulik 2019-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "SDP.Zip" provides 'Zip' - class of "Control.Applicative.ZipList"-like
    structures.
-}
module SDP.Zip ( Zip (..) ) where

import Prelude ()
import SDP.SafePrelude

default ()

--------------------------------------------------------------------------------

-- | Zip is generalization of ZipList applicative semantics (but without 'pure').
class (Functor z) => Zip z
  where
    {-# MINIMAL (zap | zipWith), all2, any2 #-}
    
    -- | ZipList-like @('<*>')@.
    zap :: z (a -> b) -> z a -> z b
    zap =  zipWith ($)
    
    all2 :: (a -> b -> Bool)                     -> z a -> z b -> Bool
    all3 :: (a -> b -> c -> Bool)                -> z a -> z b -> z c -> Bool
    all4 :: (a -> b -> c -> d -> Bool)           -> z a -> z b -> z c -> z d -> Bool
    all5 :: (a -> b -> c -> d -> e -> Bool)      -> z a -> z b -> z c -> z d -> z e -> Bool
    all6 :: (a -> b -> c -> d -> e -> f -> Bool) -> z a -> z b -> z c -> z d -> z e -> z f -> Bool
    
    all3 f          = all2 ($) ... zipWith f
    all4 f as       = all2 ($) ... zipWith3 f as
    all5 f as bs    = all2 ($) ... zipWith4 f as bs
    all6 f as bs cs = all2 ($) ... zipWith5 f as bs cs
    
    any2 :: (a -> b -> Bool)                     -> z a -> z b -> Bool
    any3 :: (a -> b -> c -> Bool)                -> z a -> z b -> z c -> Bool
    any4 :: (a -> b -> c -> d -> Bool)           -> z a -> z b -> z c -> z d -> Bool
    any5 :: (a -> b -> c -> d -> e -> Bool)      -> z a -> z b -> z c -> z d -> z e -> Bool
    any6 :: (a -> b -> c -> d -> e -> f -> Bool) -> z a -> z b -> z c -> z d -> z e -> z f -> Bool
    
    any3 f          = any2 ($) ... zipWith f
    any4 f as       = any2 ($) ... zipWith3 f as
    any5 f as bs    = any2 ($) ... zipWith4 f as bs
    any6 f as bs cs = any2 ($) ... zipWith5 f as bs cs
    
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
    
    zipWith             = zap ... fmap
    zipWith3 f          = zap ... zipWith  f
    zipWith4 f as       = zap ... zipWith3 f as
    zipWith5 f as bs    = zap ... zipWith4 f as bs
    zipWith6 f as bs cs = zap ... zipWith5 f as bs cs

--------------------------------------------------------------------------------

instance Zip []
  where
    zap (f : fs) (x : xs) = f x : zap fs xs
    zap    _        _     = []
    
    all2 f (a:as) (b:bs) = f a b && all2 f as bs
    all2 _ _ _ = True
    
    all3 f (a:as) (b:bs) (c:cs) = f a b c && all3 f as bs cs
    all3 _ _ _ _ = True
    
    all4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d && all4 f as bs cs ds
    all4 _ _ _ _ _ = True
    
    all5 f (a:as) (b:bs) (c:cs) (d:ds) (e:es) = f a b c d e && all5 f as bs cs ds es
    all5 _ _ _ _ _ _ = True
    
    all6 f (a:as) (b:bs) (c:cs) (d:ds) (e:es) (g:gs) = f a b c d e g && all6 f as bs cs ds es gs
    all6 _ _ _ _ _ _ _ = True
    
    any2 f (a:as) (b:bs) = f a b || any2 f as bs
    any2 _ _ _ = False
    
    any3 f (a:as) (b:bs) (c:cs) = f a b c || any3 f as bs cs
    any3 _ _ _ _ = False
    
    any4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d || any4 f as bs cs ds
    any4 _ _ _ _ _ = False
    
    any5 f (a:as) (b:bs) (c:cs) (d:ds) (e:es) = f a b c d e || any5 f as bs cs ds es
    any5 _ _ _ _ _ _ = False
    
    any6 f (a:as) (b:bs) (c:cs) (d:ds) (e:es) (g:gs) = f a b c d e g || any6 f as bs cs ds es gs
    any6 _ _ _ _ _ _ _ = False
    
    zipWith f (a:as) (b:bs) = f a b:zipWith f as bs
    zipWith _ _ _ = []
    
    zipWith3 f (a:as) (b:bs) (c:cs) = f a b c:zipWith3 f as bs cs
    zipWith3 _ _ _ _ = []
    
    zipWith4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d:zipWith4 f as bs cs ds
    zipWith4 _ _ _ _ _ = []
    
    zipWith5 f (a:as) (b:bs) (c:cs) (d:ds) (e:es) = f a b c d e:zipWith5 f as bs cs ds es
    zipWith5 _ _ _ _ _ _ = []
    
    zipWith6 f (a:as) (b:bs) (c:cs) (d:ds) (e:es) (g:gs) = f a b c d e g:zipWith6 f as bs cs ds es gs
    zipWith6 _ _ _ _ _ _ _ = []



