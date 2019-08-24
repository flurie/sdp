{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DefaultSignatures #-}

{- |
    Module      :  SDP.IndexedM
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC-extensions)
  
  IndexedM is service class of SDP, designed to read and write mutable indexable
  data structures.
-}
module SDP.IndexedM
  (
    module SDP.LinearM,
    module SDP.Indexed,
    
    IndexedM (..),
    Freeze   (..),
    Thaw     (..)
  )
where

import Prelude ()
import SDP.SafePrelude

import SDP.LinearM
import SDP.Indexed

import SDP.Simple

infixl 5 !#>

default ()

--------------------------------------------------------------------------------

-- | Class for work with mutable indexed structures.
class (Monad m, Index i) => IndexedM m v i e | v -> m, v -> i, v -> e
  where
    {-# MINIMAL fromAssocs', fromIndexed', fromIndexedM, overwrite, ((!>)|(!?>)) #-}
    
    {-# INLINE fromAssocs #-}
    -- | fromAssocs returns new mutable structure created from assocs.
    fromAssocs :: (i, i) -> [(i, e)] -> m v
    fromAssocs bnds ascs =  fromAssocs' bnds (undEx "fromAssocs") ascs
    
    -- | fromAssocs' return new mutable structure created from assocs and default element
    fromAssocs' :: (i, i) -> e -> [(i, e)] -> m v
    
    -- | (!#>) is unsafe monadic offset-based reader.
    {-# INLINE (!#>) #-}
    default (!#>) :: (BorderedM m v i e) => v -> Int -> m e
    (!#>) :: v -> Int -> m e
    es !#> i = do bnds <- getBounds es; es >! index bnds i
    
    -- | (>!) is unsafe monadic reader.
    {-# INLINE (>!) #-}
    (>!) :: v -> i -> m e
    es >! i = es !> i
    
    -- | (!>) is well-safe monadic reader.
    {-# INLINE (!>) #-}
    (!>) :: v -> i -> m e
    (!>) dat i = fromMaybe (undEx "(!)") <$> dat !?> i
    
    -- | (!?>) is completely safe monadic reader.
    default (!?>) :: (BorderedM m v i e) => v -> i -> m (Maybe e)
    (!?>) :: v -> i -> m (Maybe e)
    es !?> i = getIndexOf es ?> (es !>) $ i
    
    default writeM_ :: (BorderedM m v i e) => v -> Int -> e -> m ()
    writeM_ :: v -> Int -> e -> m ()
    writeM_ es i e = do bnds <- getBounds es; void $ overwrite es [(index bnds i, e)]
    
    default writeM :: (BorderedM m v i e) => v -> i -> e -> m ()
    writeM :: v -> i -> e -> m ()
    writeM es i e = do b <- getIndexOf es i; when b . void $ overwrite es [(i, e)]
    
    -- | fromIndexed' is overloaded version of thaw.
    fromIndexed' :: (Bordered v' j e, Indexed v' j e) => v' -> m v
    
    -- | fromIndexed converts one mutable structure to other.
    fromIndexedM :: (BorderedM m v' j e, IndexedM m v' j e) => v' -> m v
    
    -- | overwrite rewrites mutable structure using assocs.
    overwrite :: v -> [(i, e)] -> m v
    overwrite es ies = mapM_ (uncurry $ writeM es) ies >> return es
    
    -- | updateM updates elements with specified indices by function.
    updateM :: v -> [i] -> (i -> e -> e) -> m v
    updateM es is f = forM is (\ i -> do e <- es !> i; return (i, f i e)) >>= overwrite es
    
    -- | (.?) is monadic version of (.$).
    (.?) :: (e -> Bool) -> v -> m (Maybe i)
    f .? es = listToMaybe <$> f *? es
    
    -- | (*?) is monadic version of (*$).
    default (*?) :: (BorderedM m v i e) => (e -> Bool) -> v -> m [i]
    (*?) :: (e -> Bool) -> v -> m [i]
    p *? marr = fsts . filter (p . snd) <$> getAssocs marr

--------------------------------------------------------------------------------

-- | Service class of immutable-mutable converters.
class (Monad m) => Thaw m v v' | v' -> m
  where
    -- | thaw converts an immutable structure to a mutable.
    thaw :: v -> m v'

--------------------------------------------------------------------------------

-- | Service class of mutable-immutable converters.
class (Monad m) => Freeze m v' v | v' -> m
  where
    -- | freeze converts a mutable structure to a immutable.
    freeze :: v' -> m v

--------------------------------------------------------------------------------

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.IndexedM" ++ msg



