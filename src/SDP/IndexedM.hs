{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DefaultSignatures #-}

{- |
    Module      :  SDP.IndexedM
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC-extensions)
    Stability   :  stable
  
  IndexedM is service class of SDP, designed to read and write mutable indexable
  data structures.
-}

module SDP.IndexedM
  (
    IndexedM (..)
  )
where

import Prelude ( (++) )
import SDP.SafePrelude

import SDP.LinearM

import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | Class for work with mutable indexed structures.
class (Monad m, Index i) => IndexedM m v i e | v -> m, v -> i, v -> e
  where
    {-# MINIMAL fromAssoc', overwrite, (!?>), (*?) #-}
    
    {-# INLINE fromAssoc #-}
    -- | fromAssocs returns new mutable structure created from assocs.
    fromAssoc :: (i, i) -> [(i, e)] -> m v
    fromAssoc bnds ascs =  fromAssoc' bnds (undEx "fromAssoc") ascs
    
    -- | fromAssocs' return new mutable structure created from assocs and default element
    fromAssoc' :: (i, i) -> e -> [(i, e)] -> m v
    
    -- | (>!) is unsafe monadic reader.
    {-# INLINE (>!) #-}
    (>!)     :: v -> i -> m e
    es >! i =  es !> i
    
    -- | (!>) is well-safe monadic reader.
    {-# INLINE (!>) #-}
    (!>)  :: v -> i -> m e
    (!>) dat i = fromMaybe (undEx "(!)") <$> dat !?> i
    
    -- | (!?>) is completely safe monadic reader.
    (!?>) :: v -> i -> m (Maybe e)
    
    default (!?>) :: (BorderedM m v i e) => v -> i -> m (Maybe e)
    es !?> i = getIndexOf es ?> (es !>) $ i
    
    -- | overwrite rewrites mutable structure using assocs.
    overwrite :: v -> [(i, e)] -> m ()
    
    -- | updateM updates elements with specified indices by function.
    updateM :: v -> [i] -> (i -> e -> e) -> m ()
    updateM es is f = sequence [ do e <- es !> i; return (i, f i e) | i <- is ] >>= overwrite es
    
    -- | (.?) is monadic version of (.$).
    (.?) :: (e -> Bool) -> v -> m (Maybe i)
    f .? es = listToMaybe <$> f *? es
    
    -- | (*?) is monadic version of (*$).
    (*?) :: (e -> Bool) -> v -> m [i]

--------------------------------------------------------------------------------

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.IndexedM" ++ msg

