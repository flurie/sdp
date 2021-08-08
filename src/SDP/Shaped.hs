{-# LANGUAGE Safe, MultiParamTypeClasses, TypeOperators #-}

{- |
    Module      :  SDP.Shaped
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Shaped" provides advanced operations on 'SDP.Indexed.Indexed'
    structures, generalized by 'Index' type.
-}
module SDP.Shaped
(
  -- * Shaped
  Shaped (..)
)
where

import SDP.Linear

default ()

--------------------------------------------------------------------------------

{- |
  Service class for structures with arbitrary bounds. Allows you to extract
  subsequences corresponding to index subranges.
-}
class Shaped s e
  where
    {-# MINIMAL (!!), slices, unslice #-}
    
    -- | @es !! ij@ returns subshape @ij@ of @es@.
    (!!) :: (SubIndex i j) => s i e -> i :|: j -> s j e
    
    -- | Returns list of @es@ subshapes.
    slices :: (SubIndex i j) => s i e -> [s j e]
    
    -- | Unslice subshapes.
    unslice :: (Foldable f, SubIndex i j) => f (s j e) -> s i e




