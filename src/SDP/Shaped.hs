{-# LANGUAGE MultiParamTypeClasses, TypeOperators #-}

{- |
    Module      :  SDP.Shaped
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    @SDP.Shaped@ provides advanced operations on 'Indexed' structures,
    generalized by 'Index' type.
-}
module SDP.Shaped
(
  -- * Shaped
  Shaped (..)
)
where

import SDP.Index

default ()

--------------------------------------------------------------------------------

{- |
  Service class for structures with arbitrary bounds. Allows you to extract
  subsequences corresponding to index subranges.
-}
class Shaped s e
  where
    {-# MINIMAL reshape, (!!), slicesOf #-}
    
    -- | Set new bounds of same type, may shrink.
    rebound :: (Index i) => s i e -> (i, i) -> s i e
    rebound =  reshape
    
    -- | Set new bounds, may shrink.
    reshape :: (Index i, Index j) => s i e -> (j, j) -> s j e
    
    -- | @es !! ij@ returns subshape @ij@ of @es@.
    (!!) :: (SubIndex i j) => s i e -> i :|: j -> s j e
    
    -- | @slicesOf es@ returns list of @es@ subshapes.
    slicesOf :: (SubIndex i j) => s i e -> [s j e]





