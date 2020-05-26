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
    -- | Set new bounds, may shrink.
    reshape :: (Index i, Index j) => s i e -> (j, j) -> s j e
    
    -- | @sliceOf es ij@ returns subshape @ij@ of @es@.
    sliceOf :: (SubIndex i j) => s i e -> i :|: j -> s j e




