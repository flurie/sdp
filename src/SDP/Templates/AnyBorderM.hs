{-# LANGUAGE TypeFamilies, DeriveDataTypeable, DeriveGeneric, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
    Module      :  SDP.Templates.AnyBorder
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Plate.AnyBorder" provides 'AnyBorder' - template of generalized by
    index type structure, based on 'Int'-indexed primitive.
-}
module SDP.Templates.AnyBorderM
(
  -- * Export
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * BorderM template
  AnyBorderM (..)
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.SortM

import GHC.Generics ( Generic )

import Data.Typeable
import Data.Property
import Data.Coerce
import Data.Field

default ()

--------------------------------------------------------------------------------

{- |
  'AnyBorderM' is template, that makes structure's bounds mutable.
  
  'AnyBorderM' doesn't store any additional fields and doesn't preserve the
  original behavior of the structure, except that you no longer need to worry
  about the correctness of references to the structure when changing its size
  ('prepend', 'append', etc) and some black-box operations (e.g., 'overwrite').
-}
newtype AnyBorderM m rep = AnyBorderM (Var m rep)
  deriving ( Typeable, Generic )

--------------------------------------------------------------------------------

{- Eq instance. -}

instance (MonadVar m, Eq (Var m rep)) => Eq (AnyBorderM m rep)
  where
    (==) = on (==) unpack

--------------------------------------------------------------------------------

{- NullableM instance. -}

instance (MonadVar m, NullableM m rep) => NullableM m (AnyBorderM m rep)
  where
    newNull = packM   =<< newNull
    nowNull = nowNull <=< unpackM

--------------------------------------------------------------------------------

{- BorderedM, LinearM and SplitM instances. -}

instance (MonadVar m, Index i, BorderedM m rep i) => BorderedM m (AnyBorderM m rep) i
  where
    nowIndexIn  es i = flip nowIndexIn  i =<< unpackM es
    getIndexOf  es i = flip getIndexOf  i =<< unpackM es
    getOffsetOf es i = flip getOffsetOf i =<< unpackM es
    
    getIndices = getIndices <=< unpackM
    getSizesOf = getSizesOf <=< unpackM
    getBounds  = getBounds  <=< unpackM
    getSizeOf  = getSizeOf  <=< unpackM
    getLower   = getLower   <=< unpackM
    getUpper   = getUpper   <=< unpackM

instance (MonadVar m, LinearM m rep e) => LinearM m (AnyBorderM m rep) e
  where
    singleM =  packM  <=< singleM
    getHead = getHead <=< unpackM
    getLast = getLast <=< unpackM
    
    -- Returns the same structure.
    prepend e es@(AnyBorderM rep) = es <$ (setField this rep =<<  prepend  e  =<< unpackM es)
    
    -- Returns the same structure.
    append  es@(AnyBorderM rep) e = es <$ (setField this rep =<< (`append` e) =<< unpackM es)
    
    fromFoldableM = packM <=<  fromFoldableM
    newLinearN    = packM <=<< newLinearN
    newLinear     = packM <=<  newLinear
    
    getRight = getRight <=< unpackM
    getLeft  = getLeft  <=< unpackM
    es !#> i = (!#> i) =<< unpackM es
    
    copied' es l n = do rep <- unpackM es; packM =<< copied' rep l n
    writeM  es i e = do rep <- unpackM es; writeM rep i e
    
    reversed  = packM <=< reversed <=< unpackM
    reversed' = reversed' <=< unpackM
    
    merged = packM <=<  merged <=< foldr (liftA2 (:) . unpackM) (return [])
    copied = packM <=< copied <=< unpackM
    filled = packM <=<< filled
    
    ofoldrM f base = ofoldrM f base <=< unpackM
    ofoldlM f base = ofoldlM f base <=< unpackM
    foldrM  f base = foldrM  f base <=< unpackM
    foldlM  f base = foldlM  f base <=< unpackM
    
    foldrM1 f = foldrM1 f <=< unpackM
    foldlM1 f = foldlM1 f <=< unpackM
    
    swapM es i j = do rep <- unpackM es; swapM rep i j
    
    copyTo src so trg to n = do
      src' <- unpackM src
      trg' <- unpackM trg
      copyTo src' so trg' to n

instance (MonadVar m, SplitM m rep e) => SplitM m (AnyBorderM m rep) e
  where
    takeM n = packM <=< takeM n <=< unpackM
    dropM n = packM <=< dropM n <=< unpackM
    keepM n = packM <=< keepM n <=< unpackM
    sansM n = packM <=< sansM n <=< unpackM
    
    eachM   n = packM       <=< eachM   n <=< unpackM
    splitM  n = bothA packM <=< splitM  n <=< unpackM
    divideM n = bothA packM <=< divideM n <=< unpackM
    
    partsM   ns = mapM packM <=< partsM   ns <=< unpackM
    chunksM  ns = mapM packM <=< chunksM  ns <=< unpackM
    splitsM  ns = mapM packM <=< splitsM  ns <=< unpackM
    dividesM ns = mapM packM <=< dividesM ns <=< unpackM
    
    prefixM f = prefixM f <=< unpackM
    suffixM f = suffixM f <=< unpackM
    mprefix f = mprefix f <=< unpackM
    msuffix f = msuffix f <=< unpackM

--------------------------------------------------------------------------------

{- MapM and IndexedM instances. -}

instance (MonadVar m, MapM m rep key e) => MapM m (AnyBorderM m rep) key e
  where
    getAssocs = getAssocs <=< unpackM
    getKeys   = getKeys   <=< unpackM
    
    newMap' = packM <=<< newMap'
    newMap  = packM <=<  newMap
    
    es !?> k = (!?> k) =<< unpackM es
    es !>  k = (!>  k) =<< unpackM es
    es >!  k = (>!  k) =<< unpackM es
    
    overwrite es ascs = es <$ (flip overwrite ascs =<< unpackM es)
    
    writeM' es k e = do rep <- unpackM es; writeM' rep k e
    
    updateM  es f = do rep <- unpackM es; packM =<< updateM rep f
    memberM' es k = flip memberM' k =<< unpackM es
    
    p .? es = (p .?) =<< unpackM es
    p *? es = (p *?) =<< unpackM es
    
    kfoldrM  f base = kfoldrM  f base <=< unpackM
    kfoldlM  f base = kfoldlM  f base <=< unpackM
    kfoldrM' f base = kfoldrM' f base <=< unpackM
    kfoldlM' f base = kfoldlM' f base <=< unpackM

--------------------------------------------------------------------------------

instance (MonadVar m, IndexedM m rep i e) => IndexedM m (AnyBorderM m rep) i e
  where
    fromAssocs' = (packM <=<<) . fromAssocs'
    fromAssocs  =  packM <=<< fromAssocs
    
    swapM' es i j = do rep <- unpackM es; swapM' rep i j
    
    fromIndexed' = packM <=< fromIndexed'
    fromIndexedM = packM <=< fromIndexedM
    
    fromAccum f es ascs = do rep <- unpackM es; fromAccum f rep ascs >>= packM
    reshaped  bnds es f = reshaped bnds es f >>= packM

--------------------------------------------------------------------------------

unpack :: AnyBorderM m rep -> Var m rep
unpack =  coerce

unpackM :: (MonadVar m) => AnyBorderM m rep -> m rep
unpackM =  get this . unpack

packM :: (MonadVar m) => rep -> m (AnyBorderM m rep)
packM =  fmap AnyBorderM . var


