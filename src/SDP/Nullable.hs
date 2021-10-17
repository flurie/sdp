{-# LANGUAGE CPP, MagicHash, PatternSynonyms, ViewPatterns, DefaultSignatures #-}
{-# LANGUAGE Trustworthy, ConstraintKinds #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Nullable
    Copyright   :  (c) Andrey Mulik 2020-2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Nullable" provides 'Nullable' - class of types with empty values.
-}
module SDP.Nullable
(
  -- * Nullable
  Nullable (..), Nullable1, Nullable2, pattern NULL, pattern Z,
  
#if __GLASGOW_HASKELL__ >= 806
  Nullable', Nullable''
#endif
)
where

import Foreign.Ptr

import GHC.ForeignPtr
import GHC.Stable
import GHC.Base
import GHC.Exts

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

{- |
  'Nullable' is class of types which have empty values.
  
  Nullable instances must follow some rules:
  
  @
    isNull Z === True
    x == Z ==> isNull x == True
    x == y === isNull x == isNull y
    
    -- For 'Foldable' instances
    toList Z === []
    fold   Z === mempty
    isNull x === null x
    isNull x === length x == 0
    
    sum      Z === 0
    product  Z === 1
    elem   x Z === False
    foldr  f Z === foldl  f Z === id
    foldr1 f Z === foldl1 f Z === undefined
  @
-}
class Nullable e
  where
    -- | Empty value.
    lzero  :: e
    
    -- | Is value empty? Has default since @0.2.1@
    default isNull :: (Eq e) => e -> Bool
    isNull :: e -> Bool
    isNull =  (== lzero)

--------------------------------------------------------------------------------

{-# COMPLETE Z,    (:) #-}
{-# COMPLETE NULL, (:) #-}

-- | 'NULL' pattern correspond 'isNull' values, @'NULL' = 'lzero'@.
pattern NULL :: (Nullable e) => e
pattern NULL <- (isNull -> True) where NULL = lzero

{- |
  Other empty value pattern: @Z = NULL@.
-}
pattern Z :: (Nullable e) => e
pattern Z =  NULL

--------------------------------------------------------------------------------

-- | @since 0.2.1 'Nullable' contraint for @(Type -> Type)@-kind types.
type Nullable1 rep e = Nullable (rep e)

-- | @since 0.3 'Nullable' contraint for @(Type -> Type -> Type)@-kind types.
type Nullable2 rep i e = Nullable (rep i e)

#if __GLASGOW_HASKELL__ >= 806

{- |
  @since 0.3 'Nullable' contraint for @(Type -> Type)@-kind types.
  
  Only for GHC >= 8.6.1
-}
type Nullable' rep = forall e . Nullable (rep e)

{- |
  @since 0.3 'Nullable' contraint for @(Type -> Type -> Type)@-kind types.
  
  Only for GHC >= 8.6.1
-}
type Nullable'' rep = forall i e . Nullable (rep i e)

#endif

--------------------------------------------------------------------------------

-- | @since 0.2.1
instance Nullable () where isNull = const True; lzero = ()

instance Nullable (Maybe e)
  where
    isNull = \ mx -> case mx of {Nothing -> True; _ -> False}
    lzero  = Nothing

instance Nullable [e] where isNull = null; lzero  = []

instance Nullable (Ptr e) where lzero = nullPtr

-- Stolen from @bytestring@ package.
instance Nullable (ForeignPtr e)
  where
#if MIN_VERSION_base(4,15,0)
    lzero = ForeignPtr nullAddr# FinalPtr
#else
    lzero =
      let ex = UnreachableException "in SDP.Nullable.lzero :: ForeignPtr e"
      in  ForeignPtr nullAddr# (throw ex)
#endif
    isNull (ForeignPtr addr# _) = Ptr addr# == nullPtr

instance Nullable (StablePtr e) where lzero = StablePtr (unsafeCoerce# 0#)

-- | @since 0.2.1
instance Nullable (FunPtr e) where lzero = nullFunPtr

