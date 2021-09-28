{-# LANGUAGE Safe, CPP, ConstraintKinds #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints, RankNTypes #-}
#endif

{- |
    Module      :  SDP.Forceable
    Copyright   :  (c) Andrey Mulik 2021
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Forceable" provides 'Forceable' class.
    
    @since 0.3
-}
module SDP.Forceable
(
  -- * Forceable
  Forceable (..), Forceable1, Forceable2,
  
#if __GLASGOW_HASKELL__ >= 806
  
  Forceable', Forceable'',
  
#endif
)
where

default ()

--------------------------------------------------------------------------------

{- |
  @since 0.3
  
  'Forceable' is a type class, the values ​​of which can have equal, but
  structurally different values ​​(that is, more or less efficient in terms of a
  number of practical criteria).
  
  For example, @take n es@ and can be O(1) or worse. In most cases, the result
  of @take m es@ is used with @es@ and/or @drop m es@, or structure/remainder
  size is small enough, or the lifetime of the object itself is small, so in
  general case an O(1) implementation will, is preferable.
  
  But however, sometimes it becomes necessary to work with big structures, when
  we are only interested (at the moment or in general) in a small part of it.
  Then O(1) implementation can lead to memory leaks and performance problems.
  In this case, 'force' will adjust the performance in bad cases.
  
  Also note that that force can significantly reduce performance if used
  inappropriately.
-}
class Forceable f
  where
    -- | Force the value, default: @force = id@.
    force :: f -> f
    force =  id

-- | @since 0.3 'Forceable' contraint for @(Type -> Type)@-kind types.
type Forceable1 f e = Forceable (f e)

-- | @since 0.3 'Forceable' contraint for @(Type -> Type -> Type)@-kind types.
type Forceable2 f i e = Forceable (f i e)

#if __GLASGOW_HASKELL__ >= 806

{- |
  @since 0.3 'Forceable' contraint for @(Type -> Type@-kind types.
  
  Only for GHC >= 8.6.1
-}
type Forceable' f = forall e . Forceable (f e)

{- |
  @since 0.3 'Forceable' contraint for @(Type -> Type -> Type)@-kind types.
  
  Only for GHC >= 8.6.1
-}
type Forceable'' f = forall i e . Forceable (f i e)

#endif

--------------------------------------------------------------------------------

{- |
  The list doesn't imply variations in values or memory leaks that can be
  corrected by rebuilding it (rather the opposite), so @force = id@.
-}
instance Forceable [a]




