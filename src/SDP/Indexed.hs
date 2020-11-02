{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures, ConstraintKinds #-}

{- |
    Module      :  SDP.Indexed
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
  
  @SDP.Indexed@ provides 'Indexed' - class of indexed data structures.
-}
module SDP.Indexed
(
  -- * Exports
  module SDP.Linear,
  module SDP.Map,
  
  -- * Indexed
  Indexed (..), Indexed1, Indexed2,
  
  -- * Freeze
  Freeze (..), Freeze1,
  
  -- * Related functions
  binaryContain
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.Internal
import SDP.Linear
import SDP.Map

default ()

--------------------------------------------------------------------------------

-- | Class of indexed data structures.
class (Linear v e, Bordered v i, Map v i e) => Indexed v i e | v -> i, v -> e
  where
    {-# MINIMAL fromIndexed #-}
    
    {- |
      @assoc bnds ascs@ create new structure from list of associations, without
      default element. Note that @bnds@ is @ascs@ bounds and may not match with
      the result bounds (not always possible).
    -}
    assoc :: (i, i) -> [(i, e)] -> v
    assoc =  flip assoc' (undEx "assoc {default}")
    
    {- |
      @assoc' bnds def ascs@ creates new structure from list of associations
      with default element. Note that @bnds@ is @ascs@ bounds and may not match
      with the result bounds (not always possible).
    -}
    assoc' :: (i, i) -> e -> [(i, e)] -> v
    assoc' bnds defvalue = toMap' defvalue . filter (inRange bnds . fst)
    
    -- | 'fromIndexed' converts this indexed structure to another one.
    fromIndexed :: (Indexed m j e) => m -> v
    
    -- | Safe index-based immutable writer.
    {-# INLINE write' #-}
    write' :: v -> i -> e -> v
    write' es = write es . offsetOf es
    
    {- |
      @'accum' f es ies@ create a new structure from @es@ elements selectively
      updated by function @f@ and @ies@ associations list.
    -}
    accum :: (e -> e' -> e) -> v -> [(i, e')] -> v
    accum f es ies = bounds es `assoc` [ (i, es!i `f` e') | (i, e') <- ies ]
    
    -- | 'imap' creates new indexed structure from old with reshaping.
    imap :: (Map m j e) => (i, i) -> m -> (i -> j) -> v
    imap bnds es f = assoc bnds [ (i, es!f i) | i <- range bnds ]
    
    -- | Create new structure from old by indexed mapping.
    (/>) :: v -> (i -> e -> e) -> v
    (/>) es f = assoc (bounds es) [ (i, f i (es!i)) | i <- indices es ]

--------------------------------------------------------------------------------

-- | Service class of mutable to immutable conversions.
class (Monad m) => Freeze m v' v | v' -> m
  where
    {- |
      @freeze@ is a safe way to convert a mutable structure to a immutable.
      @freeze@ should copy the old structure or ensure that it will not be used
      after calling the procedure.
    -}
    freeze :: v' -> m v
    
    {- |
      @unsafeFreeze@ is unsafe version of 'freeze'. @unsafeFreeze@ doesn't
      guarantee that the structure will be copied or locked. It only guarantees
      that if the old structure isn't used, no error will occur.
    -}
    unsafeFreeze :: v' -> m v
    unsafeFreeze =  freeze

--------------------------------------------------------------------------------

-- | Kind @(* -> *)@ 'Indexed' structure.
type Indexed1 v i e = Indexed (v e) i e

-- | Kind @(* -> * -> *)@ 'Indexed' structure.
type Indexed2 v i e = Indexed (v i e) i e

-- | Kind @(* -> *)@ 'Freeze'.
type Freeze1 m v' v e = Freeze m (v' e) (v e)

--------------------------------------------------------------------------------

instance Indexed [e] Int e
  where
    assoc' bnds e = toMap' e . filter (inRange bnds . fst)
    
    fromIndexed es = (es !) <$> indices es

--------------------------------------------------------------------------------

-- | binaryContain checks that sorted structure has equal element.
binaryContain :: (Linear v e, Bordered v i) => Compare e -> e -> v -> Bool
binaryContain _ _ Z  = False
binaryContain f e es =
  let
    contain l u = not (l > u) && case f e (es !^ j) of
        LT -> contain l (j - 1)
        EQ -> True
        GT -> contain (j + 1) u
      where
        j = u - l `div` 2 + l
  in  f e (head es) /= LT && f e (last es) /= GT && contain 0 (sizeOf es - 1)

undEx :: String -> a
undEx =  throw . UndefinedValue . showString "in SDP.Indexed."

