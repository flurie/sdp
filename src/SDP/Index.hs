{-# LANGUAGE TypeFamilies, TypeOperators, DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE CPP, OverloadedLists #-}

{- |
    Module      :  SDP.Index
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
  
  The 'Index' class is a fork of @Ix@ with a richer interface, more convenient
  function names and generalized indexes.
-}
module SDP.Index
(
  -- * Exports
  module SDP.Finite,
  module SDP.Tuple,
  module SDP.Shape,
  
  module Data.Word,
  module Data.Int,
  
  -- * Service types
  InBounds (..), Bounds,
  
  -- * Indices
  Index (..),
  
  -- * Helpers
  toGBounds, fromGBounds, offsetIntegral, defaultBoundsUnsign, checkBounds
)
where

import Prelude ( (++) )
import SDP.SafePrelude

import SDP.Finite
import SDP.Tuple
import SDP.Shape

import Data.Tuple
import Data.Word
import Data.Int

import SDP.Internal.Commons

default ()

--------------------------------------------------------------------------------

{- Service types. -}

{- |
  Type synonym for very long type annotation, e.g. @Bounds (Int, Int, Int, Int)@
  is same as @((Int, Int, Int, Int), (Int, Int, Int, Int))@.
-}
{-# DEPRECATED Bounds "in favour tuple type synonyms" #-}
type Bounds i = (i, i)

-- | InBounds - service type that specifies index and bounds status.
data InBounds = ER {- ^ Empty Range     -}
              | UR {- ^ Underflow Range -}
              | IN {- ^ Index IN range  -}
              | OR {- ^ Overflow Range  -}
  deriving ( Eq, Show, Read, Enum )

--------------------------------------------------------------------------------

{- |
  Index is service class based on @base@ Ix and @repa@ Shape.
  
  Basic rules:
  
  > rank i == rank   (j `asTypeOf` i)
  > rank i == length (sizes (i, i))
  
  > size bnds >= 0
  > size bnds == product (sizes bnds)
  
  > isEmpty bnds == (size bnds == 0)
  > isEmpty bnds == inRange bnds (safeElem bnds i)
  
  > isEmpty bnds => isOverflow  bnds i
  > isEmpty bnds => isUnderflow bnds i
  
  > inRange bnds i /= isEmpty     bnds
  > inRange bnds i /= isOverflow  bnds i
  > inRange bnds i /= isUnderflow bnds i
  > inRange bnds i == (safeElem bnds i == i)
  
  Note:
  * E is (and should remain) the one and only one index of rank 0.
  * Index is a generalization of Enum, so all rank 1 indices must satisfy Enum
  laws.
  * The cardinality of the set of permissible values for indices mustn't
  exceed 1 (cardinality of a series of natural numbers). This is a purely
  practical limitation.
-}
class (Ord i, Shape i, Shape (DimLast i), Shape (DimInit i), Shape (GIndex i)) => Index i
  where
    {- Basic functions. -}
    
    -- | Returns the size of range.
    {-# INLINE size #-}
    default size :: (Enum i) => (i, i) -> Int
    size :: (i, i) -> Int
    size bnds@(l, u) = isEmpty bnds ? 0 $ u -. l + 1
    
    -- | Returns the sizes of range dimensionwise.
    {-# INLINE sizes #-}
    default sizes :: (Index (GIndex i)) => (i, i) -> [Int]
    sizes :: (i, i) -> [Int]
    sizes =  sizes . toGBounds
    
    -- | Returns the index belonging to the given range.
    {-# INLINE safeElem #-}
    safeElem :: (i, i) -> i -> i
    safeElem (l, u) = min u . max l
    
    -- | Returns bounds of nonempty range.
    {-# INLINE ordBounds #-}
    ordBounds :: (i, i) -> (i, i)
    ordBounds = \ bs -> isEmpty bs ? swap bs $ bs
    
    -- | Size of biggest range, that may be represented by this type of index.
    default defLimit :: (Integral i, Bounded i) => i -> Integer
    defLimit :: i -> Integer
    defLimit i = toInteger (maxBound `asTypeOf` i) + 1
    
    -- | Returns default range by size.
    {-# INLINE defaultBounds #-}
    defaultBounds :: Int -> (i, i)
    defaultBounds n = (unsafeIndex 0, unsafeIndex $ max 0 n - 1)
    
    -- | Returns index by offset in default range.
    {-# INLINE unsafeIndex #-}
    default unsafeIndex :: (Enum i) => Int -> i
    unsafeIndex :: Int -> i
    unsafeIndex =  toEnum
    
    {- Checkers -}
    
    -- | Checks if the bounds is empty.
    {-# INLINE isEmpty #-}
    isEmpty :: (i, i) -> Bool
    isEmpty =  uncurry (>)
    
    -- | Checks the index status in bounds.
    inBounds :: (i, i) -> i -> InBounds
    inBounds (l, u) i | l > u = ER | i > u = OR | i < l = UR | True = IN
    
    -- | Checks if the index is overflow.
    {-# INLINE isOverflow  #-}
    isOverflow :: (i, i) -> i -> Bool
    isOverflow (l, u) i = i > u || l > u
    
    -- | Checks if the index is underflow.
    {-# INLINE isUnderflow #-}
    isUnderflow :: (i, i) -> i -> Bool
    isUnderflow (l, u) i = i < l || l > u
    
    -- | Checks if the index is in range.
    {-# INLINE inRange #-}
    inRange :: (i, i) -> i -> Bool
    inRange (l, u) i = l <= i && i <= u
    
    {- Enum-like operations -}
    
    -- | Returns previous index in range.
    default prev  :: (Enum i) => (i, i) -> i -> i
    prev :: (i, i) -> i -> i
    prev (l, u) i | isEmpty (l, u) = er | i <= l = l | i > u = u | True = pred i
      where
        er = emptyEx "prev (default)"
    
    -- | Returns next index in range.
    default next  :: (Enum i) => (i, i) -> i -> i
    next :: (i, i) -> i -> i
    next (l, u) i | isEmpty (l, u) = er | i >= u = u | i < l = l | True = succ i
      where
        er = emptyEx "next (default)"
    
    -- | Returns offset (indent) of index in this bounds.
    {-# INLINE offset #-}
    default offset :: (Enum i) => (i, i) -> i -> Int
    offset :: (i, i) -> i -> Int
    offset bnds@(l, _) i = checkBounds bnds i (i -. l) "offset (default)"
    
    -- | Returns index by this offset (indent) in range.
    {-# INLINE index #-}
    default index :: (Enum i) => (i, i) -> Int -> i
    index :: (i, i) -> Int -> i
    index bnds@(l, _) n = checkBounds (0, size bnds - 1) n res "index (default)"
      where
        res = toEnum $ n + fromEnum l
    
    -- | Returns the ordered list of indices in this range.
    {-# INLINE range #-}
    default range :: (Enum i) => (i, i) -> [i]
    range :: (i, i) -> [i]
    range =  uncurry enumFromTo

--------------------------------------------------------------------------------

{- Basic instances -}

instance Index E
  where
    unsafeIndex  = const (emptyEx "unsafeIndex (E)")
    
    defLimit = const 0
    
    size  = const 0
    sizes = const []
    range = const []
    
    next   _ _ = E
    prev   _ _ = E
    offset _ _ = 0
    index  _ _ = emptyEx "index (E)"
    
    inBounds    _ _ = ER
    isEmpty       _ = True
    inRange     _ _ = False
    isOverflow  _ _ = False
    isUnderflow _ _ = False

instance Index ()
  where
    size  = const 1
    sizes = const [1]
    range = const [()]
    
    defLimit = const 0
    next _ _ = ()
    prev _ _ = ()
    
    inBounds    _ _ = IN
    isEmpty       _ = False
    inRange     _ _ = True
    isOverflow  _ _ = False
    isUnderflow _ _ = False
    
    defaultBounds = const ((), ())
    index         = const unsafeIndex
    offset  _  _  = 0
    
    unsafeIndex  0 = ()
    unsafeIndex  _ = emptyEx "unsafeIndex ()"

instance Index Char
  where
    defaultBounds = defaultBoundsUnsign
    defLimit      = const $ toInteger (ord maxBound)

instance Index Int     where offset = offsetIntegral
instance Index Int8    where offset = offsetIntegral
instance Index Int16   where offset = offsetIntegral
instance Index Int32   where offset = offsetIntegral
instance Index Int64   where offset = offsetIntegral

instance Index Word    where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index Word8   where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index Word16  where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index Word32  where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index Word64  where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign

--------------------------------------------------------------------------------

{- N-dimensional index instances. -}

instance (Index i, Enum i, Bounded i) => Index (E :& i)
  where
    defLimit = lim undefined
      where
        lim = const . defLimit :: (Index i) => i -> (E :& i) -> Integer
    
    size  = \ ([l], [u]) ->  size (l, u)
    sizes = \ ([l], [u]) -> [size (l, u)]
    range = \ ([l], [u]) -> [ [i] | i <- range (l, u) ]
    
    next = \ ([l], [u]) [i] -> [next (l, u) i]
    prev = \ ([l], [u]) [i] -> [prev (l, u) i]
    
    inRange     = \ ([l], [u]) [i] -> inRange     (l, u) i
    isOverflow  = \ ([l], [u]) [i] -> isOverflow  (l, u) i
    isUnderflow = \ ([l], [u]) [i] -> isUnderflow (l, u) i
    safeElem    = \ ([l], [u]) [i] -> [safeElem   (l, u) i]
    
    isEmpty   = \ ([l], [u]) -> isEmpty (l, u)
    ordBounds = \ ([l], [u]) -> let (l', u') = ordBounds (l, u) in ([l'], [u'])
    
    offset = \ ([l], [u]) [i] -> offset (l, u) i
    index  = \ ([l], [u])  n  -> [index (l, u) n]
    
    defaultBounds = both (E :&) . defaultBounds
    
    unsafeIndex   = \ n -> [unsafeIndex n]

-- [internal]: undecidable
instance (Index i, Enum i, Bounded i, Index (i' :& i)) => Index (i' :& i :& i)
  where
    defLimit i = lim (error "in defLimit") (rank i) i
      where
        lim :: (Index i) => i -> Int -> (i' :& i) -> Integer
        lim =  const ... (^) . defLimit
    
    size  (ls :& l, us :& u) = size (l, u) * size (ls, us)
    sizes (ls :& l, us :& u) = sizes (ls, us) ++ sizes (l, u)
    range (ls :& l, us :& u) = liftA2 (:&) (range (ls, us)) (range (l, u))
    
    prev bs@(ls :& l, us :& u) ix
        | isEmpty bs = emptyEx "prev (n-dimensional)"
        |   i /= l   = is :& pred i
        |  is /= ls  = prev (ls, us) is :& u
        |    True    = ls :& l
      where
        (is :& i) = safeElem bs ix
    
    next bs@(ls :& l, us :& u) ix
        | isEmpty bs = emptyEx "next (n-dimensional)"
        |   i /= u   = is :& succ i
        |  is /= us  = prev (ls, us) is :& u
        |    True    = ls :& l
      where
        (is :& i) = safeElem bs ix
    
    inBounds bs i
      |    isEmpty bs    = ER
      | isUnderflow bs i = UR
      | isOverflow  bs i = OR
      |       True       = IN
    
    inRange     (ls :& l, us :& u) (is :& i) = inRange     (l, u) i && inRange     (ls, us) is
    isOverflow  (ls :& l, us :& u) (is :& i) = isOverflow  (l, u) i || isOverflow  (ls, us) is
    isUnderflow (ls :& l, us :& u) (is :& i) = isUnderflow (l, u) i || isUnderflow (ls, us) is
    
    safeElem  (ls :& l, us :& u) (is :& i) = safeElem (ls, us) is :& safeElem (l, u) i
    isEmpty   (ls :& l, us :& u) = isEmpty (l, u) || isEmpty (ls, us)
    ordBounds (ls :& l, us :& u) = (ls' :& l', us' :& u')
      where
        (ls', us') = ordBounds (ls, us)
        (l',   u') = ordBounds (l,   u)
    
    index bnds@(ls :& l, us :& u) c = checkBounds (0, size bnds - 1) c res err
      where
        (cs, i) = c `divMod` size (l, u)
        res = index (ls, us) cs :& unsafeIndex i
        err = "index (n-dimensional)"
    
    offset bnds ix@(is :& i) = checkBounds bnds ix res "offset (n-dimensional)"
      where
        res = offset (ls, us) is * size (l, u) + offset (l, u) i
        (ls :& l, us :& u) = bnds
    
    unsafeIndex c = unsafeIndex d :& i
      where
        (d, m) = defLimit c <= lim ? (0, c) $ c `divMod` fromInteger lim
        i   = unsafeIndex m
        lim = defLimit i

--------------------------------------------------------------------------------

{- Tuple instances. -}

#define INDEX_INSTANCE(Type) instance (Ord i, Index i, Enum i, Bounded i) => Index (Type i) where\
{\
size           = size . toGBounds;\
sizes          = sizes . toGBounds;\
isEmpty        = isEmpty . toGBounds;\
defLimit       = defLimit . toGIndex;\
unsafeIndex    = fromGIndex . unsafeIndex;\
inRange     bs = inRange (toGBounds bs) . toGIndex;\
ordBounds      = fromGBounds . ordBounds . toGBounds;\
range          = fmap fromGIndex . range . toGBounds;\
next        bs = fromGIndex . next (toGBounds bs) . toGIndex;\
prev        bs = fromGIndex . prev (toGBounds bs) . toGIndex;\
safeElem    bs = fromGIndex . safeElem (toGBounds bs) . toGIndex;\
offset      bs = offset (toGBounds bs) . toGIndex;\
index       bs = fromGIndex . index (toGBounds bs);\
isOverflow  bs = isOverflow  (toGBounds bs) . toGIndex;\
isUnderflow bs = isUnderflow (toGBounds bs) . toGIndex;\
inBounds    bs i | isEmpty bs = ER | isUnderflow bs i = UR | isOverflow bs i = OR | True = IN;\
}

INDEX_INSTANCE(T2)
INDEX_INSTANCE(T3)
INDEX_INSTANCE(T4)
INDEX_INSTANCE(T5)
INDEX_INSTANCE(T6)
INDEX_INSTANCE(T7)
INDEX_INSTANCE(T8)
INDEX_INSTANCE(T9)
INDEX_INSTANCE(T10)
INDEX_INSTANCE(T11)
INDEX_INSTANCE(T12)
INDEX_INSTANCE(T13)
INDEX_INSTANCE(T14)
INDEX_INSTANCE(T15)

#undef INDEX_INSTANCE

--------------------------------------------------------------------------------

-- | Convert any index type bounds to generalized index bounds.
{-# INLINE toGBounds #-}
toGBounds :: (Shape i) => (i, i) -> (GIndex i, GIndex i)
toGBounds =  both toGIndex

-- | Convert generalized index bounds to any index type bounds.
{-# INLINE fromGBounds #-}
fromGBounds :: (Shape i) => (GIndex i, GIndex i) -> (i, i)
fromGBounds =  both fromGIndex

--------------------------------------------------------------------------------

(-.) :: (Enum i) => i -> i -> Int
(-.) =  on (-) fromEnum

-- | Default 'offset' for 'Integral' types.
{-# INLINE offsetIntegral #-}
offsetIntegral :: (Index i, Integral i) => (i, i) -> i -> Int
offsetIntegral bnds@(l, _) i = checkBounds bnds i (i -. l) "offset {default}"

-- | Default 'defaultBounds' for unsigned types.
{-# INLINE defaultBoundsUnsign #-}
defaultBoundsUnsign :: (Index i, Bounded i) => Int -> (i, i)
defaultBoundsUnsign n = n < 1 ? ub 1 0 $ ub 0 (n - 1) where ub = on (,) unsafeIndex

-- | Check bounds and 'throw' 'IndexException' if needed.
checkBounds :: (Index i) => (i, i) -> i -> res -> String -> res
checkBounds bnds i res msg = case inBounds bnds i of
  ER -> throw . EmptyRange     $ "in SDP.Index." ++ msg
  UR -> throw . IndexOverflow  $ "in SDP.Index." ++ msg
  OR -> throw . IndexUnderflow $ "in SDP.Index." ++ msg
  IN -> res

emptyEx :: String -> a
emptyEx =  throw . EmptyRange . showString "in SDP.Index."




