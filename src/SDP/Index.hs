{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances, DefaultSignatures #-}
{-# LANGUAGE CPP, OverloadedLists, ConstraintKinds, TypeOperators #-}

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
  -- * Shapes
  module SDP.Shape,
  
  (:|:), SubIndex, takeDim, dropDim, splitDim,
  
  -- * Indices
  Index (..),
  
  -- * Service types
  InBounds (..), Bounds,
  
  -- * Helpers
  offsetIntegral, defaultBoundsUnsign
)
where

import Prelude ( (++) )
import SDP.SafePrelude
import SDP.Internal
import SDP.Shape

import Data.Tuple

import Foreign.C.Types

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

-- Note: SubIndex is constraint that hide 'Sub' class.

{- |
  The constraint @SubIndex i j@ matches if upper subspaces of @i@ and @j@ have
  equivalent types, and rank @i@ isn't less than rank @j@.
-}
type SubIndex = Sub

-- | (:|:) is closed type family of shape differences.
type family i :|: j
  where
    i :|: E = i
    i :|: j = DimInit i :|: DimInit j

-- Internal class of shape differences.
class (Index i, Index j, Index (i :|: j)) => Sub i j
  where
    _takeDim :: i -> j
    
    -- | dropDim returns shape difference.
    _dropDim :: i -> j -> i :|: j

instance {-# OVERLAPS #-} (Index i, E ~~ (i :|: i)) => Sub i i
  where
    _takeDim = id
    _dropDim = \ _ _ -> E

instance {-# OVERLAPPING #-} (Sub (DimInit i) j, ij ~~ (i :|: j), Index i, Index j, Index ij, DimInit ij ~~ (DimInit i :|: j), DimLast ij ~~ DimLast i) => Sub i j
  where
    _takeDim = _takeDim . initDim
    _dropDim i' j' = let (is, i) = unconsDim i' in consDim (_dropDim is j') i

-- | takeDim returns subshape.
takeDim :: (SubIndex i j) => i -> j
takeDim =  _takeDim

-- | dropDim returns shape difference.
dropDim :: (SubIndex i j) => i -> j -> i :|: j
dropDim =  _dropDim

-- | splitDim returns pair of shape difference and subshape.
splitDim :: (SubIndex i j) => i -> (i :|: j, j)
splitDim i = (dropDim i j, j) where j = takeDim i

--------------------------------------------------------------------------------

{- |
  Index is service class based on @base@ Ix and @repa@ Shape.
  
  Basic rules:
  
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
    prev (l, u) i | isEmpty (l, u) = e | i <= l = l | i > u = u | True = pred i
      where
        e = emptyEx "prev {default}"
    
    -- | Returns next index in range.
    default next  :: (Enum i) => (i, i) -> i -> i
    next :: (i, i) -> i -> i
    next (l, u) i | isEmpty (l, u) = e | i >= u = u | i < l = l | True = succ i
      where
        e = emptyEx "next {default}"
    
    -- | Returns offset (indent) of index in this bounds.
    {-# INLINE offset #-}
    default offset :: (Enum i) => (i, i) -> i -> Int
    offset :: (i, i) -> i -> Int
    offset bnds@(l, _) i = checkBounds bnds i (i -. l) "offset {default}"
    
    -- | Returns index by this offset (indent) in range.
    {-# INLINE index #-}
    default index :: (Enum i) => (i, i) -> Int -> i
    index :: (i, i) -> Int -> i
    index bnds@(l, _) n = checkBounds (0, size bnds - 1) n res "index {default}"
      where
        res = toEnum $ n + fromEnum l
    
    -- | Returns the ordered list of indices in this range.
    {-# INLINE range #-}
    default range :: (Enum i) => (i, i) -> [i]
    range :: (i, i) -> [i]
    range =  uncurry enumFromTo
    
    {- |
      @shape bnds ij@ returns subshape of @bnds@.
      
      Checks if @ij@ in @bnds@ subshape, may 'throw' 'IndexException'.
    -}
    shape :: (Sub i j, Index (i :|: j)) => (i, i) -> i :|: j -> (j, j)
    shape (l, u) ij = checkBounds (l', u') ij (lj, uj) "shape {default}"
      where
        (l', lj) = splitDim l
        (u', uj) = splitDim u
    
    slice :: (Sub i j, Index (i :|: j)) => (i, i) -> i :|: j -> ((i :|: j, i :|: j), (j, j))
    slice (l, u) ij = checkBounds (ls, us) ij ((ls, us), (lj, uj)) "slice {default}"
      where
        (ls, lj) = splitDim l
        (us, uj) = splitDim u

--------------------------------------------------------------------------------

instance (Index i) => Estimate (i, i)
  where
    (<==>) = on (<=>) size
    (.<=.) = on (<=)  size
    (.>=.) = on (>=)  size
    (.>.)  = on (>)   size
    (.<.)  = on (<)   size
    
    (<.=>) = (<=>) . size
    (.>)   = (>)   . size
    (.<)   = (<)   . size
    (.>=)  = (>=)  . size
    (.<=)  = (<=)  . size

--------------------------------------------------------------------------------

{- Basic instances. -}

instance Index E
  where
    unsafeIndex = const (emptyEx "unsafeIndex {E}")
    
    defLimit = const (-1)
    
    size  = const 0
    sizes = const []
    range = const []
    
    next   _ _ = E
    prev   _ _ = E
    offset _ _ = emptyEx "offset {E}"
    index  _ _ = emptyEx "index {E}"
    
    inBounds    _ _ = ER
    inRange     _ _ = False
    isEmpty       _ = True
    isOverflow  _ _ = True
    isUnderflow _ _ = True

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

{- Foreign C instances. -}

instance Index CChar      where offset = offsetIntegral
instance Index CSChar     where offset = offsetIntegral
instance Index CWchar     where offset = offsetIntegral
instance Index CShort     where offset = offsetIntegral

instance Index CInt       where offset = offsetIntegral
instance Index CLong      where offset = offsetIntegral
instance Index CLLong     where offset = offsetIntegral
instance Index CIntPtr    where offset = offsetIntegral
instance Index CIntMax    where offset = offsetIntegral
instance Index CPtrdiff   where offset = offsetIntegral
instance Index CSigAtomic where offset = offsetIntegral

instance Index CSize      where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index CBool      where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index CUChar     where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index CUShort    where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign

instance Index CUInt      where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index CULong     where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index CULLong    where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index CUIntPtr   where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign
instance Index CUIntMax   where offset = offsetIntegral; defaultBounds = defaultBoundsUnsign

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
    defLimit i = lim (error "in defLimit {i' :& i :& i}") (rank i) i
      where
        lim :: (Index i) => i -> Int -> (i' :& i) -> Integer
        lim =  const ... (^) . defLimit
    
    size  (ls :& l, us :& u) = size (l, u) * size (ls, us)
    sizes (ls :& l, us :& u) = sizes (ls, us) ++ sizes (l, u)
    range (ls :& l, us :& u) = liftA2 (:&) (range (ls, us)) (range (l, u))
    
    prev bs@(ls :& l, us :& u) ix
        | isEmpty bs = emptyEx "prev {i' :& i :& i}"
        |   i /= l   = is :& pred i
        |  is /= ls  = prev (ls, us) is :& u
        |    True    = ls :& l
      where
        (is :& i) = safeElem bs ix
    
    next bs@(ls :& l, us :& u) ix
        | isEmpty bs = emptyEx "next {i' :& i :& i}"
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
        err = "index {i' :& i :& i}"
    
    offset bnds ix@(is :& i) = checkBounds bnds ix res "offset {i' :& i :& i}"
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
size        = size . toGBounds;\
sizes       = sizes . toGBounds;\
isEmpty     = isEmpty . toGBounds;\
defLimit    = defLimit . toGIndex;\
unsafeIndex = fromGIndex . unsafeIndex;\
index       = fromGIndex ... index . toGBounds;\
range       = fmap fromGIndex . range . toGBounds;\
ordBounds   = fromGBounds . ordBounds . toGBounds;\
offset      = \ bs -> offset (toGBounds bs) . toGIndex;\
inRange     = \ bs -> inRange (toGBounds bs) . toGIndex;\
isOverflow  = \ bs -> isOverflow  (toGBounds bs) . toGIndex;\
isUnderflow = \ bs -> isUnderflow (toGBounds bs) . toGIndex;\
next        = \ bs -> fromGIndex . next (toGBounds bs) . toGIndex;\
prev        = \ bs -> fromGIndex . prev (toGBounds bs) . toGIndex;\
safeElem    = \ bs -> fromGIndex . safeElem (toGBounds bs) . toGIndex;\
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
checkBounds bnds i res = case inBounds bnds i of
  ER -> throw . EmptyRange      . showString "in SDP.Index."
  OR -> throw . IndexOverflow   . showString "in SDP.Index."
  UR -> throw . IndexUnderflow  . showString "in SDP.Index."
  IN -> const res

--------------------------------------------------------------------------------

emptyEx :: String -> a
emptyEx =  throw . EmptyRange . showString "in SDP.Index."



