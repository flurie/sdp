{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, TypeOperators, DefaultSignatures #-}
{-# LANGUAGE CPP, OverloadedLists #-}

{-# OPTIONS_HADDOCK ignore-exports #-}

{- |
    Module      :  SDP.Index
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (a lot of GHC extensions)
  
  The 'Index' class is a fork of @Ix@ with a richer interface, more convenient
  function names and generalized indexes.
-}
module SDP.Index
(
  -- * Exports
  module SDP.Finite,
  module SDP.Tuple,
  
  module Data.Word,
  module Data.Int,
  
  -- * Service types
  InBounds (..), Bounds,
  
  -- * Index class
  Index (..), GIndex,
  
  -- * Helpers
  toGBounds, fromGBounds, offsetIntegral, defaultBoundsUnsign, checkBounds
)
where

import Prelude ( (++) )
import SDP.SafePrelude

import SDP.Finite
import SDP.Tuple

import GHC.Types

import Data.Word ( Word, Word8, Word16, Word32, Word64 )
import Data.Int  ( Int,  Int8,  Int16,  Int32,  Int64  )

import Data.Tuple
import Data.Char  ( ord )

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
  
  This Index implementation is unstable and may be extended.
  
  Rules:
  
  > Index i => Index (DimLast i)
  > Index i => Index (DimInit i)
  > Index i => Index (GIndex  i)
  
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
-}
class (Ord i) => Index i
  where
    {- Subspaces -}
    
    -- | Type of top dimension index.
    type DimLast i :: *
    type DimLast i =  i
    
    {- |
      The type of subspace of 'rank' @n - 1@, where @n@ is the 'rank' of the
      space specified by this 'Index' type.
    -}
    type DimInit i :: *
    type DimInit i =  E
    
    -- | Create index from generalized index.
    {-# INLINE fromGIndex #-}
    default fromGIndex :: (GIndex i ~~ (E :& i)) => GIndex i -> i
    fromGIndex :: GIndex i -> i
    fromGIndex =  \ [i] -> i
    
    -- | Create generalized index from index.
    {-# INLINE toGIndex #-}
    default toGIndex :: (GIndex i ~~ (E :& i)) => i -> GIndex i
    toGIndex :: i -> GIndex i
    toGIndex =  \ i -> [i]
    
    {- Basic functions. -}
    
    -- | Count of dimensions in represented space (must be finite and constant).
    {-# INLINE rank #-}
    default rank :: (Index (GIndex i)) => i -> Int
    rank :: i -> Int
    rank =  rank . toGIndex
    
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
    
    -- | Size of biggest range, that may be created by 'defaultBounds'.
    default defLimit :: (Bounded (DimLast i), Integral i, Bounded i) => i -> Integer
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

-- | Type operator 'GIndex' returns generalized equivalent of index.
type family GIndex i
  where
    GIndex     E     = E
    GIndex (i' :& i) = i' :& i
    GIndex     i     = GIndex (DimInit i) :& DimLast i

--------------------------------------------------------------------------------

{- Basic instances -}

instance Index E
  where
    unsafeIndex  = const (emptyEx "unsafeIndex (E)")
    fromGIndex   = id
    toGIndex     = id
    
    defLimit = const 0
    
    rank  = const 0
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
    rank  = const 1
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

instance Index Integer
  where
    rank  = const 1
    sizes = pure . size
    
    defLimit = const (error "no upper bound for defLimit")
    offset   = offsetIntegral

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
    type DimInit (E :& i) = E
    type DimLast (E :& i) = i
    
    fromGIndex = id
    toGIndex   = id
    
    defLimit = lim undefined
      where
        lim = const . defLimit :: (Index i) => i -> (E :& i) -> Integer
    
    rank = const 1
    
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
    type DimInit (i' :& i :& i) = i' :& i
    type DimLast (i' :& i :& i) = i
    
    fromGIndex = id
    toGIndex   = id
    
    defLimit i = lim (error "in defLimit") (rank i) i
      where
        lim :: (Index i) => i -> Int -> (i' :& i) -> Integer
        lim =  const ... (^) . defLimit
    
    rank = rnk undefined
      where
        rnk = const . succ . rank :: (Index i') => i' -> (i' :& i) -> Int
    
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

#define INDEX_INSTANCE(TYPE,LAST,GTYPE) instance (Index i, Enum i, Bounded i) => Index (TYPE) where\
{\
type DimLast (TYPE) = i;\
type DimInit (TYPE) = LAST;\
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
inBounds    bs i | isEmpty bs = ER | isUnderflow bs i = UR | isOverflow bs i = OR | True = IN;
-- incomplete definition, toGIndex and fromGIndex needed.

INDEX_INSTANCE(T2 i, E :& i, I2 i)
fromGIndex = \ [a,b] -> (a,b);
toGIndex       (a,b) =  [a,b];
}

INDEX_INSTANCE(T3 i, T2 i, I3 i)
fromGIndex = \ [a,b,c] -> (a,b,c);
toGIndex       (a,b,c) =  [a,b,c];
}

INDEX_INSTANCE(T4 i, T3 i, I4 i)
fromGIndex = \ [a,b,c,d] -> (a,b,c,d);
toGIndex       (a,b,c,d) =  [a,b,c,d];
}

INDEX_INSTANCE(T5 i, T4 i, I5 i)
fromGIndex = \ [a,b,c,d,e] -> (a,b,c,d,e);
toGIndex       (a,b,c,d,e) =  [a,b,c,d,e];
}

INDEX_INSTANCE(T6 i, T5 i, I6 i)
fromGIndex = \ [a,b,c,d,e,f] -> (a,b,c,d,e,f);
toGIndex       (a,b,c,d,e,f) =  [a,b,c,d,e,f];
}

INDEX_INSTANCE(T7 i, T6 i, I7 i)
fromGIndex = \ [a,b,c,d,e,f,g] -> (a,b,c,d,e,f,g);
toGIndex       (a,b,c,d,e,f,g) =  [a,b,c,d,e,f,g];
}

INDEX_INSTANCE(T8 i, T7 i, I8 i)
fromGIndex = \ [a,b,c,d,e,f,g,h] -> (a,b,c,d,e,f,g,h);
toGIndex       (a,b,c,d,e,f,g,h) =  [a,b,c,d,e,f,g,h];
}

INDEX_INSTANCE(T9 i, T8 i, I9 i)
fromGIndex = \ [a,b,c,d,e,f,g,h,i] -> (a,b,c,d,e,f,g,h,i);
toGIndex       (a,b,c,d,e,f,g,h,i) =  [a,b,c,d,e,f,g,h,i];
}

INDEX_INSTANCE(T10 i, T9 i, I10 i)
fromGIndex = \ [a,b,c,d,e,f,g,h,i,j] -> (a,b,c,d,e,f,g,h,i,j);
toGIndex       (a,b,c,d,e,f,g,h,i,j) =  [a,b,c,d,e,f,g,h,i,j];
}

INDEX_INSTANCE(T11 i, T10 i, I11 i)
fromGIndex = \ [a,b,c,d,e,f,g,h,i,j,k] -> (a,b,c,d,e,f,g,h,i,j,k);
toGIndex       (a,b,c,d,e,f,g,h,i,j,k) =  [a,b,c,d,e,f,g,h,i,j,k];
}

INDEX_INSTANCE(T12 i, T11 i, I12 i)
fromGIndex = \ [a,b,c,d,e,f,g,h,i,j,k,l] -> (a,b,c,d,e,f,g,h,i,j,k,l);
toGIndex       (a,b,c,d,e,f,g,h,i,j,k,l) =  [a,b,c,d,e,f,g,h,i,j,k,l];
}

INDEX_INSTANCE(T13 i, T12 i, I13 i)
fromGIndex = \ [a,b,c,d,e,f,g,h,i,j,k,l,m] -> (a,b,c,d,e,f,g,h,i,j,k,l,m);
toGIndex       (a,b,c,d,e,f,g,h,i,j,k,l,m) =  [a,b,c,d,e,f,g,h,i,j,k,l,m];
}

INDEX_INSTANCE(T14 i, T13 i, I14 i)
fromGIndex = \ [a,b,c,d,e,f,g,h,i,j,k,l,m,n] -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n);
toGIndex       (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =  [a,b,c,d,e,f,g,h,i,j,k,l,m,n];
}

INDEX_INSTANCE(T15 i, T14 i, I15 i)
fromGIndex = \ [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o] -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o);
toGIndex       (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) =  [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o];
}

#undef INDEX_INSTANCE

--------------------------------------------------------------------------------

-- | Convert any index type bounds to generalized index bounds.
{-# INLINE toGBounds #-}
toGBounds :: (Index i) => (i, i) -> (GIndex i, GIndex i)
toGBounds =  both toGIndex

-- | Convert generalized index bounds to any index type bounds.
{-# INLINE fromGBounds #-}
fromGBounds :: (Index i) => (GIndex i, GIndex i) -> (i, i)
fromGBounds =  both fromGIndex

--------------------------------------------------------------------------------

(-.) :: (Enum i) => i -> i -> Int
(-.) =  on (-) fromEnum

-- | offsetIntegral is default offset for 'Integral' types.
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

