{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures, TypeOperators, OverloadedLists #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_HADDOCK ignore-exports #-}

{- |
    Module      :  SDP.Index
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (a lot of GHC extensions)
  
  @SDP.Index@ provides 'Index' - service class of types that may represent
  indices, based on "Data.Ix" and @Data.Array.Repa.Shape@ (see repa). I refused
  to create a Ix-based class, because in this case I still need one new class.
  
  SDP.Index provides a generic n-dimensional index (type ':&').
  
  Also (since sdp-0.2) are available @OverloadedIndices@ - syntactic sugar based
  on the @OverloadedLists@ language extension. For example, instead of the
  inconvenient @es!(ind4 0 1 2 3)@ and just the awful @es!(E:&0:&1:&2:&3)@ you
  can write shorter and more understandable indices: @es![0, 1, 2, 3]@.
  
  OverloadedIndices works only for (':&') and requires a strictly defined number
  of subindexes. But (I hope) no one will use list comprehensions (e.g.
  @es![2..4]@ instead @es![2, 3, 4]@) to specify indexes...
-}
module SDP.Index
(
  -- * Exports
  module SDP.Tuple,
  
  module Data.Word,
  module Data.Int,
  
  -- * Service types
  InBounds (..), Bounds,
  
  -- * N-dimensional index
  E (..), (:&) (..),
  
  -- * Index class
  Index (..),
  
  -- ** Type synonyms
  I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13, I14, I15,
  
  -- ** Old constructors
  ind2,  ind3,  ind4,  ind5,  ind6,  ind7,  ind8,  ind9,
  ind10, ind11, ind12, ind13, ind14, ind15
)
where

import Prelude ( (++) )
import SDP.SafePrelude

import GHC.Exts ( IsList )
import qualified GHC.Exts as E

import GHC.Types
import GHC.Read

import Test.QuickCheck

import Data.Word ( Word, Word8, Word16, Word32, Word64 )
import Data.Int  ( Int,  Int8,  Int16,  Int32,  Int64  )

import Data.Tuple

import SDP.Tuple
import SDP.Simple

default ()

--------------------------------------------------------------------------------

{- Service types. -}

{- |
  Type synonym for very long type annotation, e.g. @Bounds (Int, Int, Int, Int)@
  is same as @((Int, Int, Int, Int), (Int, Int, Int, Int))@
-}
type Bounds i = (i, i)

-- | InBounds - service type that specifies index and bounds status.
data InBounds = ER {- ^ Empty Range     -}
              | UR {- ^ Underflow Range -}
              | IN {- ^ Index IN range  -}
              | OR {- ^ Overflow Range  -}
            deriving ( Eq, Show, Read, Enum )

--------------------------------------------------------------------------------

{- N-dimensional index type. -}

-- | Service type, that represents zero-dimensional index.
data E = E deriving ( Eq, Ord, Show, Read )

instance Arbitrary E where arbitrary = return E

instance Default E where def = E

{- |
  N-dimensional index type. The type (head :& tail) allows working with any
  finite dimension number.
-}
data tail :& head = !tail :& !head deriving ( Eq, Ord )

-- [internal]: undecidable
instance (IsList (i' :& i), E.Item (i' :& i) ~~ i, Show i) => Show (i' :& i)
  where
    showsPrec p = showsPrec p . E.toList

-- [internal]: undecidable
instance (IsList (i' :& i), E.Item (i' :& i) ~~ i, Read i) => Read (i' :& i)
  where
    readPrec = E.fromList <$> readPrec

instance (Arbitrary i, Arbitrary i') => Arbitrary (i' :& i)
  where
    arbitrary = applyArbitrary2 (:&)

instance (Enum i) => Enum (E :& i)
  where
    succ (E :& e) = E :& succ e
    pred (E :& e) = E :& pred e
    
    toEnum n          = E :& toEnum n
    fromEnum (E :& e) = fromEnum e
    
    enumFrom       (E :& f)                   = (E :&) <$> [f .. ]
    enumFromTo     (E :& f) (E :& l)          = (E :&) <$> [f .. l]
    enumFromThen   (E :& f) (E :& n)          = (E :&) <$> [f, n .. ]
    enumFromThenTo (E :& f) (E :& n) (E :& l) = (E :&) <$> [f, n .. l]

instance (Default d, Default d') => Default (d :& d') where def = def :& def

--------------------------------------------------------------------------------

{- |
  Index is service class. It's the result of combining Data.Ix (base) and
  Data.Array.Repa.Index (repa), but adds several features of its own and more
  polymorphic instances. Also module may be extended.
  
  Note that the default definitions are only valid for one-dimensional indexes
  (like Int, Word, etc.). Integral types and enumeration types are best suited
  for indexes.
-}
class (Ord i) => Index i
  where
    {- Type familes -}
    
    {- |
      Type of upper (sliceable) dimension:
      
      > DimLast (T7 Int) ~~ Int
      > DimLast (I3 Int) ~~ Int
      > DimLast Integer  ~~ Integer
    -}
    type DimLast i :: *
    type DimLast i =  i
    
    {- |
      The type of subspace, of 'rank' @N-1@, where @N@ is the 'rank' of the
      space specified by this 'Index' type.
      
      > DimInit (T2 Char) ~~ Char
      > DimInit (I3 Word) ~~ I2 Word
      > DimInit Integer   ~~ E
    -}
    type DimInit i :: *
    type DimInit i =  E
    
    {- |
      The type of generalized index (':&'), that represent same space:
      
      > GIndex (T5 Char) ~~ I5 Char
      > GIndex (I2  Int) ~~ I2 Int
      > GIndex Integer   ~~ E :& Integer
    -}
    type GIndex i :: *
    type GIndex i =  E :& i
    
    default fromGIndex :: (GIndex i ~~ (E :& i)) => GIndex i -> i
    fromGIndex :: GIndex i -> i
    fromGIndex ~[i] = i
    
    default toGIndex :: (GIndex i ~~ (E :& i)) => i -> GIndex i
    toGIndex :: i -> GIndex i
    toGIndex i = [i]
    
    toGBounds :: (i, i) -> (GIndex i, GIndex i)
    toGBounds (l, u) = (toGIndex l, toGIndex u)
    
    fromGBounds :: (GIndex i, GIndex i) -> (i, i)
    fromGBounds (l, u) = (fromGIndex l, fromGIndex u)
    
    {- Commons -}
    
    -- | Returns the number of dimensions that this type of index represents.
    default rank :: (Index (GIndex i)) => i -> Int
    rank :: i -> Int
    rank =  rank . toGIndex
    
    {- |
      Returns the size (length) of range.
      
      > size (ind3 1 2 3, ind3 4 5 6) == 64
      > size ([1, 2, 3], [4, 5, 6]) == 64 -- with OverloadedLists (since sdp-0.2)
      > size (0, 3) == 4
      > size (3, 0) == 0
    -}
    {-# INLINE size #-}
    default size :: (DimInit i ~~ E, Enum i) => (i, i) -> Int
    size :: (i, i) -> Int
    size (l, u) = u >= l ? u -. l + 1 $ 0
    
    -- | Returns default range by size.
    {-# INLINE defaultBounds #-}
    defaultBounds :: Int -> (i, i)
    defaultBounds n = (unsafeIndex 0, unsafeIndex $ max 0 n - 1)
    
    -- | Returns index by this offset default range.
    {-# INLINE unsafeIndex #-}
    default unsafeIndex :: (DimInit i ~~ E, Enum i) => Int -> i
    unsafeIndex :: Int -> i
    unsafeIndex =  toEnum
    
    {- Enum operations -}
    
    -- | Returns previous index in range.
    default prev  :: (DimInit i ~~ E, Enum i) => (i, i) -> i -> i
    prev :: (i, i) -> i -> i
    prev (l, u) i | isEmpty (l, u) = er | i <= l = l | i > u = u | True = pred i
      where
        er = throw $ EmptyRange "in SDP.Index.prev (default)"
    
    -- | Returns next index in range.
    default next  :: (DimInit i ~~ E, Enum i) => (i, i) -> i -> i
    next :: (i, i) -> i -> i
    next (l, u) i | isEmpty (l, u) = er | i >= u = u | i < l = l | True = succ i
      where
        er = throw $ EmptyRange "in SDP.Index.next (default)"
    
    -- | Returns offset (indent) of index in this bounds.
    {-# INLINE offset #-}
    default offset :: (DimInit i ~~ E, Enum i) => (i, i) -> i -> Int
    offset :: (i, i) -> i -> Int
    offset bnds i = checkBounds bnds i (i -. fst bnds) "offset (default)"
    
    -- | Returns index by this offset in range.
    {-# INLINE index #-}
    default index :: (DimInit i ~~ E, Enum i) => (i, i) -> Int -> i
    index :: (i, i) -> Int -> i
    index bnds n = checkBounds (0, size bnds - 1) n res "index (default)"
      where
        res = toEnum $ n + fromEnum (fst bnds)
    
    {- |
      Returns the list of indices in this range.
      
      > range (2, 7) == [2 .. 7]
      > range (7, 2) == []
    -}
    {-# INLINE range #-}
    default range :: (DimInit i ~~ E, Enum i) => (i, i) -> [i]
    range :: (i, i) -> [i]
    range =  uncurry enumFromTo
    
    {- Checkers -}
    
    -- | Checks if the bounds is empty.
    {-# INLINE isEmpty #-}
    isEmpty :: (i, i) -> Bool
    isEmpty =  uncurry (>)
    
    {- |
      Checks if the index is in range.
      
      > inRange (-5, 4) 3 == True
      > inRange (4, -5) 3 == False
    -}
    {-# INLINE inRange #-}
    default inRange :: (DimInit i ~~ E, Enum i) => (i, i) -> i -> Bool
    inRange :: (i, i) -> i -> Bool
    inRange (l, u) i = l <= i && i <= u
    
    -- | Returns the index and bounds status.
    default inBounds :: (DimInit i ~~ E, Enum i) => (i, i) -> i -> InBounds
    inBounds :: (i, i) -> i -> InBounds
    inBounds (l, u) i | l > u = ER | i > u = OR | i < l = UR | True = IN
    
    {- |
      Checks if the index is overflow.
      
      The default definition for @isOverflow@ and @isUnderflow@ returns True in
      the case of an empty range:
      
      > isOverflow  (5, -1) x == True
      > isUnderflow (5, -1) x == True
      
      for all x because
      
      > not . elem x $ range (5, -1) == True
      > offset (26, 0) 0 == *** Exception: empty range in ...
      > range (26, 0) == []
      > isOverflow  (26, 0) 0 == True
      > isUnderflow (26, 0) 0 == True
      
      Other definitions in this module follow this rule. This is not a strict
      requirement, but a recommended behavior (if it's relevant to a particular
      implementation).
      
      Generaly speaking (for not one-dimensional indices), @isOverflow@ and
      @isUnderflow@ are not mutually exclusive, and their conjunction doesn't
      mean that range is empty:
      
      > isOverflow  ((-3, 4), (2, 5)) (-4, 6) == True
      > isUnderflow ((-3, 4), (2, 5)) (-4, 6) == True
      
      Also, their disjunction is interchangeable with inversion of inRange (in
      default definitions).
    -}
    {-# INLINE isOverflow  #-}
    default isOverflow :: (DimInit i ~~ E, Enum i) => (i, i) -> i -> Bool
    isOverflow :: (i, i) -> i -> Bool
    isOverflow (l, u) i = i > u || l > u
    
    -- | Checks if the index is underflow.
    {-# INLINE isUnderflow #-}
    default isUnderflow :: (DimInit i ~~ E, Enum i) => (i, i) -> i -> Bool
    isUnderflow :: (i, i) -> i -> Bool
    isUnderflow (l, u) i = i < l || l > u
    
    {- Extra -}
    
    {- |
      Returns the sizes (lengths) of range dimensionwise, has constant number of
      elements.
      
      > sizes (5, 7) == [3]
      > sizes (ind4 7 (-1) 12 4, ind4 0 12 9 (4 :: Int)) == [0, 14, 0, 1]
      > sizes ([7, -1, 12, 4], [0, 12, 9, 4] :: I4 Int) == [0, 14, 0, 1]
    -}
    default sizes :: (DimInit i ~~ E, Enum i) => (i, i) -> [Int]
    sizes :: (i, i) -> [Int]
    sizes bnds = [size bnds]
    
    -- | Returns the index belonging to the given range. Service function.
    safeElem :: (i, i) -> i -> i
    safeElem (l, u) = min u . max l
    
    -- | Returns bounds of nonempty range.
    {-# INLINE ordBounds #-}
    ordBounds :: (i, i) -> (i, i)
    ordBounds bs = isEmpty bs ? swap bs $ bs

--------------------------------------------------------------------------------

{- Basic instances -}

instance Index E
  where
    type GIndex E = E
    
    fromGIndex = id
    toGIndex   = id
    
    rank  = const 0
    size  = const 0
    sizes = const []
    range = const []
    
    next _ _ = E
    prev _ _ = E
    
    inBounds    _ _ = ER
    isEmpty       _ = True
    inRange     _ _ = False
    isOverflow  _ _ = False
    isUnderflow _ _ = False
    
    unsafeIndex   _ = throw $ EmptyRange "in SDP.Index.unsafeIndex (E)"
    
    index  _ _ = throw $ EmptyRange "in SDP.Index.index (E)"
    offset _ _ = 0

instance Index ()
  where
    rank  = const 1
    size  = const 1
    sizes = const [1]
    range = const [()]
    
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
    
    unsafeIndex 0 = ()
    unsafeIndex _ = throw $ EmptyRange "in SDP.Index.unsafeIndex ()"

instance Index Int     where offset = intOffset
instance Index Int8    where offset = intOffset
instance Index Int16   where offset = intOffset
instance Index Int32   where offset = intOffset
instance Index Int64   where offset = intOffset
instance Index Integer where offset = intOffset

instance Index Char    where defaultBounds = defUB
instance Index Word    where defaultBounds = defUB; offset = intOffset
instance Index Word8   where defaultBounds = defUB; offset = intOffset
instance Index Word16  where defaultBounds = defUB; offset = intOffset
instance Index Word32  where defaultBounds = defUB; offset = intOffset
instance Index Word64  where defaultBounds = defUB; offset = intOffset

--------------------------------------------------------------------------------

{- N-dimensional index instances. -}

instance (Index i, Enum i) => Index (E :& i)
  where
    type GIndex  (E :& i) = E :& i
    type DimInit (E :& i) = E
    type DimLast (E :& i) = i
    
    fromGIndex = id
    toGIndex   = id
    
    rank = const 1
    
    size  (~[l], ~[u]) = size  (l, u)
    sizes (~[l], ~[u]) = sizes (l, u)
    range (~[l], ~[u]) = (E :&) <$> range (l, u)
    
    next  (~[l], ~[u]) ~[i] = [next (l, u) i]
    prev  (~[l], ~[u]) ~[i] = [prev (l, u) i]
    
    inRange     (~[l], ~[u]) ~[i] = inRange       (l, u) i
    isOverflow  (~[l], ~[u]) ~[i] = isOverflow    (l, u) i
    isUnderflow (~[l], ~[u]) ~[i] = isUnderflow   (l, u) i
    safeElem    (~[l], ~[u]) ~[i] = E :& safeElem (l, u) i
    
    isEmpty     (~[l], ~[u]) = isEmpty (l, u)
    ordBounds   (~[l], ~[u]) = let (l', u') = ordBounds (l, u) in ([l'], [u'])
    
    offset ~([l], [u]) ~[i] = offset     (l, u) i
    index  ~([l], [u])   n  = E :& index (l, u) n
    
    unsafeIndex n = [unsafeIndex n]

-- [internal]: undecidable
instance (Index i, Enum i, Bounded i, Index (i' :& i)) => Index (i' :& i :& i)
  where
    type GIndex  (i' :& i :& i) = i' :& i :& i
    type DimInit (i' :& i :& i) = i' :& i
    type DimLast (i' :& i :& i) = i
    
    fromGIndex = id
    toGIndex   = id
    
    rank  (rs :& _) = rank rs + 1
    
    size  (ls :& l, us :& u) = size (l, u) * size (ls, us)
    -- [internal]: O(n ^ 2) sizes. Not critial, but needed to rewrite.
    sizes (ls :& l, us :& u) = sizes (ls, us) ++ sizes (l, u)
    range (ls :& l, us :& u) = liftA2 (:&) (range (ls, us)) (range (l, u))
    
    -- [internal]: prev and next uses safeElem. Needed to rewrite.
    prev bnds@(ls :& l, us :& u) ix
        | isEmpty bnds = throw $ EmptyRange "in SDP.Index.prev (n-dimensional)"
        |    i /= l    = is :& pred i
        |   is /= ls   = prev (ls, us) is :& u
        |     True     = ls :& l
      where
        (is :& i) = safeElem bnds ix
    
    next bnds@(ls :& l, us :& u) ix
        | isEmpty bnds = throw $ EmptyRange "in SDP.Index.next (n-dimensional)"
        |    i /= u    = is :& succ i
        |   is /= us   = prev (ls, us) is :& u
        |     True     = ls :& l
      where
        (is :& i) = safeElem bnds ix
    
    inBounds bs i
      |    isEmpty bs    = ER
      | isUnderflow bs i = UR
      | isOverflow  bs i = OR
      |       True       = IN
    
    inRange     (ls :& l, us :& u) (is :& i) = inRange     (l, u) i && inRange     (ls, us) is
    isOverflow  (ls :& l, us :& u) (is :& i) = isOverflow  (l, u) i || isOverflow  (ls, us) is
    isUnderflow (ls :& l, us :& u) (is :& i) = isUnderflow (l, u) i || isUnderflow (ls, us) is
    
    safeElem    (ls :& l, us :& u) (is :& i) = safeElem (ls, us) is :& safeElem (l, u) i
    isEmpty     (ls :& l, us :& u) = isEmpty (l, u) || isEmpty (ls, us)
    ordBounds   (ls :& l, us :& u) = (ls' :& l', us' :& u')
      where
        (ls', us') = ordBounds (ls, us)
        (l', u')   = ordBounds (l, u)
    
    index bnds@(ls :& l, us :& u) c = checkBounds (0, size bnds - 1) c res err
      where
        (cs, i) = divMod c $ size (l, u)
        res = index (ls, us) cs :& (unsafeIndex i)
        err = "index (n-dimensional)"
    
    offset bnds ix@(is :& i) = checkBounds bnds ix res "offset (n-dimensional)"
      where
        res = offset (ls, us) is * size (l, u) + offset (l, u) i
        (ls :& l, us :& u) = bnds
    
    unsafeIndex c = let (cs, i) = c `divMod` maxBound in unsafeIndex cs :& unsafeIndex i

--------------------------------------------------------------------------------

{- Overloaded indices. -}

instance (Index i) => IsList (E :& i)
  where
    type Item (E :& i) = i
    
    fromList [i] = E :& i
    fromList  _  = error "unexpected rank in SDP.Index.{IsList (i' :& i)}fromList"
    
    toList (E :& i) = [i]

instance (Index (i' :& i :& i), E.Item (i' :& i) ~~ i, IsList (i' :& i)) => IsList (i' :& i :& i)
  where
    type Item (i' :& i :& i) = i
    
    toList (i' :& i) = E.toList i' ++ [i]
    
    fromList is = let (init', last') = unsnoc is in E.fromList init' :& last'

--------------------------------------------------------------------------------

{- Type synonyms are declared up to 15 dimensions. -}

-- | 2-dimensional index
type I2  i = E :& i  :& i
-- | 3-dimensional index
type I3  i = (I2  i) :& i
-- | 4-dimensional index
type I4  i = (I3  i) :& i
-- | 5-dimensional index
type I5  i = (I4  i) :& i
-- | 6-dimensional index
type I6  i = (I5  i) :& i
-- | 7-dimensional index
type I7  i = (I6  i) :& i
-- | 8-dimensional index
type I8  i = (I7  i) :& i
-- | 9-dimensional index
type I9  i = (I8  i) :& i
-- | 10-dimensional index
type I10 i = (I9  i) :& i
-- | 11-dimensional index
type I11 i = (I10 i) :& i
-- | 12-dimensional index
type I12 i = (I11 i) :& i
-- | 13-dimensional index
type I13 i = (I12 i) :& i
-- | 14-dimensional index
type I14 i = (I13 i) :& i
-- | i-think-you-guessed-how-much-dimensional index
type I15 i = (I14 i) :& i

-- | 2-dimensional index clever constructor.
ind2  :: (Index i, Enum i, Bounded i) => i -> i                                                                  -> I2  i
-- | 3-dimensional index clever constructor.
ind3  :: (Index i, Enum i, Bounded i) => i -> i -> i                                                             -> I3  i
-- | 4-dimensional index clever constructor.
ind4  :: (Index i, Enum i, Bounded i) => i -> i -> i -> i                                                        -> I4  i
-- | 5-dimensional index clever constructor.
ind5  :: (Index i, Enum i, Bounded i) => i -> i -> i -> i -> i                                                   -> I5  i
-- | 6-dimensional index clever constructor.
ind6  :: (Index i, Enum i, Bounded i) => i -> i -> i -> i -> i -> i                                              -> I6  i
-- | 7-dimensional index clever constructor.
ind7  :: (Index i, Enum i, Bounded i) => i -> i -> i -> i -> i -> i -> i                                         -> I7  i
-- | 8-dimensional index clever constructor.
ind8  :: (Index i, Enum i, Bounded i) => i -> i -> i -> i -> i -> i -> i -> i                                    -> I8  i
-- | 9-dimensional index clever constructor.
ind9  :: (Index i, Enum i, Bounded i) => i -> i -> i -> i -> i -> i -> i -> i -> i                               -> I9  i
-- | 10-dimensional index clever constructor.
ind10 :: (Index i, Enum i, Bounded i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i                          -> I10 i

-- | 11-dimensional index clever constructor.
ind11 :: (Index i, Enum i, Bounded i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i                     -> I11 i
-- | 12-dimensional index clever constructor.
ind12 :: (Index i, Enum i, Bounded i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i                -> I12 i
-- | 13-dimensional index clever constructor.
ind13 :: (Index i, Enum i, Bounded i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i           -> I13 i
-- | 14-dimensional index clever constructor.
ind14 :: (Index i, Enum i, Bounded i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i      -> I14 i
-- | 15-dimensional index clever constructor.
ind15 :: (Index i, Enum i, Bounded i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> I15 i

ind2  a b                           = [a,b]
ind3  a b c                         = [a,b,c]
ind4  a b c d                       = [a,b,c,d]
ind5  a b c d e                     = [a,b,c,d,e]
ind6  a b c d e f                   = [a,b,c,d,e,f]
ind7  a b c d e f g                 = [a,b,c,d,e,f,g]
ind8  a b c d e f g h               = [a,b,c,d,e,f,g,h]
ind9  a b c d e f g h i             = [a,b,c,d,e,f,g,h,i]
ind10 a b c d e f g h i j           = [a,b,c,d,e,f,g,h,i,j]

ind11 a b c d e f g h i j k         = [a,b,c,d,e,f,g,h,i,j,k]
ind12 a b c d e f g h i j k l       = [a,b,c,d,e,f,g,h,i,j,k,l]
ind13 a b c d e f g h i j k l m     = [a,b,c,d,e,f,g,h,i,j,k,l,m]
ind14 a b c d e f g h i j k l m n   = [a,b,c,d,e,f,g,h,i,j,k,l,m,n]
ind15 a b c d e f g h i j k l m n o = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o]

--------------------------------------------------------------------------------

{- Tuple instances. -}

#define INDEX_INSTANCE(TYPE,LAST,GTYPE) instance (Index i, Enum i, Bounded i) => Index (TYPE) where\
{\
type DimLast (TYPE) = i;\
type DimInit (TYPE) = LAST;\
type GIndex  (TYPE) = GTYPE;\
\
size        = size . toGBounds;\
sizes       = sizes . toGBounds;\
isEmpty     = isEmpty . toGBounds;\
range       = fmap fromGIndex . range . toGBounds;\
inRange  bs = inRange (toGBounds bs) . toGIndex;\
next     bs = fromGIndex . next (toGBounds bs) . toGIndex;\
prev     bs = fromGIndex . prev (toGBounds bs) . toGIndex;\
safeElem bs = fromGIndex . safeElem (toGBounds bs) . toGIndex;\
offset   bs = offset (toGBounds bs) . toGIndex;\
index    bs = fromGIndex . index (toGBounds bs);\
ordBounds   = fromGBounds . ordBounds . toGBounds;\
unsafeIndex = fromGIndex . unsafeIndex;\
\
isOverflow  bs = isOverflow  (toGBounds bs) . toGIndex;\
isUnderflow bs = isUnderflow (toGBounds bs) . toGIndex;\
inBounds    bs i | isEmpty bs = ER | isUnderflow bs i = UR | isOverflow bs i = OR | True = IN;
-- incomplete definition, toGIndex and fromGIndex needed.

INDEX_INSTANCE(T2 i, i, I2 i)
fromGIndex ~[a,b] = (a,b);
toGIndex    (a,b) = [a,b];
}

INDEX_INSTANCE(T3 i, T2 i, I3 i)
fromGIndex ~[a,b,c] = (a,b,c);
toGIndex    (a,b,c) = [a,b,c];
}

INDEX_INSTANCE(T4 i, T3 i, I4 i)
fromGIndex ~[a,b,c,d] = (a,b,c,d);
toGIndex    (a,b,c,d) = [a,b,c,d];
}

INDEX_INSTANCE(T5 i, T4 i, I5 i)
fromGIndex ~[a,b,c,d,e] = (a,b,c,d,e);
toGIndex    (a,b,c,d,e) = [a,b,c,d,e];
}

INDEX_INSTANCE(T6 i, T5 i, I6 i)
fromGIndex ~[a,b,c,d,e,f] = (a,b,c,d,e,f);
toGIndex    (a,b,c,d,e,f) = [a,b,c,d,e,f];
}

INDEX_INSTANCE(T7 i, T6 i, I7 i)
fromGIndex ~[a,b,c,d,e,f,g] = (a,b,c,d,e,f,g);
toGIndex    (a,b,c,d,e,f,g) = [a,b,c,d,e,f,g];
}

INDEX_INSTANCE(T8 i, T7 i, I8 i)
fromGIndex ~[a,b,c,d,e,f,g,h] = (a,b,c,d,e,f,g,h);
toGIndex    (a,b,c,d,e,f,g,h) = [a,b,c,d,e,f,g,h];
}

INDEX_INSTANCE(T9 i, T8 i, I9 i)
fromGIndex ~[a,b,c,d,e,f,g,h,i] = (a,b,c,d,e,f,g,h,i);
toGIndex    (a,b,c,d,e,f,g,h,i) = [a,b,c,d,e,f,g,h,i];
}

INDEX_INSTANCE(T10 i, T9 i, I10 i)
fromGIndex ~[a,b,c,d,e,f,g,h,i,j] = (a,b,c,d,e,f,g,h,i,j);
toGIndex    (a,b,c,d,e,f,g,h,i,j) = [a,b,c,d,e,f,g,h,i,j];
}

INDEX_INSTANCE(T11 i, T10 i, I11 i)
fromGIndex ~[a,b,c,d,e,f,g,h,i,j,k] = (a,b,c,d,e,f,g,h,i,j,k);
toGIndex    (a,b,c,d,e,f,g,h,i,j,k) = [a,b,c,d,e,f,g,h,i,j,k];
}

INDEX_INSTANCE(T12 i, T11 i, I12 i)
fromGIndex ~[a,b,c,d,e,f,g,h,i,j,k,l] = (a,b,c,d,e,f,g,h,i,j,k,l);
toGIndex    (a,b,c,d,e,f,g,h,i,j,k,l) = [a,b,c,d,e,f,g,h,i,j,k,l];
}

INDEX_INSTANCE(T13 i, T12 i, I13 i)
fromGIndex ~[a,b,c,d,e,f,g,h,i,j,k,l,m] = (a,b,c,d,e,f,g,h,i,j,k,l,m);
toGIndex    (a,b,c,d,e,f,g,h,i,j,k,l,m) = [a,b,c,d,e,f,g,h,i,j,k,l,m];
}

INDEX_INSTANCE(T14 i, T13 i, I14 i)
fromGIndex ~[a,b,c,d,e,f,g,h,i,j,k,l,m,n] = (a,b,c,d,e,f,g,h,i,j,k,l,m,n);
toGIndex    (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = [a,b,c,d,e,f,g,h,i,j,k,l,m,n];
}

INDEX_INSTANCE(T15 i, T14 i, I15 i)
fromGIndex ~[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o] = (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o);
toGIndex    (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o];
}

#undef INDEX_INSTANCE

--------------------------------------------------------------------------------

unsnoc :: [i] -> ([i], i)
unsnoc    [ ]   = error "unexpected rank in SDP.Index.{IsList (i' :& i)}fromList"
unsnoc    [i]   = ([], i)
unsnoc (i : is) = let (init', last') = unsnoc is in (i : init', last')

(-.) :: (Enum i) => i -> i -> Int
(-.) =  on (-) fromEnum

checkBounds :: (Index i) => (i, i) -> i -> res -> String -> res
checkBounds bnds i res msg = case inBounds bnds i of
  ER -> throw . EmptyRange     $ "in SDP.Index." ++ msg
  UR -> throw . IndexOverflow  $ "in SDP.Index." ++ msg
  OR -> throw . IndexUnderflow $ "in SDP.Index." ++ msg
  IN -> res

{-# INLINE intOffset #-}
intOffset :: (Index i, Num i, Enum i) => (i, i) -> i -> Int
intOffset (l, u) i = checkBounds (l, u) i (fromEnum i - fromEnum l) "offset (default)"

-- | Default bounds for unsigned numeric types.
defUB :: (Index i, Bounded i) => Int -> (i, i)
defUB n = n < 1 ? (unsafeIndex 1, unsafeIndex 0) $ (unsafeIndex 0, unsafeIndex $ n - 1)

