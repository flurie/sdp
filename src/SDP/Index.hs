{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, TypeFamilies, TypeOperators #-}
{-# LANGUAGE DefaultSignatures, CPP #-}

{-# OPTIONS_HADDOCK ignore-exports #-}

{- |
    Module      :  SDP.Index
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
  
  @SDP.Index@ provides 'Index' - service class of types that may represent
  indices, based on "Data.Ix" and @Data.Array.Repa.Shape@ (repa), but has more
  useful functions and may be eaily extended.
  
  In addition, I don't like the Ix implementation: different instances use
  different types of exceptions, some checks restrict my implementations,
  @rangeSize@ is too long to write...
  I refused to create a class based on Ix, because in this case I still need one
  new class.
  
  SDP.Index provides a generic n-dimensional index (type ':&').
  
  Also (since sdp-0.2) are available @OverloadedIndices@ - syntactic sugar based
  on the language extension @OverloadedLists@. For example, instead of the
  inconvenient @es!(ind4 0 1 2 3)@ and just the awful @es!(E:&0:&1:&2:&3)@ you
  can write shorter and more understandable indices: @es![0, 1, 2, 3]@.
  
  OverloadedIndices works only for (':&') and requires a strictly defined number
  of subindexes. But (I hope) no one will use list comprehensions (e.g.
  @es![2..4]@ instead @es![2, 3, 4]@) to specify indexes...
-}
module SDP.Index
(
  -- * Exports
  module Data.Word,
  module Data.Int,
  
  -- * Index class
  InBounds (..), Bounds, Index (..),
  
  -- * N-dimensional index
  E (..), (:&) (..),
  
  -- ** Type synonyms and short constuctors for n-dimensional indices
  I2, I3, I4, I5, I6, I7, I8, I9, I10 , I11, I12, I13, I14, I15,
  
  -- ** Comfortable index constructors
  ind2,  ind3,  ind4,  ind5,  ind6,  ind7,  ind8,  ind9,
  ind10, ind11, ind12, ind13, ind14, ind15,
  
  -- * IndexEQ class
  IndexEQ (..), unsafeBounds, toBounds
)
where

import Prelude ( (++) )
import SDP.SafePrelude

import GHC.Exts ( IsList )
import qualified GHC.Exts as E

import GHC.Types

import Test.QuickCheck

import Data.Word ( Word, Word8, Word16, Word32, Word64 )
import Data.Int  ( Int,  Int8,  Int16,  Int32,  Int64  )

import Data.Tuple

import SDP.Simple

default ()

--------------------------------------------------------------------------------

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

{- |
  Index is service class for Indexed and Bordered. It's the result of combining
  Data.Ix (base) and Data.Array.Repa.Index (repa), but adds several features of
  its own and more polymorphic instances. Also module may be extended.
  
  The default definitions is correct for all (Ord, Enum) types with rank 1.
-}
class (Ord i) => Index i
  where
    {- Commons -}
    
    -- | Returns the number of dimensions that this type of index represents.
    default rank :: (Enum i) => i -> Int
    rank :: i -> Int
    rank = const 1
    
    {- |
      Returns the size (length) of range.
      
      > size (ind3 1 2 3, ind3 4 5 6) == 64
      > size ([1, 2, 3], [4, 5, 6]) == 64 -- with OverloadedLists (since sdp-0.2)
      > size (0, 3) == 4
      > size (3, 0) == 0
    -}
    {-# INLINE size #-}
    default size :: (Enum i) => (i, i) -> Int
    size :: (i, i) -> Int
    size (l, u) = u >= l ? u -. l + 1 $ 0
    
    -- | Returns default range by size.
    {-# INLINE defaultBounds #-}
    defaultBounds :: Int -> (i, i)
    defaultBounds n = (unsafeIndex 0, unsafeIndex $ max 0 n - 1)
    
    -- | Returns index by this offset in default range.
    {-# INLINE unsafeIndex #-}
    default unsafeIndex :: (Enum i) => Int -> i
    unsafeIndex :: Int -> i
    unsafeIndex =  toEnum
    
    {- Enum operations -}
    
    -- | Returns previous index in range.
    default prev  :: (Enum i) => (i, i) -> i -> i
    prev :: (i, i) -> i -> i
    prev (l, u) i | isEmpty (l, u) = er | i <= l = l | i > u = u | True = pred i
      where
        er = throw $ EmptyRange "in SDP.Index.prev (default)"
    
    -- | Returns next index in range.
    default next  :: (Enum i) => (i, i) -> i -> i
    next :: (i, i) -> i -> i
    next (l, u) i | isEmpty (l, u) = er | i >= u = u | i < l = l | True = succ i
      where
        er = throw $ EmptyRange "in SDP.Index.next (default)"
    
    -- | Returns offset (indent) of index in this bounds.
    {-# INLINE offset #-}
    default offset :: (Enum i) => (i, i) -> i -> Int
    offset :: (i, i) -> i -> Int
    offset bnds i = checkBounds bnds i (i -. fst bnds) "offset (default)"
    
    -- | Returns index by this offset in range.
    {-# INLINE index #-}
    default index :: (Enum i) => (i, i) -> Int -> i
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
    default range :: (Enum i) => (i, i) -> [i]
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
    default inRange :: (Enum i) => (i, i) -> i -> Bool
    inRange :: (i, i) -> i -> Bool
    inRange (l, u) i = l <= i && i <= u
    
    -- | Returns the index and bounds status.
    default inBounds :: (Enum i) => (i, i) -> i -> InBounds
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
    default isOverflow :: (Enum i) => (i, i) -> i -> Bool
    isOverflow :: (i, i) -> i -> Bool
    isOverflow (l, u) i = i > u || l > u
    
    -- | Checks if the index is underflow.
    {-# INLINE isUnderflow #-}
    default isUnderflow :: (Enum i) => (i, i) -> i -> Bool
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
    default sizes :: (Enum i) => (i, i) -> [Int]
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

-- | Service type, that represents zero-dimensional index.
data E = E deriving ( Eq, Ord, Show, Read )

instance Arbitrary E where arbitrary = return E

instance Default E where def = E

instance Index E
  where
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

instance Index Char    where defaultBounds = defUB

instance Index Int     where offset = intOffset
instance Index Int8    where offset = intOffset
instance Index Int16   where offset = intOffset
instance Index Int32   where offset = intOffset
instance Index Int64   where offset = intOffset
instance Index Integer where offset = intOffset

instance Index Word    where offset = intOffset; defaultBounds = defUB
instance Index Word8   where offset = intOffset; defaultBounds = defUB
instance Index Word16  where offset = intOffset; defaultBounds = defUB
instance Index Word32  where offset = intOffset; defaultBounds = defUB
instance Index Word64  where offset = intOffset; defaultBounds = defUB

--------------------------------------------------------------------------------

{- N-dimensional index type. -}

{- |
  N-dimensional index type. The type (head :& tail) allows working with any
  finite dimension number.
-}
data tail :& head = !tail :& !head deriving (Eq, Ord, Read)

-- Derived instance doesn't have whitespaces, but I like whitespaces...
instance (Show tail, Show head) => Show (tail :& head)
  where
    show (es :& e) = shows es . showString " :& " $ show e

instance (Arbitrary i, Arbitrary i') => Arbitrary (i' :& i)
  where
    arbitrary = applyArbitrary2 (:&)

instance (Enum i) => Enum (E :& i)
  where
    succ (E :& e) = E :& succ e
    pred (E :& e) = E :& pred e
    
    toEnum n          = E :& toEnum n
    fromEnum (E :& e) = fromEnum e
    
    enumFrom       (E :& first)                       = (E :&) <$> [first .. ]
    enumFromTo     (E :& first) (E :& lst)            = (E :&) <$> [first .. lst]
    enumFromThen   (E :& first) (E :& nxt)            = (E :&) <$> [first, nxt .. ]
    enumFromThenTo (E :& first) (E :& nxt) (E :& lst) = (E :&) <$> [first, nxt .. lst]

instance (Default d, Default d') => Default (d :& d') where def = def :& def

--------------------------------------------------------------------------------

{- N-dimensional index instances. -}

instance (Index i, Enum i) => Index (E :& i)
  where
    rank  (E :& e) = rank e
    
    size  (E :& l, E :& u) = size  (l, u)
    sizes (E :& l, E :& u) = sizes (l, u)
    range (E :& l, E :& u) = (E :&) <$> range (l, u)
    
    next  (E :& l, E :& u) (E :& i) = E :& next (l, u) i
    prev  (E :& l, E :& u) (E :& i) = E :& prev (l, u) i
    
    inRange     (E :& l, E :& u) (E :& i) = inRange       (l, u) i
    isOverflow  (E :& l, E :& u) (E :& i) = isOverflow    (l, u) i
    isUnderflow (E :& l, E :& u) (E :& i) = isUnderflow   (l, u) i
    safeElem    (E :& l, E :& u) (E :& i) = E :& safeElem (l, u) i
    
    isEmpty     (E :& l, E :& u) = isEmpty (l, u)
    ordBounds   (E :& l, E :& u) = let (l', u') = ordBounds (l, u) in (E :& l', E :& u')
    
    offset (E :& l, E :& u) (E :& i) = offset     (l, u) i
    index  (E :& l, E :& u)     n    = E :& index (l, u) n
    
    unsafeIndex n = E :& unsafeIndex n

instance (Index i, Enum i, Bounded i, Index (i' :& i)) => Index (i' :& i :& i)
  where
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
      where  (is :& i) = safeElem bnds ix
    
    next bounds@(ls :& l, us :& u) ix
        | isEmpty bounds = throw $ EmptyRange "in SDP.Index.next (n-dimensional)"
        |     i /= u     = is :& succ i
        |    is /= us    = prev (ls, us) is :& u
        |      True      = ls :& l
      where (is :& i) = safeElem bounds ix
    
    inBounds bs i
      |    isEmpty bs    = ER
      | isUnderflow bs i = UR
      | isOverflow  bs i = OR
      |       True       = IN
    
    inRange     (ls :& l, us :& u) (is :& i) = inRange     (l, u) i && inRange     (ls, us) is
    isOverflow  (ls :& l, us :& u) (is :& i) = isOverflow  (l, u) i || isOverflow  (ls, us) is
    isUnderflow (ls :& l, us :& u) (is :& i) = isUnderflow (l, u) i || isUnderflow (ls, us) is
    
    safeElem    (ls :& l, us :& u) (is :& i) = safeElem    (ls, us) is :& safeElem (l, u) i
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

instance (Index (i' :& i :& i), Index (i' :& i), E.Item (i' :& i) ~~ i, IsList (i' :& i)) => IsList (i' :& i :& i)
  where
    type Item (i' :& i :& i) = i
    
    toList (i' :& i) = E.toList i' ++ [i]
    
    fromList is = let (init', last') = unsnoc is in E.fromList init' :& last'

--------------------------------------------------------------------------------

{- Type synonyms are declared up to 15 dimensions. -}

-- | 2-dimensional index
type  I2  i = E :& i  :& i
-- | 3-dimensional index
type  I3  i = (I2  i) :& i
-- | 4-dimensional index
type  I4  i = (I3  i) :& i
-- | 5-dimensional index
type  I5  i = (I4  i) :& i
-- | 6-dimensional index
type  I6  i = (I5  i) :& i
-- | 7-dimensional index
type  I7  i = (I6  i) :& i
-- | 8-dimensional index
type  I8  i = (I7  i) :& i
-- | 9-dimensional index
type  I9  i = (I8  i) :& i
-- | 10-dimensional index
type  I10 i = (I9  i) :& i
-- | 11-dimensional index
type  I11 i = (I10 i) :& i
-- | 12-dimensional index
type  I12 i = (I11 i) :& i
-- | 13-dimensional index
type  I13 i = (I12 i) :& i
-- | 14-dimensional index
type  I14 i = (I13 i) :& i
-- | i-think-you-guessed-how-much-dimensional index
type  I15 i = (I14 i) :& i

-- | 2-dimensional index clever constructor.
ind2  :: (Index i) => i -> i                                                                  -> I2  i
-- | 3-dimensional index clever constructor.
ind3  :: (Index i) => i -> i -> i                                                             -> I3  i
-- | 4-dimensional index clever constructor.
ind4  :: (Index i) => i -> i -> i -> i                                                        -> I4  i
-- | 5-dimensional index clever constructor.
ind5  :: (Index i) => i -> i -> i -> i -> i                                                   -> I5  i
-- | 6-dimensional index clever constructor.
ind6  :: (Index i) => i -> i -> i -> i -> i -> i                                              -> I6  i
-- | 7-dimensional index clever constructor.
ind7  :: (Index i) => i -> i -> i -> i -> i -> i -> i                                         -> I7  i
-- | 8-dimensional index clever constructor.
ind8  :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i                                    -> I8  i
-- | 9-dimensional index clever constructor.
ind9  :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i                               -> I9  i
-- | 10-dimensional index clever constructor.
ind10 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i                          -> I10 i

-- | 11-dimensional index clever constructor.
ind11 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i                     -> I11 i
-- | 12-dimensional index clever constructor.
ind12 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i                -> I12 i
-- | 13-dimensional index clever constructor.
ind13 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i           -> I13 i
-- | 14-dimensional index clever constructor.
ind14 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i      -> I14 i
-- | 15-dimensional index clever constructor.
ind15 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> I15 i

ind2  a b                             = E :& a :& b
ind3  a b c                           = E :& a :& b :& c
ind4  a b c d                         = E :& a :& b :& c :& d
ind5  a b c d e                       = E :& a :& b :& c :& d :& e
ind6  a b c d e f                     = E :& a :& b :& c :& d :& e :& f
ind7  a b c d e f g                   = E :& a :& b :& c :& d :& e :& f :& g
ind8  a b c d e f g h                 = E :& a :& b :& c :& d :& e :& f :& g :& h
ind9  a b c d e f g h i               = E :& a :& b :& c :& d :& e :& f :& g :& h :& i
ind10 a b c d e f g h i j             = E :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j

ind11 a b c d e f g h i j k           = E :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k
ind12 a b c d e f g h i j k l         = E :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k :& l
ind13 a b c d e f g h i j k l m       = E :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k :& l :& m
ind14 a b c d e f g h i j k l m n     = E :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k :& l :& m :& n
ind15 a b c d e f g h i j k l m n o   = E :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k :& l :& m :& n :& o

--------------------------------------------------------------------------------

-- | IndexEQ is class of index type equality.
class (Index i, Index j) => IndexEQ i j | i -> j
  where
    toIndex   :: i -> j
    fromIndex :: j -> i

instance (Index i, Enum i, Bounded i) => IndexEQ (i,i) (I2 i)
  where
    toIndex (a, b) = ind2 a b
    
    fromIndex (E :& a :& b) = (a, b)

instance (Index i, Enum i, Bounded i) => IndexEQ (i,i,i) (I3 i)
  where
    toIndex (a, b, c) = ind3 a b c
    
    fromIndex (E :& a :& b :& c) = (a, b, c)

instance (Index i, Enum i, Bounded i) => IndexEQ (i,i,i,i) (I4 i)
  where
    toIndex (a, b, c, d) = ind4 a b c d
    
    fromIndex (E :& a :& b :& c :& d) = (a, b, c, d)

instance (Index i, Enum i, Bounded i) => IndexEQ (i,i,i,i,i) (I5 i)
  where
    toIndex (a, b, c, d, e) = ind5 a b c d e
    
    fromIndex (E :& a :& b :& c :& d :& e) = (a, b, c, d, e)

instance (Index i, Enum i, Bounded i) => IndexEQ (i,i,i,i,i,i) (I6 i)
  where
    toIndex (a, b, c, d, e, f) = ind6 a b c d e f
    
    fromIndex (E :& a :& b :& c :& d :& e :& f) = (a, b, c, d, e, f)

instance (Index i, Enum i, Bounded i) => IndexEQ (i,i,i,i,i,i,i) (I7 i)
  where
    toIndex (a, b, c, d, e, f, g) = ind7 a b c d e f g
    
    fromIndex (E :& a :& b :& c :& d :& e :& f :& g) = (a, b, c, d, e, f, g)

instance (Index i, Enum i, Bounded i) => IndexEQ (i,i,i,i,i,i,i,i) (I8 i)
  where
    toIndex (a, b, c, d, e, f, g, h) = ind8 a b c d e f g h
    
    fromIndex (E :& a :& b :& c :& d :& e :& f :& g :& h) = (a, b, c, d, e, f, g, h)

instance (Index i, Enum i, Bounded i) => IndexEQ (i,i,i,i,i,i,i,i,i) (I9 i)
  where
    toIndex (a, b, c, d, e, f, g, h, i) = ind9 a b c d e f g h i
    
    fromIndex (E :& a :& b :& c :& d :& e :& f :& g :& h :& i) = (a, b, c, d, e, f, g, h, i)

instance (Index i, Enum i, Bounded i) => IndexEQ (i,i,i,i,i,i,i,i,i,i) (I10 i)
  where
    toIndex (a, b, c, d, e, f, g, h, i, j) = ind10 a b c d e f g h i j
    
    fromIndex (E :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j) = (a, b, c, d, e, f, g, h, i, j)

instance (Index i, Enum i, Bounded i) => IndexEQ (i,i,i,i,i,i,i,i,i,i,i) (I11 i)
  where
    toIndex (a, b, c, d, e, f, g, h, i, j, k) = ind11 a b c d e f g h i j k
    
    fromIndex (E :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k) = (a, b, c, d, e, f, g, h, i, j, k)

instance (Index i, Enum i, Bounded i) => IndexEQ (i,i,i,i,i,i,i,i,i,i,i,i) (I12 i)
  where
    toIndex (a, b, c, d, e, f, g, h, i, j, k, l) = ind12 a b c d e f g h i j k l
    
    fromIndex (E :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k :& l) = (a, b, c, d, e, f, g, h, i, j, k, l)

instance (Index i, Enum i, Bounded i) => IndexEQ (i,i,i,i,i,i,i,i,i,i,i,i,i) (I13 i)
  where
    toIndex (a, b, c, d, e, f, g, h, i, j, k, l, m) = ind13 a b c d e f g h i j k l m
    
    fromIndex (E :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k :& l :& m) = (a, b, c, d, e, f, g, h, i, j, k, l, m)

instance (Index i, Enum i, Bounded i) => IndexEQ (i,i,i,i,i,i,i,i,i,i,i,i,i,i) (I14 i)
  where
    toIndex (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = ind14 a b c d e f g h i j k l m n
    
    fromIndex (E :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k :& l :& m :& n) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

instance (Index i, Enum i, Bounded i) => IndexEQ (i,i,i,i,i,i,i,i,i,i,i,i,i,i,i) (I15 i)
  where
    toIndex (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = ind15 a b c d e f g h i j k l m n o
    
    fromIndex (E :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k :& l :& m :& n :& o) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

--------------------------------------------------------------------------------

{- Tuple instances. -}

#define INDEX_INSTANCE(TYPEi) instance (Index i, Enum i, Bounded i) => Index TYPEi where\
{\
rank             = const 2;\
size             = size . toBounds;\
sizes            = sizes . toBounds;\
range            = fmap fromIndex . range . toBounds;\
inRange     bs i = toBounds bs `inRange` toIndex i;\
next        bs i = fromIndex $ toBounds bs `next` toIndex i;\
prev        bs i = fromIndex $ toBounds bs `prev` toIndex i;\
inBounds    bs i | isEmpty bs = ER | isUnderflow bs i = UR | isOverflow bs i = OR | True = IN;\
isEmpty          = isEmpty . toBounds;\
isOverflow  bs i = toBounds bs `isOverflow`  toIndex i;\
isUnderflow bs i = toBounds bs `isUnderflow` toIndex i;\
safeElem    bs i = fromIndex $ toBounds bs `safeElem` (toIndex i);\
ordBounds   bs   = let (f, s) = ordBounds $ toBounds bs in (fromIndex f, fromIndex s);\
offset      bs i = toBounds bs `offset` toIndex i;\
index       bs c = fromIndex $ toBounds bs `index` c;\
unsafeIndex      = fromIndex . unsafeIndex;\
}

INDEX_INSTANCE((i,i))
INDEX_INSTANCE((i,i,i))
INDEX_INSTANCE((i,i,i,i))
INDEX_INSTANCE((i,i,i,i,i))
INDEX_INSTANCE((i,i,i,i,i,i))
INDEX_INSTANCE((i,i,i,i,i,i,i))
INDEX_INSTANCE((i,i,i,i,i,i,i,i))
INDEX_INSTANCE((i,i,i,i,i,i,i,i,i))
INDEX_INSTANCE((i,i,i,i,i,i,i,i,i,i))
INDEX_INSTANCE((i,i,i,i,i,i,i,i,i,i,i))
INDEX_INSTANCE((i,i,i,i,i,i,i,i,i,i,i,i))
INDEX_INSTANCE((i,i,i,i,i,i,i,i,i,i,i,i,i))
INDEX_INSTANCE((i,i,i,i,i,i,i,i,i,i,i,i,i,i))
INDEX_INSTANCE((i,i,i,i,i,i,i,i,i,i,i,i,i,i,i))

#undef INDEX_INSTANCE

--------------------------------------------------------------------------------

unsnoc :: [i] -> ([i], i)
unsnoc   [ ]    = error "unexpected rank in SDP.Index.{IsList (i' :& i)}fromList"
unsnoc   [i]    = ([], i)
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

-- | > toBounds (l, u) = (toIndex l, toIndex u)
toBounds :: (IndexEQ i j) => (i, i) -> (j, j)
toBounds (l, u) = (toIndex l, toIndex u)

{-# DEPRECATED unsafeBounds "unsafeBounds deprecated in favour of defaultBounds" #-}
{-# INLINE unsafeBounds #-}
{- |
  Old version of unsafeBounds n is (unsafeIndex 0, unsafeIndex $ n - 1). That
  realization, though not a crutch, but still restricts the permissible limits
  for unsigned index (without this restriction in toEnum can occur underflow),
  which is critical for the indices with a small range (Int8, Word8, etc).
  
  The actual unsafeBounds implementation is 'defaultBounds', which handles the
  case with an empty space more correctly (due to the possibility of overriding
  in instance).
-}
unsafeBounds :: (Index i) => Int -> (i, i)
unsafeBounds = defaultBounds


