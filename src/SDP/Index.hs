{-# LANGUAGE DefaultSignatures, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

{- |
    Module      :  SDP.Index
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC Extensions)
    Stability   :  stable
  
  Index is service class that replaces the more practice-oriented, but limitary
  "Data.Ix".
-}

module SDP.Index
(
  module Data.Word,
  module Data.Int,
  
  Index (..), Bounds, (:&) (..), DimLeak (..), InBounds (..),
  
  I2,   I3,  I4,  I5,  I6,  I7,  I8,  I9, I10 , I11, I12, I13, I14, I15, I16,
  ind2,  ind3,  ind4,  ind5,  ind6,  ind7,  ind8,  ind9,
  ind10, ind11, ind12, ind13, ind14, ind15, ind16,
  
  unsafeBounds
)
where

import Prelude ( (++) )
import SDP.SafePrelude

import Test.QuickCheck

import Data.Default
import Data.Word
import Data.Int

import SDP.Simple

default ()

--------------------------------------------------------------------------------

{- | InBounds - service type that specifies index and bounds status. -}
data InBounds = ER {- ^ Empty Range     -} |
                UR {- ^ Underflow Range -} |
                IN {- ^ Index IN range  -} |
                OR {- ^ Overflow Range  -} deriving ( Eq, Enum )

{- |
    Index is service class for Indexed and Bordered. It's the result of combining
  Data.Ix (base) and Data.Array.Repa.Index (repa), but adds several features of
  its own, for example, more polymorphic instances.
  
    The default definitions is correct for all (Ord, Enum) types with rank 1.
-}

class (Ord i) => Index i
  where
    -- | Returns the number of dimensions that this type of index represents.
    -- Usually - a constant function.
    --
    -- >>> rank ()
    -- 0
    --
    -- >>> rank (1, 6)
    -- 2
    rank  :: i -> Int
    
    -- | Returns the size (length) of range.
    --
    -- >>> size (ind3 1 2 3, ind3 4 5 6)
    -- 64
    --
    -- >>> size (0, 3)
    -- 4
    --
    -- If range in bounds empty, size return 0.
    --
    -- >>> size (3, 0)
    -- 0
    {-# INLINE size #-}
    size  :: (i, i) ->  Int
    
    -- | Returns the sizes (length) of range dimensionwise.
    --
    -- >>> sizes (5, 7)
    -- [3]
    --
    -- >>> sizes (ind4 7 (-1) 12 4, ind4 0 12 9 4)
    -- [0, 14, 0, 1]
    sizes :: (i, i) -> [Int]
    
    -- | Returns the list of indices in this range.
    --
    -- >>> range (2, 7)
    -- [2 .. 7]
    --
    -- but
    --
    -- >>> range (7, 2)
    -- []
    {-# INLINE range #-}
    range :: (i, i) ->  [i]
    
    -- | Returns next index in range.
    next     :: (i, i) -> i -> i
    
    -- | Returns previous index in range.
    prev     :: (i, i) -> i -> i
    
    -- | Returns the index and bounds status.
    inBounds :: (i, i) -> i -> InBounds
    
    -- | Checks if the index is in range
    --
    -- >>> inRange (-5, 4) 3
    -- True
    --
    -- but
    --
    -- >>> inRange (4, -5) 3
    -- False
    {-# INLINE inRange #-}
    inRange   :: (i, i) -> i -> Bool
    
    -- | Checks if the index is overflow.
    -- The default definition for isOverflow and isUnderflow gives the answer
    -- True in the case of an empty range:
    --
    -- >>> isOverflow  (5, -1) x
    -- True
    --
    -- >>> isUnderflow (5, -1) x
    -- True
    --
    -- for all x, because
    --
    -- >>> not $ x `elem` (range (5, -1)) -- range (5, -1) == []
    -- True
    --
    -- and
    --
    -- >>> offset (26, 0) 0
    -- *** Exception: empty range in SDP.Index.offset (default)
    --
    -- because
    --
    -- >>> range (26, 0)
    -- []
    --
    -- and
    --
    -- >>> inRange (26, 0) 0
    -- False
    --
    -- but
    --
    -- >>> isOverflow  (26, 0) 0
    -- True
    --
    -- >>> isUnderflow (26, 0) 0
    -- True
    --
    --   Other definitions in this module follow this rule.
    --
    --   This is not a strict requirement, but a recommended behavior (if it's
    -- relevant to a particular implementation).
    --
    -- Generaly speaking, isOverflow and isUnderflow are not mutually exclusive:
    --
    -- >>> isOverflow  ((-3, 4), (2, 5)) (-4, 6)
    -- True
    --
    -- >>> isUnderflow ((-3, 4), (2, 5)) (-4, 6)
    -- True
    --
    -- And their conjunction doesn't mean that range is empty
    -- Although, for example, for one-dimensional indexes, previous 2 points is
    -- true.
    --
    -- Also, their disjunction is interchangeable with inversion of inRange (in
    -- default definitions)
    {-# INLINE isOverflow  #-}
    isOverflow  :: (i, i) -> i -> Bool
    
    -- | Checks if the index is underflow.
    {-# INLINE isUnderflow #-}
    isUnderflow :: (i, i) -> i -> Bool
    
    -- | Checks if the bounds is empty.
    {-# INLINE isEmpty #-}
    isEmpty     :: (i, i) -> Bool
    
    -- | Returns the index belonging to the given range. Service function.
    safeElem    :: (i, i) -> i -> i
    -- | Returns bounds of nonempty range.
    {-# INLINE ordBounds #-}
    ordBounds   :: (i, i) -> (i, i)
    
    -- | Returns offset (indent) of index in this bounds.
    {-# INLINE offset #-}
    offset      :: (i, i) -> i -> Int
    -- | Returns index by this offset in range.
    {-# INLINE index  #-}
    index       :: (i, i) -> Int -> i
    -- | Returns index by this offset in default range.
    {-# INLINE unsafeIndex #-}
    unsafeIndex :: Int -> i
    
    rank = const 1
    
    default size  :: (Enum i) => (i, i) -> Int
    size (l, u) = u >= l ? u -. l + 1 $ 0
    
    sizes bounds = [ size bounds ]
    
    default range :: (Enum i) => (i, i) -> [i]
    range (l, u) = [ l .. u ]
    
    default prev  :: (Enum i) => (i, i) -> i -> i
    prev (l, u) i
      | isEmpty (l, u) = throw $ EmptyRange "in SDP.Index.prev (default)"
      |     i <= l     = l
      |     i >  u     = u
      |      True      = pred i
    
    default next  :: (Enum i) => (i, i) -> i -> i
    next (l, u) i
      | isEmpty (l, u) = throw $ EmptyRange "in SDP.Index.next (default)"
      |     i >= u     = u
      |     i <  l     = l
      |      True      = succ i
    
    inBounds (l, u) i
      | l > u = ER
      | i > u = OR
      | i < l = UR
      |  True = IN
    
    isEmpty     (l, u)   = l > u
    inRange     (l, u) i = l <= i && i <= u
    isOverflow  (l, u) i = i >  u || l >  u
    isUnderflow (l, u) i = i <  l || l >  u
    
    ordBounds   (f, s)   = (f <= s) ? (f, s) $ (s, f)
    safeElem    (l, u) i = min u (max l i)
    
    default offset  :: (Enum i) => (i, i) -> i -> Int
    offset (l, u) i =  checkBounds (l, u) i (i -. l) "offset (default)"
    
    default index   :: (Enum i) => (i, i) -> Int -> i
    index  (l, u) n =  checkBounds (0, size (l, u) - 1) n res "index (default)"
      where res = toEnum $ n + fromEnum l
    
    default unsafeIndex :: (Enum i) => Int -> i
    unsafeIndex o = toEnum o

--------------------------------------------------------------------------------

{- Basic instances -}

instance Index ()
  where
    rank  = const 0
    size  = const 0
    sizes = const []
    range = const []
    
    next _ _ = ()
    prev _ _ = ()
    
    inBounds    _ _ = IN
    isEmpty       _ = False
    inRange     _ _ = True
    isOverflow  _ _ = False
    isUnderflow _ _ = False
    
    unsafeIndex _ = throw $ EmptyRange "in SDP.Index.unsafeIndex ()"
    index         = const unsafeIndex
    offset  _  _  = 0

instance (Index i, Enum i, Bounded i) => Index (i, i)
  where
    rank = const 2
    
    size  ((l1, l2), (u1, u2)) = s1 * s2
      where
        s1 = size (l1, u1)
        s2 = size (l2, u2)
    
    sizes ((l1, l2), (u1, u2)) = [s1, s2]
      where
        s1 = size (l1, u1)
        s2 = size (l2, u2)
    
    range ((l1, l2), (u1, u2)) = liftA2 (,) r1 r2
      where
        r1 = range (l1, u1)
        r2 = range (l2, u2)
    
    inRange ((l1, l2), (u1, u2)) (i1, i2) = inr1 && inr2
      where
        inr1 = inRange (l1, u1) i1
        inr2 = inRange (l2, u2) i2
    
    next bnds@((_, l2), (u1, u2)) (i1, i2)
        | isEmpty bnds = throw $ EmptyRange "in SDP.Index.prev (i, i)"
        |    s /= u2   = (f, succ s)
        |    f /= u1   = (succ f, l2)
        |     True     = (f, s)
      where
        (f, s) = safeElem bnds (i1, i2)
    
    prev bnds@((l1, l2), (_, u2)) (i1, i2)
        | isEmpty bnds = throw $ EmptyRange "in SDP.Index.prev (i, i)"
        |    s /= l2   = (f, pred s)
        |    f /= l1   = (pred f, u2)
        |     True     = (f, s)
      where
        (f, s) = safeElem bnds (i1, i2)
    
    inBounds bnds i
      |    isEmpty bnds    = ER
      | isUnderflow bnds i = UR
      | isOverflow  bnds i = OR
      |        True        = IN
    
    isEmpty ((l1, l2), (u1, u2)) = e1 || e2
      where
        e1 = isEmpty (l1, u1)
        e2 = isEmpty (l2, u2)
    
    isOverflow  ((l1, l2), (u1, u2)) (i1, i2) = ovr1 || ovr2
      where
        ovr1 = isOverflow  (l1, u1) i1
        ovr2 = isOverflow  (l2, u2) i2
    
    isUnderflow ((l1, l2), (u1, u2)) (i1, i2) = und1 || und2
      where
        und1 = isUnderflow (l1, u1) i1
        und2 = isUnderflow (l2, u2) i2
    
    safeElem ((l1, l2), (u1, u2)) (i1, i2) = (se1, se2)
      where
        se1 = safeElem (l1, u1) i1
        se2 = safeElem (l2, u2) i2
    
    ordBounds ((l1, l2), (u1, u2)) = ((f1, f2), (s1, s2))
      where
        (f1, s1) = ordBounds (l1, u1)
        (f2, s2) = ordBounds (l2, u2)
    
    offset ((l1, l2), (u1, u2)) (i1, i2) = o1 * n + o2
      where
        o1 = offset (l1, u1) i1
        o2 = offset (l2, u2) i2; n = size (l2, u2)
    
    index ((l1, l2), (u1, u2)) c = (index (l1, u1) i', j)
      where
        (i', j) = c /. (l2, u2)
    
    unsafeIndex c = (unsafeIndex i', j)
      where
        (i', j) = uns c

instance (Index i, Enum i, Bounded i) => Index (i, i, i)
  where
    rank = const 3
    
    size  ((l1, l2, l3), (u1, u2, u3)) = s1 * s2 * s3
      where
        s1 = size (l1, u1)
        s2 = size (l2, u2)
        s3 = size (l3, u3)
    
    sizes ((l1, l2, l3), (u1, u2, u3)) = [s1, s2, s3]
      where
        s1 = size (l1, u1)
        s2 = size (l2, u2)
        s3 = size (l3, u3)
    
    range ((l1, l2, l3), (u1, u2, u3)) = liftA3 (,,) r1 r2 r3
      where
        r1 = range (l1, u1)
        r2 = range (l2, u2)
        r3 = range (l3, u3)
    
    next bnds@((_, l2, l3), (u1, u2, u3)) (i1, i2, i3)
        | isEmpty bnds = throw $ EmptyRange "in SDP.Index.prev (i, i, i)"
        |    t /= u3   = (f, s, succ t)
        |    s /= u2   = (f, succ s, l3)
        |    f /= u1   = (succ f, l2, l3)
        |     True     = (f, s, t)
      where
        (f, s, t) = safeElem bnds (i1, i2, i3)
    
    prev bnds@((l1, l2, l3), (_, u2, u3)) (i1, i2, i3)
        | isEmpty bnds = throw $ EmptyRange "in SDP.Index.prev (i, i, i)"
        |    t /= l3   = (f, s, pred t)
        |    s /= l2   = (f, pred s, u3)
        |    f /= l1   = (pred f, u2, u3)
        |     True     = (f, s, t)
      where
        (f, s, t) = safeElem bnds (i1, i2, i3)
    
    inBounds bnds i
      |    isEmpty bnds    = ER
      | isUnderflow bnds i = UR
      | isOverflow  bnds i = OR
      |        True        = IN
    
    isEmpty ((l1, l2, l3), (u1, u2, u3)) = e1 || e2 || e3
      where
        e1 = isEmpty (l1, u1)
        e2 = isEmpty (l2, u2)
        e3 = isEmpty (l3, u3)
    
    inRange ((l1, l2, l3), (u1, u2, u3)) (i1, i2, i3) = inr1 && inr2 && inr3
      where
        inr1 = inRange (l1, u1) i1
        inr2 = inRange (l2, u2) i2
        inr3 = inRange (l3, u3) i3
    
    isOverflow  ((l1, l2, l3), (u1, u2, u3)) (i1, i2, i3) = ovr1 || ovr2 || ovr3
      where
        ovr1 = isOverflow  (l1, u1) i1
        ovr2 = isOverflow  (l2, u2) i2
        ovr3 = isOverflow  (l3, u3) i3
    
    isUnderflow ((l1, l2, l3), (u1, u2, u3)) (i1, i2, i3) = und1 || und2 || und3
      where
        und1 = isUnderflow (l1, u1) i1
        und2 = isUnderflow (l2, u2) i2
        und3 = isUnderflow (l3, u3) i3
    
    safeElem ((l1, l2, l3), (u1, u2, u3)) (i1, i2, i3) = (se1, se2, se3)
      where
        se1 = safeElem (l1, u1) i1
        se2 = safeElem (l2, u2) i2
        se3 = safeElem (l3, u3) i3
    
    ordBounds ((l1, l2, l3), (u1, u2, u3)) = ((f1, f2, f3), (s1, s2, s3))
      where
        (f1, s1) = ordBounds (l1, u1)
        (f2, s2) = ordBounds (l2, u2)
        (f3, s3) = ordBounds (l3, u3)
    
    offset ((l1, l2, l3), (u1, u2, u3)) (i1, i2, i3) = (o1 * n + o2) * m + o3
      where
        o1 = offset (l1, u1) i1
        o2 = offset (l2, u2) i2; n = size (l2, u2)
        o3 = offset (l3, u3) i3; m = size (l3, u3)
    
    index ((l1, l2, l3), (u1, u2, u3)) c = (index (l1, u1) i', j, k)
      where
        (inj, k) = c   /. (l3, u3)
        (i', j)  = inj /. (l2, u2)
    
    unsafeIndex c = (unsafeIndex i', j, k)
      where
        (inj, k) = uns c
        (i', j)  = uns inj

instance (Index i, Enum i, Bounded i) => Index (i, i, i, i)
  where
    rank = const 4
    
    size  ((l1, l2, l3, l4), (u1, u2, u3, u4)) = s1 * s2 * s3 * s4
      where
        s1 = size (l1, u1)
        s2 = size (l2, u2)
        s3 = size (l3, u3)
        s4 = size (l4, u4)
    
    sizes ((l1, l2, l3, l4), (u1, u2, u3, u4)) = [s1, s2, s3, s4]
      where
        s1 = size (l1, u1)
        s2 = size (l2, u2)
        s3 = size (l3, u3)
        s4 = size (l4, u4)
    
    range ((l1, l2, l3, l4), (u1, u2, u3, u4)) = liftA3 (,,,) r1 r2 r3 <*> r4
      where
        r1 = range (l1, u1)
        r2 = range (l2, u2)
        r3 = range (l3, u3)
        r4 = range (l4, u4)
    
    next bnds@((_, l2, l3, l4), (u1, u2, u3, u4)) (i1, i2, i3, i4)
        | isEmpty bnds = throw $ EmptyRange "in SDP.Index.prev (i, i, i, i)"
        |   f' /= u4   = (f, s, t, succ f')
        |    t /= u3   = (f, s, succ t, l4)
        |    s /= u2   = (f, succ s, l3, l4)
        |    f /= u1   = (succ f, l2, l3, l4)
        |     True     = (f, s, t, f')
      where
        (f, s, t, f') = safeElem bnds (i1, i2, i3, i4)
    
    prev bnds@((l1, l2, l3, l4), (_, u2, u3, u4)) (i1, i2, i3, i4)
        | isEmpty bnds = throw $ EmptyRange "in SDP.Index.prev (i, i, i, i)"
        |   f' /= l4   = (f, s,   t, pred f')
        |    t /= l3   = (f, s,   pred t, u4)
        |    s /= l2   = (f,  pred s, u3, u4)
        |    f /= l1   = (pred f, u2, u3, u4)
        |     True     = (f, s, t, f')
      where
        (f, s, t, f') = safeElem bnds (i1, i2, i3, i4)
    
    inBounds bnds i
      |    isEmpty bnds    = ER
      | isUnderflow bnds i = UR
      | isOverflow  bnds i = OR
      |        True        = IN
    
    isEmpty ((l1, l2, l3, l4), (u1, u2, u3, u4)) = e1 || e2 || e3 || e4
      where
        e1 = isEmpty (l1, u1)
        e2 = isEmpty (l2, u2)
        e3 = isEmpty (l3, u3)
        e4 = isEmpty (l4, u4)
    
    inRange ((l1, l2, l3, l4), (u1, u2, u3, u4)) (i1, i2, i3, i4) = inr1 && inr2 && inr3 && inr4
      where
        inr1 = inRange (l1, u1) i1
        inr2 = inRange (l2, u2) i2
        inr3 = inRange (l3, u3) i3
        inr4 = inRange (l4, u4) i4
    
    isOverflow  ((l1, l2, l3, l4), (u1, u2, u3, u4)) (i1, i2, i3, i4) = ovr1 || ovr2 || ovr3 || ovr4
      where
        ovr1 = isOverflow  (l1, u1) i1
        ovr2 = isOverflow  (l2, u2) i2
        ovr3 = isOverflow  (l3, u3) i3
        ovr4 = isOverflow  (l4, u4) i4
    
    isUnderflow ((l1, l2, l3, l4), (u1, u2, u3, u4)) (i1, i2, i3, i4) = und1 || und2 || und3 || und4
      where
        und1 = isUnderflow (l1, u1) i1
        und2 = isUnderflow (l2, u2) i2
        und3 = isUnderflow (l3, u3) i3
        und4 = isUnderflow (l4, u4) i4
    
    safeElem ((l1, l2, l3, l4), (u1, u2, u3, u4)) (i1, i2, i3, i4) = (se1, se2, se3, se4)
      where
        se1 = safeElem (l1, u1) i1
        se2 = safeElem (l2, u2) i2
        se3 = safeElem (l3, u3) i3
        se4 = safeElem (l4, u4) i4
    
    ordBounds ((l1, l2, l3, l4), (u1, u2, u3, u4)) = ((f1, f2, f3, f4), (s1, s2, s3, s4))
      where
        (f1, s1) = ordBounds (l1, u1)
        (f2, s2) = ordBounds (l2, u2)
        (f3, s3) = ordBounds (l3, u3)
        (f4, s4) = ordBounds (l4, u4)
    
    offset ((l1, l2, l3, l4), (u1, u2, u3, u4)) (i1, i2, i3, i4) = ((o1 * n + o2) * m + o3) * v + o4
      where
        o1 = offset (l1, u1) i1
        o2 = offset (l2, u2) i2; n = size (l2, u2)
        o3 = offset (l3, u3) i3; m = size (l3, u3)
        o4 = offset (l4, u4) i4; v = size (l4, u4)
    
    index ((l1, l2, l3, l4), (u1, u2, u3, u4)) c = (index (l1, u1) i', j, k, l)
      where
        (injmk, l) = c     /. (l4, u4)
        (inj, k)   = injmk /. (l3, u3)
        (i', j)    = inj   /. (l2, u2)
    
    unsafeIndex c = (unsafeIndex i', j, k, l)
      where
        (injmk, l) = uns c
        (inj, k)   = uns injmk
        (i', j)    = uns inj

instance (Index i, Enum i, Bounded i) => Index (i, i, i, i, i)
  where
    rank = const 5
    
    size  ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) = s1 * s2 * s3 * s4 * s5
      where
        s1 = size (l1, u1)
        s2 = size (l2, u2)
        s3 = size (l3, u3)
        s4 = size (l4, u4)
        s5 = size (l5, u5)
    
    sizes ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) = [s1, s2, s3, s4, s5]
      where
        s1 = size (l1, u1)
        s2 = size (l2, u2)
        s3 = size (l3, u3)
        s4 = size (l4, u4)
        s5 = size (l5, u5)
    
    range ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) = liftA3 (,,,,) r1 r2 r3 <*> r4 <*> r5
      where
        r1 = range (l1, u1)
        r2 = range (l2, u2)
        r3 = range (l3, u3)
        r4 = range (l4, u4)
        r5 = range (l5, u5)
    
    next bnds@((_, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) (i1, i2, i3, i4, i5)
        | isEmpty bnds = throw $ EmptyRange "in SDP.Index.prev (i, i, i, i, i)"
        |  f'' /= u5   = (f, s, t, f',  succ f'')
        |   f' /= u4   = (f, s, t,  succ f', f'')
        |    t /= u3   = (f, s,   succ t, l4, l5)
        |    s /= u2   = (f,  succ s, l3, l4, l5)
        |    f /= u1   = (succ f, l2, l3, l4, l5)
        |     True     = (f, s, t, f', f'')
      where
        (f, s, t, f', f'') = safeElem bnds (i1, i2, i3, i4, i5)
    
    prev bnds@((l1, l2, l3, l4, l5), (_, u2, u3, u4, u5)) (i1, i2, i3, i4, i5)
        | isEmpty bnds = throw $ EmptyRange "in SDP.Index.prev (i, i, i, i, i)"
        |  f'' /= l5   = (f, s, t, f',   pred f'')
        |   f' /= l4   = (f, s, t,   pred f', u5)
        |    t /= l3   = (f, s,   pred t, u4, u5)
        |    s /= l2   = (f,  pred s, u3, u4, u5)
        |    f /= l1   = (pred f, u2, u3, u4, u5)
        |     True     = (f, s, t, f', f'')
      where
        (f, s, t, f', f'') = safeElem bnds (i1, i2, i3, i4, i5)
    
    inBounds bnds i
      |    isEmpty bnds    = ER
      | isUnderflow bnds i = UR
      | isOverflow  bnds i = OR
      |        True        = IN
    
    isEmpty ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) = e1 || e2 || e3 || e4 || e5
      where
        e1 = isEmpty (l1, u1)
        e2 = isEmpty (l2, u2)
        e3 = isEmpty (l3, u3)
        e4 = isEmpty (l4, u4)
        e5 = isEmpty (l5, u5)
    
    inRange ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) (i1, i2, i3, i4, i5) = inr1 && inr2 && inr3 && inr4 && inr5
      where
        inr1 = inRange (l1, u1) i1
        inr2 = inRange (l2, u2) i2
        inr3 = inRange (l3, u3) i3
        inr4 = inRange (l4, u4) i4
        inr5 = inRange (l5, u5) i5
    
    isOverflow  ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) (i1, i2, i3, i4, i5) = ovr1 || ovr2 || ovr3 || ovr4 || ovr5
      where
        ovr1 = isOverflow  (l1, u1) i1
        ovr2 = isOverflow  (l2, u2) i2
        ovr3 = isOverflow  (l3, u3) i3
        ovr4 = isOverflow  (l4, u4) i4
        ovr5 = isOverflow  (l5, u5) i5
    
    isUnderflow ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) (i1, i2, i3, i4, i5) = und1 || und2 || und3 || und4 || und5
      where
        und1 = isUnderflow (l1, u1) i1
        und2 = isUnderflow (l2, u2) i2
        und3 = isUnderflow (l3, u3) i3
        und4 = isUnderflow (l4, u4) i4
        und5 = isUnderflow (l5, u5) i5
    
    safeElem ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) (i1, i2, i3, i4, i5) = (se1, se2, se3, se4, se5)
      where
        se1 = safeElem (l1, u1) i1
        se2 = safeElem (l2, u2) i2
        se3 = safeElem (l3, u3) i3
        se4 = safeElem (l4, u4) i4
        se5 = safeElem (l5, u5) i5
    
    ordBounds ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) = ((f1, f2, f3, f4, f5), (s1, s2, s3, s4, s5))
      where
        (f1, s1) = ordBounds (l1, u1)
        (f2, s2) = ordBounds (l2, u2)
        (f3, s3) = ordBounds (l3, u3)
        (f4, s4) = ordBounds (l4, u4)
        (f5, s5) = ordBounds (l5, u5)
    
    offset ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) (i1, i2, i3, i4, i5) = (((o1 * n + o2) * m + o3) * v + o4) * h + o5
      where
        o1 = offset (l1, u1) i1
        o2 = offset (l2, u2) i2; n = size (l2, u2)
        o3 = offset (l3, u3) i3; m = size (l3, u3)
        o4 = offset (l4, u4) i4; v = size (l4, u4)
        o5 = offset (l5, u5) i5; h = size (l5, u5)
    
    index ((l1, l2, l3, l4, l5), (u1, u2, u3, u4, u5)) c = (index (l1, u1) i', j, k, l, t)
      where
        (injmkhl, t) = c       /. (l5, u5)
        (injmk, l)   = injmkhl /. (l4, u4)
        (inj, k)     = injmk   /. (l3, u3)
        (i', j)      = inj     /. (l2, u2)
    
    unsafeIndex c  = (unsafeIndex i', j, k, l, t)
      where
        (injmkhl, t) = uns c
        (injmk, l)   = uns injmkhl
        (inj, k)     = uns injmk
        (i', j)      = uns inj

--------------------------------------------------------------------------------

-- | Service type for future use.
newtype DimLeak = DimLeak Word deriving (Eq, Ord, Show, Read)

--------------------------------------------------------------------------------

-- | Char indices begins from \SOH, because type is unsigned.
instance Index Char    where unsafeIndex = defUI

instance Index Integer where offset = intOffset

instance Index Int     where offset = intOffset
instance Index Int8    where offset = intOffset
instance Index Int16   where offset = intOffset
instance Index Int32   where offset = intOffset
instance Index Int64   where offset = intOffset

-- | Word   indices begins from 1, because type is unsigned.
instance Index Word    where offset = intOffset; unsafeIndex = defUI
-- | Word8  indices begins from 1, because type is unsigned.
instance Index Word8   where offset = intOffset; unsafeIndex = defUI
-- | Word16 indices begins from 1, because type is unsigned.
instance Index Word16  where offset = intOffset; unsafeIndex = defUI
-- | Word32 indices begins from 1, because type is unsigned.
instance Index Word32  where offset = intOffset; unsafeIndex = defUI
-- | Word64 indices begins from 1, because type is unsigned.
instance Index Word64  where offset = intOffset; unsafeIndex = defUI

--------------------------------------------------------------------------------

instance (Index i, Enum i) => Index (() :& i)
  where
    rank  (() :& e) = rank e
    
    size  (() :& l, () :& u) = size  (l, u)
    sizes (() :& l, () :& u) = sizes (l, u)
    range (() :& l, () :& u) = (() :&) <$> range (l, u)
    
    next  (() :& l, () :& u) (() :& i) = () :& next (l, u) i
    prev  (() :& l, () :& u) (() :& i) = () :& prev (l, u) i
    
    inRange     (() :& l, () :& u) (() :& i) = inRange        (l, u) i
    isOverflow  (() :& l, () :& u) (() :& i) = isOverflow     (l, u) i
    isUnderflow (() :& l, () :& u) (() :& i) = isUnderflow    (l, u) i
    safeElem    (() :& l, () :& u) (() :& i) = () :& safeElem (l, u) i
    
    isEmpty     (() :& l, () :& u) = isEmpty (l, u)
    ordBounds   (() :& l, () :& u) = (() :& l', () :& u')
      where
        (l', u') = ordBounds (l, u)
    
    offset (() :& l, () :& u) (() :& i) = offset      (l, u) i
    index  (() :& l, () :& u)     n     = () :& index (l, u) n
    
    unsafeIndex n = () :& unsafeIndex n

instance (Index i, Enum i, Bounded i, Index (i' :& i)) => Index (i' :& i :& i)
  where
    rank (rs :& _) = rank rs + 1
    
    size  (ls :& l, us :& u) = size (l, u) * size (ls, us)
    -- [internal]: O(n ^ 2) sizes. Not critial, but needed to rewrite.
    sizes (ls :& l, us :& u) = sizes (ls, us) ++ sizes (l, u)
    range (ls :& l, us :& u) = liftA2 (:&) (range (ls, us)) (range (l, u))
    
    -- [internal]: prev and next uses safeElem. Needed to rewrite.
    prev bounds@(ls :& l, us :& u) ix
        | isEmpty bounds = throw $ EmptyRange "in SDP.Index.prev (n-dimensional)"
        |     i /= l     = is :& pred i
        |    is /= ls    = prev (ls, us) is :& u
        |      True      = ls :& l
      where (is :& i) = safeElem bounds ix
    
    next bounds@(ls :& l, us :& u) ix
        | isEmpty bounds = throw $ EmptyRange "in SDP.Index.next (n-dimensional)"
        |     i /= u     = is :& succ i
        |    is /= us    = prev (ls, us) is :& u
        |      True      = ls :& l
      where (is :& i) = safeElem bounds ix
    
    inRange     (ls :& l, us :& u) (is :& i) = inRange     (l, u) i && inRange     (ls, us) is
    isOverflow  (ls :& l, us :& u) (is :& i) = isOverflow  (l, u) i || isOverflow  (ls, us) is
    isUnderflow (ls :& l, us :& u) (is :& i) = isUnderflow (l, u) i || isUnderflow (ls, us) is
    -- [internal]: safeElem - service function, may be removed after rewriting prev and next.
    safeElem    (ls :& l, us :& u) (is :& i) = safeElem    (ls, us) is :& safeElem (l, u) i
    isEmpty     (ls :& l, us :& u) = isEmpty (l, u) || isEmpty (ls, us)
    ordBounds   (ls :& l, us :& u) = (ls' :& l', us' :& u')
      where
        (ls', us') = ordBounds (ls, us)
        (l', u')   = ordBounds (l, u)
    
    index bnds c = checkBounds (0, size bnds - 1) c res "index (n-dimensional)"
      where
        res = index (ls, us) cs :& (unsafeIndex i)
        (cs, i) = divMod c $ size (l, u)
        (ls :& l, us :& u) = bnds
    
    offset bnds ix@(is :& i) = checkBounds bnds ix res "offset (n-dimensional)"
      where
        res = offset (ls, us) is * size (l, u) + offset (l, u) i
        (ls :& l, us :& u) = bnds
    
    unsafeIndex  c  = (unsafeIndex cs) :& (unsafeIndex i)
      where
        (cs, i) = divMod c $ maxBound

--------------------------------------------------------------------------------

{- |
  N-dimensional  index  type. The  type  (head :& tail) allows  working with any
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

instance (Enum i) => Enum (() :& i)
  where
    succ (() :& e) = () :& succ e
    pred (() :& e) = () :& pred e
    
    toEnum n           = () :& toEnum n
    fromEnum (() :& e) = fromEnum e
    
    enumFrom       (() :& first)                         = (() :&) <$> [first .. ]
    enumFromTo     (() :& first) (() :& lst)             = (() :&) <$> [first .. lst]
    enumFromThen   (() :& first) (() :& nxt)             = (() :&) <$> [first, nxt .. ]
    enumFromThenTo (() :& first) (() :& nxt) (() :& lst) = (() :&) <$> [first, nxt .. lst]

instance (Default d, Default d') => Default (d :& d') where def = def :& def


-- | Type synonym for very long type annotations, e.g.
-- Bounds (Int, Int, Int, Int, Int) is same as
-- ((Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int))
type Bounds i = (i, i)

{-
  Type synonyms are declared up to 16 dimensions (Fortran 2003 permits up to 15,
  and I would like to compete with a 16-years old language at least in this).
  Although, for example, with the tuple library (supports 32-element tuples)
  I am still far away.
-}

-- | 2-dimensional index
type  I2  i  = () :& i :& i
-- | 3-dimensional index
type  I3  i  = (I2  i) :& i
-- | 4-dimensional index
type  I4  i  = (I3  i) :& i
-- | 5-dimensional index
type  I5  i  = (I4  i) :& i
-- | 6-dimensional index
type  I6  i  = (I5  i) :& i
-- | 7-dimensional index
type  I7  i  = (I6  i) :& i
-- | 8-dimensional index
type  I8  i  = (I7  i) :& i
-- | 9-dimensional index
type  I9  i  = (I8  i) :& i
-- | 10-dimensional index
type  I10 i  = (I9  i) :& i
-- | 11-dimensional index
type  I11 i  = (I10 i) :& i
-- | 12-dimensional index
type  I12 i  = (I11 i) :& i
-- | 13-dimensional index
type  I13 i  = (I12 i) :& i
-- | 14-dimensional index
type  I14 i  = (I13 i) :& i
-- | 15-dimensional index
type  I15 i  = (I14 i) :& i
-- | i-think-you-guessed-how-much-dimensional index
type  I16 i  = (I15 i) :& i

-- | 2-dimensional index clever constructor.
ind2  :: (Index i) => i -> i                                                                       -> I2  i
-- | 3-dimensional index clever constructor.
ind3  :: (Index i) => i -> i -> i                                                                  -> I3  i
-- | 4-dimensional index clever constructor.
ind4  :: (Index i) => i -> i -> i -> i                                                             -> I4  i
-- | 5-dimensional index clever constructor.
ind5  :: (Index i) => i -> i -> i -> i -> i                                                        -> I5  i
-- | 6-dimensional index clever constructor.
ind6  :: (Index i) => i -> i -> i -> i -> i -> i                                                   -> I6  i
-- | 7-dimensional index clever constructor.
ind7  :: (Index i) => i -> i -> i -> i -> i -> i -> i                                              -> I7  i
-- | 8-dimensional index clever constructor.
ind8  :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i                                         -> I8  i
-- | 9-dimensional index clever constructor.
ind9  :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i                                    -> I9  i
-- | 10-dimensional index clever constructor.
ind10 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i                               -> I10 i

-- | 11-dimensional index clever constructor.
ind11 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i                          -> I11 i
-- | 12-dimensional index clever constructor.
ind12 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i                     -> I12 i
-- | 13-dimensional index clever constructor.
ind13 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i                -> I13 i
-- | 14-dimensional index clever constructor.
ind14 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i           -> I14 i
-- | 15-dimensional index clever constructor.
ind15 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i      -> I15 i
-- | 16-dimensional index clever constructor.
ind16 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> I16 i

ind2  a b                             = () :& a :& b
ind3  a b c                           = () :& a :& b :& c
ind4  a b c d                         = () :& a :& b :& c :& d
ind5  a b c d e                       = () :& a :& b :& c :& d :& e
ind6  a b c d e f                     = () :& a :& b :& c :& d :& e :& f
ind7  a b c d e f g                   = () :& a :& b :& c :& d :& e :& f :& g
ind8  a b c d e f g h                 = () :& a :& b :& c :& d :& e :& f :& g :& h
ind9  a b c d e f g h i               = () :& a :& b :& c :& d :& e :& f :& g :& h :& i
ind10 a b c d e f g h i j             = () :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j

ind11 a b c d e f g h i j k           = () :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k
ind12 a b c d e f g h i j k l         = () :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k :& l
ind13 a b c d e f g h i j k l m       = () :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k :& l :& m
ind14 a b c d e f g h i j k l m n     = () :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k :& l :& m :& n
ind15 a b c d e f g h i j k l m n o   = () :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k :& l :& m :& n :& o
ind16 a b c d e f g h i j k l m n o p = () :& a :& b :& c :& d :& e :& f :& g :& h :& i :& j :& k :& l :& m :& n :& o :& p

--------------------------------------------------------------------------------

(-.) :: (Enum i) => i -> i -> Int
(-.) =  on (-) fromEnum

(/.) :: (Index i) => Int -> (i, i) -> (Int, i)
(/.) a bnds = (d, index bnds m)
  where
    (d, m) = a `divMod` (size bnds)

uns :: (Index i, Bounded i) => Int -> (Int, i)
uns a = case a `divMod` maxBound of (d, m) -> (d, unsafeIndex m)

checkBounds :: (Index i) => (i, i) -> i -> res -> String -> res
checkBounds bnds i res msg = case inBounds bnds i of
  ER -> throw . EmptyRange     $ "in SDP.Index." ++ msg
  UR -> throw . IndexOverflow  $ "in SDP.Index." ++ msg
  OR -> throw . IndexUnderflow $ "in SDP.Index." ++ msg
  IN -> res

{-# INLINE intOffset #-}
intOffset :: (Index i, Num i, Enum i) => (i, i) -> i -> Int
intOffset (l, u) i = checkBounds (l, u) i (fromEnum i - fromEnum l) "offset (default)"

{-# INLINE defUI #-}
defUI   :: (Enum i) => Int -> i
defUI o =  toEnum $ max 0 (succ o)


{-# INLINE unsafeBounds #-}
-- | unsafeBounds n is shortcut for (unsafeIndex 0, unsafeIndex $ n - 1)
unsafeBounds :: (Index i) => Int -> (i, i)
unsafeBounds n = (unsafeIndex 0, unsafeIndex $ n - 1)

