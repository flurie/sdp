{-# LANGUAGE DefaultSignatures, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module SDP.Index
(
  module Data.Word,
  module Data.Int,
  
  Index (..), Bounds (..), (:&) (..),
  
  I2  (..), I3  (..), I4  (..), I5  (..), I6  (..), I7  (..), I8  (..), I9 (..),
  I10 (..), I11 (..), I12 (..), I13 (..), I14 (..), I15 (..), I16 (..),
  ind2,  ind3,  ind4,  ind5,  ind6,  ind7,  ind8,  ind9,
  ind10, ind11, ind12, ind13, ind14, ind15, ind16
)
where

import Prelude ( (++) )
import SDP.SafePrelude

import SDP.Simple
import Data.Word
import Data.Int

--------------------------------------------------------------------------------

{-
    Index is service class for Indexed and Bordered it's the result of combining
  Data.Ix and Data.Array.Repa.Index, but  adds several features of its own,  for
  example, more polymorphic instances.
  
    The default definitions is correct for all (Ord, Enum) types with rank 1.
  
    The bounds  must be specified  in ascending order  in all functions,  except
  ordBounds:
    inRange (-5, 4) 3  == True     but inRange (4, -5) 3  == False
    size    ('a', 'd') == 4        but size    ('d', 'a') == 0
    range   (2, 7)     == [2 .. 7] but range   (7, 2)     == []
  
    The default definition for isOverflow and isUnderflow gives the answer  True
  in the case of an empty range:
    isOverflow (5, -1) x == True -- not (x `elem` [])
  Other definitions in this module follow this rule.
  
    This isn't a strict requirement, but recommended behavior, where appropriate
  If the behavior  of a particular implementation of a function isn't explicitly
  specified  in the documentation (and is not self-evident for this type),  then
  any other behavior should be considered as unplanned functionality.
  
  Generaly speaking, isOverflow and isUnderflow are not mutually exclusive:
    isOverflow  ((-3, 4), (2, 5)) (-4, 6) == True
    isUnderflow ((-3, 4), (2, 5)) (-4, 6) == True
  And their conjunction is not interchangeable with inversion of inRange:
    offset ('z', 'a') 'a'
    >>> *** Exception: empty range in SDP.Index.offset (default)
  because
    range       ('z', 'a')     ==  [ ]
    inRange     ('z', 'a') 'a' == False
    isOverflow  ('z', 'a') 'a' == False
    isUnderflow ('z', 'a') 'a' == False
-}

class (Ord i) => Index i
  where
    rank  ::    i   ->  Int
    size  :: (i, i) ->  Int
    sizes :: (i, i) -> [Int]
    range :: (i, i) ->  [i]
    
    next  :: (i, i) ->   i   -> i
    prev  :: (i, i) ->   i   -> i
    
    isEmpty     :: (i, i) -> Bool
    inRange     :: (i, i) -> i -> Bool
    isOverflow  :: (i, i) -> i -> Bool
    isUnderflow :: (i, i) -> i -> Bool
    
    safeElem    :: (i, i) -> i -> i
    ordBounds   :: (i, i) -> (i, i)
    offset      :: (i, i) -> i -> Int
    index       :: (i, i) -> Int -> i
    
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
    unsafeIndex = toEnum

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
    isEmpty  = const False
    index    = const unsafeIndex
    inRange     _ _ = True
    isOverflow  _ _ = False
    isUnderflow _ _ = False
    unsafeIndex x = case x <=> 0 of
      EQ -> ()
      GT -> throw $ IndexOverflow  "in SDP.Index.unsafeIndex ()"
      LT -> throw $ IndexUnderflow "in SDP.Index.unsafeIndex ()"
    
    offset _ _ = 0

instance Index Char

instance Index Integer where offset = intOffset

instance Index Int     where offset = intOffset
instance Index Int8    where offset = intOffset
instance Index Int16   where offset = intOffset
instance Index Int32   where offset = intOffset
instance Index Int64   where offset = intOffset

instance Index Word    where offset = intOffset
instance Index Word8   where offset = intOffset
instance Index Word16  where offset = intOffset
instance Index Word32  where offset = intOffset
instance Index Word64  where offset = intOffset

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
    
    size  (ls :& l, us :& u) = size  (l, u)   *  size  (ls, us)
    -- [internal]: O(n ^ 2) sizes. Not critial, but needed to rewrite.
    sizes (ls :& l, us :& u) = sizes (ls, us) ++ sizes (l, u)
    range (ls :& l, us :& u) = liftA2 (:&) (range (ls, us)) (range (l, u))
    
    -- [internal]: prev and next uses safeElem. Needed to rewrite.
    prev bounds@(ls :& l, us :& u) index
        | isEmpty bounds = throw $ EmptyRange "in SDP.Index.prev (n-dimensional)"
        |     i /= l     = is :& pred i
        |    is /= ls    = prev (ls, us) is :& u
        |      True      = ls :& l
      where (is :& i) = safeElem bounds index
    
    next bounds@(ls :& l, us :& u) index
        | isEmpty bounds = throw $ EmptyRange "in SDP.Index.next (n-dimensional)"
        |     i /= u     = is :& succ i
        |    is /= us    = prev (ls, us) is :& u
        |      True      = ls :& l
      where (is :& i) = safeElem bounds index
    
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

{-
  N-dimensional  index  type. The  type  (head :& tail) allows  working with any
  finite dimension number.
-}

data tail :& head = !tail :& !head deriving (Eq, Ord, Read)

-- Derived instance doesn't have whitespaces, but I like whitespaces...
instance (Show tail, Show head) => Show (tail :& head)
  where
    show (es :& e) = shows es . showString " :& " $ show e

instance (Enum i) => Enum (() :& i)
  where
    succ (() :& e) = () :& succ e
    pred (() :& e) = () :& pred e
    
    toEnum n           = () :& toEnum n
    fromEnum (() :& e) = fromEnum e
    
    enumFrom       (() :& fst)                         = (() :&) <$> [fst .. ]
    enumFromTo     (() :& fst) (() :& lst)             = (() :&) <$> [fst .. lst]
    enumFromThen   (() :& fst) (() :& nxt)             = (() :&) <$> [fst, nxt .. ]
    enumFromThenTo (() :& fst) (() :& nxt) (() :& lst) = (() :&) <$> [fst, nxt .. lst]

{-
  Type synonyms are declared up to 16 dimensions (Fortran 2003 permits up to 15,
  and I would like to compete with a language 15 years ago at least in this).
  Although, for example, with the tuple library (supports 32-element tuples)
  I am still far away.
-}

type Bounds i = (i, i)

type  I2  i  = () :& i :& i
type  I3  i  = (I2  i) :& i
type  I4  i  = (I3  i) :& i
type  I5  i  = (I4  i) :& i
type  I6  i  = (I5  i) :& i
type  I7  i  = (I6  i) :& i
type  I8  i  = (I7  i) :& i
type  I9  i  = (I8  i) :& i
type  I10 i  = (I9  i) :& i
type  I11 i  = (I10 i) :& i
type  I12 i  = (I11 i) :& i
type  I13 i  = (I12 i) :& i
type  I14 i  = (I13 i) :& i
type  I15 i  = (I14 i) :& i
type  I16 i  = (I15 i) :& i

ind2  :: (Index i) => i -> i                                                                       -> I2  i
ind3  :: (Index i) => i -> i -> i                                                                  -> I3  i
ind4  :: (Index i) => i -> i -> i -> i                                                             -> I4  i
ind5  :: (Index i) => i -> i -> i -> i -> i                                                        -> I5  i
ind6  :: (Index i) => i -> i -> i -> i -> i -> i                                                   -> I6  i
ind7  :: (Index i) => i -> i -> i -> i -> i -> i -> i                                              -> I7  i
ind8  :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i                                         -> I8  i
ind9  :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i                                    -> I9  i
ind10 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i                               -> I10 i

ind11 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i                          -> I11 i
ind12 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i                     -> I12 i
ind13 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i                -> I13 i
ind14 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i           -> I14 i
ind15 :: (Index i) => i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i -> i      -> I15 i
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

checkBounds :: (Index i) => (i, i) -> i -> res -> String -> res
checkBounds bnds ix res msg
  |    isEmpty  bnds    = throw . EmptyRange     $ "in SDP.Index." ++ msg
  | isOverflow  bnds ix = throw . IndexOverflow  $ "in SDP.Index." ++ msg
  | isUnderflow bnds ix = throw . IndexUnderflow $ "in SDP.Index." ++ msg
  |      otherwise      = res

intOffset :: (Index i, Num i, Enum i) => (i, i) -> i -> Int
intOffset (l, u) i = checkBounds (l, u) i (fromEnum i - fromEnum l) "offset (default)"

