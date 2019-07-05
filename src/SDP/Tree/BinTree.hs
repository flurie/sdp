{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

{- |
    Module      :  SDP.Tree.BinTree
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (imports SDP.Indexed)
    Stability   :  experimental
    
    SDP.Tree.BinTree provides immutable lazy boxed tree.
    This implementation of a binary tree is self-balancing (when adding elements),
    but sensitive to adding and removing several elements due to a simplified
    balancing algorithm. In the next versions this problem will be solved.
    
    This tree has additional fields for quick calculations (size and height),
    but it still has terrible performance. This mainly concerns the construction
    and deconstruction.
-}

module SDP.Tree.BinTree ( BinTree (..) ) where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.Indexed

import GHC.Show ( appPrec )

import Text.Read
import Text.Read.Lex ( expect )

import SDP.Unrolled.Unlist
import SDP.Simple

default ()

--------------------------------------------------------------------------------

-- | BinTree is just binary tree implementation.
data BinTree a = BinEmpty
               | BinNode
                !(BinTree a)        {- left  branch -}
                !a                  {-    element   -}
                {-# UNPACK #-} !Int {-     size     -}
                {-# UNPACK #-} !Int {-    height    -}
                !(BinTree a)        {- right branch -}

{-# COMPLETE Z, BinNode #-}

--------------------------------------------------------------------------------

{- Eq and Eq1 instances. -}

instance (Eq e) => Eq (BinTree e) where (==) = eq1

instance Eq1 BinTree
  where
    liftEq f xs ys = liftEq f (toList xs) (toList ys)

--------------------------------------------------------------------------------

{- Ord and Ord1 instances. -}

instance (Ord e) => Ord (BinTree e) where compare = compare1

instance Ord1 BinTree
  where
    liftCompare f xs ys = liftCompare f (toList xs) (toList ys)

--------------------------------------------------------------------------------

{- Semigroup, Monoid and Default instances. -}

instance Semigroup (BinTree e) where xs <> ys = xs ++ ys

instance Monoid    (BinTree e) where mempty = BinEmpty

instance Default   (BinTree e) where def = BinEmpty

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance (Show a) => Show (BinTree a)
  where
    showsPrec p tree = showParen (p > appPrec) $ showString "bintree "
                                               . shows (bounds tree)
                                               . showChar ' '
                                               . shows (assocs tree)

instance (Read a) => Read (BinTree a)
  where
    readList = readListDefault
    readPrec = parens $ prec appPrec (lift . expect $ Ident "bintree") >> liftA2 assoc (step readPrec) (step readPrec)

{-

-- componentwise show

instance (Show a) => Show (BinTree a)
  where
    showsPrec p      BinEmpty       = showParen (p > appPrec) $ showString "Z"
    showsPrec p (BinNode Z e _ _ Z) = showParen (p > appPrec) $ shows e
    showsPrec p (BinNode l e _ _ r) = showParen (p > appPrec) shows'
      where
        shows' = showString "BinNode" . left . e' . right
        right  = showChar ' ' . showsPrec appPrec1 r
        left   = showChar ' ' . showsPrec appPrec1 l
        e'     = showChar ' ' . shows e

-}

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances -}

instance Functor BinTree
  where
    fmap _ BinEmpty = BinEmpty
    fmap f (BinNode l e h n r) = BinNode (f <$> l) (f e) h n (f <$> r)

instance Zip BinTree
  where
    zipWith  f (BinNode l1 e1 n1 _ r1)
               (BinNode l2 e2 n2 _ r2) = BinNode l (f e1 e2) n h r
      where
        l = zipWith  f l1 l2
        r = zipWith  f r1 r2
        n = minimum [n1, n2]
        h = 1 + max (height l) (height r)
    zipWith  _ _ _ = Z
    
    zipWith3 f (BinNode l1 e1 n1 _ r1)
               (BinNode l2 e2 n2 _ r2)
               (BinNode l3 e3 n3 _ r3) = BinNode l (f e1 e2 e3) n h r
      where
        l = zipWith3 f l1 l2 l3
        r = zipWith3 f r1 r2 r3
        n = minimum [n1, n2, n3]
        h = 1 + max (height l) (height r)
    zipWith3 _ _ _ _ = Z
    
    zipWith4 f (BinNode l1 e1 n1 _ r1)
               (BinNode l2 e2 n2 _ r2)
               (BinNode l3 e3 n3 _ r3)
               (BinNode l4 e4 n4 _ r4) = BinNode l (f e1 e2 e3 e4) n h r
      where
        l = zipWith4 f l1 l2 l3 l4
        r = zipWith4 f r1 r2 r3 r4
        n = minimum [n1, n2, n3, n4]
        h = 1 + max (height l) (height r)
    zipWith4 _ _ _ _ _ = Z
    
    zipWith5 f (BinNode l1 e1 n1 _ r1)
               (BinNode l2 e2 n2 _ r2)
               (BinNode l3 e3 n3 _ r3)
               (BinNode l4 e4 n4 _ r4)
               (BinNode l5 e5 n5 _ r5) = BinNode l (f e1 e2 e3 e4 e5) n h r
      where
        l = zipWith5 f l1 l2 l3 l4 l5
        r = zipWith5 f r1 r2 r3 r4 r5
        n = minimum [n1, n2, n3, n4, n5]
        h = 1 + max (height l) (height r)
    zipWith5 _ _ _ _ _ _ = Z
    
    zipWith6 f (BinNode l1 e1 n1 _ r1)
               (BinNode l2 e2 n2 _ r2)
               (BinNode l3 e3 n3 _ r3)
               (BinNode l4 e4 n4 _ r4)
               (BinNode l5 e5 n5 _ r5)
               (BinNode l6 e6 n6 _ r6) = BinNode l (f e1 e2 e3 e4 e5 e6) n h r
      where
        l = zipWith6 f l1 l2 l3 l4 l5 l6
        r = zipWith6 f r1 r2 r3 r4 r5 r6
        n = minimum [n1, n2, n3, n4, n5, n6]
        h = 1 + max (height l) (height r)
    zipWith6 _ _ _ _ _ _ _ = Z

instance Applicative BinTree
  where
    pure = single
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable, Scan and Traversable instances. -}

instance Foldable BinTree
  where
    fold Z = mempty
    fold (BinNode l e _ _ r) = fold l <> e <> fold r
    
    foldMap _    Z = mempty
    foldMap f    (BinNode l e _ _ r) = foldMap f l <> f e <> foldMap f r
    
    foldr _ base Z = base
    foldr f base (BinNode l e _ _ r) = foldr f (f e $ foldr f base r) l
    
    foldl _ base Z = base
    foldl f base (BinNode l e _ _ r) = foldl f (foldl f base l `f` e) r
    
    {-# INLINE length #-}
    length es = case es of {BinNode _ _ n _ _ -> max 0 n; _ -> 0}
    
    {-# INLINE null #-}
    null   es = case es of {BinNode _ _ n h _ -> n < 1 || h < 0; _ -> True}

-- instance Scan BinTree

instance Traversable BinTree where traverse f tree = fromList <$> traverse f (toList tree)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance Linear (BinTree e) e
  where
    {-# INLINE isNull #-}
    isNull es = null es
    
    lzero = BinEmpty
    
    listL es = toList es
    
    {-# INLINE single #-}
    single x = BinNode Z x 1 1 Z
    
    uncons Z = pfailEx "(:>)"
    uncons (BinNode Z e _ _ r) = (e, r)
    uncons (BinNode l e n _ r) = (head', BinNode l' e (n - 1) (height' l' r) r)
      where
        (head', l') = uncons l
    
    toHead x xs = balance $ add x xs
      where
        add e Z = single e
        add e (BinNode l e' n _ r) = let l' = add e l in BinNode l' e' (n + 1) (height' l' r) r
    
    {-# INLINE head #-}
    head Z = pfailEx "(:>)"
    head (BinNode l e _ _ _) = case l of {Z -> e; _ -> head l}
    
    {-# INLINE last #-}
    last Z = pfailEx "(:<)"
    last (BinNode _ e _ _ r) = case r of {Z -> e; _ -> last r}
    
    unsnoc Z = pfailEx "(:<)"
    unsnoc (BinNode l e _ _ Z) = (l, e)
    unsnoc (BinNode l e n _ r) = (BinNode l e (n - 1) (height' l r') r', last')
      where
        (r', last') = unsnoc r
    
    toLast xs x = balance $ add x xs
      where
        add e Z = single e
        add e (BinNode l e' n _ r) = let r' = add e r in BinNode l e' (n + 1) (height' l r') r'
    
    {-# INLINE tail #-}
    tail Z = pfailEx "(:>)"
    tail (BinNode l e n _ r) = let l' = tail l in case l of {Z -> r; _ -> BinNode l' e (n - 1) (height' l' r) r}
    
    {-# INLINE init #-}
    init Z = pfailEx "(:<)"
    init (BinNode l e n _ r) = let r' = init r in case r of {Z -> l; _ -> BinNode l e (n - 1) (height' l r') r'}
    
    replicate n x = fromList $ replicate n x
    
    reverse Z = Z
    reverse (BinNode l e n h r) = BinNode (reverse r) e n h (reverse l)
    
    {-# INLINE fromList #-}
    fromList     es = fromFoldable es
    
    {-# INLINE fromFoldable #-}
    fromFoldable es = foldr (:>) Z es

instance Split (BinTree e) e
  where
    take n es = fromList . take n $ toList es
    drop n es = fromList . drop n $ toList es
    
    prefix _ Z = 0
    prefix p (BinNode l e _ _ r) = p e && pl == sl ? sl + pr + 1 $ prefix p l
      where
        pl = prefix p l
        pr = prefix p r
        sl = length l
    
    suffix _ Z = 0
    suffix p (BinNode l e _ _ r) = p e && pr == sr ? pl + sr + 1 $ suffix p r
      where
        pl = suffix p l
        pr = suffix p r
        sr = length r

instance Bordered (BinTree e) Int e
  where
    lower  _    = 0
    upper  tree = length tree - 1
    
    sizeOf tree = length tree

--------------------------------------------------------------------------------

{- Indexed instance. -}

instance Indexed (BinTree e) Int e
  where
    -- BinTree assoc' uses Unlist assoc' as backend.
    assoc' bnds defvalue ascs = fromUnlist $ assoc' bnds defvalue ascs
      where
        fromUnlist = fromFoldable :: Unlist e -> BinTree e
    
    -- BinTree (//) uses Unlist (//) as backend.
    es // ies = fromFoldable $ (toUnlist es) // ies
      where
        toUnlist = fromFoldable :: BinTree e -> Unlist e
    
    Z ! _ = throw $ EmptyRange "in SDP.BinTree"
    es@(BinNode _ _ s _ _) ! n
        | Z <- es = throw $ EmptyRange     msg
        | n  <  0 = throw $ IndexUnderflow msg
        | n  >= s = throw $ IndexOverflow  msg
        |  True   = es .! n
      where
        msg = "in SDP.BinTree"
    
    (BinNode l e _ _ r) .! n = let s = length l in case n <=> s of {LT -> l .! n; EQ -> e; GT -> r .! (n - s - 1)}
    _ .! _ = error "wrong using of (.!) from SDP.Tree.BinTree"
    
    p .$ es = p .$ toList es
    p *$ es = p *$ toList es

--------------------------------------------------------------------------------

instance Set (BinTree e) e
  where
    setWith f es = fromList . setWith f $ toList es
    
    intersectionWith f xs ys = fromList $ intersectionWith f (toList xs) (toList ys)
    
    unionWith f xs ys = fromList $ unionWith f (toList xs) (toList ys)
    
    differenceWith f xs ys = fromList $ differenceWith f (toList xs) (toList ys)
    
    insertWith f e es' = balance $ insert' e es'
      where
        insert' e0 Z = single e0
        insert' e0 es@(BinNode l e1 _ _ r) = balance $ case e0 `f` e1 of
          LT -> BinNode l' e1 s1 h1 r
          EQ -> es
          GT -> BinNode l  e1 s2 h2 r'
          where
            l' = insert' e0 l
            r' = insert' e0 r
            
            s1 = length' l' r; h1 = height' l' r
            s2 = length' r' l; h2 = height' r' l
    
    deleteWith f e es' = balance $ delete' e es'
      where
        delete' _ Z = Z
        delete' e0 (BinNode l e1 _ _ r) = case e0 `f` e1 of
            LT -> deleteL
            EQ | height l < height r -> replaceR
               |       null  l       -> r
               |         True        -> replaceL
            GT -> deleteR
          where
            -- delete elem from left  branch
            deleteL = let l' = deleteWith f e0 l in BinNode l' e1 (length' l' r) (height' l' r) r
            -- delete elem from right branch
            deleteR = let r' = deleteWith f e0 r in BinNode l  e1 (length' r' l) (height' r' l) r'
            
            -- lift adjacent elem from left branch
            replaceL = let (l' :< e2) = l in BinNode l' e2 (length' l' r) (height' l' r) r
            -- lift adjacent elem from right branch
            replaceR = let (e2 :> r') = r in BinNode l  e2 (length' r' l) (height' r' l) r'
    
    isContainedIn _ _           Z           = False
    isContainedIn f e0 (BinNode l e1 _ _ r) = case e0 `f` e1 of
      LT -> isContainedIn f e0 l
      EQ -> True
      GT -> isContainedIn f e0 r

--------------------------------------------------------------------------------

instance (Arbitrary e) => Arbitrary (BinTree e) where arbitrary = fromList <$> arbitrary

instance Estimate BinTree where xs <==> ys = length xs <=> length ys

--------------------------------------------------------------------------------

balance   :: BinTree e -> BinTree e
balance Z =  Z
balance (BinNode l e n _ r)
    | abs factor < 2 = res
    |  factor ==  2  = rotateLeft  res
    |  factor == -2  = rotateRight res
    |      True      = error "in SDP.BinTree.balance"
  where
    factor = height r' - height l'
    
    res = BinNode l' e n (height' l' r') r'
    l'  = balance l
    r'  = balance r

rotateLeft  :: BinTree e -> BinTree e
rotateLeft  (BinNode l a _ _ (BinNode (BinNode m c _ _ n) b _ _ r)) = acb
  where
    acb = BinNode lam c sc hc nbr
    
    lam = BinNode l a sa ha m
    nbr = BinNode n b sb hb r
    
    sa = length l + length m + 1
    sb = length n + length r + 1
    sc = sa + sb + 1
    
    ha = height' l m
    hb = height' n r
    hc = ha + hb + 1
rotateLeft _ = throw $ UnreachableException "in SDP.Tree.BinTree.rotateLeft"

rotateRight :: BinTree e -> BinTree e
rotateRight (BinNode (BinNode l b _ _ c') a _ _ r) = height c' > height l ? big $ small
  where
    big    = BinNode (BinNode l b sb hb m) c sc hc (BinNode n a sa ha r)
      where
        sa = length' n r; sb = length' l m; sc = sa + sb + 1
        ha = height' l m; hb = height' n r; hc = 1 + max ha hb
        
        (BinNode m c _ _ n) = c'
    
    small  = BinNode l b sb hb (BinNode c' a sa ha r)
      where
        sa = length' c' r; sb = length l + sa + 1
        ha = height' c' r; hb = 1 + max (height l) ha
rotateRight _ = throw $ UnreachableException "in SDP.Tree.BinTree.rotateRight"

{-

-- "good" height (by definition), but slow: O(n).
height_ :: BinTree e -> Int
height_ Z  = 0
height_ (BinNode l _ _ _ r) = 1 + max (height_ l) (height_ r)

-}

-- cached height, not bad and fast: O(1).
{-# INLINE height #-}
height :: BinTree e -> Int
height Z  = 0
height (BinNode _ _ _ h _) = h

-- better version of cached height for internal use.
{-# INLINE height' #-}
height' :: BinTree e -> BinTree e -> Int
height' l r = 1 + max (height l) (height r)

-- service version of length for internal use.
{-# INLINE length' #-}
length' :: BinTree e -> BinTree e -> Int
length' l r = length l + length r + 1

pfailEx     :: String -> a
pfailEx msg =  throw . PatternMatchFail $ "in SDP.Tree.BinTree." ++ msg



