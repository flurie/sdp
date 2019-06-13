{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module SDP.Tree.BinTree ( BinTree (..) ) where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import GHC.Show -- ( appPrec, appPrec1 )

import Text.Read
import Text.Read.Lex ( expect )

import SDP.Indexed
import SDP.Simple

--------------------------------------------------------------------------------

data BinTree a = BinEmpty
               | BinNode
                !(BinTree a)        {- left  branch -}
                !a                  {-    element   -}
                {-# UNPACK #-} !Int {-    height    -}
                !(BinTree a)        {- right branch -}

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
    showsPrec p     BinEmpty      = showParen (p > appPrec) $ showString "Z"
    showsPrec p (BinNode Z e _ Z) = showParen (p > appPrec) $ shows e
    showsPrec p (BinNode l e _ r) = showParen (p > appPrec) shows'
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
    fmap f (BinNode l e n r) = BinNode (f <$> l) (f e) n (f <$> r)

instance Zip BinTree
  where
    zipWith  f (BinNode l1 e1 _ r1) (BinNode l2 e2 _ r2) = BinNode l (f e1 e2) n r
      where
        l = zipWith  f l1 l2
        r = zipWith  f r1 r2
        n = 1 + max (height l) (height r)
    zipWith  _ _ _ = Z
    
    zipWith3 f (BinNode l1 e1 _ r1) (BinNode l2 e2 _ r2) (BinNode l3 e3 _ r3) = BinNode l (f e1 e2 e3) n r
      where
        l = zipWith3 f l1 l2 l3
        r = zipWith3 f r1 r2 r3
        n = 1 + max (height l) (height r)
    zipWith3 _ _ _ _ = Z
    
    zipWith4 f (BinNode l1 e1 _ r1) (BinNode l2 e2 _ r2) (BinNode l3 e3 _ r3) (BinNode l4 e4 _ r4) = BinNode l (f e1 e2 e3 e4) n r
      where
        l = zipWith4 f l1 l2 l3 l4
        r = zipWith4 f r1 r2 r3 r4
        n = 1 + max (height l) (height r)
    zipWith4 _ _ _ _ _ = Z
    
    zipWith5 f (BinNode l1 e1 _ r1) (BinNode l2 e2 _ r2) (BinNode l3 e3 _ r3) (BinNode l4 e4 _ r4) (BinNode l5 e5 _ r5) = BinNode l (f e1 e2 e3 e4 e5) n r
      where
        l = zipWith5 f l1 l2 l3 l4 l5
        r = zipWith5 f r1 r2 r3 r4 r5
        n = 1 + max (height l) (height r)
    zipWith5 _ _ _ _ _ _ = Z
    
    zipWith6 f (BinNode l1 e1 _ r1) (BinNode l2 e2 _ r2) (BinNode l3 e3 _ r3) (BinNode l4 e4 _ r4) (BinNode l5 e5 _ r5) (BinNode l6 e6 _ r6) = BinNode l (f e1 e2 e3 e4 e5 e6) n r
      where
        l = zipWith6 f l1 l2 l3 l4 l5 l6
        r = zipWith6 f r1 r2 r3 r4 r5 r6
        n = 1 + max (height l) (height r)
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
    fold (BinNode l e _ r) = (fold l) <> e <> (fold r)
    
    foldMap _    Z = mempty
    foldMap f    (BinNode l e _ r) = (foldMap f l) <> f e <> (foldMap f r)
    
    foldr _ base Z = base
    foldr f base (BinNode l e _ r) = foldr f (f e $ foldr f base r) l
    
    foldl _ base Z = base
    foldl f base (BinNode l e _ r) = foldl f (f (foldl f base l) e) r
    
    toList Z = []
    toList (BinNode l e _ r)       = toList' l (e : toList r)
      where
        toList' Z lst = lst
        toList' (BinNode l' e' _ r') lst = toList' l' $ e' : toList' r' lst
    
    null es = case es of {BinEmpty -> True; _ -> False}

-- instance Scan BinTree

instance Traversable BinTree
  where
    traverse f tree = fromList <$> traverse f (toList tree)

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance Linear (BinTree e) e
  where
    isNull es = case es of {BinEmpty -> True; _ -> False}
    
    lzero = BinEmpty
    listL = toList
    
    {-# INLINE single #-}
    single x = BinNode Z x 1 Z
    
    toHead x xs = balance $ add x xs
      where
        add e Z = single e
        add e (BinNode l e' _ r) = let l' = add e l in BinNode l' e' (height' l' r) r
    
    toLast xs x = balance $ add x xs
      where
        add e Z = single e
        add e (BinNode l e' _ r) = let r' = add e r in BinNode l e' (height' l r') r'
    
    uncons Z = empEx "(:>)"
    uncons (BinNode Z e _ r) = (e, r)
    uncons (BinNode l e _ r) = (head', BinNode l' e (height' l' r) r)
      where
        (head', l') = uncons l
    
    unsnoc Z = empEx "(:<)"
    unsnoc (BinNode l e _ Z) = (l, e)
    unsnoc (BinNode l e _ r) = (BinNode l e (height' l r') r', last')
      where
        (r', last') = unsnoc r
    
    replicate n x = fromList $ replicate n x
    
    head Z = empEx "(:>)"
    head (BinNode l e _ _) = case l of {Z -> e; _ -> head l}
    
    last Z = empEx "(:<)"
    last (BinNode _ e _ r) = case r of {Z -> e; _ -> last r}
    
    tail Z = empEx "(:>)"
    tail (BinNode l e _ r) = case l of {Z -> r; _ -> BinNode l' e (height' l' r) r}
      where
        l' = tail l
    
    init Z = empEx "(:<)"
    init (BinNode l e _ r) = case r of {Z -> l; _ -> BinNode l e (height' l r') r'}
      where
        r' = init r
    
    reverse Z = Z
    reverse (BinNode l e n r) = BinNode (reverse r) e n (reverse l)
    
    fromList     es = foldr (:>) Z es
    fromFoldable es = foldr (:>) Z es

instance Split (BinTree e) e
  where
    take n es
        |    n <= 0 || null es    = Z
        |      n >= length es     = es
        | (BinNode l e _ r) <- es = case n <=> length l of
          EQ -> l
          LT -> take n l
          GT -> let r' = take (n - length l) r in balance $ BinNode l e (height' l r') r'
    
    drop n es
      |           n <= 0          = es
      | n >= length es || null es = Z
      |  (BinNode l e _ r) <- es  = case n <=> length l of
        EQ -> e :> r
        GT -> e :> drop n r
        LT -> let l' = drop n l in balance $ BinNode l' e (height' l' r) r

instance Bordered (BinTree e) Int e
  where
    assocs  tree = zip (indices tree) (toList tree)
    indices tree = [0 .. length tree - 1]
    bounds  tree = (0, length tree - 1)
    lower    _   = 0
    upper   tree = length tree - 1

--------------------------------------------------------------------------------

{- Indexed instance. -}

instance Indexed (BinTree e) Int e

--------------------------------------------------------------------------------

instance (Arbitrary e) => Arbitrary (BinTree e)
  where
    arbitrary = fromList <$> arbitrary

--------------------------------------------------------------------------------

balance   :: BinTree e -> BinTree e
balance Z =  Z
balance (BinNode l e _ r)
    | abs factor < 2 = res
    |  factor ==  2  = rotateLeft  res
    |  factor == -2  = rotateRight res
    |      True      = error "in SDP.BinTree.balance"
  where
    factor = height r' - height l'
    
    res = BinNode l' e (height' l' r') r'
    l'  = balance l
    r'  = balance r

rotateLeft :: BinTree e -> BinTree e
rotateLeft (BinNode l a _ (BinNode (BinNode m c _ n) b _ r)) = BinNode (BinNode l a lm m) c ab (BinNode n b nr r)
  where
    lm = height' l m
    nr = height' n r
    ab = 1 + max lm nr

rotateRight :: BinTree e -> BinTree e
rotateRight (BinNode (BinNode l b _ c') a _ r) = height c' <= height l ? small $ big
  where
    small = BinNode l b la (BinNode c' a rc r)
    big   = BinNode (BinNode l b lm m) c ba (BinNode n a nr r)
    
    (BinNode m c _ n) = c'
    
    hl = height l
    hr = height r
    
    rc = 1 + max hr (height c')
    nr = 1 + max hr (height n)
    ba = 1 + max nr lm
    
    lm = 1 + max hl (height m)
    la = 1 + max hl rc

-- "good" height (by definition), but slow: O(n).
height_ :: BinTree e -> Int
height_ Z  = 0
height_ (BinNode l _ _ r) = 1 + max (height_ l) (height_ r)

-- cached height, not bad and fast: O(1).
{-# INLINE height #-}
height :: BinTree e -> Int
height Z  = 0
height (BinNode _ _ h _) = h

-- better version of cached height for internal use.
{-# INLINE height' #-}
height' :: BinTree e -> BinTree e -> Int
height' l r = 1 + max (height l) (height r)

empEx     :: String -> a
empEx msg =  throw . EmptyRange $ "in SDP.Tree.BinTree." ++ msg
