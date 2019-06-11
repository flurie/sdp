{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module SDP.Tree.BinTree where

import Prelude ()
import SDP.SafePrelude

import GHC.Show -- ( appPrec, appPrec1 )

import SDP.Indexed
import SDP.Simple

data BinTree a = BinEmpty
               | BinNode
                !(BinTree a)        {- left  branch -}
                !a                  {-    element   -}
                {-# UNPACK #-} !Int {-     size     -}
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
    showsPrec p     BinEmpty      = showParen (p > appPrec) $ showString "BinEmpty"
    showsPrec p (BinNode l e n r) = showParen (p > appPrec) shows'
      where
        shows' = showString "BinNode " . shows n . left . e' . right
        right  = showChar ' ' . showsPrec appPrec1 r
        left   = showChar ' ' . showsPrec appPrec1 l
        e'     = showChar ' ' . shows e

-- instance (Read a) => Read (BinTree a) where

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances -}

instance Functor BinTree
  where
    fmap _ BinEmpty = BinEmpty
    fmap f (BinNode l e n r) = BinNode (f <$> l) (f e) n (f <$> r)

instance Zip BinTree
  where
    zipWith  f
        (BinNode l1 e1 n1 r1)
        (BinNode l2 e2 n2 r2)
          = BinNode l (f e1 e2) n r
      where
        l = zipWith  f l1 l2
        r = zipWith  f r1 r2
        n = min n1 n2
    zipWith  _ _ _ = Z
    
    zipWith3 f
        (BinNode l1 e1 n1 r1)
        (BinNode l2 e2 n2 r2)
        (BinNode l3 e3 n3 r3)
          = BinNode l (f e1 e2 e3) n r
      where
        l = zipWith3 f l1 l2 l3
        r = zipWith3 f r1 r2 r3
        n = minimum [n1, n2, n3]
    zipWith3 _ _ _ _ = Z
    
    zipWith4 f
        (BinNode l1 e1 n1 r1)
        (BinNode l2 e2 n2 r2)
        (BinNode l3 e3 n3 r3)
        (BinNode l4 e4 n4 r4)
          = BinNode l (f e1 e2 e3 e4) n r
      where
        l = zipWith4 f l1 l2 l3 l4
        r = zipWith4 f r1 r2 r3 r4
        n = minimum [n1, n2, n3, n4]
    zipWith4 _ _ _ _ _ = Z
    
    zipWith5 f
        (BinNode l1 e1 n1 r1)
        (BinNode l2 e2 n2 r2)
        (BinNode l3 e3 n3 r3)
        (BinNode l4 e4 n4 r4)
        (BinNode l5 e5 n5 r5)
          = BinNode l (f e1 e2 e3 e4 e5) n r
      where
        l = zipWith5 f l1 l2 l3 l4 l5
        r = zipWith5 f r1 r2 r3 r4 r5
        n = minimum [n1, n2, n3, n4, n5]
    zipWith5 _ _ _ _ _ _ = Z
    
    zipWith6 f
        (BinNode l1 e1 n1 r1)
        (BinNode l2 e2 n2 r2)
        (BinNode l3 e3 n3 r3)
        (BinNode l4 e4 n4 r4)
        (BinNode l5 e5 n5 r5)
        (BinNode l6 e6 n6 r6)
          = BinNode l (f e1 e2 e3 e4 e5 e6) n r
      where
        l = zipWith6 f l1 l2 l3 l4 l5 l6
        r = zipWith6 f r1 r2 r3 r4 r5 r6
        n = minimum [n1, n2, n3, n4, n5, n6]
    zipWith6 _ _ _ _ _ _ _ = Z

-- instance Applicative BinTree

--------------------------------------------------------------------------------

{- Foldable, Scan and Traversable instances. -}

instance Foldable BinTree
  where
    fold Z = mempty
    fold (BinNode l e _ r) = (fold l) <> e <> (fold r)
    
    foldMap _ Z = mempty
    foldMap f   (BinNode l e _ r) = (foldMap f l) <> f e <> (foldMap f r)
    
    foldr _ base Z = base
    foldr f base (BinNode l e _ r) = foldr f (f e $ foldr f base r) l
    
    foldl _ base Z = base
    foldl f base (BinNode l e _ r) = foldl f (f (foldl f base l) e) r
    
    toList Z = []
    toList (BinNode l e _ r)       = toList' l (e : toList r)
      where
        toList' Z lst = lst
        toList' (BinNode l' e' _ r') lst = toList' l' $ e' : toList' r' lst
    
    null   es = case es of {Z -> True; _ -> False}
    
    length es = case es of {BinNode _ _ n _ -> n; _ -> 0}

-- instance Scan BinTree

-- instance Traversable BinTree

--------------------------------------------------------------------------------

{- Linear, Split and Bordered instances. -}

instance Linear (BinTree e) e
  where
    isNull es = case es of {BinEmpty -> True; BinNode _ _ _ _ -> False}
    
    lzero  = BinEmpty
    listL  = toList
    
    {-# INLINE single #-}
    single x = BinNode Z x 1 Z
    
    toHead x xs = balance $ add x xs
      where
        add e Z = single e
        add e (BinNode l e' n r) = BinNode (add e l) e' (n + 1) r
    
    toLast xs x = balance $ add x xs
      where
        add e Z = single e
        add e (BinNode l e' n r) = BinNode l e' (n + 1) (add e r)
    
    uncons Z = empEx "(:>)"
    uncons (BinNode Z e _ r) = (e, r)
    uncons (BinNode l e n r) = (head', BinNode l' e (n - 1) r)
      where
        (head', l') = uncons l
    
    unsnoc Z = empEx "(:<)"
    unsnoc (BinNode l e _ Z) = (l, e)
    unsnoc (BinNode l e n r) = (BinNode l e (n - 1) r', last')
      where
        (r', last') = unsnoc r
    
    replicate n x = fromList $ replicate n x
    
    head Z = empEx "(:>)"
    head (BinNode l e _ _) = case l of {Z -> e; _ -> head l}
    
    last Z = empEx "(:<)"
    last (BinNode _ e _ r) = case r of {Z -> e; _ -> last r}
    
    tail Z = empEx "(:>)"
    tail (BinNode l e n r) = case l of {Z -> r; _ -> BinNode (tail l) e (n - 1) r}
    
    init Z = empEx "(:<)"
    init (BinNode l e n r) = case r of {Z -> r; _ -> BinNode l e (n - 1) (init r)}
    
    reverse Z = Z
    reverse (BinNode l e n r) = BinNode (reverse r) e n (reverse l)

instance Split (BinTree e) e
  where
    take n es = (n < 0) ? Z $ take' n es
      where
        take' _ Z = Z
        take' 0 _ = Z
        take' n' (BinNode l e s r) = case n' <=> nl of
            EQ -> l
            LT -> take' n' l
            GT -> BinNode l e (s - n') r'
          where
            r' = take' (n - nl) r
            nl = length l
    
    drop n es = (n < 1) ? es $ drop' n es
      where
        drop' _ Z   = Z
        drop' 0 es' = es'
        drop' n' (BinNode l e s r) = case n' <=> nl of
            EQ -> e :> r
            GT -> e :> drop' n' r
            LT -> BinNode l' e (s - n') r
          where
            l' = drop' n' l
            nl = length l

instance Bordered (BinTree e) Int e
  where
    assocs  tree = zip (indices tree) (toList tree)
    indices tree = [0 .. length tree]
    bounds  tree = (0, length tree)
    lower    _   = 0
    upper   tree = length tree

--------------------------------------------------------------------------------

instance Indexed (BinTree e) Int e
  where
    

--------------------------------------------------------------------------------

balance :: BinTree e -> BinTree e
balance = id

empEx     :: String -> a
empEx msg =  throw . EmptyRange $ "in SDP.Tree.BinTree." ++ msg
