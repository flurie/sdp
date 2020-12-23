{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE Trustworthy, UndecidableInstances, BangPatterns #-}

{- |
    Module      :  SDP.Templates.AnyChunks
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC extensions)
    
    "SDP.Templates.AnyChunks" provides 'AnyChunks' - list of data chunks.
-}
module SDP.Templates.AnyChunks
(
  -- * Export
  module SDP.IndexedM,
  module SDP.Scan,
  
  -- * Chunk list
  AnyChunks (..)
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM
import SDP.Internal
import SDP.SortM
import SDP.Scan

import qualified GHC.Exts as E

import GHC.Generics

import Data.Typeable
import Data.Data

import Text.Read.SDP
import Text.Show.SDP

import SDP.SortM.Tim

default ()

--------------------------------------------------------------------------------

-- | 'AnyChunks' is list of data chunks.
data AnyChunks rep e = AnyChunks [rep e] deriving ( Typeable, Data, Generic )

--------------------------------------------------------------------------------

{- Eq and Ord instances. -}

instance (Eq (rep e), Bordered1 rep Int e, Split1 rep e) => Eq (AnyChunks rep e)
  where
    Z == Z = True
    xs@(AnyChunks (x : xs')) == ys@(AnyChunks (y : ys')) = if n1 > n2
        then take n2 x == y && drop n2 xs == AnyChunks ys'
        else take n1 y == x && drop n1 ys == AnyChunks xs'
      where
        n1 = sizeOf x
        n2 = sizeOf y
    _ == _ = False

instance (Ord (rep e), Bordered1 rep Int e, Split1 rep e) => Ord (AnyChunks rep e)
  where
    compare Z Z = EQ
    compare xs@(AnyChunks (x : xs')) ys@(AnyChunks (y : ys')) = if n1 > n2
        then (take n2 x <=> y) <> (drop n2 xs <=> AnyChunks ys')
        else (x <=> take n1 y) <> (AnyChunks xs' <=> drop n1 ys)
      where
        n1 = sizeOf x
        n2 = sizeOf y
    compare Z _ = LT
    compare _ _ = GT

--------------------------------------------------------------------------------

{- Show and Read instances. -}

instance {-# OVERLAPPABLE #-} (Indexed1 rep Int e, Show e) => Show (AnyChunks rep e)
  where
    showsPrec = assocsPrec "unlist "

instance (Indexed1 rep Int Char) => Show (AnyChunks rep Char)
  where
    showsPrec = shows ... const listL

instance (Indexed1 rep Int e, Read e) => Read (AnyChunks rep e)
  where
    readPrec = indexedPrec' "ublist"
    readList = readListDefault

--------------------------------------------------------------------------------

{- Semigroup, Monoid, Nullable, Default and Estimate instances. -}

instance (Nullable (rep e)) => Nullable (AnyChunks rep e)
  where
    isNull = \ (AnyChunks es) -> all isNull es
    lzero  = AnyChunks []

instance Semigroup (AnyChunks rep e)
  where
    (AnyChunks xs) <> (AnyChunks ys) = AnyChunks (xs ++ ys)

instance Monoid  (AnyChunks rep e) where mempty = AnyChunks []
instance Default (AnyChunks rep e) where def    = AnyChunks []

instance (Bordered1 rep Int e) => Estimate (AnyChunks rep e)
  where
    (<==>) = go 0
      where
        go o (AnyChunks [])   (AnyChunks []) = o <=> 0
        go o (AnyChunks [])               ys = o <=.> ys
        go o xs               (AnyChunks []) = xs <.=> (-o)
        go o (AnyChunks (x : xs)) (AnyChunks (y : ys)) =
          go (o + sizeOf x - sizeOf y) (AnyChunks xs) (AnyChunks ys)
    
    (AnyChunks []) <.=> n = 0 <=> n
    (AnyChunks (x : xs)) <.=> n = c > n ? GT $ AnyChunks xs <.=> (n - c)
      where
        c = sizeOf x

--------------------------------------------------------------------------------

{- Overloaded Lists and Strings support. -}

instance (Linear1 (AnyChunks rep) Char) => IsString (AnyChunks rep Char)
  where
    fromString = fromList

instance (Linear1 (AnyChunks rep) e) => E.IsList (AnyChunks rep e)
  where
    type Item (AnyChunks rep e) = e
    
    fromListN = fromListN
    fromList  = fromList
    toList    = listL

--------------------------------------------------------------------------------

{- Functor and Applicative instances. -}

instance (Functor rep) => Functor (AnyChunks rep)
  where
    fmap f (AnyChunks es) = AnyChunks (fmap f <$> es)

instance (Applicative rep) => Applicative (AnyChunks rep)
  where
    (AnyChunks fs) <*> (AnyChunks es) = AnyChunks $ liftA2 (<*>) fs es
    pure e = AnyChunks [pure e]

--------------------------------------------------------------------------------

{- Foldable and Traversable instances. -}

instance (Foldable rep) => Foldable (AnyChunks rep)
  where
    foldr f base (AnyChunks es) = flip (foldr f) `foldr` base $ es
    foldl f base (AnyChunks es) = foldl (foldl f) base es
    
    elem e (AnyChunks es) = foldr ((||) . elem e) False es
    length (AnyChunks es) = foldr' ((+) . length) 0 es

instance (Traversable rep) => Traversable (AnyChunks rep)
  where
    traverse f (AnyChunks es) = AnyChunks <$> traverse (traverse f) es

--------------------------------------------------------------------------------

{- Bordered, Linear and Split instances. -}

instance (Bordered1 rep Int e) => Bordered (AnyChunks rep e) Int
  where
    sizeOf (AnyChunks es) = foldr' ((+) . sizeOf) 0 es
    
    indexIn es = \ i -> i >= 0 && i <. es
    
    -- | Quick unchecked offset.
    offsetOf = const id
    
    -- | Quick unchecked index.
    indexOf  = const id
    
    lower   _ = 0
    upper  es = sizeOf es - 1
    bounds es = (0, sizeOf es - 1)

instance (Bordered1 rep Int e, Linear1 rep e) => Linear (AnyChunks rep e) e
  where
    single e = AnyChunks [single e]
    
    toHead e (AnyChunks es@(x : xs)) = AnyChunks $ sizeOf x < lim ? (e :> x) : xs $ single e : es
    toHead e _ = single e
    
    toLast (AnyChunks (xs :< x)) e = isNull x ? AnyChunks xs :< e $ AnyChunks (xs :< (x :< e))
    toLast _ e = single e
    
    uncons = go . unpack
      where
        go ((x :> xs) : xss) = (x, AnyChunks (xs : xss))
        go _ = pfailEx "(:>)"
    
    unsnoc = go . unpack
      where
        go (xss :< (xs :< x)) = (AnyChunks (xss :< xs), x)
        go _ = pfailEx "(:<)"
    
    fromList = AnyChunks . fmap fromList . chunks lim
    listL    = foldr ((++) . listL) [] . unpack
    
    (AnyChunks (x : xs)) !^ i = i < sizeOf x ? x !^ i $ AnyChunks xs !^ (i - sizeOf x)
    _ !^ _ = error "in SDP.Unrolled.Unlist.(!^)"
    
    write es'@(AnyChunks es) n e =
      let
        go i (x : xs) = i < c ? write x i e : xs $ x : go (i - c) xs
          where
            c = sizeOf x
        go _ xs = xs
      in  n < 0 ? es' $ AnyChunks (go n es)
    
    -- | Deduplicated chunks.
    replicate n e = AnyChunks $ replicate count chunk :< rest
      where
        (count, rst) = n `divMod` lim
        
        chunk = replicate lim e
        rest  = replicate rst e
    
    reverse (AnyChunks es) = AnyChunks $ reverse <$> reverse es
    
    partition p = both fromList . partition p . listL
    
    select  f (AnyChunks es) = concatMap (select f) es
    extract f (AnyChunks es) = bimap concat AnyChunks . unzip $ extract f <$> es
    
    selects fs = second fromList . selects fs . listL
    
    ofoldr f' base' = go 0 f' base' . unpack
      where
        go o f base (x : xs) = ofoldr (f . (o +)) (go (o + sizeOf x) f base xs) x
        go _ _ base _ = base
    
    ofoldl f' base' = go 0 f' base' . unpack
      where
        go o f base (x : xs) = go (o + sizeOf x) f (ofoldl (f . (o +)) base x) xs
        go _ _ base _ = base
    
    o_foldr f base = foldr (flip $ o_foldr f) base . unpack
    o_foldl f base = foldl (o_foldl f) base . unpack

instance (Bordered1 rep Int e, Split1 rep e) => Split (AnyChunks rep e) e
  where
    take n = AnyChunks . go n . unpack
      where
        go c (x : xs) = let s = sizeOf x in case c <=> s of
          GT -> x : go (c - s) xs
          LT -> [take c x]
          EQ -> [x]
        go _    []    = []

    drop n = AnyChunks . go n . unpack
      where
        go c (x : xs) = let s = sizeOf x in case c <=> s of
          GT -> go (c - s) xs
          LT -> drop c x : xs
          EQ -> xs
        go _    []    = []
    
    prefix f = foldr' (\ e n -> let p = prefix f e in p ==. e ? p + n $ p) 0 . unpack
    suffix f = foldl' (\ n e -> let s = suffix f e in s ==. e ? s + n $ s) 0 . unpack
    combo  f = foldr' (\ e n -> let c = combo  f e in c ==. e ? c + n $ c) 0 . unpack

--------------------------------------------------------------------------------

{- BorderedM, LinearM and SplitM instances. -}

instance (BorderedM1 m rep Int e) => BorderedM m (AnyChunks rep e) Int
  where
    getLower _  = return 0
    getUpper es = do n <- getSizeOf es; return (n - 1)
    
    getSizeOf (AnyChunks es) = foldr (liftA2 (+) . getSizeOf) (return 0) es
    
    getIndices es = do n <- getSizeOf es; return [0 .. n - 1]
    nowIndexIn es = \ i -> i < 0 ? return False $ do n <- getSizeOf es; return (i < n)

instance (BorderedM1 m rep Int e, SplitM1 m rep e) => LinearM m (AnyChunks rep e) e
  where
    nowNull = fmap null . unpackM
    newNull = return (AnyChunks [])
    getHead = getHead . head <=< unpackM
    getLast = getLast . last <=< unpackM
    
    prepend e' es' = fmap AnyChunks . go e' =<< unpackM es'
      where
        go e es@(x : xs) = do n <- getSizeOf x; n < lim ? (: xs) <$> prepend e x $ (: es) <$> newLinear [e]
        go e _ = pure <$> newLinear [e]
    
    append es' e' = fmap AnyChunks . go e' =<< unpackM es'
      where
        go e es@(xs :< x) = do n <- getSizeOf x; n < lim ? (xs :<) <$> append x e $ (es :<) <$> newLinear [e]
        go e _ = pure <$> newLinear [e]
    
    newLinear = fmap AnyChunks . mapM newLinear . chunks lim
    
    (!#>) (AnyChunks es) = go es
      where
        go (x : xs) i = do n <- getSizeOf x; i < n ? x !#> i $ go xs (i - n)
        go _ _ = overEx "(>!)"
    
    {-# INLINE writeM #-}
    writeM (AnyChunks es) = go es
      where
        go (x : xs) i e = do n <- getSizeOf x; i < n ? writeM x i e $ go xs (i - n) e
        go _ _ _ = return ()
    
    getLeft  (AnyChunks es) = concat <$> mapM getLeft es
    getRight (AnyChunks es) = (concat . reverse) <$> mapM getRight es
    reversed (AnyChunks es) = (AnyChunks . reverse) <$> mapM reversed es
    
    filled c e = AnyChunks <$> sequence (replicate d (filled lim e) :< filled n e)
      where
        (d, n) = c `divMod` lim
    
    -- TODO: rewrite without SplitM
    copyTo src os trg ot c = when (c > 0) $ do
        when (os < 0 || ot < 0) $ underEx "copyTo"
        src' <- dropM os src
        trg' <- dropM ot trg
        go c src' trg'
      where
        go n xs@(AnyChunks (x : _)) ys@(AnyChunks (y : _)) = do
          n1 <- getSizeOf x
          n2 <- getSizeOf y
          let n' = minimum [n1, n2, n]
          
          copyTo x 0 y 0 n'
          xs' <- dropM n' xs
          ys' <- dropM n' ys
          go (n - n') xs' ys'
        go n _ _ = when (n > 0) $ overEx "copyTo"
    
    -- | Unsafe, returns joined stream of existing chunks.
    merged = return . AnyChunks . foldr (\ (AnyChunks es) ls -> es ++ ls) []
    
    ofoldrM f base' = ofoldrCh 0 base' <=< unpackM
      where
        ofoldrCh !o base (x : xs) = do
          n   <- getSizeOf x
          xs' <- ofoldrCh (o + n) base xs
          ofoldrM (f . (o +)) xs' x
        ofoldrCh _ base _ = return base
    
    ofoldlM f base' = ofoldlCh 0 base' <=< unpackM
      where
        ofoldlCh !o base (x : xs) = do
          n  <- getSizeOf x
          x' <- ofoldlM (f . (o +)) base x
          ofoldlCh (o + n) x' xs
        ofoldlCh _ base _ = return base
    
    o_foldlM f base (AnyChunks es) = foldl (flip $ (=<<) . flip (o_foldlM f)) (return base) es
    o_foldrM f base (AnyChunks es) = foldr ((=<<) . flip (o_foldrM f)) (return base) es

instance (BorderedM1 m rep Int e, SplitM1 m rep e) => SplitM m (AnyChunks rep e) e
  where
    takeM n (AnyChunks (e : es)) = n < 1 ? newNull $ do
      c <- getSizeOf e
      case n <=> c of
        EQ -> return (AnyChunks [e])
        LT -> do t <- takeM n e; return (AnyChunks [t])
        GT -> do (AnyChunks ts) <- takeM (n - c) (AnyChunks es); return $ AnyChunks (e : ts)
    takeM _ es = return es
    
    dropM n es'@(AnyChunks (e : es)) = n < 1 ? return es' $ do
      c <- getSizeOf e
      case n <=> c of
        EQ -> return (AnyChunks es)
        GT -> dropM (n - c) (AnyChunks es)
        LT -> do d <- dropM n e; return $ AnyChunks (d : es)
    dropM _ es = return es
    
    prefixM f (AnyChunks es) = foldr (\ e p -> do n <- getSizeOf e; c <- prefixM f e; c == n ? (+ c) <$> p $ return c) (return 0) es
    suffixM f (AnyChunks es) = foldl (\ p e -> do n <- getSizeOf e; c <- suffixM f e; c == n ? (+ c) <$> p $ return c) (return 0) es
    mprefix f (AnyChunks es) = foldr (\ e p -> do n <- getSizeOf e; c <- mprefix f e; c == n ? (+ c) <$> p $ return c) (return 0) es
    msuffix f (AnyChunks es) = foldl (\ p e -> do n <- getSizeOf e; c <- msuffix f e; c == n ? (+ c) <$> p $ return c) (return 0) es

--------------------------------------------------------------------------------

{- Set and Scan instances. -}

instance (Nullable (AnyChunks rep e), SetWith1 (AnyChunks rep) e, Ord e) => Set (AnyChunks rep e) e

instance (SetWith1 rep e, Linear1 rep e, Ord (rep e), Bordered1 rep Int e) => SetWith (AnyChunks rep e) e
  where
    insertWith f' e' = AnyChunks . go f' e' . unpack
      where
        go f e (x : xs) = memberWith f e x ? insertWith f e x : xs $ x : go f e xs
        go _ e _ = [single e]
    
    deleteWith f' e' = AnyChunks . go f' e' . unpack
      where
        go f e (x : xs) = memberWith f e x ? deleteWith f e x : xs $ x : go f e xs
        go _ _ _ = []
    
    intersectionWith f = fromList ... on (intersectionWith f) listL
    unionWith        f = fromList ... on (unionWith        f) listL
    differenceWith   f = fromList ... on (differenceWith   f) listL
    symdiffWith      f = fromList ... on (symdiffWith      f) listL
    
    lookupLTWith f o = foldr ((<|>) . lookupLTWith f o) Nothing . unpack
    lookupLEWith f o = foldr ((<|>) . lookupLEWith f o) Nothing . unpack
    lookupGTWith f o = foldr ((<|>) . lookupGTWith f o) Nothing . unpack
    lookupGEWith f o = foldr ((<|>) . lookupGEWith f o) Nothing . unpack
    
    memberWith f x (AnyChunks es) = memberWith f x `any` es
    
    isSubsetWith f xs ys = o_foldr (\ e b -> memberWith f e ys && b) True xs

instance (Linear1 (AnyChunks rep) e) => Scan (AnyChunks rep e) e

--------------------------------------------------------------------------------

{- Indexed instance. -}

instance (Indexed1 rep Int e) => Map (AnyChunks rep e) Int e
  where
    toMap ascs = isNull ascs ? Z $ assoc (ascsBounds ascs) ascs
    
    toMap' defvalue ascs = isNull ascs ? Z $ assoc' (ascsBounds ascs) defvalue ascs
    
    (.!) = (!^)
    
    Z // ascs = toMap ascs
    (AnyChunks es) // ascs = AnyChunks (go 0 es ascs)
      where
        go _    []     _  = []
        go _    xs    [ ] = xs
        go l (x : xs) ies = x // as : go n xs bs
          where
            (as, bs) = partition (inRange (l, n - 1) . fst) ies
            n = l + sizeOf es
    
    (.$) p (AnyChunks (x : xs)) = p .$ x <|> (+ sizeOf x) <$> p .$ AnyChunks xs
    (.$) _ _ = Nothing
    
    (*$) p (AnyChunks (x : xs)) = p *$ x ++ fmap (+ sizeOf x) (p *$ AnyChunks xs)
    (*$) _ _ = []
    
    kfoldr f base es = let bnds = bounds es in kfoldr (f . index bnds) base es
    kfoldl f base es = let bnds = bounds es in kfoldl (f . index bnds) base es

instance (Indexed1 rep Int e) => Indexed (AnyChunks rep e) Int e
  where
    assoc bnds ascs = AnyChunks (go bnds ascs)
      where
        go (l, u) ies = isEmpty (l, u) ? [] $ assoc (l, n) as : go (n + 1, u) bs
          where
            (as, bs) = partition (inRange (l, n) . fst) ies
            n = min u (l + lim)
    
    assoc' bnds defvalue ascs = AnyChunks (go bnds ascs)
      where
        go (l, u) ies = isEmpty (l, u) ? [] $ assoc' (l, n) defvalue as : go (n + 1, u) bs
          where
            (as, bs) = partition (inRange (l, n) . fst) ies
            n = min u (l + lim)
    
    fromIndexed es = AnyChunks [fromIndexed es]

--------------------------------------------------------------------------------

{- MapM, IndexedM and SortM instances. -}

instance (SplitM1 m rep e, MapM1 m rep Int e, BorderedM1 m rep Int e) => MapM m (AnyChunks rep e) Int e
  where
    newMap ascs = AnyChunks <$> sequence (go (ascsBounds ascs) ascs)
      where
        go (l, u) ies = isEmpty (l, u) ? [] $ newMap as : go (n + 1, u) bs
          where
            (as, bs) = partition (inRange (l, n) . fst) ies
            n = min u (l + lim)
    
    newMap' defvalue ascs = AnyChunks <$> sequence (go (ascsBounds ascs) ascs)
      where
        go (l, u) ies = newMap' defvalue as : go (n + 1, u) bs
          where
            (as, bs) = partition (inRange (l, n) . fst) ies
            n = min u (l + lim)
    
    {-# INLINE (>!) #-}
    es >! i = i < 0 ? overEx "(>!)" $ es !#> i
    
    overwrite es'@(AnyChunks []) ascs = isNull ascs ? return es' $ newMap ascs
    overwrite es'@(AnyChunks es) ascs = es' <$ go 0 es ((< 0) . fst `except` ascs)
      where
        go o (x : xs) ie = unless (null ie) $ do
          n <- getSizeOf x
          let (as, bs) = partition (\ (i, _) -> i < n + o) ie
          overwrite x as >> go (n + o) xs bs
        go _ _ _ = return ()
    
    kfoldrM = ofoldrM
    kfoldlM = ofoldlM

instance (SplitM1 m rep e, IndexedM1 m rep Int e) => IndexedM m (AnyChunks rep e) Int e
  where
    fromAssocs bnds ascs = AnyChunks <$> sequence (go bnds ascs)
      where
        go (l, u) ies = isEmpty (l, u) ? [] $ fromAssocs (l, n) as : go (n + 1, u) bs
          where
            (as, bs) = partition (inRange (l, n) . fst) ies
            n = min u (l + lim)
    
    fromAssocs' bnds defvalue ascs = AnyChunks <$> sequence (go bnds ascs)
      where
        go (l, u) ies = fromAssocs' (l, n) defvalue as : go (n + 1, u) bs
          where
            (as, bs) = partition (inRange (l, n) . fst) ies
            n = min u (l + lim)
    
    {-# INLINE writeM' #-}
    writeM' es i e = (i < 0) `unless` writeM es i e
    
    fromIndexed' = newLinear  .  listL
    fromIndexedM = newLinear <=< getLeft

instance (BorderedM1 m rep Int e, SplitM1 m rep e, LinearM1 m rep e) => SortM m (AnyChunks rep e) e
  where
    sortMBy = timSortBy

--------------------------------------------------------------------------------

-- | Creates new local immutable structure and thaw it as fast, as possible.
instance {-# OVERLAPPABLE #-} (Linear1 imm e, Thaw1 m imm mut e) => Thaw m (AnyChunks imm e) (mut e)
  where
    unsafeThaw (AnyChunks es) = unsafeThaw (concat es)
    thaw       (AnyChunks es) = unsafeThaw (concat es)

-- | Creates one-chunk mutable stream, may be memory inefficient.
instance {-# OVERLAPPABLE #-} (Thaw1 m imm mut e) => Thaw m (imm e) (AnyChunks mut e)
  where
    unsafeThaw = fmap (AnyChunks . single) . unsafeThaw
    thaw       = fmap (AnyChunks . single) . thaw

instance {-# OVERLAPS #-} (Thaw1 m imm mut e) => Thaw m (AnyChunks imm e) (AnyChunks mut e)
  where
    unsafeThaw (AnyChunks imm) = AnyChunks <$> mapM unsafeThaw imm
    thaw       (AnyChunks imm) = AnyChunks <$> mapM thaw imm

-- | Creates one-chunk immutable stream, may be memory inefficient.
instance {-# OVERLAPPABLE #-} (Freeze1 m mut imm e) => Freeze m (mut e) (AnyChunks imm e)
  where
    unsafeFreeze = fmap (AnyChunks . single) . unsafeFreeze
    freeze       = fmap (AnyChunks . single) . freeze

-- | Creates new immutable structure using 'merged'.
instance {-# OVERLAPPABLE #-} (LinearM1 m mut e, Freeze1 m mut imm e) => Freeze m (AnyChunks mut e) (imm e)
  where
    unsafeFreeze (AnyChunks es) = unsafeFreeze =<< merged es
    freeze       (AnyChunks es) = freeze       =<< merged es

instance {-# OVERLAPS #-} (Freeze1 m mut imm e) => Freeze m (AnyChunks mut e) (AnyChunks imm e)
  where
    unsafeFreeze (AnyChunks mut) = AnyChunks <$> mapM unsafeFreeze mut
    freeze       (AnyChunks mut) = AnyChunks <$> mapM freeze mut

--------------------------------------------------------------------------------

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Templates.AnyChunks."

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Templates.AnyChunks."

pfailEx :: String -> a
pfailEx =  throw . PatternMatchFail . showString "in SDP.Templates.AnyChunks."

unpack :: (Nullable (rep e)) => AnyChunks rep e -> [rep e]
unpack =  \ (AnyChunks es) -> except isNull es

unpackM :: (BorderedM1 m rep Int e) => AnyChunks rep e -> m [rep e]
unpackM (AnyChunks es) = go es
  where
    go (x : xs) = do n <- getSizeOf x; n < 1 ? go xs $ (x :) <$> go xs
    go _ = return []

lim :: Int
lim =  1024


