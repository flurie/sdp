{-# LANGUAGE Unsafe, MagicHash, UnboxedTuples, BangPatterns, RoleAnnotations #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module SDP.Array.Immutable
(
  module SDP.Array.Mutable,
  
  module SDP.Indexed,
  module SDP.Linear,
  module SDP.Scan,
  
  Array (..)
)
where

import Prelude ()
import SDP.SafePrelude

import GHC.Base
  (
    MutableArray#, Array#, Int (..),
    newArray#, unsafeFreezeArray#, writeArray#, indexArray#,
    isTrue#, (+#), (-#), (==#)
  )

import GHC.Show ( appPrec, appPrec1 )
import GHC.ST   ( ST (..), STRep, runST )

import Text.Read
import Text.Read.Lex ( expect )

import SDP.Array.Mutable
import SDP.Indexed
import SDP.Linear
import SDP.Scan
-- import SDP.Set

import SDP.Simple

--------------------------------------------------------------------------------

{-
  This Array type definition is no different from the standard GHC.Arr,
  but I have to redefine it because of the limitation of the Ix type.
-}

data Array i e = Array
               !i                  {- lower  bound -}
               !i                  {- upper  bound -}
               {-# UNPACK #-} !Int {-     size     -}
               (Array# e)          {-   elements   -}

type role Array nominal representational

--------------------------------------------------------------------------------

{- Eq and Eq1 instances -}

instance (Index i, Eq e) => Eq (Array i e) where (==) = eq1

instance (Index i) => Eq1 (Array i)
  where
    liftEq eq xs ys = (n1 == 0 && n2 == 0) || (l1 == l2 && u1 == u2 && elemsEq)
      where
        elemsEq = and [ (xs !# i) `eq` (ys !# i) | i <- [0 .. n1 - 1] ]
        
        (Array l1 u1 n1 _) = xs
        (Array l2 u2 n2 _) = ys

--------------------------------------------------------------------------------

{- Ord and Ord1 instances -}

instance (Index i, Ord e) => Ord (Array i e)
  where
    compare = compare `on` assocs


instance (Index i) => Ord1 (Array i)
  where
    liftCompare cmp xs ys = liftCompare cmp' (assocs xs) (assocs ys)
      where
        cmp' (ix, x) (iy, y) = (ix <=> iy) <> (cmp x y)

--------------------------------------------------------------------------------

{- Show and Read instances -}

instance (Index i, Show i, Show e) => Show (Array i e)
  where
    showsPrec p arr@(Array l u _ _) = showParen (p > appPrec) shows'
      where
        shows' = showString "array " . shows (l, u) . showChar ' ' . shows (toList arr)

instance (Index i, Read i, Read e) => Read (Array i e)
  where
    readList = readListDefault
    readPrec = parens $ prec appPrec (lift . expect $ Ident "array") >> liftA2 assoc (step readPrec) (step readPrec)

--------------------------------------------------------------------------------

{- Functor, Zip and Applicative instances -}

instance (Index i) => Functor (Array i)
  where
    fmap f arr@(Array l u n@(I# n#) _) = runST
      (
        ST $ \ s1# ->
          case newArray# n# (undEx "fmap") s1# of
            (# s2#, marr# #) ->
              let go i s# = if i == n
                  then done (l, u) n marr# s#
                  else fill marr# (i, f $ arr !# i) (go $ i + 1) s#
              in go 0 s2#
      )

instance (Index i) => Zip (Array i)
  where
    zipWith f as bs              = fromListN size $ apply <$> range (0, size - 1)
      where
        apply i = f (as !# i) (bs !# i)
        size    = eminimum [EList as, EList bs]
    
    zipWith3 f as bs cs          = fromListN size $ apply <$> range (0, size - 1)
      where
        apply i = f (as !# i) (bs !# i) (cs !# i)
        size    = eminimum [EList as, EList bs, EList cs]
    
    zipWith4 f as bs cs ds       = fromListN size $ apply <$> range (0, size - 1)
      where
        apply i = f (as !# i) (bs !# i) (cs !# i) (ds !# i)
        size    = eminimum [EList as, EList bs, EList cs, EList ds]
    
    zipWith5 f as bs cs ds es    = fromListN size $ apply <$> range (0, size - 1)
      where
        apply i = f (as !# i) (bs !# i) (cs !# i) (ds !# i) (es !# i)
        size    = eminimum [EList as, EList bs, EList cs, EList ds, EList es]
    
    zipWith6 f as bs cs ds es fs = fromListN size $ apply <$> range (0, size - 1)
      where
        apply i = f (as !# i) (bs !# i) (cs !# i) (ds !# i) (es !# i) (fs !# i)
        size    = eminimum [EList as, EList bs, EList cs, EList ds, EList es, EList fs]

instance (Index i) => Applicative (Array i)
  where
    pure = single
    fs <*> es = (<$> es) `concatMap` fs

--------------------------------------------------------------------------------

{- Foldable, Scan and Traversable instances -}

instance (Index i) => Foldable (Array i)
  where
    foldr  f init arr = go 0
      where
        go i = arr ==. i ? init $ f (arr !# i) (go $ i + 1)
    
    foldl  f init arr = go $ length arr - 1
      where
        go i = i == -1 ? init $ f (go $ i - 1) (arr !# i)
    
    foldr' f init arr = go (length arr - 1) init
      where
        go i a = i == -1 ? a $ go (i - 1) (f (arr !# i) $! a)
    
    foldl' f init arr = go 0 init
      where
        go i a = (arr ==. i) ? a $ go (i + 1) ((f $! a) $ arr !# i)
    
    foldr1 f arr = null arr ? undEx "foldr1" $ go 0
      where
        go i = arr ==. (i + 1) ? e $ f e (go $ i + 1) where e = arr !# i
    
    foldl1 f arr = null arr ? undEx "foldl1" $ go (length arr - 1)
      where
        go i = i == 0 ? (arr !# 0) $ f (go $ i - 1) (arr !# i)
    
    toList arr@(Array _ _ n _) = (arr !#) <$> [0 .. n - 1]
    
    null       (Array _ _ n _) = n < 1
    
    length     (Array _ _ n _) = n

instance (Index i) => Scan (Array i)
  where
    scanl f w es = null es ? single w $ fromListN l (w : res w 0)
      where
        l = length es + 1
        -- res generates infinite list, but fromListN catches it.
        res !curr !n = next : res next (n + 1)
          where
            next = f curr (es !# n)
    
    scanr f w es = null es ? single w $ fromListN l (res w (l - 2) [w])
      where
        l = length es + 1
        res !curr !n ws = n < 0 ? ws $ res prev (n - 1) (prev : ws)
          where
            prev = f (es !# n) curr
    
    scanl' f w es = null es ? single w $ fromListN l (w : res w 0)
      where
        l = length es + 1
        res !curr !n = next : res next (n + 1)
          where
            next = f curr (es !# n)
    
    scanl1 f es = null es ? undEx "scanl1" $ fromListN l (res w 0)
      where
        l = length es
        w = head es
        res !curr !n = next : res next (n + 1)
          where
            next = f curr (es !# n)
    
    scanr1 f es = null es ? undEx "scanr1" $ fromList (res w (l - 2) [w])
      where
        l = length es
        w = last es
        res !curr !n ws = n < 0 ? ws $ res prev (n - 1) (prev : ws)
          where
            prev = f (es !# n) curr

instance (Index i) => Traversable (Array i)
  where
    traverse f arr = fromList <$> traverse f (toList arr)

--------------------------------------------------------------------------------

{- Linear and Bordered instances. -}

instance (Index i) => Linear (Array i)
  where
    fromListN n@(I# n#) es = runST $ ST 
        (
          \ s1# -> case newArray# n# (undEx "fromList") s1# of
              (# s2#, marr# #) ->
                let go y r = \ i# s3# -> case writeArray# marr# i# y s3# of
                      s4# -> if isTrue# (i# ==# n# -# 1#)
                                then s4#
                                else r (i# +# 1#) s4#
                in done (l, u) n marr#
                (
                  if n == 0 then s2# else foldr go (\ _ s# -> s#) es 0# s2#
                )
        )
      where
        l = unsafeIndex 0
        u = unsafeIndex (n - 1)
    
    -- O(n + m) concatenation
    xs ++ ys = fromList $ on (++) toList xs ys
    
    toHead e es = fromList (e : toList es)
    toLast es e = fromList (foldr (:) [e] es)
    
    head arr = arr !# 0
    last arr = arr !# (length arr - 1)
    
    tail es = fromListN (length es - 1) $ toList es
    init es = fromListN (length es - 1) $ toList es
    
    -- O (n) take.
    take n es = n < 1 ? Z  $ fromListN (length es - n) $ toList es
    
    -- O (n) drop.
    drop n es = n < 1 ? es $ fromListN n [ es !# i | i <- [n .. length es - 1] ]
    
    -- O(n) reverse
    reverse es = fromListN n $ listR es where n = length es
    
    -- O(n) right list view
    listR   es = [es !# i | i <- [n - 1, n - 2 .. 0] ] where n = length es
    
    -- O(n * m) concatenation
    concatMap f = concat . map f . toList
    
    -- No more than O(n) comparing.
    isPrefixOf xs ys = xs .<=. ys && and equals
      where
        equals = [ xs !# i == ys !# i | i <- [0 .. ly - 1] ]
        ly = length ys
    
    -- No more than O(n) comparing.
    isSuffixOf xs ys = xs .<=. ys && and equals
      where
        equals = [ xs !# i == xs !# (i + offset) | i <- [0 .. ly - 1] ]
        offset = length xs - ly
        ly = length ys
    
    -- Default isInfixOf, no more than O(n) comparing.
    
    takeWhile pred es = take (pred `prefix` es) es
    dropWhile pred es = drop (pred `prefix` es) es
    
    takeEnd   pred es = drop (pred `suffix` es) es
    dropEnd   pred es = take (pred `suffix` es) es
    
    concat = fromList . toList'
      where
        -- the same as "fmap toList . toList" but on bit faster
        toList' = foldr (\ a l -> toList a ++ l) []

instance (Index i) => Bordered (Array i) i
  where
    lower   (Array l _ _ _) = l
    upper   (Array _ u _ _) = u
    bounds  (Array l u _ _) = (l, u)
    indices (Array l u _ _) = range (l, u)
    assocs  arr = zip (indices arr) (toList arr)

--------------------------------------------------------------------------------

{- Indexed instance. -}

instance (Index i) => Indexed (Array i) i
  where
    assoc' bnds def assocs = runST (ST $ \ s1# -> case newArray# n# def s1# of (# s2#, marr# #) -> writes marr# s2#)
      where
        writes marr# = foldr (fill marr#) (done bnds n marr#) ies
          where ies  = [ (offset bnds i, e) | (i, e) <- assocs ]
        n@(I# n#)    = size bnds
    
    arr@(Array l u n@(I# n#) arr#) // assocs = runST $ thaw >>= writes
      where
        writes (STArray l u n marr#) = ST $ foldr (fill marr#) (done (l, u) n marr#) ies
          where ies = [ (offset (l, u) i, e) | (i, e) <- assocs ]
        
        thaw = ST $ \s1# -> case newArray# n# (undEx "(//)") s1# of
          (# s2#, marr# #) ->
            let copy i@(I# i#) s3# = if i == n then s3# else copy (i + 1) (writeArray# marr# i# (arr !# i) s3#)
            in case copy 0 s2# of s3# -> (# s3#, STArray l u n marr# #)
    
    (.!) arr@(Array l u _ _) i = arr !# offset (l, u) i
    (!)  arr@(Array l u _ _) i = arr !# offset (l, u) i
    (!?) arr@(Array l u _ _)   = (not . inRange (l, u)) ?: (arr !)
    
    (.$) pred es = find (pred . (es !)) $ indices es
    (*$) pred es = fromList . filter (pred . (es !)) $ indices es

--------------------------------------------------------------------------------

instance (Index i) => Estimate (Array i)
  where
    (Array _ _ n1 _) <==> (Array _ _ n2 _) = n2 <=> n2
    (Array _ _ n1 _) .>.  (Array _ _ n2 _) = n1  >  n2
    (Array _ _ n1 _) .<.  (Array _ _ n2 _) = n1  <  n2
    (Array _ _ n1 _) .>=. (Array _ _ n2 _) = n1  >= n2
    (Array _ _ n1 _) .<=. (Array _ _ n2 _) = n1  <= n2
    
    (Array _ _ n1 _) >.  n2 = n1 >  n2
    (Array _ _ n1 _) <.  n2 = n1 <  n2
    (Array _ _ n1 _) >=. n2 = n1 >= n2
    (Array _ _ n1 _) <=. n2 = n1 <= n2

-- [internal]: write Set instance.
-- instance (Index i) => Set (Array i)

--------------------------------------------------------------------------------

{-# INLINE (!#) #-}
(!#) :: Array i e -> Int -> e
(!#) (Array _ _ _ arr#) (I# i#) = case indexArray# arr# i# of (# e #) -> e

{-# INLINE done #-}
done :: (i, i) -> Int -> MutableArray# s e -> STRep s (Array i e)
done (l, u) n marr# = \s1# -> case unsafeFreezeArray# marr# s1# of
  (# s2#, arr# #) -> (# s2#, Array l u n arr# #)

prefix :: (Index i) => (e -> Bool) -> (Array i e) -> Int
prefix pred arr@(Array _ _ n _) = last' 0
  where
    last'   c = (inRange (0, n - 1) c && satisfy c) ? (last' $! c + 1) $! c
    satisfy c = pred $ arr !# c

suffix :: (Index i) => (e -> Bool) -> (Array i e) -> Int
suffix pred arr@(Array _ _ n _) = init' $ n - 1
  where
    init'   c = (inRange (0, n - 1) c && satisfy c) ? (init' $! c - 1) $! c + 1
    satisfy c = pred $ arr !# c

undEx msg = throw . UndefinedValue $ "in SDP.Array." ++ msg

