{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DefaultSignatures #-}

{- |
    Module      :  SDP.IndexedM
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (requires non-portable-modules)
  
  @SDP.IndexedM@ provides 'IndexedM', 'IFoldM', 'Freeze' and 'Thaw' classes.
-}
module SDP.IndexedM
  (
    -- * Exports
    module SDP.LinearM,
    module SDP.Indexed,
    
    -- * IndexedM
    IndexedM (..),
    
    -- * IFoldM
    IFoldM (..),
    
    -- * Freeze and Thaw
    Freeze (..), Thaw (..),
    
    -- * Related functions
    swapM
  )
where

import Prelude ()
import SDP.SafePrelude

import SDP.LinearM
import SDP.Indexed

import SDP.Simple

default ()

infixl 5 !#>, >!, !>, !?>

--------------------------------------------------------------------------------

-- | Class for work with mutable indexed structures.
class (Monad m, Index i) => IndexedM m v i e | v -> m, v -> i, v -> e
  where
    {-# MINIMAL fromAssocs', fromIndexed', fromIndexedM, overwrite, ((>!)|(!?>)) #-}
    
    {-# INLINE fromAssocs #-}
    -- | fromAssocs returns new mutable structure created from assocs.
    fromAssocs :: (i, i) -> [(i, e)] -> m v
    fromAssocs bnds ascs =  fromAssocs' bnds (undEx "fromAssocs") ascs
    
    -- | fromAssocs' return new mutable structure created from assocs and default element
    fromAssocs' :: (i, i) -> e -> [(i, e)] -> m v
    
    -- | (!#>) is unsafe monadic offset-based reader.
    {-# INLINE (!#>) #-}
    default (!#>) :: (BorderedM m v i e) => v -> Int -> m e
    (!#>) :: v -> Int -> m e
    es !#> i = do bnds <- getBounds es; es >! index bnds i
    
    -- | (>!) is unsafe monadic reader.
    {-# INLINE (>!) #-}
    (>!) :: v -> i -> m e
    es >! i = fromMaybe (undEx "(!)") <$> es !?> i
    
    -- | (!>) is well-safe monadic reader.
    {-# INLINE (!>) #-}
    (!>) :: v -> i -> m e
    default (!>) :: (BorderedM m v i e) => v -> i -> m e
    (!>) es i = getBounds es >>= \ bnds -> case inBounds bnds i of
        IN -> es >! i
        ER -> throw $ EmptyRange     msg
        OR -> throw $ IndexOverflow  msg
        UR -> throw $ IndexUnderflow msg
      where
        msg = "in SDP.IndexedM.(!>) [default]"
    
    -- | (!?>) is completely safe monadic reader.
    default (!?>) :: (BorderedM m v i e) => v -> i -> m (Maybe e)
    (!?>) :: v -> i -> m (Maybe e)
    (!?>) es = getIndexOf es ?> (es >!)
    
    default writeM_ :: (BorderedM m v i e) => v -> Int -> e -> m ()
    writeM_ :: v -> Int -> e -> m ()
    writeM_ es i e = do bnds <- getBounds es; void $ overwrite es [(index bnds i, e)]
    
    default writeM :: (BorderedM m v i e) => v -> i -> e -> m ()
    writeM :: v -> i -> e -> m ()
    writeM es i e = do b <- getIndexOf es i; when b . void $ overwrite es [(i, e)]
    
    -- | fromIndexed' is overloaded version of thaw.
    fromIndexed' :: (Bordered v' j e, Indexed v' j e) => v' -> m v
    
    -- | fromIndexed converts one mutable structure to other.
    fromIndexedM :: (BorderedM m v' j e, IndexedM m v' j e) => v' -> m v
    
    -- | overwrite rewrites mutable structure using assocs.
    overwrite :: v -> [(i, e)] -> m v
    overwrite es ies = mapM_ (uncurry $ writeM es) ies >> return es
    
    -- | reshape creates new indexed structure from old with reshaping function.
    reshape :: (IndexedM m v' j e) => (i, i) -> v' -> (i -> j) -> m v
    reshape bnds es f = fromAssocs bnds =<< range bnds `forM` \ i -> do e <- es !> f i; return (i, e)
    
    default fromAccum :: (BorderedM m v i e) => (e -> e' -> e) -> v -> [(i, e')] -> m v
    fromAccum :: (e -> e' -> e) -> v -> [(i, e')] -> m v
    fromAccum f es ascs = bindM2 (getBounds es) ies fromAssocs
      where
        ies = sequence [ do e <- es !> i; return (i, f e e') | (i, e') <- ascs ]
    
    -- | updateM updates elements with specified indices by function.
    updateM :: v -> [i] -> (i -> e -> e) -> m v
    updateM es is f = forM is (\ i -> do e <- es !> i; return (i, f i e)) >>= overwrite es
    
    -- | (.?) is monadic version of (.$).
    (.?) :: (e -> Bool) -> v -> m (Maybe i)
    f .? es = listToMaybe <$> f *? es
    
    -- | (*?) is monadic version of (*$).
    default (*?) :: (BorderedM m v i e) => (e -> Bool) -> v -> m [i]
    (*?) :: (e -> Bool) -> v -> m [i]
    p *? marr = fsts . filter (p . snd) <$> getAssocs marr

--------------------------------------------------------------------------------

-- | IFoldM is monadic version of IFold.
class (IndexedM m v i e) => IFoldM m v i e
  where
    {-# MINIMAL ifoldrM, ifoldlM #-}
    
    -- | ifoldrM is right monadic fold with index
    ifoldrM  :: (i -> e -> r -> m r) -> r -> v -> m r
    
    -- | ifoldlM is left  monadic fold with index
    ifoldlM  :: (i -> r -> e -> m r) -> r -> v -> m r
    
    i_foldrM :: (e -> r -> m r) -> r -> v -> m r
    i_foldlM :: (r -> e -> m r) -> r -> v -> m r
    
    -- | i_foldr is just foldrM in IFoldM context
    i_foldrM f = ifoldrM (const f)
    
    -- | i_foldl is just foldlM in IFoldM context
    i_foldlM f = ifoldlM (const f)

--------------------------------------------------------------------------------

-- | Service class of immutable-mutable converters.
class (Monad m) => Thaw m v v' | v' -> m
  where
    {- |
      thaw is safe way to convert a immutable structure to a mutable.
      
      thaw should copy the old structure or ensure that it will not be used
      after calling the procedure.
    -}
    thaw :: v -> m v'
    
    {- |
      unsafeThaw is unsafe version of 'thaw'.
      
      unsafeThaw doesn't guarantee that the structure will be copied or locked.
      It only guarantees that if the old structure is not used, no error will
      occur.
    -}
    unsafeThaw :: v -> m v'
    unsafeThaw =  thaw

--------------------------------------------------------------------------------

-- | Service class of mutable-immutable converters.
class (Monad m) => Freeze m v' v | v' -> m
  where
    {- |
      freeze is a safe way to convert a mutable structure to a immutable.
      
      freeze should copy the old structure or ensure that it will not be used
      after calling the procedure.
    -}
    freeze :: v' -> m v
    
    {- |
      unsafeFreeze is unsafe version of 'freeze'.
      
      unsafeFreeze doesn't guarantee that the structure will be copied or locked.
      It only guarantees that if the old structure is not used, no error will
      occur.
    -}
    unsafeFreeze :: v' -> m v
    unsafeFreeze =  freeze

--------------------------------------------------------------------------------

{-# INLINE swapM #-}
-- | swaps two elements. And yet, I'm tired of writing this swap everywhere.
swapM :: (IndexedM m v i e) => v -> Int -> Int -> m ()
swapM es i j = do ei <- es !#> i; writeM_ es i =<< es !#> j; writeM_ es j ei

--------------------------------------------------------------------------------

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.IndexedM" ++ msg




