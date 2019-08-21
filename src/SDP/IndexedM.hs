{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DefaultSignatures #-}

{- |
    Module      :  SDP.IndexedM
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (GHC-extensions)
  
  IndexedM is service class of SDP, designed to read and write mutable indexable
  data structures.
-}

module SDP.IndexedM
  (
    module SDP.LinearM,
    module SDP.Indexed,
    
    IndexedM (..),
    
    arrcopy
  )
where

import Prelude ()
import SDP.SafePrelude

import SDP.LinearM
import SDP.Indexed

import SDP.Simple

infixl 5 !#>

default ()

--------------------------------------------------------------------------------

-- | Class for work with mutable indexed structures.
class (Monad m, Index i) => IndexedM m v i e | v -> m, v -> i, v -> e
  where
    {-# MINIMAL fromAssocs', overwrite, ((!>)|(!?>)) #-}
    
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
    es >! i = es !> i
    
    -- | (!>) is well-safe monadic reader.
    {-# INLINE (!>) #-}
    (!>) :: v -> i -> m e
    (!>) dat i = fromMaybe (undEx "(!)") <$> dat !?> i
    
    -- | (!?>) is completely safe monadic reader.
    default (!?>) :: (BorderedM m v i e) => v -> i -> m (Maybe e)
    (!?>) :: v -> i -> m (Maybe e)
    es !?> i = getIndexOf es ?> (es !>) $ i
    
    default writeM_ :: (BorderedM m v i e) => v -> Int -> e -> m ()
    writeM_ :: v -> Int -> e -> m ()
    writeM_ es i e = do bnds <- getBounds es; void $ overwrite es [(index bnds i, e)]
    
    default writeM :: (BorderedM m v i e) => v -> i -> e -> m ()
    writeM :: v -> i -> e -> m ()
    writeM es i e = do b <- getIndexOf es i; when b . void $ overwrite es [(i, e)]
    
    -- | overwrite rewrites mutable structure using assocs.
    overwrite :: v -> [(i, e)] -> m v
    overwrite es ies = mapM_ (uncurry $ writeM es) ies >> return es
    
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

{-# INLINE arrcopy #-}
{- |
  arrcopy is a utility function that copies a fragment of the first array to the
  second array. This function is not intended for copying to an adjacent memory
  area. The first 2 Int arguments are the offsets in the first and second
  arrays, respectively, the third is the number of elements to be copied.
-}
arrcopy :: (IndexedM m v i e) => v -> v -> Int -> Int -> Int -> m ()
arrcopy xs ys ix iy count = copy ix iy (max 0 count)
  where
    -- I chose 0 as the recursion base because -1 doesn't look pretty.
    copy ox oy 0 = xs !#> ox >>= writeM_ ys oy
    copy ox oy c = xs !#> ox >>= writeM_ ys oy >> copy (ox + 1) (oy + 1) (c - 1)

--------------------------------------------------------------------------------

undEx :: String -> a
undEx msg = throw . UndefinedValue $ "in SDP.IndexedM" ++ msg




