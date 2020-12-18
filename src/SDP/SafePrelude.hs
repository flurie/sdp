{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE Safe #-}

{- |
    Module      :  SDP.SafePrelude
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    Module of @sdp@ common definitions.
-}
module SDP.SafePrelude
(
  -- Exports
  module Control.Applicative,
  module Control.Monad,
  module Data.Foldable,
  module SDP.Comparing,
  module SDP.Estimate,
  
  module Prelude,
  
  (?), (?+), (?-), (?^), (?:), (+?), (...), (<=<<), (>>=>), (>>=<<)
)
where

import Prelude hiding
  (
    -- defined in SDP.Zip and Data.List
    zip, zip3, zipWith, zipWith3,
    
    -- defined in SDP.Scan and Data.List
    scanl, scanr, scanl1, scanr1,
    
    -- defined in SDP.Linear and Data.List
    head, tail, init, last, take, drop, (!!), (++), reverse, filter, lookup,
    concat, concatMap, replicate, takeWhile, dropWhile, iterate,
    
    -- defined in SDP.IO (sdp-io extension) and System.IO
    readFile, writeFile, appendFile, getContents, getLine, putStr, putStrLn
  )

import Control.Applicative
import Control.Monad

import Data.Foldable hiding ( concat, concatMap )

import SDP.Comparing
import SDP.Estimate

infixl 8 ?+, ?-
infixr 1  ?  -- Lowest priority, compatible with infixr 0 $
infixr 0 ...

default ()

--------------------------------------------------------------------------------

{- |
  Ternary operator.
  
  > (odd 1 ? "is True" $ "is False") == "is True"
-}
{-# INLINE (?) #-}
(?) :: Bool -> a -> a -> a
(?) =  \ p t e -> if p then t else e

-- | @p ?+ f $ a@ returns @'Just' (f a)@ if @(p a)@ and 'Nothing' otherwise.
{-# INLINE (?+) #-}
(?+) :: (a -> Bool) -> (a -> b) -> a -> Maybe b
(?+) =  \ p f a -> p a ? Just (f a) $ Nothing

-- | @p ?- f $ a@ returns 'Nothing' if @(p a)@ and @'Just' (f a)@ otherwise.
{-# INLINE (?-) #-}
(?-) :: (a -> Bool) -> (a -> b) -> a -> Maybe b
(?-) =  \ p f a -> p a ? Nothing $ Just (f a)

-- | Prepends 'Maybe' to list.
{-# INLINE (?:) #-}
(?:) :: Maybe a -> [a] -> [a]
(?:) =  \ mx xs -> case mx of {(Just x) -> x : xs; _ -> xs}

-- | Short version of 'fromMaybe'.
{-# INLINE (+?) #-}
(+?) :: a -> Maybe a -> a
(+?) =  \ x' mx -> case mx of {(Just x) -> x; _ -> x'}

-- | @(...) = (.) . (.)@.
{-# INLINE (...) #-}
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) =  (.) . (.)

--------------------------------------------------------------------------------

{-# INLINE (?^) #-}
-- | Lifted @('?')@.
(?^) :: (Monad m) => m Bool -> m a -> m a -> m a
(?^) =  \ mb mt me -> do b <- mb; if b then mt else me

-- | Monadic version of @('...')@.
(<=<<) :: (Monad m) => (c -> m d) -> (a -> b -> m c) -> (a -> b -> m d)
(<=<<) =  \ mg mf a b -> mf a b >>= mg

-- | Monadic vesion of @('...')@ with reversed arguments.
(>>=>) :: (Monad m) => (a -> b -> m c) -> (c -> m d) -> (a -> b -> m d)
(>>=>) =  \ mf mg a b -> mf a b >>= mg

-- | @ma >>=<< mb@ is composition of 'join' and 'liftM2'.
{-# INLINE (>>=<<) #-}
(>>=<<) :: (Monad m) => m a -> m b -> (a -> b -> m c) -> m c
(>>=<<) = \ ma mb f -> join $ liftM2 f ma mb



