{-# LANGUAGE Safe #-}

{- |
    Module      :  Control.Monad.Rope
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    "Control.Monad.Rope" provides 'RopeM' - lazy stream of dependent monadic
    calculations.
-}
module Control.Monad.Rope
(
  -- * Rope
  RopeM (..),
  
  evalInit, nextR, runRope
)
where

import Prelude ()
import SDP.SafePrelude

default ()

--------------------------------------------------------------------------------

-- | 'RopeM' is primitive monadic sequence representation.
data RopeM m a = RopeEnd | RopeM (m (a, RopeM m a))

-- | 'runRope' returns full rope evaluation.
runRope :: (Monad m) => RopeM m a -> m [a]
runRope RopeEnd = return []
runRope (RopeM rope) = do
  (a, rope') <- rope
  as <- runRope rope'
  return (a : as)

{- |
  @evalInit rope n@ evaluates @n@ steps of the @rope@, returns a list of results
  and the tail of the @rope@.
-}
evalInit :: (Monad m) => RopeM m a -> Int -> m ([a], RopeM m a)
evalInit =  \ rope n -> n < 1 ? return ([], rope) $ eval n rope
  where
    eval 0     rope     = return ([], rope)
    eval _   RopeEnd    = return ([], RopeEnd)
    eval n (RopeM rope) = do
      (a, rope') <- rope
      (as, rest) <- eval (n - 1) rope'
      return (a : as, rest)

-- | @nextR rope@ is just @evalInit rope 1@.
nextR :: (Monad m) => RopeM m a -> m ([a], RopeM m a)
nextR =  (`evalInit` 1)




