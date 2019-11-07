{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  Control.Monad.Rope
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @Control.Monad.Rope@ provides 'RopeM' - lazy monadic stream.
-}
module Control.Monad.Rope
(
  RopeM (..),
  
  evalInit, nextR, runRope
)
where

import Prelude ()
import SDP.SafePrelude

default ()

--------------------------------------------------------------------------------

data RopeM m a = RopeEnd | RopeM (m (a, RopeM m a))

--------------------------------------------------------------------------------

runRope :: (Monad m) => RopeM m a -> m [a]
runRope RopeEnd = return []
runRope (RopeM rope) = do
  (a, rope') <- rope
  as <- runRope rope'
  return (a : as)

evalInit :: (Monad m) => RopeM m a -> Int -> m ([a], RopeM m a)
evalInit = \ rope n -> n < 1 ? return ([], rope) $ eval n rope
  where
    eval 0    rope   = return ([], rope)
    eval _  RopeEnd  = return ([], RopeEnd)
    eval n (RopeM rope) = do
      (a, rope') <- rope
      (as, rest) <- eval (n - 1) rope'
      return (a : as, rest)

nextR :: (Monad m) => RopeM m a -> m ([a], RopeM m a)
nextR rope = evalInit rope 1

