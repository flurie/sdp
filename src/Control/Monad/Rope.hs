{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

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
nextR    RopeEnd   = return ([], RopeEnd)
nextR (RopeM rope) = (\ (a, rope') -> ([a], rope')) <$> rope




