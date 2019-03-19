module SDP.Scan ( Scan (..) ) where

import Prelude ()
import SDP.SafePrelude

import qualified Data.List as L

--------------------------------------------------------------------------------

{-
    Scan is an auxiliary class. It is needed  not so much for practical use, but
  for generalization.
-}

class (Foldable s) => Scan s
  where
    {-# MINIMAL scanl', scanr, scanl1, scanr1 #-}
    
    scanl   :: (w -> e -> w) -> w -> s e -> s w
    scanr   :: (e -> w -> w) -> w -> s e -> s w
    scanl'  :: (w -> e -> w) -> w -> s e -> s w
    
    scanl1  :: (e -> e -> e) -> s e -> s e
    scanr1  :: (e -> e -> e) -> s e -> s e
    
    scanl   = scanl'

--------------------------------------------------------------------------------

instance Scan []
  where
    scanl   = L.scanl
    scanr   = L.scanr
    scanl'  = L.scanl'
    scanl1  = L.scanl1
    scanr1  = L.scanr1

--------------------------------------------------------------------------------
