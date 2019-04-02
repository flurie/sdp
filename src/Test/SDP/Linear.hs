module Test.SDP.Linear
(
  Arbitrary (..),
  
  TestLinear,
  
  quickCheck,
  testLinear
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck
import SDP.Linear

default ()

--------------------------------------------------------------------------------

type TestLinear l e = Int -> e -> l e -> Bool

testLinear :: (Linear l, Eq e, Eq (l e), Arbitrary (l e)) => Int -> e -> l e -> Bool
testLinear n e line = and
  [
    null (fromList [] `asTypeOf` line), null (lzero `asTypeOf` line)
    
    , single e == (fromList [e] `asTypeOf` line)
    , not $ null (single e `asTypeOf` line)
    
    , null line || fromList (toList line) == line
    
    , null line || head line == head (toList line)
    , null line || last line == last (toList line)
    
    , null line || toList (init line) == init (toList line)
    , null line || toList (tail line) == tail (toList line)
    
    , toHead e line == fromList (e : toList line)
    , toLast line e == fromList (toList line ++ [e])
    
    , not . null $ toHead e line
    , not . null $ toLast line e
    
    , reverse (toList line) == toList (reverse line)
    , reverse (toList line) == listR line
    
    , all ( == e) ((replicate n e) `asTypeOf` line)
    , n < 0 || length ((replicate n e) `asTypeOf` line) == n
    
    , not $ n > 0 && null ((replicate n e) `asTypeOf` line)
    
    , toList line ++ listR line == toList (line ++ reverse line)
    
    , Z ++ line == line
    , line ++ Z == line
    
    , concat [line, reverse line] == line ++ reverse line
    , concat [line, reverse line] == fromList (concat [toList line, listR line])
    
    , null $ take (-n') line
    , null $ drop (length line) line
    
    , toList (take n' line) == take n' (toList line)
    , toList (drop n' line) == drop n' (toList line)
    
  ]
  where n' = max 0 n
