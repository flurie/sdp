module Test.SDP.Set
(
  Arbitrary (..),
  
  TestSet,
  
  quickCheck,
  testSet
)
where

import Prelude ()
import SDP.SafePrelude

import Test.QuickCheck

import SDP.Set

type TestSet s o = o -> s o -> s o -> Bool

testSet :: (Set s, Ord (s o), Ord o) => o -> s o -> s o -> Bool
testSet e sx sy = and
  [
    null sx == null sx' && null sy == null sy'
    
    , set sx' == sx'
    
    , (sx' \?/ sx') /= (sx' /?\ sx')
    
    , insert e sx' == sx' || not (e `elem` sx)
    , delete e sx' == sx' || (e `elem` sx)
    
    , (is `isSubseqOf` sx') && (is `isSubseqOf` sy') && (is `isSubseqOf` un)
    , (sx' `isSubseqOf` un) && (sy' `isSubseqOf` un)
    
    , (cp `isSubseqOf` sx') && (null cp || not (cp `isSubseqOf` sy')) && (cp `isSubseqOf` un)
    
    , (null sd && null is || (sd /?\ is)) && (sd `isSubseqOf` un)
    
    , (e `elem` sx) == (e `isSetElem` sx')
    , (e `elem` sx) == (e `elem` sx')
  ]
  where
    sx' = set sx
    sy' = set sy
    
    is = sx' /\  sy'
    un = sx' \/  sy'
    cp = sx' \\  sy'
    sd = sx' \^/ sy'

