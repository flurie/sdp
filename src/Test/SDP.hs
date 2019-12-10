{- |
    Module      :  Test.SDP
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  non-portable (imports non-portable modules)
  
  @Test.SDP@ reexports all tests in SDP, except "Test.SDP.Index".
-}
module Test.SDP
  (
    -- * Exports
    module Test.SDP.Estimate,
    module Test.SDP.Indexed,
    module Test.SDP.Linear,
    module Test.SDP.Sort,
    module Test.SDP.Set,
    module Test.SDP.Ord,
    module Test.SDP.Eq
  )
where

import Prelude ()

import Test.SDP.Estimate
import Test.SDP.Indexed
import Test.SDP.Linear
import Test.SDP.Sort
import Test.SDP.Set
import Test.SDP.Ord
import Test.SDP.Eq




