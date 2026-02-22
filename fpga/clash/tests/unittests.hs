import Prelude

import Test.Tasty

import qualified Tests.NrCbSegment
import qualified Tests.CbCrcInterleave

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.NrCbSegment.cbSegTests
  , Tests.CbCrcInterleave.cbCrcInterleaveTests
  ]
