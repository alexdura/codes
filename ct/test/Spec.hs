import Test.Hspec

import qualified MatrixExtrasSpec
import qualified GaloisSpec
import qualified LinearCodeSpec

main :: IO ()
main = hspec $ do {
  MatrixExtrasSpec.tests;
  GaloisSpec.tests;
  LinearCodeSpec.tests;
  }
