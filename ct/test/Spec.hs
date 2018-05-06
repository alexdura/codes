import Test.Hspec

import qualified MatrixExtrasSpec
import qualified GaloisSpec

main :: IO ()
main = hspec $ do {
  MatrixExtrasSpec.tests;
  GaloisSpec.tests;
  }
