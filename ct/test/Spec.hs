import Test.Hspec

import qualified MatrixExtrasSpec

main :: IO ()
main = hspec $ do {
  MatrixExtrasSpec.tests;
  -- GaloisSpec.tests;
  }
