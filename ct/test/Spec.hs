import Test.Hspec

import qualified MatrixExtrasSpec
import qualified GaloisSpec
import qualified LinearCodeSpec
import qualified RSCodeSpec

main :: IO ()
main = hspec $ do {
  MatrixExtrasSpec.tests;
  GaloisSpec.tests;
  LinearCodeSpec.tests;
  RSCodeSpec.tests;
  }
