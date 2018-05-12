import Test.Hspec

import qualified MatrixExtrasSpec
import qualified GaloisFieldSpec
import qualified LinearCodeSpec
import qualified RSCodeSpec

main :: IO ()
main = hspec $ do {
  MatrixExtrasSpec.tests;
  GaloisFieldSpec.tests;
  LinearCodeSpec.tests;
  RSCodeSpec.tests;
  }
