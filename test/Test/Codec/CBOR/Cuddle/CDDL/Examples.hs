module Test.Codec.CBOR.Cuddle.CDDL.Examples (spec) where

import Codec.CBOR.Cuddle.CDDL.Prelude (prependPrelude)
import Codec.CBOR.Cuddle.CDDL.Resolve (fullResolveCDDL)
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Data.Either (isRight)
import Data.Functor (($>))
import Data.Text.IO qualified as T
import Test.Hspec
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

validateFile :: FilePath -> Spec
validateFile filePath = it ("Successfully validates " <> filePath) $ do
  contents <- T.readFile filePath
  cddl <- case parse pCDDL "" contents of
    Right x -> pure $ prependPrelude x
    Left x -> fail $ "Failed to parse the file:\n" <> errorBundlePretty x
  fullResolveCDDL (cddl $> ()) `shouldSatisfy` isRight

spec :: Spec
spec = do
  validateFile "example/cddl-files/byron.cddl"
  validateFile "example/cddl-files/conway.cddl"
  validateFile "example/cddl-files/shelley.cddl"

-- TODO this one does not seem to terminate
-- validateFile "example/cddl-files/basic_assign.cddl"
