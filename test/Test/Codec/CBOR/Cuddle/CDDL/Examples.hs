{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.Examples (spec) where

import Codec.CBOR.Cuddle.CDDL (Value (..), ValueVariant (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTree (..), CTreeRoot')
import Codec.CBOR.Cuddle.CDDL.Postlude (PTerm (..))
import Codec.CBOR.Cuddle.CDDL.Prelude (prependPrelude)
import Codec.CBOR.Cuddle.CDDL.Resolve (
  MonoRef,
  NameResolutionFailure (..),
  OrRef (..),
  fullResolveCDDL,
 )
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Data.Functor.Identity (Identity)
import Data.Text.IO qualified as T
import Test.HUnit (assertFailure)
import Test.Hspec
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

tryValidateFile :: FilePath -> IO (Either NameResolutionFailure (CTreeRoot' Identity MonoRef))
tryValidateFile filePath = do
  contents <- T.readFile filePath
  cddl <- case parse pCDDL "" contents of
    Right x -> pure $ prependPrelude x
    Left x -> fail $ "Failed to parse the file:\n" <> errorBundlePretty x
  pure $ fullResolveCDDL cddl

validateExpectSuccess :: FilePath -> Spec
validateExpectSuccess filePath = it ("Successfully validates " <> filePath) $ do
  res <- tryValidateFile filePath
  case res of
    Right _ -> pure ()
    Left err -> assertFailure $ "Failed to validate:\n" <> show err

validateExpectFailure :: FilePath -> NameResolutionFailure -> Spec
validateExpectFailure filePath expectedFailure = it ("Fails to validate " <> filePath) $ do
  res <- tryValidateFile filePath
  case res of
    Right _ -> assertFailure "Expected a failure, but succeeded instead"
    Left e -> e `shouldBe` expectedFailure

spec :: Spec
spec = do
  describe "Validator" $ do
    describe "Positive" $ do
      validateExpectSuccess "example/cddl-files/byron.cddl"
      validateExpectSuccess "example/cddl-files/conway.cddl"
      validateExpectSuccess "example/cddl-files/shelley.cddl"
      validateExpectSuccess "example/cddl-files/basic_assign.cddl"
      validateExpectSuccess "example/cddl-files/issue80-min.cddl"
      validateExpectSuccess "example/cddl-files/pretty.cddl"
    describe "Negative" $ do
      validateExpectFailure "example/cddl-files/validator/negative/unknown-name.cddl" $
        UnboundReference "a"
      validateExpectFailure "example/cddl-files/validator/negative/too-few-args.cddl" $
        MismatchingArgs "foo" ["a", "b"]
      validateExpectFailure "example/cddl-files/validator/negative/too-many-args.cddl" $
        MismatchingArgs "foo" ["a"]
      validateExpectFailure "example/cddl-files/validator/negative/args-to-postlude.cddl" $
        ArgsToPostlude PTUInt [It (Literal (Value (VUInt 3) mempty))]
