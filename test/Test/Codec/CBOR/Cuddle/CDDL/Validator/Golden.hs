{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.Validator.Golden (spec) where

import Codec.CBOR.Cuddle.CBOR.Validator (validateCBOR)
import Codec.CBOR.Cuddle.CBOR.Validator.Trace (prettyValidationResult)
import Codec.CBOR.Cuddle.CDDL (Name)
import Codec.CBOR.Cuddle.CDDL.Resolve (fullResolveCDDL)
import Codec.CBOR.Cuddle.Huddle (Huddle, toCDDL)
import Codec.CBOR.Cuddle.IndexMappable (mapCDDLDropExt, mapIndex)
import Codec.CBOR.Term (Term (..), encodeTerm)
import Codec.CBOR.Write qualified as CBOR
import Data.Either (fromRight)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String qualified as PP
import System.FilePath ((</>))
import Test.Codec.CBOR.Cuddle.CDDL.Examples.Huddle (huddleRangeArray)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Golden (Golden (..), defaultGolden)

huddleRangeArrayTermTwoStrings :: Term
huddleRangeArrayTermTwoStrings =
  TList
    [ TInt 0
    , TInt 1
    , TString "one"
    , TString "two"
    ]

validatorPrettyGolden :: String -> Huddle -> Name -> Term -> Spec
validatorPrettyGolden testName huddle n term =
  it testName $
    (defaultGolden testName str)
      { goldenFile = "golden" </> testName <> ".txt"
      , actualFile = Nothing
      }
  where
    bs = CBOR.toStrictByteString $ encodeTerm term
    treeRoot =
      fromRight (error "Failed to resolve CDDL") . fullResolveCDDL . mapCDDLDropExt $
        toCDDL huddle
    str =
      PP.renderString
        . layoutPretty defaultLayoutOptions
        $ prettyValidationResult (validateCBOR bs n $ mapIndex treeRoot)

spec :: Spec
spec = describe "golden" $ do
  describe "error messages" $ do
    validatorPrettyGolden
      "huddleRangeArrayTwoStrings"
      huddleRangeArray
      "a"
      huddleRangeArrayTermTwoStrings
