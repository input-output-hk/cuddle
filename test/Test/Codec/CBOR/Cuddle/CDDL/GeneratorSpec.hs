{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.GeneratorSpec (spec) where

import Codec.CBOR.Cuddle.CBOR.Gen (GenPhase, generateFromName)
import Codec.CBOR.Cuddle.CBOR.Validator (validateCBOR)
import Codec.CBOR.Cuddle.CDDL (Name)
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, MonoSimple, fullResolveCDDL)
import Codec.CBOR.Cuddle.Huddle (Huddle, toCDDL)
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..), mapCDDLDropExt)
import Codec.CBOR.Pretty (prettyHexEnc)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (Term (..), decodeTerm, encodeTerm)
import Codec.CBOR.Write (toStrictByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Test.AntiGen (ZapResult (..), prettyZapResult, runAntiGen, zapAntiGenResult)
import Test.Codec.CBOR.Cuddle.CDDL.Examples.Huddle (
  bytesExample,
  customGenExample,
  opCertExample,
  optionalMapExample,
  rangeListExample,
  rangeMapExample,
  refTermExample,
  sizeBytesExample,
  sizeTextExample,
 )
import Test.Codec.CBOR.Cuddle.CDDL.Validator (expectInvalid)
import Test.Hspec (HasCallStack, Spec, describe, runIO, shouldSatisfy)
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, Testable (..), counterexample)
import Text.Pretty.Simple (pShow)

generateCDDL :: CTreeRoot GenPhase -> Gen Term
generateCDDL cddl = runAntiGen $ generateFromName cddl "root"

tryResolveHuddle :: HasCallStack => Huddle -> SpecM () (CTreeRoot MonoReferenced)
tryResolveHuddle huddle = do
  case fullResolveCDDL . mapCDDLDropExt $ toCDDL huddle of
    Right x -> pure x
    Left err -> runIO . fail $ "Failed to resolve CDDL:\n" <> show err

expectZapInvalidates :: CTreeRoot MonoReferenced -> Name -> Property
expectZapInvalidates cddl name = property $ do
  res@ZapResult {zrValue} <- zapAntiGenResult 1 $ generateFromName (mapIndex cddl) name
  let
    bs = toStrictByteString $ encodeTerm zrValue
    validationRes = validateCBOR bs name $ mapIndex cddl
    failMsg =
      unlines
        [ case deserialiseFromBytes decodeTerm (LBS.fromStrict bs) of
            Right (_, t) -> prettyHexEnc (encodeTerm t)
            Left _ -> mempty
        , mempty
        , T.unpack $ prettyZapResult res
        ]
  pure . counterexample failMsg $ expectInvalid validationRes

zapInvalidatesHuddle :: String -> Huddle -> Spec
zapInvalidatesHuddle n huddle = do
  cddl <- tryResolveHuddle huddle
  prop n . counterexample (TL.unpack . pShow $ mapIndex @_ @_ @MonoSimple cddl) $
    expectZapInvalidates cddl "root"

spec :: Spec
spec = do
  describe "Negative generator" $ do
    describe "Zapped value fails to validate" $ do
      zapInvalidatesHuddle "customGen" customGenExample
      zapInvalidatesHuddle "refTerm" refTermExample
      zapInvalidatesHuddle "opCert" opCertExample
      zapInvalidatesHuddle "sizeText" sizeTextExample
      zapInvalidatesHuddle "sizeBytes" sizeBytesExample
      zapInvalidatesHuddle "rangeList" rangeListExample
      zapInvalidatesHuddle "rangeMap" rangeMapExample
      zapInvalidatesHuddle "optionalMapExample" optionalMapExample

  describe "Custom generators" $ do
    describe "Huddle" $ do
      customGenExampleCddl <- tryResolveHuddle customGenExample
      prop "If a term has a custom generator then it is used" $ do
        res <- generateCDDL $ mapIndex customGenExampleCddl
        pure $
          res `shouldSatisfy` \case
            TInt i -> i > 3 && i < 7
            _ -> False
      refTermExampleCddl <- tryResolveHuddle refTermExample
      prop "Custom generator works when called via reference" $ do
        res <- generateCDDL $ mapIndex refTermExampleCddl
        pure $
          res `shouldSatisfy` \case
            TList [TInt 0, TInt i] -> i > 3 && i < 7
            TListI [TInt 0, TInt i] -> i > 3 && i < 7
            _ -> False
      bytesExampleCddl <- tryResolveHuddle bytesExample
      prop "Bytes are generated correctly" $ do
        res <- generateCDDL $ mapIndex bytesExampleCddl
        pure $
          res `shouldSatisfy` \case
            TBytes "\x01\x02\x03\xff" -> True
            TBytesI "\x01\x02\x03\xff" -> True
            _ -> False
