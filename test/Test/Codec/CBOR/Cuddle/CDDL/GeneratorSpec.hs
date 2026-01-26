{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.GeneratorSpec (spec) where

import Codec.CBOR.Cuddle.CBOR.Gen (generateFromName)
import Codec.CBOR.Cuddle.CBOR.Validator (validateCBOR)
import Codec.CBOR.Cuddle.CDDL (Name)
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (WrappedTerm (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, fullResolveCDDL)
import Codec.CBOR.Cuddle.Huddle (
  Huddle,
  HuddleItem (..),
  Value (..),
  a,
  arr,
  collectFrom,
  sized,
  toCDDL,
  withGenerator,
  (=:=),
 )
import Codec.CBOR.Cuddle.Huddle qualified as H
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..), mapCDDLDropExt)
import Codec.CBOR.Term (Term (..), encodeTerm)
import Codec.CBOR.Term qualified as C
import Codec.CBOR.Write (toStrictByteString)
import Data.Word (Word64)
import Test.AntiGen (runAntiGen, zapAntiGen)
import Test.Codec.CBOR.Cuddle.CDDL.Validator (expectInvalid)
import Test.Hspec (HasCallStack, Spec, describe, runIO, shouldBe, shouldSatisfy)
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, Testable (..), choose)

simpleRule :: Name -> H.Rule
simpleRule n = withGenerator (S . C.TInt <$> choose (4, 6)) $ n =:= arr [1, 2, 3]

simpleTermExample :: Huddle
simpleTermExample =
  collectFrom
    [ HIRule $ simpleRule "root"
    ]

refTermExample :: Huddle
refTermExample =
  collectFrom
    [ HIRule $ "root" =:= arr [0, a $ simpleRule "bar"]
    ]

bytesExample :: Huddle
bytesExample =
  collectFrom
    [ HIRule $ "root" =:= H.bstr "010203ff"
    ]

opCertExample :: Huddle
opCertExample =
  collectFrom
    [ HIRule $
        "root"
          =:= arr
            [ a (VBytes `sized` (32 :: Word64))
            , a VUInt
            , a VUInt
            , a (VBytes `sized` (64 :: Word64))
            ]
    ]

generateCDDL :: CTreeRoot MonoReferenced -> Gen Term
generateCDDL cddl = runAntiGen $ generateFromName cddl "root"

tryResolveHuddle :: HasCallStack => Huddle -> SpecM () (CTreeRoot MonoReferenced)
tryResolveHuddle huddle = do
  case fullResolveCDDL . mapCDDLDropExt $ toCDDL huddle of
    Right x -> pure x
    Left err -> runIO . fail $ "Failed to resolve CDDL:\n" <> show err

expectZapInvalidates :: CTreeRoot MonoReferenced -> Name -> Property
expectZapInvalidates cddl name = property $ do
  value <- zapAntiGen 1 $ generateFromName cddl name
  let
    bs = toStrictByteString $ encodeTerm value
    validationRes = validateCBOR bs name $ mapIndex cddl
  pure $ expectInvalid validationRes

spec :: Spec
spec = do
  describe "Negative generator" $ do
    describe "Zapped value fails to validate" $ do
      do
        cddl <- tryResolveHuddle simpleTermExample
        prop "simpleTerm" $ expectZapInvalidates cddl "root"
      do
        cddl <- tryResolveHuddle refTermExample
        prop "refTerm" $ expectZapInvalidates cddl "root"
      do
        cddl <- tryResolveHuddle opCertExample
        prop "opCert" $ expectZapInvalidates cddl "root"

  describe "Custom generators" $ do
    describe "Huddle" $ do
      simpleTermExampleCddl <- tryResolveHuddle simpleTermExample
      prop "If a term has a custom generator then it is used" $ do
        res <- generateCDDL simpleTermExampleCddl
        pure $
          res `shouldSatisfy` \case
            TInt i -> i > 3 && i < 7
            _ -> False
      refTermExampleCddl <- tryResolveHuddle refTermExample
      prop "Custom generator works when called via reference" $ do
        res <- generateCDDL refTermExampleCddl
        pure $
          res `shouldSatisfy` \case
            TList [TInt 0, TInt i] -> i > 3 && i < 7
            _ -> False
      bytesExampleCddl <- tryResolveHuddle bytesExample
      prop "Bytes are generated correctly" $ do
        res <- generateCDDL bytesExampleCddl
        pure $ res `shouldBe` TBytes "\x01\x02\x03\xff"
