{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.GeneratorSpec (spec) where

import Codec.CBOR.Cuddle.CBOR.Gen (GenPhase, generateFromName)
import Codec.CBOR.Cuddle.CBOR.Validator (validateCBOR)
import Codec.CBOR.Cuddle.CDDL (Name)
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (WrappedTerm (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, MonoSimple, fullResolveCDDL)
import Codec.CBOR.Cuddle.Huddle (
  CanQuantify (..),
  Huddle,
  HuddleItem (..),
  Value (..),
  a,
  arr,
  asKey,
  collectFrom,
  mp,
  sized,
  toCDDL,
  withGenerator,
  (...),
  (<+),
  (=:=),
  (==>),
 )
import Codec.CBOR.Cuddle.Huddle qualified as H
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..), mapCDDLDropExt)
import Codec.CBOR.Pretty (prettyHexEnc)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (Term (..), decodeTerm, encodeTerm)
import Codec.CBOR.Term qualified as C
import Codec.CBOR.Write (toStrictByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Lazy qualified as TL
import Data.Word (Word64)
import Test.AntiGen (runAntiGen, zapAntiGen)
import Test.Codec.CBOR.Cuddle.CDDL.Validator (expectInvalid)
import Test.Hspec (HasCallStack, Spec, describe, runIO, shouldBe, shouldSatisfy)
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, Testable (..), choose, counterexample)
import Text.Pretty.Simple (pShow)

simpleRule :: Name -> H.Rule
simpleRule n = withGenerator (\_ -> S . C.TInt <$> choose (4, 6)) $ n =:= arr [1, 2, 3]

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

sizeTextExample :: Huddle
sizeTextExample =
  collectFrom
    [HIRule $ "root" =:= VText `sized` (0 :: Word64, 32 :: Word64)]

sizeBytesExample :: Huddle
sizeBytesExample =
  collectFrom
    [HIRule $ "root" =:= VBytes `sized` (0 :: Word64, 32 :: Word64)]

rangeListExample :: Huddle
rangeListExample =
  collectFrom
    [ HIRule $
        "root"
          =:= arr
            [ 3 <+ a VInt +> 7
            ]
    ]

rangeMapExample :: Huddle
rangeMapExample =
  collectFrom
    [ HIRule $
        "root"
          =:= mp
            [ 3 <+ asKey VInt ==> VBool +> 7
            ]
    ]

optionalMapExample :: Huddle
optionalMapExample =
  collectFrom
    [ HIRule $
        "root"
          =:= mp
            [ 10 <+ asKey ((0 :: Integer) ... (10 :: Integer)) ==> VBool
            ]
    ]

generateCDDL :: CTreeRoot GenPhase -> Gen Term
generateCDDL cddl = runAntiGen $ generateFromName cddl "root"

tryResolveHuddle :: HasCallStack => Huddle -> SpecM () (CTreeRoot MonoReferenced)
tryResolveHuddle huddle = do
  case fullResolveCDDL . mapCDDLDropExt $ toCDDL huddle of
    Right x -> pure x
    Left err -> runIO . fail $ "Failed to resolve CDDL:\n" <> show err

expectZapInvalidates :: CTreeRoot MonoReferenced -> Name -> Property
expectZapInvalidates cddl name = property $ do
  value <- zapAntiGen 1 $ generateFromName (mapIndex cddl) name
  let
    bs = toStrictByteString $ encodeTerm value
    validationRes = validateCBOR bs name $ mapIndex cddl
    failMsg = case deserialiseFromBytes decodeTerm (LBS.fromStrict bs) of
      Right (_, t) -> prettyHexEnc (encodeTerm t)
      Left _ -> mempty
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
      zapInvalidatesHuddle "simpleTerm" simpleTermExample
      zapInvalidatesHuddle "refTerm" refTermExample
      zapInvalidatesHuddle "opCert" opCertExample
      zapInvalidatesHuddle "sizeText" sizeTextExample
      zapInvalidatesHuddle "sizeBytes" sizeBytesExample
      zapInvalidatesHuddle "rangeList" rangeListExample
      zapInvalidatesHuddle "rangeMap" rangeMapExample
      zapInvalidatesHuddle "optionalMapExample" optionalMapExample

  describe "Custom generators" $ do
    describe "Huddle" $ do
      simpleTermExampleCddl <- tryResolveHuddle simpleTermExample
      prop "If a term has a custom generator then it is used" $ do
        res <- generateCDDL $ mapIndex simpleTermExampleCddl
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
            _ -> False
      bytesExampleCddl <- tryResolveHuddle bytesExample
      prop "Bytes are generated correctly" $ do
        res <- generateCDDL $ mapIndex bytesExampleCddl
        pure $ res `shouldBe` TBytes "\x01\x02\x03\xff"
