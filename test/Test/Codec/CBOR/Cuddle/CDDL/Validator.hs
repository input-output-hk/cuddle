{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.Validator (spec) where

import Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm)
import Codec.CBOR.Cuddle.CBOR.Validator (
  CBORTermResult (..),
  CDDLResult (..),
  isValid,
  validateCBOR,
 )
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.Postlude (appendPostlude)
import Codec.CBOR.Cuddle.CDDL.Resolve (fullResolveCDDL)
import Codec.CBOR.Cuddle.Huddle (
  Huddle,
  HuddleItem (..),
  Value (..),
  a,
  arr,
  asKey,
  collectFrom,
  idx,
  mp,
  opt,
  toCDDL,
  (<+),
  (=:=),
  (==>),
 )
import Codec.CBOR.Cuddle.IndexMappable (mapCDDLDropExt, mapIndex)
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Codec.CBOR.Term (Term (..), encodeTerm)
import Codec.CBOR.Write (toStrictByteString)
import Control.Monad (forM_)
import Data.Either (fromRight)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as LT
import Paths_cuddle (getDataFileName)
import Test.Hspec (
  Spec,
  describe,
  it,
  runIO,
  shouldSatisfy,
 )
import Test.Hspec.QuickCheck
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  Positive (..),
  counterexample,
  elements,
  forAll,
  listOf,
  noShrinking,
  oneof,
  shuffle,
  sublistOf,
 )
import Test.QuickCheck.Random (mkQCGen)
import Text.Megaparsec (runParser)
import Text.Pretty.Simple (pShow)

genAndValidateFromFile :: FilePath -> Spec
genAndValidateFromFile path = do
  contents <- runIO $ T.readFile =<< getDataFileName path
  let
    cddl = fromRight (error "Failed to parse CDDL") $ runParser pCDDL path contents
    resolverError x =
      error $ "Failed to resolve the CDDL from file " <> show path <> ":\n" <> show x
    resolvedCddl@(CTreeRoot m) =
      either resolverError id . fullResolveCDDL . appendPostlude $
        mapCDDLDropExt cddl
    isRule CTree.Group {} = False
    isRule _ = True
  describe path $
    forM_ (Map.keys $ Map.filter isRule m) $ \name@(Name n) ->
      prop (T.unpack n) . noShrinking $ \seed -> do
        let
          gen = mkQCGen seed
          cborTerm = generateCBORTerm resolvedCddl name gen
          generatedCbor = toStrictByteString $ encodeTerm cborTerm
          res = validateCBOR generatedCbor name (mapIndex resolvedCddl)
          extraInfo =
            unlines
              [ "Term result:"
              , LT.unpack $ pShow res
              , "====="
              , "CBOR term:"
              , LT.unpack $ pShow cborTerm
              ]
        counterexample extraInfo $
          res `shouldSatisfy` \case
            CBORTermResult _ Valid {} -> True
            _ -> False

huddleMap :: Huddle
huddleMap =
  collectFrom
    [ HIRule $
        "a"
          =:= mp
            [ idx 1 ==> arr [0 <+ a VUInt]
            , opt $ idx 2 ==> VBool
            , 0 <+ asKey VText ==> VInt
            ]
    ]

genMapTerm :: [(Term, Term)] -> Gen Term
genMapTerm x = elements [TMap x, TMapI x]

genStringTerm :: Text -> Gen Term
genStringTerm x = elements [TString x, TStringI (LT.fromStrict x)]

genFullMap :: Gen Term
genFullMap = do
  field1 <- do
    es <- listOf $ TInt . getPositive <$> arbitrary
    pure (TInt 1, TList es)
  lField2 <-
    oneof
      [ do
          b <- arbitrary
          pure [(TInt 2, TBool b)]
      , pure []
      ]
  strFieldsSet <-
    Set.fromList <$> listOf ((,) <$> (genStringTerm . T.pack =<< arbitrary) <*> (TInt <$> arbitrary))
  strFields <- sublistOf $ Set.toList strFieldsSet
  allFields <- shuffle $ field1 : lField2 <> strFields
  genMapTerm allFields

genBadMapInvalidIndex :: Gen Term
genBadMapInvalidIndex =
  pure $
    TMap
      [ (TInt 1, TList [])
      , (TInt 99, TList [])
      ]

validateHuddle :: Term -> Huddle -> Name -> CBORTermResult
validateHuddle term huddle name = do
  let
    resolvedCddl = case fullResolveCDDL . mapCDDLDropExt $ toCDDL huddle of
      Right root -> root
      Left err -> error $ show err
    bs = toStrictByteString $ encodeTerm term
  validateCBOR bs name (mapIndex resolvedCddl)

spec :: Spec
spec = describe "Validator" $ do
  describe "Generate and validate from file" $ do
    genAndValidateFromFile "example/cddl-files/basic_assign.cddl"
    genAndValidateFromFile "example/cddl-files/conway.cddl"
    genAndValidateFromFile "example/cddl-files/costmdls_min.cddl"
    genAndValidateFromFile "example/cddl-files/issue80-min.cddl"
    genAndValidateFromFile "example/cddl-files/pretty.cddl"
    genAndValidateFromFile "example/cddl-files/shelley.cddl"
    genAndValidateFromFile "example/cddl-files/validator.cddl"
  describe "Term tests" $ do
    describe "Positive" $ do
      prop "Validates a full map" . forAll genFullMap $ \cbor ->
        validateHuddle cbor huddleMap "a" `shouldSatisfy` isValid
    describe "Negative" $ do
      it "Fails to validate a map with an unexpected index"
        . forAll genBadMapInvalidIndex
        $ \cbor ->
          validateHuddle cbor huddleMap "a" `shouldSatisfy` not . isValid
