{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.Validator (spec) where

import Codec.CBOR.Cuddle.CBOR.Gen (generateFromName)
import Codec.CBOR.Cuddle.CBOR.Validator (
  CBORTermResult (..),
  ValidatorStage,
  ValidatorStageSimple,
  isCBORTermResultValid,
  showSimple,
  validateCBOR,
 )
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (CustomValidatorResult (..))
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
  int,
  mp,
  opt,
  toCDDL,
  withValidator,
  (+>),
  (<+),
  (=:=),
  (==>),
 )
import Codec.CBOR.Cuddle.IndexMappable (mapCDDLDropExt, mapIndex)
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Codec.CBOR.Term (Term (..), encodeTerm)
import Codec.CBOR.Write (toStrictByteString)
import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Either (fromRight)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as LT
import Paths_cuddle (getDataFileName)
import Test.AntiGen (runAntiGen)
import Test.Hspec (
  Expectation,
  HasCallStack,
  Spec,
  describe,
  expectationFailure,
  runIO,
 )
import Test.Hspec.QuickCheck
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  NonNegative (..),
  choose,
  counterexample,
  elements,
  forAll,
  infiniteListOf,
  listOf,
  listOf1,
  oneof,
  scale,
  shuffle,
  vectorOf,
 )
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
  describe path $ do
    forM_ (Map.keys $ Map.filter isRule m) $ \name@(Name n) ->
      prop (T.unpack n) $ do
        cborTerm <- runAntiGen $ generateFromName resolvedCddl name
        let
          generatedCbor = toStrictByteString $ encodeTerm cborTerm
          res = validateCBOR generatedCbor name (mapIndex resolvedCddl)
          extraInfo =
            unlines
              [ "Term result:"
              , LT.unpack . pShow $ mapIndex @_ @_ @ValidatorStageSimple res
              , "====="
              , "CBOR term:"
              , LT.unpack $ pShow cborTerm
              ]
        pure . counterexample extraInfo $
          unless (isCBORTermResultValid res) $
            expectationFailure $
              "Predicate failed on result:\n" <> showSimple res

huddleMap :: Huddle
huddleMap =
  collectFrom
    [ HIRule $
        "a"
          =:= mp
            [ idx 1 ==> arr [0 <+ a VUInt]
            , 1 <+ asKey VBytes ==> VAny
            , opt $ idx 2 ==> VBool
            , 0 <+ asKey VText ==> VInt
            ]
    ]

huddleRangeMap :: Huddle
huddleRangeMap =
  collectFrom
    [ HIRule $
        "a"
          =:= mp
            [ 5 <+ asKey VInt ==> VBool +> 10
            ]
    ]

genInfiniteUniqueList :: Ord a => Gen a -> Gen [a]
genInfiniteUniqueList = fmap nubOrd . infiniteListOf

genHuddleRangeMap :: (Int, Int) -> Gen Term
genHuddleRangeMap rng@(lo, hi) = do
  n <- choose rng
  let genKV = (,) <$> fmap TInt arbitrary <*> fmap TBool arbitrary
  genMapTerm . take n =<< scale (const $ max lo hi) (genInfiniteUniqueList genKV)

huddleArray :: Huddle
huddleArray =
  collectFrom
    [ HIRule $
        "a"
          =:= arr
            [ 0 <+ a VBool
            , 1 <+ a VInt
            , opt $ a VText
            , a VUInt
            ]
    ]

genHuddleArrayRequiredTerms :: Gen [Term]
genHuddleArrayRequiredTerms = do
  ints <- listOf1 $ TInt <$> arbitrary
  text <-
    oneof
      [ (: []) <$> (genStringTerm . T.pack =<< arbitrary)
      , pure []
      ]
  lastInt <- TInt . getNonNegative <$> arbitrary
  pure $ ints <> text <> [lastInt]

genHuddleArrayTerms :: Gen [Term]
genHuddleArrayTerms = do
  bools <- listOf $ TBool <$> arbitrary
  required <- genHuddleArrayRequiredTerms
  pure $ bools <> required

genHuddleArray :: Gen Term
genHuddleArray = genHuddleArrayTerms >>= genArrayTerm

genBadArrayReversed :: Gen Term
genBadArrayReversed = genHuddleArrayTerms >>= genArrayTerm . reverse . (TBool True :)

genBadArrayMissingLastInt :: Gen Term
genBadArrayMissingLastInt =
  genHuddleArrayTerms >>= genArrayTerm . reverse . dropWhile isNonNegativeInt . reverse
  where
    isNonNegativeInt (TInt x) | x >= 0 = True
    isNonNegativeInt _ = False

huddleRangeArray :: Huddle
huddleRangeArray =
  collectFrom
    [ HIRule $
        "a"
          =:= arr
            [ opt $ a VInt
            , 2 <+ a VInt +> 3
            , a VBool +> 3
            , 3 <+ a VText
            ]
    ]

genHuddleRangeArray :: Gen Term
genHuddleRangeArray = do
  numInts <- choose (3, 4)
  ints <- vectorOf numInts $ TInt <$> arbitrary
  numBools <- choose (0, 3)
  bools <- vectorOf numBools $ TBool <$> arbitrary
  numTexts <- choose (3, 10)
  texts <- vectorOf numTexts $ genStringTerm . T.pack =<< arbitrary
  genArrayTerm $ ints <> bools <> texts

genArrayTerm :: [Term] -> Gen Term
genArrayTerm xs = elements [TList xs, TListI xs]

genMapTerm :: [(Term, Term)] -> Gen Term
genMapTerm x = elements [TMap x, TMapI x]

genStringTerm :: Text -> Gen Term
genStringTerm x = elements [TString x, TStringI (LT.fromStrict x)]

genBytesTerm :: ByteString -> Gen Term
genBytesTerm x = elements [TBytes x, TBytesI $ LBS.fromStrict x]

arbitraryByteString :: Gen ByteString
arbitraryByteString = BS.pack <$> arbitrary

arbitraryTerm :: Gen Term
arbitraryTerm =
  oneof
    [ TInt <$> arbitrary
    , TInteger <$> arbitrary
    , TBytes . BS.pack <$> arbitrary
    , TBytesI . LBS.pack <$> arbitrary
    , TString . T.pack <$> arbitrary
    , TStringI . LT.pack <$> arbitrary
    , TList <$> listOf (scale (`div` 2) arbitraryTerm)
    , TListI <$> listOf (scale (`div` 2) arbitraryTerm)
    , TMap <$> listOf (scale (`div` 2) $ (,) <$> arbitraryTerm <*> arbitraryTerm)
    , TMapI <$> listOf (scale (`div` 2) $ (,) <$> arbitraryTerm <*> arbitraryTerm)
    , -- TODO properly implement tagged generation
      -- , TTagged <$> arbitrary <*> arbitraryTerm
      TBool <$> arbitrary
    , pure TNull
    , pure $ TSimple 23 -- TODO add other values once they are supported by cuddle
    , THalf <$> arbitrary
    , TFloat <$> arbitrary
    , TDouble <$> arbitrary
    ]

genFullMap :: Gen Term
genFullMap = do
  field1 <- do
    es <- listOf $ TInt . getNonNegative <$> arbitrary
    pure (TInt 1, TList es)
  lField2 <-
    oneof
      [ do
          b <- arbitrary
          pure [(TInt 2, TBool b)]
      , pure []
      ]
  strFields <-
    nubOrdOn fst <$> listOf ((,) <$> (genStringTerm . T.pack =<< arbitrary) <*> (TInt <$> arbitrary))
  bytesFields <-
    nubOrdOn fst
      <$> listOf1 ((,) <$> (genBytesTerm =<< arbitraryByteString) <*> arbitraryTerm)
  allFields <- shuffle $ field1 : lField2 <> strFields <> bytesFields
  genMapTerm allFields

genBadMapInvalidIndex :: Gen Term
genBadMapInvalidIndex =
  pure $
    TMap
      [ (TInt 1, TList [])
      , (TInt 99, TList [])
      , (TBytes "foo", TBytes "bar")
      ]

validateHuddle ::
  HasCallStack =>
  Huddle ->
  Name ->
  Term ->
  CBORTermResult ValidatorStage
validateHuddle huddle name term = do
  let
    resolvedCddl = case fullResolveCDDL . mapCDDLDropExt $ toCDDL huddle of
      Right root -> root
      Left err -> error $ show err
    bs = toStrictByteString $ encodeTerm term
  validateCBOR bs name (mapIndex resolvedCddl)

expectValid :: CBORTermResult ValidatorStage -> Expectation
expectValid x | True <- isCBORTermResultValid x = pure ()
expectValid x = expectationFailure $ "Expected a success, got\n" <> showSimple x

expectInvalid :: CBORTermResult ValidatorStage -> Expectation
expectInvalid x | False <- isCBORTermResultValid x = pure ()
expectInvalid x = expectationFailure $ "Expected a failure, but got\n" <> showSimple x

_shouldResultIn :: CBORTermResult ValidatorStage -> CBORTermResult ValidatorStage -> Expectation
_shouldResultIn got expected
  | let gotSimple = mapIndex @_ @_ @ValidatorStageSimple got
  , let expectedSimple = mapIndex @_ @_ @ValidatorStageSimple expected
  , gotSimple == expectedSimple =
      pure ()
_shouldResultIn got expected =
  expectationFailure $
    "Expected:\n" <> showSimple expected <> "\nActual:\n" <> showSimple got

stringValidator :: Term -> CustomValidatorResult
stringValidator (TString _) = ValidatorSuccess
stringValidator (TStringI _) = ValidatorSuccess
stringValidator t = ValidatorFailure $ "Expected a string, got\n" <> T.pack (show t)

bytesValidator :: Term -> CustomValidatorResult
bytesValidator (TBytes _) = ValidatorSuccess
bytesValidator (TBytesI _) = ValidatorSuccess
bytesValidator t = ValidatorFailure $ "Expected bytes, got\n" <> T.pack (show t)

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
    describe "Maps and arrays" $ do
      describe "Positive" $ do
        prop "Validates a full map" . forAll genFullMap $
          expectValid . validateHuddle huddleMap "a"
        prop "Validates array" . forAll genHuddleArray $
          expectValid . validateHuddle huddleArray "a"
        prop "Validates map with correct number of range elements"
          . forAll (genHuddleRangeMap (5, 10))
          $ expectValid . validateHuddle huddleRangeMap "a"
        prop "Validates array with ranges" . forAll genHuddleRangeArray $
          expectValid . validateHuddle huddleRangeArray "a"
      describe "Negative" $ do
        prop "Fails to validate a map with an unexpected index"
          . forAll genBadMapInvalidIndex
          $ expectInvalid . validateHuddle huddleMap "a"
        prop "Fails to validate reversed array" . forAll genBadArrayReversed $
          expectInvalid . validateHuddle huddleArray "a"
        prop "Fails to validate array with missing non-negative int at the end"
          . forAll genBadArrayMissingLastInt
          $ expectInvalid . validateHuddle huddleArray "a"
        prop "Fails to validate map with too few range elements"
          . forAll (genHuddleRangeMap (0, 4))
          $ expectInvalid . validateHuddle huddleRangeMap "a"
        prop "Fails to validate map with too many range elements"
          . forAll (genHuddleRangeMap (11, 20))
          $ expectInvalid . validateHuddle huddleRangeMap "a"

  describe "Custom validator" $ do
    describe "Positive" $ do
      prop "Validates with custom validator" $ do
        term <- genStringTerm . T.pack =<< arbitrary
        num <- arbitrary
        let
          huddle =
            collectFrom
              [ HIRule . withValidator stringValidator $ "a" =:= int num
              ]
        pure . expectValid $ validateHuddle huddle "a" term
      prop "Validates with custom validator behind a reference" $ do
        stringTerm <- genStringTerm . T.pack =<< arbitrary
        arrTerm <- genArrayTerm [stringTerm]
        num <- arbitrary
        let
          ruleA = withValidator stringValidator $ "a" =:= int num
          huddle =
            collectFrom
              [ HIRule ruleA
              , HIRule $ "b" =:= arr [a ruleA]
              ]
        pure . expectValid $ validateHuddle huddle "b" arrTerm
    describe "Negative" $ do
      prop "Fails if term is valid against the Huddle, but not the custom validator" $ do
        stringTerm <- genStringTerm . T.pack =<< arbitrary
        let
          huddle =
            collectFrom
              [ HIRule . withValidator bytesValidator $ "a" =:= VText
              ]
        pure . expectInvalid $ validateHuddle huddle "a" stringTerm
      prop "Fails if term is valid against the Huddle, but not the custom validator, behind a reference" $ do
        stringTerm <- genStringTerm . T.pack =<< arbitrary
        let
          ruleA = withValidator bytesValidator $ "a" =:= VText
          ruleB = "b" =:= arr [a ruleA]
          huddle =
            collectFrom
              [ HIRule ruleA
              , HIRule ruleB
              ]
        pure . expectInvalid $ validateHuddle huddle "a" stringTerm
