{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.Validator (
  spec,
  expectValid,
  expectInvalid,
  genAndValidateCddl,
  genAndValidateRule,
  validateCBOR_,
) where

import Codec.CBOR.Cuddle.CBOR.Canonical (toCanonical)
import Codec.CBOR.Cuddle.CBOR.Gen (generateFromName)
import Codec.CBOR.Cuddle.CBOR.Term (
  CBORTerm (..),
  encodeCBORTerm,
  mkTermArray,
  mkTermBytes,
  mkTermBytesI,
  mkTermMap,
  mkTermNInt,
  mkTermString,
  mkTermStringI,
  mkTermTag,
  mkTermUInt,
  toNInt,
  unsignedToBytes,
 )
import Codec.CBOR.Cuddle.CBOR.Validator (
  ValidatorPhase,
  validateCBOR,
 )
import Codec.CBOR.Cuddle.CBOR.Validator.Trace (
  Evidenced (..),
  SValidity (..),
  ValidationTrace,
  defaultTraceOptions,
  prettyValidationTrace,
 )
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.Custom.Core (RuleTerm (..))
import Codec.CBOR.Cuddle.CDDL.Custom.Generator (GenConfig (..), runCBORGen)
import Codec.CBOR.Cuddle.CDDL.Custom.Validator (TermValidator)
import Codec.CBOR.Cuddle.CDDL.Postlude (appendPostlude)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, fullResolveCDDL)
import Codec.CBOR.Cuddle.Huddle (
  CanQuantify (..),
  GroupDef,
  Huddle,
  HuddleItem (..),
  Value (..),
  a,
  arr,
  collectFrom,
  int,
  opt,
  toCDDL,
  withValidator,
  (=:=),
  (=:~),
 )
import Codec.CBOR.Cuddle.Huddle qualified as H
import Codec.CBOR.Cuddle.IndexMappable (mapCDDLDropExt, mapIndex)
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Codec.CBOR.Pretty (prettyHexEnc)
import Codec.CBOR.Write (toStrictByteString)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Containers.ListUtils (nubOrdOn)
import Data.Either (fromRight)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as LT
import Paths_cuddle (getDataFileName)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Terminal (renderStrict)
import Test.AntiGen (runAntiGen)
import Test.Codec.CBOR.Cuddle.CDDL.Examples.Huddle (
  huddleArray,
  huddleMap,
  huddleRangeArray,
  huddleRangeMap,
 )
import Test.Hspec (
  Expectation,
  HasCallStack,
  Spec,
  describe,
  expectationFailure,
  it,
  runIO,
 )
import Test.Hspec.QuickCheck
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
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

validateCBOR_ ::
  HasCallStack =>
  BS.ByteString ->
  Name ->
  CTreeRoot ValidatorPhase ->
  Evidenced ValidationTrace
validateCBOR_ bs n cddl = either (\e -> error $ "validateCBOR failed:\n" <> show e) id $ validateCBOR bs n cddl

-- | Test that a specific rule in a resolved CDDL generates valid values
genAndValidateRule :: String -> Name -> CTreeRoot MonoReferenced -> Spec
genAndValidateRule description name resolvedCddl =
  prop description $ do
    let
      genCfg =
        GenConfig
          { gcRoot = mapIndex resolvedCddl
          , gcTwiddle = True
          }
    cborTerm <- runAntiGen . runCBORGen genCfg $ generateFromName name
    let
      generatedCbor = toStrictByteString $ encodeCBORTerm cborTerm
      res = validateCBOR_ generatedCbor name (mapIndex resolvedCddl)
      extraInfo =
        unlines
          [ "CBOR term:"
          , prettyHexEnc $ encodeCBORTerm cborTerm
          ]
    pure . counterexample extraInfo $ expectValid res

-- | Test that all rules in a resolved CDDL generate valid values
genAndValidateCddl :: String -> CTreeRoot MonoReferenced -> Spec
genAndValidateCddl description resolvedCddl@(CTreeRoot m) = do
  let
    isRule CTree.Group {} = False
    isRule _ = True
  describe description $ do
    forM_ (Map.keys $ Map.filter isRule m) $ \name@(Name n) ->
      genAndValidateRule (T.unpack n) name resolvedCddl

genAndValidateFromFile :: FilePath -> Spec
genAndValidateFromFile path = do
  contents <- runIO $ T.readFile =<< getDataFileName path
  let
    cddl = fromRight (error "Failed to parse CDDL") $ runParser pCDDL path contents
    resolverError x =
      error $ "Failed to resolve the CDDL from file " <> show path <> ":\n" <> show x
    resolvedCddl =
      either resolverError id . fullResolveCDDL . appendPostlude $
        mapCDDLDropExt cddl
  genAndValidateCddl path resolvedCddl

genInfiniteUniqueListOn :: Ord b => (a -> b) -> Gen a -> Gen [a]
genInfiniteUniqueListOn f = fmap (nubOrdOn f) . infiniteListOf

genHuddleRangeMap :: (Int, Int) -> Gen CBORTerm
genHuddleRangeMap rng@(lo, hi) = do
  n <- choose rng
  let genKV = (,) <$> fmap termInt arbitrary <*> fmap termBool arbitrary
  genMapTerm . take n
    =<< scale (const $ max lo hi) (genInfiniteUniqueListOn (toCanonical . fst) genKV)

genHuddleArrayRequiredTerms :: Gen [CBORTerm]
genHuddleArrayRequiredTerms = do
  ints <- listOf1 $ termInt <$> arbitrary
  text <-
    oneof
      [ (: []) <$> (genStringTerm . T.pack =<< arbitrary)
      , pure []
      ]
  lastInt <- mkTermUInt <$> arbitrary
  pure $ ints <> text <> [lastInt]

genHuddleArrayTerms :: Gen [CBORTerm]
genHuddleArrayTerms = do
  bools <- listOf $ termBool <$> arbitrary
  required <- genHuddleArrayRequiredTerms
  pure $ bools <> required

genHuddleArray :: Gen CBORTerm
genHuddleArray = genHuddleArrayTerms >>= genArrayTerm

genBadArrayReversed :: Gen CBORTerm
genBadArrayReversed = genHuddleArrayTerms >>= genArrayTerm . reverse . (TermSimple 21 :)

genBadArrayMissingLastInt :: Gen CBORTerm
genBadArrayMissingLastInt =
  genHuddleArrayTerms >>= genArrayTerm . reverse . dropWhile isNonNegativeInt . reverse
  where
    isNonNegativeInt (TermUInt _) = True
    isNonNegativeInt _ = False

genHuddleRangeArray :: Gen CBORTerm
genHuddleRangeArray = do
  numInts <- choose (3, 4)
  ints <- vectorOf numInts $ mkTermUInt <$> arbitrary
  numBools <- choose (0, 3)
  bools <- vectorOf numBools $ TermSimple <$> elements [20, 21]
  numTexts <- choose (3, 10)
  texts <- vectorOf numTexts $ genStringTerm . T.pack =<< arbitrary
  genArrayTerm $ ints <> bools <> texts

genArrayTerm :: [CBORTerm] -> Gen CBORTerm
genArrayTerm xs = elements [mkTermArray xs, TermArrayI xs]

genMapTerm :: [(CBORTerm, CBORTerm)] -> Gen CBORTerm
genMapTerm x = elements [mkTermMap x, TermMapI x]

genStringTerm :: Text -> Gen CBORTerm
genStringTerm x = elements [mkTermString x, mkTermStringI [LT.fromStrict x]]

genBytesTerm :: ByteString -> Gen CBORTerm
genBytesTerm x = elements [mkTermBytes x, mkTermBytesI [LBS.fromStrict x]]

termInt :: Int -> CBORTerm
termInt n
  | n >= 0 = mkTermUInt $ fromIntegral n
  | otherwise = mkTermNInt . fromJust . toNInt $ toInteger n

termBool :: Bool -> CBORTerm
termBool False = TermSimple 20
termBool True = TermSimple 21

arbitraryByteString :: Gen ByteString
arbitraryByteString = BS.pack <$> arbitrary

genFullMap :: Gen CBORTerm
genFullMap = do
  field1 <- do
    es <- listOf $ mkTermUInt <$> arbitrary
    pure (mkTermUInt 1, mkTermArray es)
  lField2 <-
    oneof
      [ do
          b <- arbitrary
          pure [(mkTermUInt 2, termBool b)]
      , pure []
      ]
  strFields <-
    nubOrdOn (toCanonical . fst)
      <$> listOf ((,) <$> (genStringTerm . T.pack =<< arbitrary) <*> (termInt <$> arbitrary))
  bytesFields <-
    nubOrdOn (toCanonical . fst)
      <$> listOf1 ((,) <$> (genBytesTerm =<< arbitraryByteString) <*> arbitrary)
  allFields <- shuffle $ field1 : lField2 <> strFields <> bytesFields
  genMapTerm allFields

genBadMapInvalidIndex :: Gen CBORTerm
genBadMapInvalidIndex =
  pure $
    mkTermMap
      [ (mkTermUInt 1, mkTermArray [])
      , (mkTermUInt 99, mkTermArray [])
      , (mkTermBytes "foo", mkTermBytes "bar")
      ]

validateHuddle ::
  HasCallStack =>
  Huddle ->
  Name ->
  CBORTerm ->
  Evidenced ValidationTrace
validateHuddle huddle name term = do
  let
    resolvedCddl = case fullResolveCDDL . mapCDDLDropExt $ toCDDL huddle of
      Right root -> root
      Left err -> error $ show err
    bs = toStrictByteString $ encodeCBORTerm term
  validateCBOR_ bs name (mapIndex resolvedCddl)

resolveCDDLText :: HasCallStack => Text -> CTreeRoot MonoReferenced
resolveCDDLText cddlText =
  let cddl = fromRight (error "Failed to parse CDDL") $ runParser pCDDL "" cddlText
   in either (error . show) id . fullResolveCDDL . appendPostlude $ mapCDDLDropExt cddl

-- | Validate a single term against a rule in an inline CDDL snippet.
validateTermText :: HasCallStack => Text -> Name -> CBORTerm -> Evidenced ValidationTrace
validateTermText cddlText name term =
  validateCBOR_
    (toStrictByteString $ encodeCBORTerm term)
    name
    (mapIndex $ resolveCDDLText cddlText)

expectValid :: Evidenced ValidationTrace -> Expectation
expectValid (Evidenced SValid _) = pure ()
expectValid (Evidenced SInvalid t) =
  expectationFailure . T.unpack $
    "Expected a success, got failure:\n"
      <> renderStrict (layoutPretty defaultLayoutOptions $ prettyValidationTrace defaultTraceOptions t)

expectInvalid :: Evidenced ValidationTrace -> Expectation
expectInvalid (Evidenced SValid t) =
  expectationFailure . T.unpack $
    "Expected a failure, but got success:\n"
      <> renderStrict (layoutPretty defaultLayoutOptions $ prettyValidationTrace defaultTraceOptions t)
expectInvalid _ = pure ()

stringValidator :: TermValidator
stringValidator (SingleTerm (TermString _)) = pure ()
stringValidator (SingleTerm (TermStringI _)) = pure ()
stringValidator t = fail $ "Expected a string, got\n" <> show t

bytesValidator :: TermValidator
bytesValidator (SingleTerm (TermBytes _)) = pure ()
bytesValidator (SingleTerm (TermBytesI _)) = pure ()
bytesValidator t = fail $ "Expected bytes, got\n" <> show t

spec :: Spec
spec = describe "Validator" $ do
  describe "Generate and validate from file" $ do
    genAndValidateFromFile "cddl/basic_assign.cddl"
    genAndValidateFromFile "cddl/conway.cddl"
    genAndValidateFromFile "cddl/costmdls_min.cddl"
    genAndValidateFromFile "cddl/issue80-min.cddl"
    genAndValidateFromFile "cddl/pretty.cddl"
    genAndValidateFromFile "cddl/shelley.cddl"
    genAndValidateFromFile "cddl/validator.cddl"
    genAndValidateCddl "inline: value ranges and bignum literals" . resolveCDDLText $
      T.unlines
        [ "closedRange = 1 .. 5"
        , "singletonRange = 5 .. 5"
        , "halfOpenRange = 1 ... 5"
        , "negativeRange = -5 .. -1"
        , "floatRange = 0.0 .. 1.0"
        , "halfOpenFloatRange = 0.0 ... 1.0"
        , "positiveBignum = 18446744073709551616"
        , "negativeBignum = -18446744073709551617"
        ]
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

    -- Regression tests showing that optional ('?') validation is broken when
    -- the optional element is inside a group (`grp` / `=:~`) that is itself
    -- referenced inside an array. When the optional element is absent,
    -- `consumeGroup` receives a corrupted leftover-term list (due to `(<>)`
    -- on the result tuple accumulating terms from the failed consume path),
    -- causing the continuation to see phantom extra terms and report them as
    -- `ListValidationLeftoverTerms` instead of succeeding.
    describe "Group" $ do
      describe "Optional inside a group referenced from an array" $ do
        let
          -- @
          --   root = [innerGrp, int]
          --   innerGrp = (int, ? bool)
          -- @
          -- => [int, ? bool, int]
          optionalInGroupHuddle :: Huddle
          optionalInGroupHuddle =
            let
              innerGrp :: GroupDef
              innerGrp = "innerGrp" =:~ [a VInt, opt $ a VBool]
             in
              collectFrom
                [ HIRule $ "root" =:= arr [a innerGrp, a VInt]
                ]
        it "Validates when optional entry is present" $
          expectValid $
            validateHuddle
              optionalInGroupHuddle
              "root"
              (mkTermArray [mkTermUInt 1, termBool True, mkTermUInt 2])
        it "Validates when optional entry is absent" $
          expectValid $
            validateHuddle optionalInGroupHuddle "root" (mkTermArray [mkTermUInt 1, mkTermUInt 2])
      describe "ZeroOrMore (*) inside a group referenced from an array" $ do
        let
          -- @
          --   root = [zeroOrMoreGrp, text]
          --   zeroOrMoreGrp = (* bool, int)
          -- @
          -- => [* bool, int, text]
          zeroOrMoreInGroupHuddle :: Huddle
          zeroOrMoreInGroupHuddle =
            let
              zeroOrMoreGrp :: GroupDef
              zeroOrMoreGrp = "zeroOrMoreGrp" =:~ [0 <+ a VBool, a VInt]
             in
              collectFrom
                [ HIRule $ "root" =:= arr [a zeroOrMoreGrp, a VText]
                ]

        it "Validates when zero-or-more entry has elements" $
          expectValid $
            validateHuddle
              zeroOrMoreInGroupHuddle
              "root"
              (mkTermArray [termBool True, termBool False, mkTermUInt 42, mkTermString "hi"])
        it "Validates when zero-or-more entry has zero elements" $
          expectValid $
            validateHuddle zeroOrMoreInGroupHuddle "root" (mkTermArray [mkTermUInt 42, mkTermString "hi"])
      describe "Bounded (n..m) inside a group referenced from an array" $ do
        -- @
        --   root = [boundedGrp, text]
        --   boundedGrp = (0..2 bool, int)
        -- @
        -- => [0..2 bool, int, text]
        let boundedInGroupHuddle :: Huddle
            boundedInGroupHuddle =
              let boundedGrp :: GroupDef
                  boundedGrp = "boundedGrp" =:~ [0 <+ a VBool +> 2, a VInt]
               in collectFrom
                    [ HIRule $ "root" =:= arr [a boundedGrp, a VText]
                    ]
        it "Validates when bounded entry has elements (within bounds)" $
          expectValid $
            validateHuddle
              boundedInGroupHuddle
              "root"
              (mkTermArray [termBool True, mkTermUInt 42, mkTermString "hi"])
        it "Validates when bounded entry has zero elements (lower bound is 0)" $
          expectValid $
            validateHuddle boundedInGroupHuddle "root" (mkTermArray [mkTermUInt 42, mkTermString "hi"])

    describe "Bignums" $ do
      let
        bignum = 2 ^ (64 :: Int) :: Integer
        bignumTerm = mkTermTag 2 . mkTermBytes $ unsignedToBytes bignum
      it "Validates a bignum >= 2^64 against biguint" $
        expectValid $
          validateTermText "a = biguint" "a" bignumTerm
      it "Validates a bignum >= 2^64 against bigint" $
        expectValid $
          validateTermText "a = bigint" "a" bignumTerm
      it "Validates a bignum >= 2^64 against integer" $
        expectValid $
          validateTermText "a = integer" "a" bignumTerm
      it "Rejects a bignum >= 2^64 against int" $
        expectInvalid $
          validateTermText "a = int" "a" bignumTerm
      it "Rejects a bignum >= 2^64 against uint" $
        expectInvalid $
          validateTermText "a = uint" "a" bignumTerm

    describe "Bignum literals" $ do
      let bignum = 2 ^ (64 :: Int) :: Integer
      it "Accepts a matching positive bignum" $
        expectValid $
          validateTermText "a = 18446744073709551616" "a" $
            mkTermTag 2 (mkTermBytes (unsignedToBytes bignum))
      it "Rejects a positive bignum with a different value" $
        expectInvalid $
          validateTermText "a = 18446744073709551616" "a" $
            mkTermTag 2 (mkTermBytes (unsignedToBytes (bignum + 1)))
      it "Accepts a matching negative bignum" $
        -- tag 3 content n denotes -1 - n: -18446744073709551617 = -1 - 2^64
        expectValid $
          validateTermText "a = -18446744073709551617" "a" $
            mkTermTag 3 (mkTermBytes (unsignedToBytes bignum))

    describe "Value ranges" $ do
      describe "Integer" $ do
        it "Accepts the bounds of a closed range" $ do
          expectValid $ validateTermText "a = 1 .. 5" "a" (mkTermUInt 1)
          expectValid $ validateTermText "a = 1 .. 5" "a" (mkTermUInt 5)
        it "Rejects values outside a closed range" $ do
          expectInvalid $ validateTermText "a = 1 .. 5" "a" (mkTermUInt 0)
          expectInvalid $ validateTermText "a = 1 .. 5" "a" (mkTermUInt 6)
        it "Excludes the upper bound of a half-open range" $ do
          expectValid $ validateTermText "a = 1 ... 5" "a" (mkTermUInt 4)
          expectInvalid $ validateTermText "a = 1 ... 5" "a" (mkTermUInt 5)
        it "Accepts the single member of a singleton range" $
          expectValid $
            validateTermText "a = 5 .. 5" "a" (mkTermUInt 5)
        it "Handles a range with two negative bounds" $ do
          expectValid $ validateTermText "a = -5 .. -1" "a" (termInt (-3))
          expectInvalid $ validateTermText "a = -5 .. -1" "a" (mkTermUInt 0)
      describe "Float" $ do
        it "Accepts a float16 within the range" $
          expectValid $
            validateTermText "a = 0.0 .. 1.0" "a" (TermHalf 0.5)
        it "Rejects a float16 outside the range" $
          expectInvalid $
            validateTermText "a = 0.0 .. 1.0" "a" (TermHalf 100.0)
        it "Accepts a float32 within the range" $
          expectValid $
            validateTermText "a = 0.0 .. 1.0" "a" (TermFloat 0.5)
        it "Rejects a float32 outside the range" $
          expectInvalid $
            validateTermText "a = 0.0 .. 1.0" "a" (TermFloat 100.0)
        it "Accepts a float64 within the range" $
          expectValid $
            validateTermText "a = 0.0 .. 1.0" "a" (TermDouble 0.5)
        it "Rejects a float64 outside the range" $
          expectInvalid $
            validateTermText "a = 0.0 .. 1.0" "a" (TermDouble 100.0)
        it "Excludes the upper bound of a half-open range" $
          expectInvalid $
            validateTermText "a = 0.0 ... 1.0" "a" (TermDouble 1.0)

    describe "Size control" $ do
      it "Measures text size in UTF-8 bytes, not characters" $ do
        expectValid $ validateTermText "a = text .size (0 .. 2)" "a" (mkTermString "é")
        expectInvalid $ validateTermText "a = text .size (0 .. 2)" "a" (mkTermString "€")
      it "Handles a bytes size range with a negative lower bound" $ do
        expectValid $ validateTermText "a = bytes .size (-2 .. 3)" "a" (mkTermBytes "")
        expectValid $ validateTermText "a = bytes .size (-2 .. 3)" "a" (mkTermBytes "ab")
        expectInvalid $ validateTermText "a = bytes .size (-2 .. 3)" "a" (mkTermBytes "abcd")

    describe "Simple values" $ do
      it "Validates an unassigned simple value against any" $
        expectValid $
          validateTermText "a = any" "a" (TermSimple 0)

    describe "Huddle integer literals" $ do
      it "Infers nint for negative literals" $
        expectValid $
          validateHuddle (collectFrom [HIRule $ "a" =:= int (-5)]) "a" (termInt (-5))
      it "Infers uint for the maximum Word64" $
        expectValid $
          validateHuddle
            (collectFrom [HIRule $ "a" =:= int 18446744073709551615])
            "a"
            (mkTermUInt maxBound)

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
      prop "Validates with custom validator in a choice within a group" $ do
        bs <- BS.pack <$> listOf1 arbitrary
        let
          ruleA = withValidator bytesValidator $ "a" =:= VText
          huddle =
            collectFrom
              [ HIRule ruleA
              , HIRule $ "b" =:= arr [0, a (ruleA H./ VNil)]
              ]
        arrTerm <- genArrayTerm [mkTermUInt 0, mkTermBytes bs]
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
