{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.Validator (spec) where

import Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm)
import Codec.CBOR.Cuddle.CBOR.Validator (
  CBORTermResult (..),
  ValidatorStage,
  ValidatorStageSimple,
  isCBORTermResultValid,
  showSimple,
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
import Test.Hspec (
  Expectation,
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
  noShrinking,
  oneof,
  scale,
  shuffle,
  vectorOf,
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
              , LT.unpack . pShow $ mapIndex @_ @_ @ValidatorStageSimple res
              , "====="
              , "CBOR term:"
              , LT.unpack $ pShow cborTerm
              ]
        counterexample extraInfo $
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

-- TODO make this complete
arbitraryTerm :: Gen Term
arbitraryTerm = oneof [TBool <$> arbitrary, TInt <$> arbitrary, TString . T.pack <$> arbitrary]

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

validateHuddle :: Term -> Huddle -> Name -> (CBORTermResult ValidatorStage -> Bool) -> Expectation
validateHuddle term huddle name predicate = do
  let
    resolvedCddl = case fullResolveCDDL . mapCDDLDropExt $ toCDDL huddle of
      Right root -> root
      Left err -> error $ show err
    bs = toStrictByteString $ encodeTerm term
    res = validateCBOR bs name (mapIndex resolvedCddl)
  unless (predicate res) $ do
    expectationFailure $ "Predicate failed on result:\n" <> showSimple res

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
        validateHuddle cbor huddleMap "a" isCBORTermResultValid
      prop "Validates array" . forAll genHuddleArray $ \cbor ->
        validateHuddle cbor huddleArray "a" isCBORTermResultValid
      prop "Validates map with correct number of range elements"
        . forAll (genHuddleRangeMap (5, 10))
        $ \cbor ->
          validateHuddle cbor huddleRangeMap "a" isCBORTermResultValid
      prop "Validates array with ranges" . forAll genHuddleRangeArray $ \cbor ->
        validateHuddle cbor huddleRangeArray "a" isCBORTermResultValid
    describe "Negative" $ do
      prop "Fails to validate a map with an unexpected index"
        . forAll genBadMapInvalidIndex
        $ \cbor ->
          validateHuddle cbor huddleMap "a" (not . isCBORTermResultValid)
      prop "Fails to validate reversed array" . forAll genBadArrayReversed $ \cbor ->
        validateHuddle cbor huddleArray "a" (not . isCBORTermResultValid)
      prop "Fails to validate array with missing non-negative int at the end"
        . forAll genBadArrayMissingLastInt
        $ \cbor ->
          validateHuddle cbor huddleArray "a" (not . isCBORTermResultValid)
      prop "Fails to validate map with too few range elements"
        . forAll (genHuddleRangeMap (0, 4))
        $ \cbor ->
          validateHuddle cbor huddleRangeMap "a" (not . isCBORTermResultValid)
      prop "Fails to validate map with too many range elements"
        . forAll (genHuddleRangeMap (11, 20))
        $ \cbor ->
          validateHuddle cbor huddleRangeMap "a" (not . isCBORTermResultValid)
