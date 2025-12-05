{-# LANGUAGE LambdaCase #-}

module Test.Codec.CBOR.Cuddle.CDDL.Validator (spec) where

import Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm)
import Codec.CBOR.Cuddle.CBOR.Validator (
  CBORTermResult (..),
  CDDLResult (..),
  validateCBOR,
 )
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.Resolve (fullResolveCDDL)
import Codec.CBOR.Cuddle.IndexMappable (mapCDDLDropExt, mapIndex)
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Codec.CBOR.Term (encodeTerm)
import Codec.CBOR.Write (toStrictByteString)
import Control.Monad (forM_)
import Data.Either (fromRight)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as LT
import Paths_cuddle (getDataFileName)
import Test.Hspec (Spec, describe, runIO, shouldSatisfy)
import Test.Hspec.QuickCheck
import Test.QuickCheck (counterexample)
import Test.QuickCheck.Modifiers (NoShrink (..))
import Test.QuickCheck.Random (mkQCGen)
import Text.Megaparsec (runParser)
import Text.Pretty.Simple (pShow)

genAndValidateFromFile :: FilePath -> Spec
genAndValidateFromFile path = do
  contents <- runIO $ T.readFile =<< getDataFileName path
  let
    cddl = fromRight (error "Failed to parse CDDL") $ runParser pCDDL path contents
    resolvedCddl@(CTreeRoot m) = either (error . show) id . fullResolveCDDL $ mapCDDLDropExt cddl
    isRule CTree.Group {} = False
    isRule _ = True
  describe path $
    forM_ (Map.keys $ Map.filter isRule m) $ \name@(Name n) ->
      prop (T.unpack n) $ \(NoShrink seed) -> do
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

spec :: Spec
spec =
  describe "Generate and validate from file" $ do
    genAndValidateFromFile "example/cddl-files/conway.cddl"
    genAndValidateFromFile "example/cddl-files/costmdls_min.cddl"
