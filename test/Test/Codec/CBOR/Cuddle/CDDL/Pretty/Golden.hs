{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.Pretty.Golden (spec) where

import Codec.CBOR.Cuddle.IndexMappable (mapIndex)
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Codec.CBOR.Cuddle.Pretty (PrettyStage, renderCDDL)
import Control.Monad ((<=<))
import Data.Text.IO qualified as T
import Paths_cuddle (getDataFileName)
import Prettyprinter (defaultLayoutOptions)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Golden (Golden (..))
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

prettyPrintGolden :: String -> FilePath -> Spec
prettyPrintGolden testName cddlPath = do
  cddl <- runIO $ do
    absolutePath <- getDataFileName cddlPath
    contents <- T.readFile absolutePath
    case parse pCDDL "" contents of
      Left err -> fail $ "Failed to parse CDDL:\n" <> errorBundlePretty err
      Right x -> pure x
  it testName $
    Golden
      { goldenFile = "golden" </> "pretty" </> testName <> ".txt"
      , readFromFile = T.readFile <=< getDataFileName
      , writeToFile = \fp txt -> getDataFileName fp >>= (`T.writeFile` txt)
      , actualFile = Nothing
      , output = renderCDDL defaultLayoutOptions $ mapIndex @_ @_ @PrettyStage cddl
      , encodePretty = show
      , failFirstTime = False
      }

spec :: Spec
spec = describe "golden" $ do
  prettyPrintGolden "basic_assign" "example/cddl-files/basic_assign.cddl"
  prettyPrintGolden "pretty" "example/cddl-files/pretty.cddl"
  prettyPrintGolden "costmdls_min" "example/cddl-files/costmdls_min.cddl"
  prettyPrintGolden "issue80-min" "example/cddl-files/issue80-min.cddl"
