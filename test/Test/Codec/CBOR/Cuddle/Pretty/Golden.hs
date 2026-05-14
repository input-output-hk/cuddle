{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.Pretty.Golden (spec) where

import Codec.CBOR.Cuddle.Comments ((//-))
import Codec.CBOR.Cuddle.Huddle (HuddleItem (..), a, (=:=))
import Codec.CBOR.Cuddle.Huddle qualified as H
import Codec.CBOR.Cuddle.IndexMappable (mapIndex)
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Codec.CBOR.Cuddle.Pretty (PrettyPhase, renderCDDL)
import Control.Monad ((<=<))
import Data.Text (Text)
import Data.Text.IO qualified as T
import Paths_cuddle (getDataFileName)
import Prettyprinter (defaultLayoutOptions)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Golden (Golden (..))
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)

mkGolden :: String -> Text -> Golden Text
mkGolden testName rendered =
  Golden
    { goldenFile = "golden" </> "pretty" </> testName <> ".txt"
    , readFromFile = T.readFile <=< getDataFileName
    , writeToFile = \fp txt -> getDataFileName fp >>= (`T.writeFile` txt)
    , actualFile = Nothing
    , output = rendered
    , encodePretty = show
    , failFirstTime = False
    }

prettyPrintGolden :: String -> FilePath -> Spec
prettyPrintGolden testName cddlPath = do
  cddl <- runIO $ do
    absolutePath <- getDataFileName cddlPath
    contents <- T.readFile absolutePath
    case parse pCDDL "" contents of
      Left err -> fail $ "Failed to parse CDDL:\n" <> errorBundlePretty err
      Right x -> pure x
  it testName $
    mkGolden testName $
      renderCDDL defaultLayoutOptions $
        mapIndex @_ @_ @PrettyPhase cddl

huddleGolden :: String -> [HuddleItem] -> Spec
huddleGolden testName items =
  it testName $
    mkGolden testName $
      renderCDDL defaultLayoutOptions $
        mapIndex @_ @_ @PrettyPhase $
          H.toCDDLNoRoot $
            H.collectFrom items

spec :: Spec
spec = describe "golden" $ do
  prettyPrintGolden "basic_assign" "cddl/basic_assign.cddl"
  prettyPrintGolden "pretty" "cddl/pretty.cddl"
  prettyPrintGolden "costmdls_min" "cddl/costmdls_min.cddl"
  prettyPrintGolden "issue80-min" "cddl/issue80-min.cddl"
  prettyPrintGolden "conway" "cddl/conway.cddl"
  describe "(//-) on a term" $ do
    huddleGolden
      "term_comment_rule"
      [HIRule $ "a" =:= (H.bool True //- "trailing")]
    huddleGolden
      "term_comment_appends_to_existing"
      [HIRule $ H.comment "existing" $ "a" =:= (H.bool True //- "trailing")]
    huddleGolden
      "term_comment_chained"
      [HIRule $ "a" =:= (H.bool True //- "first" //- "second")]
    huddleGolden
      "term_comment_empty"
      [HIRule $ "a" =:= (H.bool True //- "")]
    huddleGolden
      "term_comment_multiline"
      [HIRule $ "a" =:= (H.bool True //- "line one\nline two")]
    huddleGolden
      "term_comment_array_entries"
      [ HIRule $
          "triple"
            =:= H.arr
              [ a H.VUInt //- "first element"
              , a H.VText //- "second element"
              , a (H.bool True) //- "third element"
              ]
      ]
  describe "(//-) on a rule" $ do
    huddleGolden
      "rule_comment_rule"
      [HIRule $ "a" =:= H.bool True //- "trailing"]
    huddleGolden
      "rule_comment_appends_to_existing"
      [HIRule $ H.comment "existing" ("a" =:= H.bool True) //- "trailing"]
    huddleGolden
      "rule_comment_chained"
      [HIRule $ "a" =:= H.bool True //- "first" //- "second"]
    huddleGolden
      "rule_comment_empty"
      [HIRule $ "a" =:= H.bool True //- ""]
    huddleGolden
      "rule_comment_multiline"
      [HIRule $ "a" =:= H.bool True //- "line one\nline two"]
    huddleGolden
      "rule_comment_multi_rule"
      [ HIRule $ "port" =:= H.VUInt //- "the listening port"
      , HIRule $ "host" =:= H.VText //- "the host name"
      , HIRule $ "secure" =:= H.bool True //- "TLS only"
      ]
