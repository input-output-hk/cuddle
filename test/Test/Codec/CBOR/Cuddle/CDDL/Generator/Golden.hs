{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.Generator.Golden (spec) where

import Codec.CBOR.Cuddle.CBOR.Gen (GenPhase, generateFromName)
import Codec.CBOR.Cuddle.CDDL (Name)
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced)
import Codec.CBOR.Cuddle.Huddle (Huddle)
import Codec.CBOR.Cuddle.IndexMappable (mapIndex)
import Control.Monad ((<=<))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Paths_cuddle (getDataFileName)
import System.FilePath ((</>))
import Test.AntiGen (prettyZapResult, zapAntiGenResult)
import Test.Codec.CBOR.Cuddle.CDDL.Examples.Huddle (
  cborControlExample,
  choicesExample,
  customGenZapExample,
  opCertExample,
  rangeListExample,
  rangeMapExample,
  refExample,
  sizeBytesExample,
  sizeTextExample,
 )
import Test.Codec.CBOR.Cuddle.CDDL.Utils (tryResolveHuddle)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Golden (Golden (..))
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

annotationGolden :: String -> CTreeRoot MonoReferenced -> Name -> Int -> Spec
annotationGolden testName cddl name seed =
  it testName $
    Golden
      { goldenFile = "golden" </> testName <> ".txt"
      , readFromFile = T.readFile <=< getDataFileName
      , writeToFile = \fp txt -> getDataFileName fp >>= (`T.writeFile` txt)
      , actualFile = Nothing
      , output = str
      , encodePretty = T.unpack
      , failFirstTime = False
      }
  where
    gen = zapAntiGenResult 1 $ generateFromName (mapIndex @_ @_ @GenPhase cddl) name
    res = unGen gen (mkQCGen seed) 30
    str = prettyZapResult res

annotationGoldenHuddle :: String -> Huddle -> Name -> Int -> Spec
annotationGoldenHuddle testName huddle name seed = do
  cddl <- tryResolveHuddle huddle
  annotationGolden testName cddl name seed

spec :: Spec
spec = describe "golden" $ do
  describe "generator annotations" $ do
    annotationGoldenHuddle "annotationOpCert" opCertExample "root" 42
    annotationGoldenHuddle "annotationRangeList" rangeListExample "root" 42
    annotationGoldenHuddle "annotationChoices" choicesExample "root" 42
    annotationGoldenHuddle "annotationSizeBytes" sizeBytesExample "root" 42
    annotationGoldenHuddle "annotationSizeText" sizeTextExample "root" 42
    annotationGoldenHuddle "annotationRangeMap" rangeMapExample "root" 42
    annotationGoldenHuddle "annotationCborControl" cborControlExample "root" 42
    annotationGoldenHuddle "annotationRef" refExample "root" 42
    annotationGoldenHuddle "annotationCustomGenZap" customGenZapExample "root" 42
