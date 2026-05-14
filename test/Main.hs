module Main (main) where

import System.IO (BufferMode (..), hSetBuffering, hSetEncoding, stdout, utf8)
import Test.Codec.CBOR.Cuddle.Examples qualified as Examples
import Test.Codec.CBOR.Cuddle.GeneratorSpec qualified as Generator
import Test.Codec.CBOR.Cuddle.Huddle (huddleSpec)
import Test.Codec.CBOR.Cuddle.Parser (parserSpec)
import Test.Codec.CBOR.Cuddle.Pretty (roundtripSpec)
import Test.Codec.CBOR.Cuddle.Pretty.Golden qualified as PrettyGolden
import Test.Codec.CBOR.Cuddle.Validator qualified as Validator
import Test.Codec.CBOR.Cuddle.Validator.Golden qualified as ValidatorGolden
import Test.Hspec
import Test.Hspec.Runner

hspecConfig :: Config
hspecConfig =
  defaultConfig
    { configColorMode = ColorAlways
    }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hspecWith hspecConfig $ do
    describe "Parser" parserSpec
    describe "Huddle" huddleSpec
    describe "Examples" Examples.spec
    describe "Generator" Generator.spec
    describe "Pretty" $ do
      describe "Golden" PrettyGolden.spec
      describe "Roundtrip" roundtripSpec
    describe "Validator" $ do
      describe "Properties" Validator.spec
      describe "Golden" ValidatorGolden.spec
