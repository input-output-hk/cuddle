module Main (main) where

import Test.Codec.CBOR.Cuddle.CDDL.Examples qualified as Examples
import Test.Codec.CBOR.Cuddle.CDDL.Parser (parserSpec)
import Test.Codec.CBOR.Cuddle.Huddle (huddleSpec)
import Test.Hspec
import Test.Hspec.Runner

hspecConfig :: Config
hspecConfig =
  defaultConfig
    { configColorMode = ColorAlways
    }

main :: IO ()
main =
  hspecWith hspecConfig $ do
    describe "cddlParser" parserSpec
    describe "Huddle" huddleSpec
    describe "Examples" Examples.spec
