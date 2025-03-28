module Main (main) where

import System.IO (BufferMode (..), hSetBuffering, hSetEncoding, stdout, utf8)
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
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hspecWith hspecConfig $ do
    describe "cddlParser" parserSpec
    describe "Huddle" huddleSpec
    describe "Examples" Examples.spec
