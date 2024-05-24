module Main (main) where

import Test.Codec.CBOR.Cuddle.CBOR.Validate (validateSpec)
import Test.Codec.CBOR.Cuddle.CDDL.Parser (parserSpec)
import Test.Codec.CBOR.Cuddle.Huddle (huddleSpec)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "cddlParser" parserSpec
    describe "Huddle" huddleSpec
    describe "cbor" validateSpec
