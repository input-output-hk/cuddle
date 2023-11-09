{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.Parser
import Control.Applicative hiding (some)
import Data.Text (Text)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = hspec $
  describe "cddlParser" $ do
    describe "pValue" $ do
      it "Parses integer" $
        parse pValue "" "123" `shouldParse` VNum 123
      it "Parses text" $
        parse pValue "" "\"Hello World\"" `shouldParse` VText "Hello World"
    describe "pOccur" $ do
      it "Parses OneOrMore" $
        parse pOccur "" "+" `shouldParse` OIOneOrMore
      it "Parses ZeroOrMore" $
        parse pOccur "" "*" `shouldParse` OIZeroOrMore
      it "Parses Optional" $
        parse pOccur "" "?" `shouldParse` OIOptional
      it "Parses Lower Bounded" $
        parse pOccur "" "3*" `shouldParse` OIBounded (Just 3) Nothing
      it "Parses Upper Bounded" $
        parse pOccur "" "*9" `shouldParse` OIBounded Nothing (Just 9)
      it "Parses bounded on both sides" $
        parse pOccur "" "3*9" `shouldParse` OIBounded (Just 3) (Just 9)

-- it "result of parsing satisfies what it should" $
--   parse myParser "" "aaaa" `parseSatisfies` ((== 4) . length)
