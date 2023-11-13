{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.Parser
import Data.List.NonEmpty qualified as NE
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main = hspec $
  describe "cddlParser" $ do
    valueSpec
    occurSpec
    nameSpec
    type2Spec
    grpEntrySpec

valueSpec :: Spec
valueSpec = describe "pValue" $ do
  it "Parses integer" $
    parse pValue "" "123" `shouldParse` VNum 123
  it "Parses text" $
    parse pValue "" "\"Hello World\"" `shouldParse` VText "Hello World"

occurSpec :: Spec
occurSpec = describe "pOccur" $ do
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

nameSpec :: SpecWith ()
nameSpec = describe "pName" $ do
  it "Parses a boring name" $
    parse pName "" "coin" `shouldParse` Name "coin"
  it "Allows . in the middle" $
    parse pName "" "coin.me" `shouldParse` Name "coin.me"
  it "Allows $ as the last character" $
    parse pName "" "coin.me$" `shouldParse` Name "coin.me$"
  it "Doesn't allow . as a last character" $
    parse pName "" "coin." `shouldFailWith` err 5 ueof

type2Spec :: SpecWith ()
type2Spec = describe "type2" $ do
  describe "Value" $ do
    it "Parses a value" $
      parse pType2 "" "123" `shouldParse` T2Value (VNum 123)
  describe "Map" $ do
    it "Parses a basic group" $
      parse pType2 "" "{ int => string }"
        `shouldParse` T2Map
          ( Group
              ( NE.singleton
                  [ GEType
                      Nothing
                      (Just (MKType (Type1 (T2Name (Name "int") Nothing) Nothing)))
                      (Type0 (NE.singleton (Type1 (T2Name (Name "string") Nothing) Nothing)))
                  ]
              )
          )
    it "Parses a table" $
      parse pType2 "" "{ * int => string }"
        `shouldParse` T2Map
          ( Group
              ( NE.singleton
                  [ GEType
                      (Just OIZeroOrMore)
                      (Just (MKType (Type1 (T2Name (Name "int") Nothing) Nothing)))
                      (Type0 (NE.singleton (Type1 (T2Name (Name "string") Nothing) Nothing)))
                  ]
              )
          )
  describe "Array" $ do
    it "Parses an array with an alternative" $
      parse pType2 "" "[int // string]"
        `shouldParse` T2Array
          ( Group
              ( [ GEType
                    Nothing
                    Nothing
                    ( Type0
                        ( Type1
                            (T2Name (Name "int") Nothing)
                            Nothing
                            NE.:| []
                        )
                    )
                ]
                  NE.:| [ [ GEType
                              Nothing
                              Nothing
                              ( Type0
                                  ( Type1
                                      (T2Name (Name "string") Nothing)
                                      Nothing
                                      NE.:| []
                                  )
                              )
                          ]
                        ]
              )
          )

    it "Parses an array with a value alternative" $
      parse pType2 "" "[0 // 1]"
        `shouldParse` T2Array
          ( Group
              ( [ GEType
                    Nothing
                    Nothing
                    (Type0 (NE.singleton (Type1 (T2Value (VNum 0)) Nothing)))
                ]
                  NE.:| [ [ GEType
                              Nothing
                              Nothing
                              (Type0 (NE.singleton (Type1 (T2Value (VNum 0)) Nothing)))
                          ]
                        ]
              )
          )

grpEntrySpec :: SpecWith ()
grpEntrySpec = describe "GroupEntry" $ do
  it "Prefers GEType over GERef for names" $
    parse pGrpEntry "" "int"
      `shouldParse` GEType
        Nothing
        Nothing
        ( Type0
            ( Type1
                (T2Name (Name "int") Nothing)
                Nothing
                NE.:| []
            )
        )
