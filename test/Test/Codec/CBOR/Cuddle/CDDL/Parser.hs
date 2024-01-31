{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.Parser where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.CtlOp qualified as CtlOp
import Codec.CBOR.Cuddle.Parser
import Codec.CBOR.Cuddle.Pretty ()
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Prettyprinter (Pretty, defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Codec.CBOR.Cuddle.CDDL.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (Gen, PropertyT, failure, footnote, footnoteShow, forAll, hedgehog, (===))
import Test.Hspec.Megaparsec
import Text.Megaparsec (errorBundlePretty, parse)

parserSpec :: Spec
parserSpec = do
  valueSpec
  occurSpec
  nameSpec
  type1Spec
  type2Spec
  grpEntrySpec
  grpChoiceSpec
  genericSpec
  roundtripSpec

roundtripSpec :: Spec
roundtripSpec = describe "Roundtripping should be id" $ do
  it "Trip Name" $ trip Gen.genName pName
  it "Trip Rule" $ trip Gen.genRule pRule
  where
    -- We show that, for a printed CDDL document p, print (parse p) == p. Note
    -- that we do not show that parse (print p) is p for a given generated
    -- 'CDDL' doc, since CDDL contains some statements that allow multiple
    -- parsings.
    trip :: (Show a, Pretty a) => Gen a -> Parser a -> PropertyT IO ()
    trip g pa = hedgehog $ do
      x <- forAll g
      let printed = printText x
      footnoteShow printed
      case parse pa "" printed of
        Left e -> do
          footnote $ errorBundlePretty e
          failure
        Right parsed -> do
          footnoteShow parsed
          printed === printText parsed
    printText :: (Pretty a) => a -> T.Text
    printText = renderStrict . layoutPretty defaultLayoutOptions . pretty

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

genericSpec :: Spec
genericSpec = describe "generics" $ do
  it "Parses a simple value generic" $
    parse pRule "" "a = b<0>"
      `shouldParse` Rule
        (Name "a")
        Nothing
        AssignEq
        ( TOGType
            ( Type0
                ( Type1
                    ( T2Name
                        (Name "b")
                        ( Just
                            ( GenericArg
                                ( Type1
                                    (T2Value (VNum 0))
                                    Nothing
                                    NE.:| []
                                )
                            )
                        )
                    )
                    Nothing
                    NE.:| []
                )
            )
        )
  it "Parses a range as a generic" $
    parse pRule "" "a = b<0 ... 1>"
      `shouldParse` Rule
        (Name "a")
        Nothing
        AssignEq
        ( TOGType
            ( Type0
                ( Type1
                    ( T2Name
                        (Name "b")
                        ( Just
                            ( GenericArg
                                ( Type1
                                    (T2Value (VNum 0))
                                    (Just (RangeOp ClOpen, T2Value (VNum 1)))
                                    NE.:| []
                                )
                            )
                        )
                    )
                    Nothing
                    NE.:| []
                )
            )
        )

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
                              (Type0 (NE.singleton (Type1 (T2Value (VNum 1)) Nothing)))
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
  it "Should parse part of a group alternative" $
    parse pGrpEntry "" "int // notConsideredHere"
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
  it "Should parse a generic" $
    parse pGrpEntry "" "a<0 ... #6(0)>"
      `shouldParse` GEType
        Nothing
        Nothing
        ( Type0
            ( Type1
                ( T2Name
                    (Name "a")
                    ( Just
                        ( GenericArg
                            ( Type1
                                (T2Value (VNum 0))
                                ( Just
                                    ( RangeOp ClOpen,
                                      T2Tag Nothing (Type0 (Type1 (T2Value (VNum 0)) Nothing NE.:| []))
                                    )
                                )
                                NE.:| []
                            )
                        )
                    )
                )
                Nothing
                NE.:| []
            )
        )
  it "Parses a GEType with an Occurrence Indicator" $
    parse pGrpEntry "" "0* a"
      `shouldParse` GEType
        (Just (OIBounded (Just 0) Nothing))
        Nothing
        (Type0 (Type1 (T2Name (Name "a") Nothing) Nothing NE.:| []))

grpChoiceSpec :: SpecWith ()
grpChoiceSpec = describe "GroupChoice" $ do
  it "Should parse part of a group alternative" $
    parse pGrpChoice "" "int // string"
      `shouldParse` [ GEType
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

type1Spec :: Spec
type1Spec = describe "Type1" $ do
  describe "CtlOp" $ do
    it "Should parse a basic control operator" $
      parse pType1 "" "uint .size 3"
        `shouldParse` Type1
          (T2Name (Name "uint") Nothing)
          (Just (CtrlOp CtlOp.Size, T2Value (VNum 3)))
  describe "RangeOp" $ do
    it "Should parse a closed range operator" $
      parse pType1 "" "0 .. 3"
        `shouldParse` Type1
          (T2Value (VNum 0))
          (Just (RangeOp Closed, T2Value (VNum 3)))
    it "Should parse a clopen range operator" $
      parse pType1 "" "0 ... 3"
        `shouldParse` Type1
          (T2Value (VNum 0))
          (Just (RangeOp ClOpen, T2Value (VNum 3)))
