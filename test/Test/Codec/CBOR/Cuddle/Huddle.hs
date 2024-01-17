{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.Huddle where

import Codec.CBOR.Cuddle.CDDL (CDDL)
import Codec.CBOR.Cuddle.Huddle
import Codec.CBOR.Cuddle.Parser
import Data.Text qualified as T
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

huddleSpec :: Spec
huddleSpec = describe "huddle" $ do
  basicAssign
  arraySpec
  mapSpec
  nestedSpec

basicAssign :: Spec
basicAssign = describe "basic assignment" $ do
  it "Can assign a primitive" $
    toCDDL ["port" =:= VUInt]
      `shouldMatchParseCDDL` "port = uint"
  it "Can assign an int" $
    toCDDL ["one" =:= (1 :: Int)]
      `shouldMatchParseCDDL` "one = 1"
  -- it "Can assign a float" $
  --   toCDDL ["onepointone" =:= (1.1 :: Float)]
  --     `shouldMatchParseCDDL` "onepointone = 1.1"
  it "Can assign a text string" $
    toCDDL ["hello" =:= ("Hello World" :: T.Text)]
      `shouldMatchParseCDDL` "hello = \"Hello World\""
  it "Can handle multiple assignments" $
    toCDDL ["age" =:= VUInt, "location" =:= VText]
      `shouldMatchParseCDDL` "age = uint\n location = text"

arraySpec :: Spec
arraySpec = describe "Arrays" $ do
  it "Can assign a small array" $
    toCDDL ["asl" =:= arr [a VUInt, a VBool, a VText]]
      `shouldMatchParseCDDL` "asl = [ uint, bool, text ]"
  it "Can quantify an upper bound" $
    toCDDL ["age" =:= ([a VUInt +> 64] :: ArrayChoice)]
      `shouldMatchParseCDDL` "age = [ *64 uint ]"
  it "Can quantify an optional" $
    toCDDL ["age" =:= ([0 <+ a VUInt +> 1] :: ArrayChoice)]
      `shouldMatchParseCDDL` "age = [ ? uint ]"
  it "Can handle a choice" $
    toCDDL ["ageOrSex" =:= arr [a VUInt] // arr [a VBool]]
      `shouldMatchParseCDDL` "ageOrSex = [ uint // bool ]"
  it "Can handle choices of groups" $
    toCDDL
      [ "asl"
          =:= arr [a VUInt, a VBool, a VText]
          // arr
            [ a (1 :: Int),
              a ("Hello" :: T.Text)
            ]
      ]
      `shouldMatchParseCDDL` "asl = [ uint, bool, text // 1, \"Hello\" ]"

mapSpec :: Spec
mapSpec = describe "Maps" $ do
  it "Can assign a small map" $
    toCDDL ["asl" =:= mp ["age" ==> VUInt, "sex" ==> VBool, "location" ==> VText]]
      `shouldMatchParseCDDL` "asl = { age : uint, sex : bool, location : text }"
  it "Can quantify an upper bound" $
    toCDDL ["age" =:= mp ["years" ==> VUInt +> 64]]
      `shouldMatchParseCDDL` "age = { *64 years : uint }"
  it "Can handle a choice" $
    toCDDL ["ageOrSex" =:= mp ["age" ==> VUInt] // mp ["sex" ==> VBool]]
      `shouldMatchParseCDDL` "ageOrSex = { age : uint // sex : bool }"

nestedSpec :: Spec
nestedSpec = describe "Nesting" $ do
  it "Handles references" $
    let headerBody = "header_body" =:= arr ["block_number" ==> VUInt, "slot" ==> VUInt]
     in toCDDL
          [ headerBody,
            "header" =:= arr [a (Ref headerBody), "body_signature" ==> VBytes]
          ]
          `shouldMatchParseCDDL` "header_body = [block_number : uint, slot : uint]\n header = [header_body, body_signature : bytes]"

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

shouldMatchParse ::
  (Text.Megaparsec.ShowErrorComponent e, Show a, Eq a) =>
  a ->
  Text.Megaparsec.Parsec e T.Text a ->
  String ->
  Expectation
shouldMatchParse x parseFun input = parse parseFun "" (T.pack input) `shouldParse` x

shouldMatchParseCDDL ::
  CDDL ->
  String ->
  Expectation
shouldMatchParseCDDL x = shouldMatchParse x pCDDL
