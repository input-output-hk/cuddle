{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Redundant bracket" -}

module Test.Codec.CBOR.Cuddle.Huddle where

import Codec.CBOR.Cuddle.CDDL (CDDL, sortCDDL)
import Codec.CBOR.Cuddle.Comments (Comment)
import Codec.CBOR.Cuddle.Huddle
import Codec.CBOR.Cuddle.Parser
import Data.Text qualified as T
import Test.Codec.CBOR.Cuddle.CDDL.Pretty qualified as Pretty
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Prelude hiding ((/))

huddleSpec :: Spec
huddleSpec = describe "huddle" $ do
  basicAssign
  arraySpec
  mapSpec
  grpSpec
  nestedSpec
  genericSpec
  constraintSpec
  Pretty.spec

basicAssign :: Spec
basicAssign = describe "basic assignment" $ do
  it "Can assign a primitive" $
    toSortedCDDLPretty ["port" =:= VUInt]
      `shouldMatchParseCDDL` "port = uint"
  it "Can assign an int" $
    toSortedCDDLPretty ["one" =:= (int 1)]
      `shouldMatchParseCDDL` "one = 1"
  -- it "Can assign a float" $
  --   toSortedCDDLPretty ["onepointone" =:= (1.1 :: Float)]
  --     `shouldMatchParseCDDL` "onepointone = 1.1"
  it "Can assign a text string" $
    toSortedCDDLPretty ["hello" =:= ("Hello World" :: T.Text)]
      `shouldMatchParseCDDL` "hello = \"Hello World\""
  it "Can handle multiple assignments" $
    toSortedCDDLPretty ["age" =:= VUInt, "location" =:= VText]
      `shouldMatchParseCDDL` "age = uint\n location = text"

arraySpec :: Spec
arraySpec = describe "Arrays" $ do
  it "Can assign a small array" $
    toSortedCDDLPretty ["asl" =:= arr [a VUInt, a VBool, a VText]]
      `shouldMatchParseCDDL` "asl = [ uint, bool, text ]"
  it "Can quantify an upper bound" $
    toSortedCDDLPretty ["age" =:= arr [a VUInt +> 64]]
      `shouldMatchParseCDDL` "age = [ *64 uint ]"
  it "Can quantify an optional" $
    toSortedCDDLPretty ["age" =:= arr [0 <+ a VUInt +> 1]]
      `shouldMatchParseCDDL` "age = [ ? uint ]"
  it "Can handle a choice" $
    toSortedCDDLPretty ["ageOrSex" =:= arr [a VUInt] / arr [a VBool]]
      `shouldMatchParseCDDL` "ageOrSex = [ uint // bool ]"
  it "Can handle choices of groups" $
    toSortedCDDLPretty
      [ "asl"
          =:= arr [a VUInt, a VBool, a VText]
          / arr
            [ a (int 1)
            , a ("Hello" :: T.Text)
            ]
      ]
      `shouldMatchParseCDDL` "asl = [ uint, bool, text // 1, \"Hello\" ]"

mapSpec :: Spec
mapSpec = describe "Maps" $ do
  it "Can assign a small map" $
    toSortedCDDLPretty ["asl" =:= mp ["age" ==> VUInt, "sex" ==> VBool, "location" ==> VText]]
      `shouldMatchParseCDDL` "asl = { age : uint, sex : bool, location : text }"
  it "Can quantify a lower bound" $
    toSortedCDDLPretty ["age" =:= mp [0 <+ "years" ==> VUInt]]
      `shouldMatchParseCDDL` "age = { * years : uint }"
  it "Can quantify an upper bound" $
    toSortedCDDLPretty ["age" =:= mp ["years" ==> VUInt +> 64]]
      `shouldMatchParseCDDL` "age = { *64 years : uint }"
  it "Can handle a choice" $
    toSortedCDDLPretty ["ageOrSex" =:= mp ["age" ==> VUInt] / mp ["sex" ==> VBool]]
      `shouldMatchParseCDDL` "ageOrSex = { age : uint // sex : bool }"
  it "Can handle a choice with an entry" $
    toSortedCDDLPretty ["mir" =:= arr [a (int 0 / int 1), a $ mp [0 <+ "test" ==> VUInt]]]
      `shouldMatchParseCDDL` "mir = [ 0 / 1, { * test : uint }]"

grpSpec :: Spec
grpSpec = describe "Groups" $ do
  it "Can handle a choice in a group entry" $
    let g1 = "g1" =:~ grp [a (VUInt / VBytes), a VUInt]
     in toSortedCDDLPretty (collectFrom [HIRule $ "a1" =:= arr [a g1]])
          `shouldMatchParseCDDL` "a1 = [g1]\n g1 = ( uint / bytes, uint )"
  it "Can handle keys in a group entry" $
    let g1 = "g1" =:~ grp ["bytes" ==> VBytes]
     in toSortedCDDLPretty (collectFrom [HIRule $ "a1" =:= arr [a g1]])
          `shouldMatchParseCDDL` "a1 = [g1]\n g1 = (bytes : bytes)"

-- it "Can handle a group in a map" $
--   let g1 = "g1" =:~ grp ["bytes"==> VBytes]
--   in toSortedCDDLPretty (collectFrom ["a1" =:= mp [g1]])
--     `shouldMatchParseCDDL` "a1 = [g1]\n g1 = (bytes : bytes)"

nestedSpec :: Spec
nestedSpec =
  describe "Nesting" $
    it "Handles references" $
      let headerBody = "header_body" =:= arr ["block_number" ==> VUInt, "slot" ==> VUInt]
       in toSortedCDDLPretty
            [ headerBody
            , "header" =:= arr [a headerBody, "body_signature" ==> VBytes]
            ]
            `shouldMatchParseCDDL` "header = [header_body, body_signature : bytes]\n header_body = [block_number : uint, slot : uint]"

genericSpec :: Spec
genericSpec =
  describe "Generics" $
    let set :: IsType0 t0 => t0 -> GRuleCall DHuddle
        set = binding $ \x -> "set" =:= arr [0 <+ a x]

        dict :: (IsType0 t0, IsType0 t1) => t0 -> t1 -> GRuleCall DHuddle
        dict = binding2 $ \k v -> "dict" =:= mp [0 <+ asKey k ==> v]
     in do
          it "Should bind a single parameter" $
            toSortedCDDLPretty (collectFrom [HIRule $ "intset" =:= set VUInt])
              `shouldMatchParseCDDL` "intset = set<uint>\n set<a0> = [* a0]"
          it "Should bind two parameters" $
            toSortedCDDLPretty (collectFrom [HIRule $ "mymap" =:= dict VUInt VText])
              `shouldMatchParseCDDL` "dict<a0, b0> = {* a0 => b0}\n mymap = dict<uint, text>"

constraintSpec :: Spec
constraintSpec =
  describe "Constraints" $ do
    it "Size can take a Word" $
      toSortedCDDLPretty (collectFrom [HIRule $ "sz" =:= VUInt `sized` (2 :: Word)])
        `shouldMatchParseCDDL` "sz = uint .size 2"

    it "Range bound can take a reference" $
      let b = "b" =:= (16 :: Integer)
       in toSortedCDDLPretty (collectFrom [HIRule $ "b" =:= (16 :: Integer), HIRule $ "c" =:= int 0 ... b])
            `shouldMatchParseCDDL` "b = 16\n c = 0 .. b"

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
  CDDL Comment ->
  String ->
  Expectation
shouldMatchParseCDDL x = shouldMatchParse x pCDDL

toSortedCDDLPretty :: Huddle DHuddle -> CDDL Comment
toSortedCDDLPretty = sortCDDL . fmap dhComment . toCDDLNoRoot
