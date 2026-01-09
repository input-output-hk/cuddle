{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.GeneratorSpec (spec) where

import Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm, generateCBORTerm')
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (WrappedTerm (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (fullResolveCDDL)
import Codec.CBOR.Cuddle.Huddle (
  GRef (..),
  Huddle,
  HuddleItem (..),
  IsType0,
  Value (..),
  a,
  arr,
  binding,
  collectFrom,
  idx,
  mp,
  toCDDL,
  withGenerator,
  (<+),
  (=:=),
  (==>),
 )
import Codec.CBOR.Cuddle.Huddle qualified as H
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..), mapCDDLDropExt)
import Codec.CBOR.Term (Term)
import Codec.CBOR.Term qualified as C
import Control.Monad (replicateM)
import System.Random (mkStdGen)
import System.Random.Stateful (FrozenGen (..), UniformRange (..))
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

foo :: H.Rule
foo = withGenerator (\_ g -> S . C.TInt <$> uniformRM (4, 6) g) $ "foo" =:= arr [1, 2, 3]

simpleTermExample :: Huddle
simpleTermExample =
  collectFrom
    [ HIRule foo
    ]

refTermExample :: Huddle
refTermExample =
  collectFrom
    [ HIRule foo
    , HIRule $ "bar" =:= arr [0, a foo]
    ]

bytesExample :: Huddle
bytesExample =
  collectFrom
    [ HIRule $ "foo" =:= H.bstr "010203ff"
    ]

genericExample :: Huddle
genericExample =
  collectFrom
    [ HIRule $
        "foo"
          =:= mp
            [ idx 0 ==> hSet VUInt
            , idx 1 ==> hSet VBool
            ]
    ]
  where
    hSet :: IsType0 t => t -> H.GRuleCall
    hSet = binding $ \x@(GRef xRef) ->
      let
        setGenerator ctr g = do
          n <- uniformRM (0, 10) g
          t <- replicateM n $ generateCBORTerm' ctr (Name xRef) g
          undefined
       in
        withGenerator setGenerator $
          "set" =:= arr [0 <+ a x]

huddleShouldGenerate :: Huddle -> Term -> Expectation
huddleShouldGenerate huddle term = do
  let g = mkStdGen 12345
  ct <- case fullResolveCDDL . mapCDDLDropExt $ toCDDL huddle of
    Right x -> pure x
    Left err -> fail $ "Failed to resolve CDDL: " <> show err
  generateCBORTerm (mapIndex ct) "foo" g `shouldBe` term

spec :: Spec
spec = do
  describe "Custom generators" $ do
    describe "Huddle" $ do
      it "If a term has a custom generator then it is used" $
        simpleTermExample `huddleShouldGenerate` C.TInt 5
      it "Custom generator works when called via reference" $
        refTermExample `huddleShouldGenerate` C.TInt 5
      it "Bytes are generated correctly" $
        bytesExample `huddleShouldGenerate` C.TBytes "\x01\x02\x03\xff"
      it "Works with generic rules" $
        genericExample `huddleShouldGenerate` undefined
