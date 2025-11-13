{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.GeneratorSpec (spec) where

import Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm)
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (WrappedTerm (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (fullResolveCDDL)
import Codec.CBOR.Cuddle.Huddle (
  Huddle,
  HuddleItem (..),
  a,
  arr,
  collectFrom,
  toCDDL,
  withGenerator,
  (=:=),
 )
import Codec.CBOR.Cuddle.Huddle qualified as H
import Codec.CBOR.Cuddle.IndexMappable (mapCDDLDropExt)
import Codec.CBOR.Term (Term)
import Codec.CBOR.Term qualified as C
import System.Random (newStdGen)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

foo :: H.Rule
foo = withGenerator (\_ -> pure . S $ C.TString "bar") $ "foo" =:= arr [1, 2, 3]

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

huddleShouldGenerate :: Huddle -> Term -> Expectation
huddleShouldGenerate huddle term = do
  g <- newStdGen
  ct <- case fullResolveCDDL . mapCDDLDropExt $ toCDDL huddle of
    Right x -> pure x
    Left err -> fail $ "Failed to resolve CDDL: " <> show err
  generateCBORTerm ct "foo" g `shouldBe` term

spec :: Spec
spec = do
  describe "Custom generators" $ do
    describe "Huddle" $ do
      it "If a term has a custom generator then it is used" $
        simpleTermExample `huddleShouldGenerate` C.TString "bar"
      it "Custom generator works when called via reference" $
        refTermExample `huddleShouldGenerate` C.TString "bar"
