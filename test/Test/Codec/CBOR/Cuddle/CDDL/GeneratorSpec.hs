{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.GeneratorSpec (spec) where

import Codec.CBOR.Cuddle.CBOR.Gen (genForCTree, generateCBORTermM)
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (WrappedTerm (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (fullResolveCDDL)
import Codec.CBOR.Cuddle.Huddle (
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
  tag,
  toCDDL,
  withGenerator,
  (<+),
  (=:=),
  (==>),
 )
import Codec.CBOR.Cuddle.Huddle qualified as H
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..), mapCDDLDropExt)
import Codec.CBOR.Term (Term (..))
import Codec.CBOR.Term qualified as C
import Control.Monad (replicateM)
import Data.Containers.ListUtils (nubOrd)
import Data.IORef (newIORef)
import System.Random (mkStdGen)
import System.Random.Stateful (IOGenM (..), UniformRange (..))
import Test.Hspec (Expectation, Spec, describe, it, shouldReturn)

foo :: H.Rule
foo = withGenerator generator $ "foo" =:= arr [1, 2, 3]
  where
    generator _ g = S . C.TInt <$> uniformRM (4, 6) g

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
    hSet = binding $ \elemRef ->
      let
        setGenerator [arg] g = do
          n <- uniformRM (0, 10) g
          t <- fmap nubOrd . replicateM n $ do
            ref <- genForCTree arg g
            case ref of
              S t -> pure t
              _ -> error "Expected a single term"
          pure . S . TTagged 258 . TList $ t
        setGenerator args _ = error $ "Expected a single generic argument, got " <> show (length args)
       in
        withGenerator setGenerator $
          "set" =:= tag 258 (arr [0 <+ a elemRef])

huddleShouldGenerate :: Huddle -> Term -> Expectation
huddleShouldGenerate huddle term = do
  g <- newIORef $ mkStdGen 12345
  ct <- case fullResolveCDDL . mapCDDLDropExt $ toCDDL huddle of
    Right x -> pure x
    Left err -> fail $ "Failed to resolve CDDL: " <> show err
  generateCBORTermM (mapIndex ct) "foo" (IOGenM g) `shouldReturn` term

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
        genericExample `huddleShouldGenerate` C.TNull
