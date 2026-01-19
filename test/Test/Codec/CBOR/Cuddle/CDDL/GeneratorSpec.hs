{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CDDL.GeneratorSpec (spec) where

import Codec.CBOR.Cuddle.CBOR.Gen (generateFromName)
import Codec.CBOR.Cuddle.CDDL (Name)
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (WrappedTerm (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, fullResolveCDDL)
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
import Codec.CBOR.Term (Term (..))
import Codec.CBOR.Term qualified as C
import Test.Hspec (HasCallStack, Spec, describe, runIO)
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, Testable (..), choose, counterexample)

simpleRule :: Name -> H.Rule
simpleRule n = withGenerator (S . C.TInt <$> choose (4, 6)) $ n =:= arr [1, 2, 3]

simpleTermExample :: Huddle
simpleTermExample =
  collectFrom
    [ HIRule $ simpleRule "root"
    ]

refTermExample :: Huddle
refTermExample =
  collectFrom
    [ HIRule $ "root" =:= arr [0, a $ simpleRule "bar"]
    ]

bytesExample :: Huddle
bytesExample =
  collectFrom
    [ HIRule $ "root" =:= H.bstr "010203ff"
    ]

cddlShouldGenerateSatisfying ::
  Testable prop => CTreeRoot MonoReferenced -> (Term -> prop) -> Property
cddlShouldGenerateSatisfying cddl p = property $ p <$> generateFromName cddl "root"

tryResolveHuddle :: HasCallStack => Huddle -> SpecM () (CTreeRoot MonoReferenced)
tryResolveHuddle huddle = do
  case fullResolveCDDL . mapCDDLDropExt $ toCDDL huddle of
    Right x -> pure x
    Left err -> runIO . fail $ "Failed to resolve CDDL:\n" <> show err

spec :: Spec
spec = do
  describe "Custom generators" $ do
    describe "Huddle" $ do
      simpleTermExampleCddl <- tryResolveHuddle simpleTermExample
      prop "If a term has a custom generator then it is used" $
        simpleTermExampleCddl `cddlShouldGenerateSatisfying` \case
          TInt i -> property $ i > 3 && i < 7
          x -> counterexample (show x) False
      refTermExampleCddl <- tryResolveHuddle refTermExample
      prop "Custom generator works when called via reference" $
        refTermExampleCddl `cddlShouldGenerateSatisfying` \case
          TList [TInt 0, TInt i] -> property $ i > 3 && i < 7
          x -> counterexample (show x) False
      bytesExampleCddl <- tryResolveHuddle bytesExample
      prop "Bytes are generated correctly" $
        bytesExampleCddl `cddlShouldGenerateSatisfying` \case
          TBytes "\x01\x02\x03\xff" -> property True
          x -> counterexample (show x) False
