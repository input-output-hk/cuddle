{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Codec.CBOR.Cuddle.CDDL.Pretty (
  spec,
) where

import Codec.CBOR.Cuddle.CDDL (
  Assign (..),
  CDDL,
  Group (Group),
  GroupEntry (..),
  GroupEntryVariant (..),
  GrpChoice (..),
  Name (..),
  Rule (..),
  Type0 (..),
  Type1 (..),
  Type2 (..),
  TypeOrGroup (..),
  ValueVariant (..),
  value,
 )
import Codec.CBOR.Cuddle.Pretty ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import Data.TreeDiff (ToExpr (..), prettyExpr)
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Test.Codec.CBOR.Cuddle.CDDL.Gen ()
import Test.HUnit (assertEqual)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, xit)
import Test.Hspec.QuickCheck (xprop)
import Test.QuickCheck (counterexample)
import Prelude hiding ((/))

prettyPrintsTo :: (Pretty a, ToExpr a) => a -> String -> Expectation
prettyPrintsTo x s = assertEqual (show . prettyExpr $ toExpr x) s rendered
  where
    rendered = renderString (layoutPretty defaultLayoutOptions (pretty x))

t2Name :: Type2
t2Name = T2Name (Name "a" mempty) mempty

t1Name :: Type1
t1Name = Type1 t2Name Nothing mempty

mkType0 :: Type2 -> Type0
mkType0 t2 = Type0 $ Type1 t2 Nothing mempty :| []

spec :: Spec
spec = describe "Pretty printer" $ do
  unitSpec
  qcSpec

qcSpec :: Spec
qcSpec = describe "QuickCheck" $ do
  xprop "CDDL prettyprinter leaves no trailing spaces" $ \(cddl :: CDDL) -> do
    let
      prettyStr = T.pack . renderString . layoutPretty defaultLayoutOptions $ pretty cddl
      stripLines = T.unlines . fmap T.stripEnd . T.lines
    counterexample (show . prettyExpr $ toExpr cddl) $
      prettyStr `shouldBe` stripLines prettyStr

drep :: Rule
drep =
  Rule
    "drep"
    Nothing
    AssignEq
    ( TOGType
        ( Type0
            ( Type1
                ( T2Array
                    ( Group
                        ( GrpChoice
                            [ GroupEntry
                                Nothing
                                mempty
                                (GEType Nothing (Type0 $ Type1 (T2Value . value $ VUInt 0) Nothing mempty :| []))
                            , GroupEntry
                                Nothing
                                mempty
                                (GEType Nothing (Type0 $ Type1 (T2Name "addr_keyhash" Nothing) Nothing mempty :| []))
                            ]
                            mempty
                            :| [ GrpChoice
                                  [ GroupEntry
                                      Nothing
                                      mempty
                                      (GEType Nothing (Type0 $ Type1 (T2Value . value $ VUInt 1) Nothing mempty :| []))
                                  , GroupEntry
                                      Nothing
                                      mempty
                                      (GEType Nothing (Type0 $ Type1 (T2Name "script_hash" Nothing) Nothing mempty :| []))
                                  ]
                                  mempty
                               , GrpChoice
                                  [ GroupEntry
                                      Nothing
                                      mempty
                                      (GEType Nothing (Type0 $ Type1 (T2Value . value $ VUInt 2) Nothing mempty :| []))
                                  ]
                                  mempty
                               , GrpChoice
                                  [ GroupEntry
                                      Nothing
                                      mempty
                                      (GEType Nothing (Type0 $ Type1 (T2Value . value $ VUInt 3) Nothing mempty :| []))
                                  ]
                                  mempty
                               ]
                        )
                    )
                )
                Nothing
                mempty
                :| []
            )
        )
    )
    mempty

unitSpec :: Spec
unitSpec = describe "HUnit" $ do
  describe "Name" $ do
    it "names" $ Name "a" mempty `prettyPrintsTo` "a"
  describe "Type0" $ do
    it "name" $ Type0 (t1Name :| []) `prettyPrintsTo` "a"
  describe "Type1" $ do
    it "name" $ t1Name `prettyPrintsTo` "a"
  describe "Type2" $ do
    it "T2Name" $ t2Name `prettyPrintsTo` "a"
    describe "T2Array" $ do
      let groupEntryName = GroupEntry Nothing mempty $ GERef (Name "a" mempty) Nothing
      it "one element" $
        T2Array (Group (GrpChoice [groupEntryName] mempty :| [])) `prettyPrintsTo` "[a]"
      it "two elements" $
        T2Array
          ( Group
              ( GrpChoice
                  [ GroupEntry Nothing mempty $ GEType Nothing (mkType0 . T2Value . value $ VUInt 1)
                  , groupEntryName
                  ]
                  mempty
                  :| []
              )
          )
          `prettyPrintsTo` "[1, a]"
      it "two elements with comments" $
        T2Array
          ( Group
              ( GrpChoice
                  [ GroupEntry Nothing "one" $ GEType Nothing (mkType0 . T2Value . value $ VUInt 1)
                  , GroupEntry Nothing "two" $ GEType Nothing (mkType0 . T2Value . value $ VUInt 2)
                  ]
                  mempty
                  :| []
              )
          )
          `prettyPrintsTo` "[ 1 ; one\n, 2 ; two\n]"
      it "two elements with multiline comments" $
        T2Array
          ( Group
              ( GrpChoice
                  [ GroupEntry Nothing "first\nmultiline comment" $ GEType Nothing (mkType0 . T2Value . value $ VUInt 1)
                  , GroupEntry Nothing "second\nmultiline comment" $
                      GEType Nothing (mkType0 . T2Value . value $ VUInt 2)
                  ]
                  mempty
                  :| []
              )
          )
          `prettyPrintsTo` "[ 1 ; first\n    ; multiline comment\n, 2 ; second\n    ; multiline comment\n]"
  describe "Rule" $ do
    it "simple assignment" $
      Rule
        (Name "a" mempty)
        Nothing
        AssignEq
        (TOGType (Type0 (Type1 (T2Name (Name "b" mempty) mempty) Nothing mempty :| [])))
        mempty
        `prettyPrintsTo` "a = b"
    xit "drep" $
      drep
        `prettyPrintsTo` "drep = [0, addr_keyhash // 1, script_hash // 2 // 3]"
