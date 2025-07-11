{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Test.Codec.CBOR.Cuddle.CBOR.Validator where

import Codec.CBOR.Cuddle.CBOR.Validator qualified as CV
import Codec.CBOR.Cuddle.CDDL (Name)
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoRef (MIt), fullResolveCDDL)
import Codec.CBOR.Cuddle.Huddle
import Codec.CBOR.Cuddle.CDDL.Postlude (PTerm(PTInt, PTBool))
import Control.Monad.Reader
import Data.Functor.Identity
import Data.Map.Strict qualified as Map
import Test.Hspec

cborValidatorSpec :: Spec
cborValidatorSpec = do
  utilitySpec
  expandRuleSpec

utilitySpec :: Spec
utilitySpec = describe "Utility functions should work" $ do
  describe "mergeTrees" $ do
    it "Should prepend things to a leaf" $
      CV.mergeTrees @Bool
        [ CV.Leaf [True]
        , CV.Leaf [False]
        ]
        `shouldBe` CV.Leaf [True, False]
    it "Should nest things" $
      CV.mergeTrees @Bool
        [ CV.FilterBranch (CV.ArrayFilter True) (CV.Leaf [True])
        , CV.FilterBranch (CV.ArrayFilter False) (CV.Leaf [False])
        ]
        `shouldBe` CV.FilterBranch
          (CV.ArrayFilter True)
          (CV.FilterBranch (CV.ArrayFilter False) (CV.Leaf [True, False]))
    it "Should work 2 levels deep" $
      CV.mergeTrees @Bool
        [ F (AF True) (B [L [True], F (AF True) (L [True, True])])
        , F (AF False) (B [L [False], F (AF False) (L [False, False])])
        ]
        `shouldBe` F
          (AF True)
          ( B
              [ F (AF False) (B [L [True, False], F (AF False) (L [True, False, False])])
              , F (AF True) (F (AF False) (B [L [True, True, False], F (AF False) (L [True, True, False, False])]))
              ]
          )
  describe "clampTree" $ do
    it "Should exclude too long possibilities" $
      CV.clampTree 2 (L [1  :: Int .. 10]) `shouldBe` B []
    it "Should work within branches" $
      CV.clampTree 2 (B [L [1 :: Int, 2], L [2, 3, 4], L [3, 4]])
        `shouldBe` B [L [1, 2], L [3, 4]]

expandRuleSpec :: Spec
expandRuleSpec = describe "Expand Rule should generate appropriate expansion trees" $ do
  it "should expand a simple rule" $ do
    let rule = arr [0 <+ a VInt]
        expandedRules =
          withHuddleRule ["test" =:= rule] "test" $
            CV.expandRules 1
    expandedRules `shouldBe`
      F (AF (MIt (Postlude PTInt))) (L [MIt (Postlude PTInt)])
  it "should expand a rule with multiple productions" $ do
    -- Test expanding a rule [* int, * bool]
    -- Should generate an expansion tree that allows:
    -- - Zero or more integers followed by zero or more booleans
    -- - Each element can appear 0 to unbounded times
    -- - The total length, however, must be 3
    let rule = arr [0 <+ a VInt, 0 <+ a VBool]
        expandedRules =
          withHuddleRule ["test" =:= rule] "test" $
            CV.expandRules 3
        mI = (MIt (Postlude PTInt))
        mB = (MIt (Postlude PTBool))
    expandedRules `shouldBe`
      B [
          F (AF mI) (B [
            F (AF mI) (B [
              F (AF mI) (L [mI, mI, mI])
            , F (AF mB) (L [mI, mI, mB])
            ])
          , F (AF mB) (F (AF mB) (L [mI, mB, mB]))
          ])
        , F (AF mB) (F (AF mB) (F (AF mB) (L [mB, mB, mB])))
        ]

--------------------------------------------------------------------------------
-- Utility
--

withHuddleRule :: Huddle -> Name -> ([CV.Rule] -> Reader CV.CDDL a) -> a
withHuddleRule hdl n rdr = runReader (rdr groupProductions) cddl
  where
    cddl@(CTreeRoot tree) = case fullResolveCDDL (toCDDLNoRoot hdl) of
      Left e -> error $ show e
      Right c -> c
    groupProductions = case runIdentity $ tree Map.! n of
      MIt (Array elts) -> elts
      MIt (Map elts) -> elts
      _ -> error "Rule does not identify an array or map"

pattern F :: forall {r}. CV.Filter r -> CV.ExpansionTree' r -> CV.ExpansionTree' r
pattern F f e = CV.FilterBranch f e
pattern L :: forall {r}. [r] -> CV.ExpansionTree' r
pattern L rs = CV.Leaf rs
pattern B :: forall {r}. [CV.ExpansionTree' r] -> CV.ExpansionTree' r
pattern B xs = CV.Branch xs
pattern AF :: forall {r}. r -> CV.Filter r
pattern AF f = CV.ArrayFilter f
