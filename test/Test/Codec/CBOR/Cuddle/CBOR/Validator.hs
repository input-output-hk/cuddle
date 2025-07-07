{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Codec.CBOR.Cuddle.CBOR.Validator where

import Test.Hspec
import qualified Codec.CBOR.Cuddle.CBOR.Validator as CV
import Codec.CBOR.Cuddle.Huddle
import Codec.CBOR.Cuddle.CDDL (Name)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoRef(MIt), fullResolveCDDL)
import Control.Monad.Reader
import Codec.CBOR.Cuddle.CDDL.CTree
import Data.Functor.Identity
import Data.Map.Strict qualified as Map

cborValidatorSpec :: Spec
cborValidatorSpec = do
  expandRuleSpec

expandRuleSpec :: Spec
expandRuleSpec = describe "Expand Rule should generate appropriate expansion trees" $ do
    it "should expand a simple rule" $ do
          -- Test expanding a rule [* int, * bool]
          -- Should generate an expansion tree that allows:
          -- - Zero or more integers followed by zero or more booleans
          -- - Each element can appear 0 to unbounded times
          -- - The total length, however, must be 3
          let rule = arr [0 <+ a VInt, 0 <+ a VBool]
              expandedRules = withHuddleRule ["test" =:= rule] "test" $
                CV.expandRules 3
          print $ expandedRules
          -- length expandedRules `shouldBe` 1
          -- -- The expansion should contain branches for different combinations
          -- -- of integers and booleans in the array
          -- case expandedRules of
          --   [tree] -> do
          --     -- Check that the tree has the expected structure for repeated elements
          --     tree `shouldSatisfy` (\t -> case t of
          --       CV.Branch [] -> True
          --       _ -> False)
            -- _ -> expectationFailure "Expected exactly one expansion tree"
    it "should expand a rule with choices" $ do
      pending
    it "should expand a rule with groups" $ do
      pending
    it "should handle optional elements" $ do
      pending

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
