{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CBOR.Validate where

import Codec.CBOR.Cuddle.CBOR.Validate (AnnTerm (Prim), AnnotationContext (ruleName), PrimTerm (..), ValidatedWith (Valid), toAnnTerm, validateRule)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTreeRoot')
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoRef, fullResolveCDDL)
import Codec.CBOR.Cuddle.Huddle
import Codec.CBOR.Term qualified as Cborg
import Data.Default.Class (def)
import Data.Either (fromRight)
import Data.Functor.Identity (Identity)
import Test.Hspec (Spec, describe, it, shouldBe)

simpleSchema :: Huddle
simpleSchema = collectFrom manyPorts

port, manyPorts :: Rule
port = "port" =:= VUInt
manyPorts = "manyPorts" =:= arr [0 <+ a port]

simpleCTree :: CTreeRoot' Identity MonoRef
simpleCTree =
  -- simpleSchema should be correct as written, so this is pretty safe
  fromRight (error "Fix simpleSchema!")
    . fullResolveCDDL
    $ toCDDL simpleSchema

validateSpec :: Spec
validateSpec = describe "validate-cbor" $ do
  primSpec
  arraySpec

-- | Validate primitives
primSpec :: Spec
primSpec = describe "primitives" $ do
  it "Can validate a uint" $
    validateRule
      (toAnnTerm $ Cborg.TInt 0)
      simpleCTree
      (Name "port")
      `shouldBe` Valid (Prim (PInt 0)) (def {ruleName = "port"})

arraySpec :: Spec
arraySpec = describe "arrays" $ do
  it "Can validate a repeat array" $
    validateRule
      (toAnnTerm $ Cborg.TList [Cborg.TInt 0, Cborg.TInt 1])
      simpleCTree
      (Name "manyPorts")
      `shouldBe` Valid (Prim (PInt 0)) (def {ruleName = "port"})
