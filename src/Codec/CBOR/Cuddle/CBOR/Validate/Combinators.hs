module Codec.CBOR.Cuddle.CBOR.Validate.Combinators (failUnless, validateWithUnless, resolveIfRef) where

import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.Resolve
import Control.Lens ((#))
import Data.Functor.Identity
import Data.Map.Strict qualified as Map
import Data.Validation

failUnless :: Bool -> e -> Validation e ()
failUnless cond = validateWithUnless cond (_Success # ())

validateWithUnless :: Bool -> Validation e () -> e -> Validation e ()
validateWithUnless cond val msg
  | cond = val
  | otherwise = _Failure # msg

resolveIfRef :: CTreeRoot' Identity MonoRef -> Node MonoRef -> CTree MonoRef
resolveIfRef _ (MIt aa) = aa
resolveIfRef ct@(CTreeRoot cddl) (MRuleRef n) = do
  case Map.lookup n cddl of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val -> resolveIfRef ct $ runIdentity val
