module Codec.CBOR.Cuddle.CBOR.Validate.Types (Reason (..), CDDL', Rule') where

import Codec.CBOR.Cuddle.CDDL hiding (Group (..))
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.Resolve
import Data.Functor.Identity

type CDDL' = CTreeRoot' Identity MonoRef
type Rule' = Node MonoRef

data Reason
  = UnboundRef Name
  | InvalidTopCBOR
  | InvalidInnerCBOR
  | NotASingleTopTerm
  | NotASingleInnerTerm
  | DidNotValidate String
  | Unexpected
      -- | Expecting
      String
      -- | but got
      String

instance Show Reason where
  show (UnboundRef r) = "Unbound reference " <> show r
  show InvalidTopCBOR = "Invalid CBOR found"
  show InvalidInnerCBOR = "Invalid CBOR in a .cbor control found"
  show NotASingleTopTerm = "Parsed CBOR is not a single term"
  show NotASingleInnerTerm = "Parsed CBOR in a .cbor control is not a single term"
  show (DidNotValidate s) = "Validation failure: " <> s
  show (Unexpected expected got) = "Expected " <> expected <> " but got " <> got
