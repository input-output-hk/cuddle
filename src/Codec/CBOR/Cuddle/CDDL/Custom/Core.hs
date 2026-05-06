{-# LANGUAGE TypeFamilies #-}

module Codec.CBOR.Cuddle.CDDL.Custom.Core (MonadCddl (..), RuleTerm (..)) where

import Codec.CBOR.Cuddle.CDDL (GRef, Name)
import Codec.CBOR.Cuddle.CDDL.CTree (CTree)
import Codec.CBOR.Term (Term)
import Data.Kind (Type)

class MonadCddl m where
  type Phase m :: Type

  -- | Look up a top-level rule by name.
  lookupCddl :: Name -> m (Maybe (CTree (Phase m)))

  -- | Look up the rule bound to a generic parameter at the enclosing rule.
  -- Returns 'Nothing' outside of a custom generator/validator that was
  -- attached to a generic rule.
  lookupGRef :: GRef -> m (Maybe (CTree (Phase m)))

data RuleTerm
  = SingleTerm Term
  | PairTerm Term Term
  | GroupTerm [RuleTerm]
  deriving (Eq, Ord, Show)
