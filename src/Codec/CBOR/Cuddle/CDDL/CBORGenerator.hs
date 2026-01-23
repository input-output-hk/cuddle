module Codec.CBOR.Cuddle.CDDL.CBORGenerator (
  CBORGenerator (..),
  HasGenerator (..),
  WrappedTerm (..),
  CBORValidator (..),
  ValidationResult (..),
  HasValidator (..),
  ValidatorFailure (..),
) where

import Codec.CBOR.Term (Term)
import Data.Text (Text)
import Optics.Core (Lens')
import Test.AntiGen (AntiGen)

data WrappedTerm
  = -- | Single term
    S Term
  | -- | Pair term
    P Term Term
  | -- | Group term
    G [WrappedTerm]
  deriving (Eq, Show)

newtype CBORGenerator = CBORGenerator (AntiGen WrappedTerm)

class HasGenerator a where
  generatorL :: Lens' a (Maybe CBORGenerator)

class HasValidator a where
  validatorL :: Lens' a (Maybe CBORValidator)

newtype ValidatorFailure = ValidatorFailure Text
  deriving (Show)

data ValidationResult
  = ValidatorSuccess
  | ValidatorFail ValidatorFailure

newtype CBORValidator = CBORValidator (Term -> ValidationResult)
