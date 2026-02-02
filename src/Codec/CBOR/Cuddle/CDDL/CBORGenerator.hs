module Codec.CBOR.Cuddle.CDDL.CBORGenerator (
  CBORGenerator (..),
  HasGenerator (..),
  WrappedTerm (..),
  CBORValidator (..),
  ValidationResult (..),
  HasValidator (..),
) where

import Codec.CBOR.Term (Term)
import Data.Text (Text)
import Optics.Core (Lens')
import Test.QuickCheck (Gen)

data WrappedTerm
  = -- | Single term
    S Term
  | -- | Pair term
    P Term Term
  | -- | Group term
    G [WrappedTerm]
  deriving (Eq, Show)

newtype CBORGenerator = CBORGenerator (Gen WrappedTerm)

class HasGenerator a where
  generatorL :: Lens' a (Maybe CBORGenerator)

class HasValidator a where
  validatorL :: Lens' a (Maybe CBORValidator)

data ValidationResult
  = ValidatorSuccess
  | ValidatorFailure Text

newtype CBORValidator = CBORValidator (Term -> ValidationResult)
