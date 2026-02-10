{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.CBOR.Cuddle.CDDL.CBORGenerator (
  CBORGenerator (..),
  HasGenerator (..),
  WrappedTerm (..),
  CBORValidator (..),
  HasValidator (..),
  GenPhase,
  XXCTree (..),
  CustomValidatorResult (..),
) where

import Codec.CBOR.Cuddle.CDDL (Name)
import Codec.CBOR.Cuddle.CDDL.CTree (CTree, CTreeRoot, XXCTree)
import Codec.CBOR.Term (Term)
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core (Lens')
import Test.AntiGen (AntiGen)

type data GenPhase

data instance XXCTree GenPhase
  = GenRef Name
  | GenGenerator CBORGenerator (CTree GenPhase)

data WrappedTerm
  = -- | Single term
    S Term
  | -- | Pair term
    P Term Term
  | -- | Group term
    G [WrappedTerm]
  deriving (Eq, Show)

newtype CBORGenerator = CBORGenerator (CTreeRoot GenPhase -> AntiGen WrappedTerm)

class HasGenerator a where
  generatorL :: Lens' a (Maybe CBORGenerator)

class HasValidator a where
  validatorL :: Lens' a (Maybe CBORValidator)

data CustomValidatorResult
  = CustomValidatorSuccess
  | CustomValidatorFailure Text
  deriving (Generic, Show, Eq)

newtype CBORValidator = CBORValidator (Term -> CustomValidatorResult)
