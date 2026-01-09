{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.CBOR.Cuddle.CDDL.CBORGenerator (
  GenPhase,
  XXCTree (..),
  CBORGenerator (..),
  HasGenerator (..),
  WrappedTerm (..),
  CBORValidator (..),
  CustomValidatorResult (..),
  HasValidator (..),
) where

import Codec.CBOR.Cuddle.CDDL (Name)
import Codec.CBOR.Cuddle.CDDL.CTree (CTree, CTreeRoot, XXCTree)
import Codec.CBOR.Term (Term)
import Data.Text (Text)
import Optics.Core (Lens')
import System.Random.Stateful (StatefulGen)

type data GenPhase

data instance XXCTree GenPhase
  = GenRef Name
  | GenCustom CBORGenerator (CTree GenPhase)

data WrappedTerm
  = -- | Single term
    S Term
  | -- | Pair term
    P Term Term
  | -- | Group term
    G [WrappedTerm]
  deriving (Eq, Show)

newtype CBORGenerator
  = CBORGenerator (forall g m. StatefulGen g m => CTreeRoot GenPhase -> g -> m WrappedTerm)

class HasGenerator a where
  generatorL :: Lens' a (Maybe CBORGenerator)

class HasValidator a where
  validatorL :: Lens' a (Maybe CBORValidator)

data CustomValidatorResult
  = ValidatorSuccess
  | ValidatorFailure Text

newtype CBORValidator = CBORValidator (Term -> CustomValidatorResult)
