{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.CBOR.Cuddle.CDDL.CBORGenerator (
  GenPhase,
  XXCTree (..),
  CBORGenerator (..),
  MonadCBORGen (..),
  getDepth,
  modifyDepth,
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

class Monad m => MonadCBORGen m where
  stateDepth :: (Int -> (a, Int)) -> m a
  askCDDL :: m (CTreeRoot GenPhase)

getDepth :: MonadCBORGen m => m Int
getDepth = stateDepth $ \d -> (d, d)

modifyDepth :: MonadCBORGen m => (Int -> Int) -> m ()
modifyDepth f = stateDepth $ \d -> ((), f d)

newtype CBORGenerator
  = CBORGenerator
      (forall g m. (StatefulGen g m, MonadCBORGen m) => [CTree GenPhase] -> g -> m WrappedTerm)

class HasGenerator a where
  generatorL :: Lens' a (Maybe CBORGenerator)

class HasValidator a where
  validatorL :: Lens' a (Maybe CBORValidator)

data CustomValidatorResult
  = ValidatorSuccess
  | ValidatorFailure Text

newtype CBORValidator = CBORValidator (Term -> CustomValidatorResult)
