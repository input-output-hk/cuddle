{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.CBOR.Cuddle.CDDL.CBORGenerator (
  CBORGen (..),
  GenEnv (..),
  HasGenerator (..),
  WrappedTerm (..),
  CBORValidator (..),
  HasValidator (..),
  GenPhase,
  XXCTree (..),
  CustomValidatorResult (..),
  liftAntiGen,
  runCBORGen,
  lookupCddl,
  withAntiGen,
  withTwiddle,
) where

import Codec.CBOR.Cuddle.CDDL (Name)
import Codec.CBOR.Cuddle.CDDL.CTree (CTree, CTreeRoot (..), XXCTree)
import Codec.CBOR.Term (Term)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, mapReaderT)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core (Lens')
import Test.AntiGen (AntiGen)
import Test.QuickCheck.GenT (MonadGen (..))

type data GenPhase

-- | Generator context, parametrised over the type of the random seed
data GenEnv = GenEnv
  { geRoot :: CTreeRoot GenPhase
  , geTwiddle :: Bool
  }
  deriving (Generic)

newtype CBORGen a = CBORGen (ReaderT GenEnv AntiGen a)
  deriving (Functor, Applicative, Monad, MonadReader GenEnv)

instance MonadGen CBORGen where
  liftGen g = CBORGen $ ReaderT $ \_ -> liftGen g
  variant n (CBORGen m) = CBORGen $ mapReaderT (variant n) m
  sized f = CBORGen . ReaderT $ \env -> sized $ \n ->
    let CBORGen m = f n in runReaderT m env
  resize n (CBORGen m) = CBORGen $ mapReaderT (resize n) m
  choose rng = CBORGen . ReaderT $ \_ -> choose rng

liftAntiGen :: AntiGen a -> CBORGen a
liftAntiGen m = CBORGen . ReaderT $ const m

runCBORGen :: GenEnv -> CBORGen a -> AntiGen a
runCBORGen env (CBORGen m) = runReaderT m env

lookupCddl :: Name -> CBORGen (Maybe (CTree GenPhase))
lookupCddl n = do
  CTreeRoot root <- asks geRoot
  pure $ Map.lookup n root

withAntiGen :: (AntiGen a -> AntiGen b) -> CBORGen a -> CBORGen b
withAntiGen f (CBORGen m) = CBORGen $ ReaderT $ \env -> f (runReaderT m env)

withTwiddle :: Bool -> CBORGen a -> CBORGen a
withTwiddle t = local (\x -> x {geTwiddle = t})

data instance XXCTree GenPhase
  = GenRef Name
  | GenGenerator (CBORGen WrappedTerm) (CTree GenPhase)

data WrappedTerm
  = -- | Single term
    S Term
  | -- | Pair term
    P Term Term
  | -- | Group term
    G [WrappedTerm]
  deriving (Eq, Show)

class HasGenerator a where
  generatorL :: Lens' a (Maybe (CBORGen WrappedTerm))

class HasValidator a where
  validatorL :: Lens' a (Maybe CBORValidator)

data CustomValidatorResult
  = CustomValidatorSuccess
  | CustomValidatorFailure Text
  deriving (Generic, Show, Eq)

newtype CBORValidator = CBORValidator (Term -> CustomValidatorResult)
