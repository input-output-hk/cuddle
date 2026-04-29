{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.CBOR.Cuddle.CDDL.CBORGenerator (
  MonadCddl (..),
  GenPhase,
  ValidatorPhase,

  -- * Custom generators
  CBORGen,
  GenEnv (..),
  HasGenerator (..),
  WrappedTerm (..),
  XXCTree (..),
  runCBORGen,
  liftAntiGen,
  withAntiGen,
  withTwiddle,

  -- * Custom validators
  CBORValidator,
  CustomValidatorResult (..),
  ValidateEnv (..),
  HasValidator (..),
  runCBORValidator,
) where

import Codec.CBOR.Cuddle.CDDL (Name)
import Codec.CBOR.Cuddle.CDDL.CTree (CTree, CTreeRoot (..), XXCTree)
import Codec.CBOR.Term (Term)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, mapReaderT)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics.Core (Lens')
import Test.AntiGen (AntiGen)
import Test.QuickCheck.GenT (MonadGen (..))

class MonadCddl m where
  type Phase m :: Type

  lookupCddl :: Name -> m (Maybe (CTree (Phase m)))

type data GenPhase
type data ValidatorPhase

data instance XXCTree ValidatorPhase
  = VRuleRef Name
  | VValidator (WrappedTerm -> CBORValidator ()) (CTree ValidatorPhase)

data GenEnv = GenEnv
  { geRoot :: CTreeRoot GenPhase
  , geTwiddle :: !Bool
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

instance MonadCddl CBORGen where
  type Phase CBORGen = GenPhase

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
  deriving (Eq, Ord, Show)

class HasGenerator a where
  generatorL :: Lens' a (Maybe (CBORGen WrappedTerm))

class HasValidator a where
  validatorL :: Lens' a (Maybe (WrappedTerm -> CBORValidator ()))

data CustomValidatorResult
  = CustomValidatorSuccess
  | CustomValidatorFailure Text
  deriving (Generic, Show, Eq)

newtype ValidateEnv = ValidateEnv {veRoot :: CTreeRoot ValidatorPhase}

newtype CBORValidator a = CBORValidator (ReaderT ValidateEnv (Either Text) a)
  deriving (Functor, Applicative, Monad, MonadReader ValidateEnv)

instance MonadCddl CBORValidator where
  type Phase CBORValidator = ValidatorPhase

  lookupCddl n = do
    CTreeRoot root <- asks veRoot
    pure $ Map.lookup n root

instance MonadFail CBORValidator where
  fail msg = CBORValidator . ReaderT $ \_ -> Left $ T.pack msg

runCBORValidator :: CBORValidator () -> ValidateEnv -> CustomValidatorResult
runCBORValidator (CBORValidator (ReaderT f)) env =
  either CustomValidatorFailure (const CustomValidatorSuccess) $ f env
