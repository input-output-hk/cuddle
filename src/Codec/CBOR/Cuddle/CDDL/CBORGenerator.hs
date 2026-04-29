{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.CBOR.Cuddle.CDDL.CBORGenerator (
  MonadCddl (..),
  GenPhase,
  ValidatorPhase,

  -- * Custom generators
  CBORGen,
  GenConfig (..),
  GenEnv (..),
  HasGenerator (..),
  WrappedTerm (..),
  XXCTree (..),
  runCBORGen,
  liftAntiGen,
  withAntiGen,
  withTwiddle,
  withLocalGenBindings,

  -- * Custom validators
  CBORValidator,
  CustomValidatorResult (..),
  ValidateEnv (..),
  HasValidator (..),
  runCBORValidator,
  withLocalValidateBindings,
) where

import Codec.CBOR.Cuddle.CDDL (GRef (..), Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTree, CTreeRoot (..), XXCTree)
import Codec.CBOR.Term (Term)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, local, mapReaderT)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics.Core (Lens')
import Test.AntiGen (AntiGen)
import Test.QuickCheck.GenT (MonadGen (..))

class MonadCddl m where
  type Phase m :: Type

  -- | Look up a top-level rule by name.
  lookupCddl :: Name -> m (Maybe (CTree (Phase m)))

  -- | Look up the rule bound to a generic parameter at the enclosing rule.
  -- Returns 'Nothing' outside of a custom generator/validator that was
  -- attached to a generic rule.
  lookupGRef :: GRef -> m (Maybe (CTree (Phase m)))

type data GenPhase
type data ValidatorPhase

data instance XXCTree ValidatorPhase
  = VRuleRef Name
  | VValidator (WrappedTerm -> CBORValidator ()) (CTree ValidatorPhase)

-- | User-supplied configuration for the generator monad. Set once at the
-- top level when calling 'runCBORGen'.
data GenConfig = GenConfig
  { gcRoot :: CTreeRoot GenPhase
  , gcTwiddle :: !Bool
  }
  deriving (Generic)

-- | Runtime environment for the generator monad: the user-supplied
-- 'GenConfig' plus the bindings active at the current point.
data GenEnv = GenEnv
  { geConfig :: GenConfig
  , geLocal :: Map Name (CTree GenPhase)
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

runCBORGen :: GenConfig -> CBORGen a -> AntiGen a
runCBORGen cfg (CBORGen m) =
  runReaderT m GenEnv {geConfig = cfg, geLocal = Map.empty}

instance MonadCddl CBORGen where
  type Phase CBORGen = GenPhase

  lookupCddl n = do
    CTreeRoot root <- asks (gcRoot . geConfig)
    pure $ Map.lookup n root

  lookupGRef (GRef t) = do
    binds <- asks geLocal
    pure $ Map.lookup (Name t) binds

withAntiGen :: (AntiGen a -> AntiGen b) -> CBORGen a -> CBORGen b
withAntiGen f (CBORGen m) = CBORGen $ ReaderT $ \env -> f (runReaderT m env)

withTwiddle :: Bool -> CBORGen a -> CBORGen a
withTwiddle t =
  local (\env -> env {geConfig = (geConfig env) {gcTwiddle = t}})

-- | Run an action with the given local generic bindings installed.
-- Used to wrap custom generators attached to generic rules so that
-- 'lookupGRef' resolves to the type bound at the enclosing rule.
withLocalGenBindings ::
  Map Name (CTree GenPhase) -> CBORGen a -> CBORGen a
withLocalGenBindings binds =
  local (\env -> env {geLocal = binds `Map.union` geLocal env})

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

data ValidateEnv = ValidateEnv
  { veRoot :: CTreeRoot ValidatorPhase
  , veLocal :: Map Name (CTree ValidatorPhase)
  }

newtype CBORValidator a = CBORValidator (ReaderT ValidateEnv (Either Text) a)
  deriving (Functor, Applicative, Monad, MonadReader ValidateEnv)

instance MonadCddl CBORValidator where
  type Phase CBORValidator = ValidatorPhase

  lookupCddl n = do
    CTreeRoot root <- asks veRoot
    pure $ Map.lookup n root

  lookupGRef (GRef t) = do
    binds <- asks veLocal
    pure $ Map.lookup (Name t) binds

-- | Run an action with the given local generic bindings installed.
withLocalValidateBindings ::
  Map Name (CTree ValidatorPhase) -> CBORValidator a -> CBORValidator a
withLocalValidateBindings binds =
  local (\env -> env {veLocal = binds `Map.union` veLocal env})

instance MonadFail CBORValidator where
  fail msg = CBORValidator . ReaderT $ \_ -> Left $ T.pack msg

runCBORValidator ::
  CTreeRoot ValidatorPhase -> CBORValidator () -> CustomValidatorResult
runCBORValidator cddl (CBORValidator (ReaderT f)) =
  either CustomValidatorFailure (const CustomValidatorSuccess) $
    f ValidateEnv {veRoot = cddl, veLocal = Map.empty}
