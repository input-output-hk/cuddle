{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.CBOR.Cuddle.CDDL.Custom.Validator (
  TermValidator,
  ValidatorPhase,
  Validator,
  ValidatorError (..),
  displayValidatorError,
  CustomValidatorResult (..),
  XXCTree (..),
  HasValidator (..),
  ValidateEnv (..),
  withLocalValidateBindings,
  runValidator,
  runValidatorM,
) where

import Codec.CBOR.Cuddle.CDDL (GRef (..), Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTree, CTreeRoot (..))
import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Codec.CBOR.Cuddle.CDDL.Custom.Core (MonadCddl (..), RuleTerm)
import Codec.CBOR.Cuddle.CDDL.Custom.Generator (XXCTree)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics.Lens (Lens')

type data ValidatorPhase

data instance XXCTree ValidatorPhase
  = VRuleRef Name
  | VValidator TermValidator (CTree ValidatorPhase)

class HasValidator a where
  validatorL :: Lens' a (Maybe TermValidator)

-- | Errors that signal a malformed CDDL or an unsupported construct
-- encountered while validating. Distinct from validation traces, which
-- report ordinary schema mismatches.
data ValidatorError
  = InvalidType
  | UnsupportedControlOperator CtlOp
  | InvalidController CtlOp
  | IncompatibleRangeTypes
  | InvalidRangeBounds
  | InvalidMapElement
  | InvalidReference Name
  | InvalidGenericReference GRef
  | -- | Generic failure raised from 'MonadFail'
    CustomFailure Text
  deriving (Show)

displayValidatorError :: ValidatorError -> String
displayValidatorError = \case
  InvalidType -> "Invalid type"
  UnsupportedControlOperator op -> "Unsupported control operator: " <> show op
  InvalidController op -> "Invalid controller for operator: " <> show op
  IncompatibleRangeTypes -> "Incompatible range endpoint types"
  InvalidRangeBounds -> "Invalid range bounds"
  InvalidMapElement -> "Invalid map element"
  InvalidGenericReference (GRef n) -> "Unbound local reference: " <> T.unpack n
  InvalidReference (Name n) -> "Unbound name: " <> T.unpack n
  CustomFailure msg -> T.unpack msg

data CustomValidatorResult
  = CustomValidatorSuccess
  | CustomValidatorFailure Text
  deriving (Generic, Show, Eq)

data ValidateEnv = ValidateEnv
  { veRoot :: CTreeRoot ValidatorPhase
  , veLocal :: Map Name (CTree ValidatorPhase)
  }

newtype Validator a = Validator (ReaderT ValidateEnv (Either ValidatorError) a)
  deriving (Functor, Applicative, Monad, MonadReader ValidateEnv, MonadError ValidatorError)

type TermValidator = RuleTerm -> Validator ()

instance MonadCddl Validator where
  type Phase Validator = ValidatorPhase

  lookupCddl n = do
    CTreeRoot root <- asks veRoot
    pure $ Map.lookup n root

  lookupGRef (GRef t) = do
    binds <- asks veLocal
    pure $ Map.lookup (Name t) binds

instance MonadFail Validator where
  fail = throwError . CustomFailure . T.pack

-- | Run an action with the given local generic bindings installed.
withLocalValidateBindings ::
  Map Name (CTree ValidatorPhase) -> Validator a -> Validator a
withLocalValidateBindings binds =
  local (\env -> env {veLocal = binds `Map.union` veLocal env})

runValidator ::
  Validator a -> CTreeRoot ValidatorPhase -> CustomValidatorResult
runValidator v cddl =
  case runValidatorM v cddl of
    Right _ -> CustomValidatorSuccess
    Left err -> CustomValidatorFailure . T.pack $ displayValidatorError err

runValidatorM ::
  Validator a -> CTreeRoot ValidatorPhase -> Either ValidatorError a
runValidatorM (Validator m) cddl =
  runReaderT m ValidateEnv {veRoot = cddl, veLocal = Map.empty}
