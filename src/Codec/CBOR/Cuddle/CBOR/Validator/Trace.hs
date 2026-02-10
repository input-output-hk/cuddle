{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.CBOR.Cuddle.CBOR.Validator.Trace (
  ValidatorStage,
  XXCTree (..),
  SValidity (..),
  ValidationTrace (..),
  ValidationResult (..),
  showSimple,
  traceValidity,
  toResult,
  isValid,
  compareProgress,
) where

import Codec.CBOR.Cuddle.CDDL (Name, XTerm)
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (CBORValidator)
import Codec.CBOR.Cuddle.CDDL.CTree (CTree (..), CTreeRoot (..), Node, XXCTree, foldCTree)
import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, XXCTree (..))
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Data.Text (Text)
import Data.Type.Equality (TestEquality (..), (:~:) (..))

--------------------------------------------------------------------------------
-- ValidatorStage

type data ValidatorStage

data instance XTerm ValidatorStage = ValidatorXTerm
  deriving (Show, Eq)

data instance XXCTree ValidatorStage
  = VRuleRef Name
  | VValidator CBORValidator (CTree ValidatorStage)

instance IndexMappable CTreeRoot MonoReferenced ValidatorStage where
  mapIndex (CTreeRoot m) = CTreeRoot $ mapIndex <$> m

instance IndexMappable CTree MonoReferenced ValidatorStage where
  mapIndex = foldCTree mapExt mapIndex
    where
      mapExt (MRuleRef n) = CTreeE $ VRuleRef n
      mapExt (MGenerator _ x) = mapIndex x
      mapExt (MValidator v x) = CTreeE . VValidator v $ mapIndex x

type data ValidatorStageSimple

newtype instance XXCTree ValidatorStageSimple = VRuleRefSimple Name

instance IndexMappable CTreeRoot ValidatorStage ValidatorStageSimple where
  mapIndex (CTreeRoot m) = CTreeRoot $ mapIndex <$> m

instance IndexMappable CTree ValidatorStage ValidatorStageSimple where
  mapIndex = foldCTree mapExt mapIndex
    where
      mapExt (VRuleRef n) = CTreeE $ VRuleRefSimple n
      mapExt (VValidator _ x) = mapIndex x

showSimple ::
  ( IndexMappable a ValidatorStage ValidatorStageSimple
  , Show (a ValidatorStageSimple)
  ) =>
  a ValidatorStage -> String
showSimple = show . mapIndex @_ @_ @ValidatorStageSimple

deriving instance Eq (Node ValidatorStageSimple)

deriving instance Show (Node ValidatorStageSimple)

--------------------------------------------------------------------------------
-- Validation result

type data Validity
  = IsValid
  | IsInvalid

data SValidity (v :: Validity) where
  SInvalid :: SValidity IsInvalid
  SValid :: SValidity IsValid

deriving instance Eq (SValidity v)

deriving instance Ord (SValidity v)

deriving instance Show (SValidity v)

instance TestEquality SValidity where
  testEquality SValid SValid = Just Refl
  testEquality SInvalid SInvalid = Just Refl
  testEquality _ _ = Nothing

data ValidationTrace (v :: Validity) where
  UnapplicableRule :: CTree ValidatorStageSimple -> ValidationTrace IsInvalid
  TerminalRule :: CTree ValidatorStageSimple -> ValidationTrace IsValid
  ReferenceRule :: Name -> ValidationTrace v -> ValidationTrace v
  CustomFailure :: Text -> ValidationTrace IsInvalid
  CustomSuccess :: ValidationTrace IsValid
  UnsatisfiedControl :: CtlOp -> ValidationTrace IsInvalid
  TraceInformation :: Text -> ValidationTrace v -> ValidationTrace v

deriving instance Show (ValidationTrace v)

data ValidationResult where
  ValidationResult ::
    { vrValidity :: SValidity v
    , vrTrace :: ValidationTrace v
    } ->
    ValidationResult

deriving instance Show ValidationResult

traceValidity :: ValidationTrace v -> SValidity v
traceValidity UnapplicableRule {} = SInvalid
traceValidity TerminalRule {} = SValid
traceValidity (ReferenceRule _ x) = traceValidity x
traceValidity CustomFailure {} = SInvalid
traceValidity UnsatisfiedControl {} = SInvalid
traceValidity (TraceInformation _ x) = traceValidity x
traceValidity CustomSuccess {} = SValid

toResult :: ValidationTrace v -> ValidationResult
toResult x =
  ValidationResult
    { vrValidity = traceValidity x
    , vrTrace = x
    }

isValid :: ValidationResult -> Bool
isValid ValidationResult {vrValidity = SValid} = True
isValid _ = False

measureProgress :: ValidationTrace IsInvalid -> Int
measureProgress UnapplicableRule {} = 0
measureProgress CustomFailure {} = 0
measureProgress (TraceInformation _ x) = measureProgress x
measureProgress UnsatisfiedControl {} = 0
measureProgress (ReferenceRule _ x) = succ $ measureProgress x

compareProgress :: ValidationResult -> ValidationResult -> Ordering
compareProgress (ValidationResult SValid _) (ValidationResult SValid _) = EQ
compareProgress (ValidationResult SInvalid _) (ValidationResult SValid _) = LT
compareProgress (ValidationResult SValid _) (ValidationResult SInvalid _) = GT
compareProgress (ValidationResult SInvalid x) (ValidationResult SInvalid y) =
  measureProgress x `compare` measureProgress y
