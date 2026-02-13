{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec.CBOR.Cuddle.CBOR.Validator.Trace (
  ValidatorStage,
  ValidatorStageSimple,
  XXCTree (..),
  SValidity (..),
  Validity (..),
  ValidationTrace (..),
  ListValidationTrace (..),
  MapValidationTrace (..),
  Evidenced (..),
  showSimple,
  traceValidity,
  isValid,
  prettyValidationResult,
  mapTrace,
  compareEvidencedProgress,
  evidence,
  foldEvidenced,
) where

import Codec.CBOR.Cuddle.CDDL (Name (..), XTerm)
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (CBORValidator)
import Codec.CBOR.Cuddle.CDDL.CTree (CTree (..), CTreeRoot (..), Node, XXCTree, foldCTree)
import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, XXCTree (..))
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Codec.CBOR.Term (Term)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import Prettyprinter (Doc, Pretty (..), annotate, hang, viaShow, vsep, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color)

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
  ChoiceBranch :: Int -> ValidationTrace IsValid -> ValidationTrace IsValid
  ListTrace :: ListValidationTrace v -> ValidationTrace v
  MapTrace :: MapValidationTrace v -> ValidationTrace v

deriving instance Show (ValidationTrace v)

data ListValidationTrace (v :: Validity) where
  ListValidationDone :: ListValidationTrace IsValid
  ListValidationLeftoverTerms :: NonEmpty Term -> ListValidationTrace IsInvalid
  ListValidationUnappliedRules ::
    NonEmpty (CTree ValidatorStageSimple) -> ListValidationTrace IsInvalid
  ListValidationConsume ::
    CTree ValidatorStageSimple ->
    ValidationTrace IsValid ->
    ListValidationTrace v ->
    ListValidationTrace v
  ListValidationMissingRequired ::
    CTree ValidatorStageSimple -> ValidationTrace IsInvalid -> ListValidationTrace IsInvalid
  ListValidationConsumeGroup ::
    [Name] ->
    ListValidationTrace IsValid ->
    ListValidationTrace v ->
    ListValidationTrace v
  ListValidationBadGroup ::
    [Name] ->
    ListValidationTrace IsInvalid ->
    ListValidationTrace IsInvalid

deriving instance Show (ListValidationTrace v)

data MapValidationTrace (v :: Validity) where
  MapValidationDone :: MapValidationTrace IsValid
  MapValidationLeftoverKVs :: [(Term, Term)] -> MapValidationTrace IsInvalid
  MapValidationUnappliedRules :: NonEmpty (CTree ValidatorStageSimple) -> MapValidationTrace IsInvalid
  MapValidationInvalidValue ::
    CTree ValidatorStageSimple ->
    ValidationTrace IsValid ->
    ValidationTrace IsInvalid ->
    MapValidationTrace IsInvalid
  MapValidationConsume ::
    CTree ValidatorStageSimple ->
    ValidationTrace IsValid ->
    ValidationTrace IsValid ->
    MapValidationTrace v ->
    MapValidationTrace v

deriving instance Show (MapValidationTrace v)

mapTrace :: (forall v. Show (u v)) => (forall v. t v -> u v) -> Evidenced t -> Evidenced u
mapTrace f (Evidenced v t) = Evidenced v $ f t

data Evidenced t where
  Evidenced ::
    Show (t v) =>
    { eValidity :: SValidity v
    , eTrace :: t v
    } ->
    Evidenced t

foldEvidenced :: (forall v. t v -> a) -> Evidenced t -> a
foldEvidenced f (Evidenced v t) =
  case v of
    SValid -> f t
    SInvalid -> f t

deriving instance Show (Evidenced t)

instance IsValidationTrace t => Semigroup (Evidenced t) where
  x@(Evidenced SValid _) <> _ = x
  _ <> y@(Evidenced SValid _) = y
  Evidenced SInvalid x <> Evidenced SInvalid y =
    Evidenced SInvalid $ case compareInvalidProgress x y of
      GT -> x
      _ -> y

class IsValidationTrace (t :: Validity -> Type) where
  traceValidity :: t v -> SValidity v
  measureProgress :: t v -> Int

instance IsValidationTrace ValidationTrace where
  traceValidity = \case
    CustomSuccess {} -> SValid
    TerminalRule {} -> SValid
    ChoiceBranch {} -> SValid
    UnapplicableRule {} -> SInvalid
    CustomFailure {} -> SInvalid
    UnsatisfiedControl {} -> SInvalid
    ReferenceRule _ x -> traceValidity x
    ListTrace x -> traceValidity x
    MapTrace x -> traceValidity x

  measureProgress = \case
    TerminalRule _ -> 1
    CustomSuccess -> 1
    ChoiceBranch {} -> 1
    UnapplicableRule {} -> 0
    CustomFailure {} -> 0
    UnsatisfiedControl {} -> 0
    (ReferenceRule _ x) -> succ $ measureProgress x
    (ListTrace x) -> measureProgress x
    (MapTrace x) -> measureProgress x

instance IsValidationTrace ListValidationTrace where
  traceValidity = \case
    ListValidationDone -> SValid
    ListValidationLeftoverTerms {} -> SInvalid
    ListValidationUnappliedRules {} -> SInvalid
    ListValidationMissingRequired {} -> SInvalid
    ListValidationBadGroup _ _ -> SInvalid
    ListValidationConsume _ _ x -> traceValidity x
    ListValidationConsumeGroup _ _ x -> traceValidity x

  measureProgress = \case
    ListValidationDone -> 10
    ListValidationConsume _ _ x -> succ $ measureProgress x
    ListValidationLeftoverTerms _ -> 0
    ListValidationMissingRequired _ _ -> 0
    ListValidationUnappliedRules _ -> 0
    ListValidationConsumeGroup _ g x -> measureProgress g + measureProgress x
    ListValidationBadGroup _ x -> measureProgress x

instance IsValidationTrace MapValidationTrace where
  traceValidity = \case
    MapValidationDone -> SValid
    MapValidationLeftoverKVs _ -> SInvalid
    MapValidationUnappliedRules _ -> SInvalid
    MapValidationInvalidValue {} -> SInvalid
    MapValidationConsume _ _ _ x -> traceValidity x

  measureProgress = \case
    MapValidationDone -> 10
    MapValidationLeftoverKVs _ -> 0
    MapValidationUnappliedRules _ -> 0
    MapValidationConsume _ _ _ x -> 3 + measureProgress x
    MapValidationInvalidValue {} -> 2

evidence :: (Show (t v), IsValidationTrace t) => t v -> Evidenced t
evidence x = Evidenced (traceValidity x) x

isValid :: Evidenced t -> Bool
isValid Evidenced {eValidity = SValid} = True
isValid _ = False

compareEvidencedProgress :: IsValidationTrace t => Evidenced t -> Evidenced t -> Ordering
compareEvidencedProgress (Evidenced SValid _) (Evidenced SValid _) = EQ
compareEvidencedProgress (Evidenced SInvalid _) (Evidenced SValid _) = LT
compareEvidencedProgress (Evidenced SValid _) (Evidenced SInvalid _) = GT
compareEvidencedProgress (Evidenced SInvalid x) (Evidenced SInvalid y) =
  x `compareInvalidProgress` y

compareInvalidProgress :: IsValidationTrace t => t IsInvalid -> t IsInvalid -> Ordering
compareInvalidProgress = compare `on` measureProgress

nestContainer :: Doc AnsiStyle -> Doc AnsiStyle
nestContainer = hang 2 . (annotate (color Yellow) "└ " <>)

prettyListValidationResult :: ListValidationTrace v -> Doc AnsiStyle
prettyListValidationResult = \case
  ListValidationDone -> mempty
  ListValidationLeftoverTerms _ -> annotate (color Red) "no more rules left to apply"
  ListValidationUnappliedRules _ -> annotate (color Red) "not all required rules have been applied"
  ListValidationConsume _ t c ->
    vsep $ continue c [prettyValidationResult t]
  ListValidationMissingRequired _ t ->
    vsep
      [ annotate (color Red) "failed to apply required rule:"
      , nestContainer $ prettyValidationResult t
      ]
  ListValidationConsumeGroup refs x c -> vsep . continue c $ prettyGroup SValid refs x
  ListValidationBadGroup refs x -> vsep $ prettyGroup SInvalid refs x
  where
    -- This prevents unnecessary empty lines
    continue ListValidationDone l = l
    continue x l = l <> [prettyListValidationResult x]

    prettyGroup v refs x = go refs
      where
        go (n : ns) =
          [ "ref" <+> pretty n
          , nestContainer . vsep $ go ns
          ]
        go [] =
          [ case v of
              SValid -> "grp"
              SInvalid -> "grp" <+> annotate (color Red) "(fail)"
          , nestContainer $ prettyListValidationResult x
          ]

prettyMapValidationResult :: MapValidationTrace v -> Doc AnsiStyle
prettyMapValidationResult = \case
  MapValidationDone -> mempty
  MapValidationLeftoverKVs _ -> annotate (color Red) "no more rules left to apply"
  MapValidationUnappliedRules rs ->
    hang 2 $
      vsep
        [ annotate (color Red) "not all required rules have been applied:"
        , vsep (toList $ viaShow <$> rs)
        ]
  MapValidationConsume _ k v c ->
    vsep $
      continue
        c
        [ "key:"
        , nestContainer $ prettyValidationResult k
        , "value:"
        , nestContainer $ prettyValidationResult v
        ]
  MapValidationInvalidValue _ k v ->
    vsep
      [ "key:"
      , nestContainer $ prettyValidationResult k
      , annotate (color Red) "value:"
      , nestContainer $ prettyValidationResult v
      ]
  where
    -- This prevents unnecessary empty lines
    continue MapValidationDone l = l
    continue x l = l <> [prettyMapValidationResult x]

prettyValidationResult :: ValidationTrace v -> Doc AnsiStyle
prettyValidationResult = \case
  UnapplicableRule x -> annotate (color Red) $ "failed to apply: " <> viaShow x
  TerminalRule x -> "app: " <> viaShow x
  ChoiceBranch i c ->
    vsep
      [ "choice #" <> pretty i
      , nestContainer $ prettyValidationResult c
      ]
  ReferenceRule (Name n) x ->
    vsep
      [ "ref: " <> annotate (color Blue) (pretty n)
      , nestContainer $ prettyValidationResult x
      ]
  CustomFailure e -> annotate (color Red) $ viaShow e
  CustomSuccess -> "<custom validator>"
  UnsatisfiedControl c -> "unsatisfied control: " <> viaShow c
  ListTrace l ->
    vsep
      [ "array"
      , nestContainer $ prettyListValidationResult l
      ]
  MapTrace m ->
    vsep
      [ "map"
      , nestContainer $ prettyMapValidationResult m
      ]
