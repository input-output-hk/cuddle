{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Codec.CBOR.Cuddle.CBOR.Validator.Trace (
  ValidatorPhaseSimple,
  XXCTree (..),
  SValidity (..),
  Validity (..),
  ValidationTrace (..),
  ListValidationTrace (..),
  MapValidationTrace (..),
  Evidenced (..),
  IsValidationTrace (..),
  ControlInfo (..),
  TraceOptions (..),
  Progress (..),
  defaultTraceOptions,
  showSimple,
  isValid,
  prettyValidationTrace,
  showValidationTrace,
  mapTrace,
  compareEvidencedProgress,
  evidence,
  foldEvidenced,
) where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (ValidatorPhase)
import Codec.CBOR.Cuddle.CDDL.CTree (CTree (..), CTreeRoot (..), Node, XXCTree, foldCTree)
import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Codec.CBOR.Cuddle.CDDL.Resolve (XXCTree (..))
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Codec.CBOR.Term (Term)
import Data.Foldable (Foldable (..))
import Data.Function (on)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import Data.Word (Word64)
import Prettyprinter (
  Doc,
  Pretty (..),
  annotate,
  defaultLayoutOptions,
  hang,
  indent,
  layoutPretty,
  vsep,
  (<+>),
 )
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, italicized)
import Prettyprinter.Render.Terminal qualified as Ansi

--------------------------------------------------------------------------------
-- ValidatorPhase

type data ValidatorPhaseSimple

newtype instance XXCTree ValidatorPhaseSimple = VRuleRefSimple Name

instance Pretty (XXCTree ValidatorPhaseSimple) where
  pretty (VRuleRefSimple n) = pretty n

instance IndexMappable CTreeRoot ValidatorPhase ValidatorPhaseSimple where
  mapIndex (CTreeRoot m) = CTreeRoot $ mapIndex <$> m

instance IndexMappable CTree ValidatorPhase ValidatorPhaseSimple where
  mapIndex = foldCTree mapExt mapIndex
    where
      mapExt (VRuleRef n) = CTreeE $ VRuleRefSimple n
      mapExt (VValidator _ x) = mapIndex x

showSimple ::
  ( IndexMappable a ValidatorPhase ValidatorPhaseSimple
  , Show (a ValidatorPhaseSimple)
  ) =>
  a ValidatorPhase -> String
showSimple = show . mapIndex @_ @_ @ValidatorPhaseSimple

deriving instance Eq (Node ValidatorPhaseSimple)

deriving instance Show (Node ValidatorPhaseSimple)

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

data ControlInfo = ControlInfo
  { ciOp :: CtlOp
  , ciRule :: CTree ValidatorPhaseSimple
  }
  deriving (Show)

instance Pretty ControlInfo where
  pretty ControlInfo {..} = pretty ciOp <+> pretty ciRule

data ValidationTrace (v :: Validity) where
  UnapplicableRule :: CTree ValidatorPhaseSimple -> ValidationTrace IsInvalid
  TerminalRule :: CTree ValidatorPhaseSimple -> ValidationTrace IsValid
  ControlTrace :: ControlInfo -> ValidationTrace IsValid -> ValidationTrace IsValid
  ReferenceRule :: Name -> ValidationTrace v -> ValidationTrace v
  CustomFailure :: Text -> ValidationTrace IsInvalid
  CustomSuccess :: ValidationTrace IsValid
  UnsatisfiedControl :: CtlOp -> CTree ValidatorPhaseSimple -> ValidationTrace IsInvalid
  ChoiceBranch :: Int -> ValidationTrace IsValid -> ValidationTrace IsValid
  ListTrace :: ListValidationTrace v -> ValidationTrace v
  MapTrace :: MapValidationTrace v -> ValidationTrace v
  TagTrace :: Word64 -> ValidationTrace v -> ValidationTrace v
  InvalidTag :: Word64 -> Word64 -> ValidationTrace IsInvalid

deriving instance Show (ValidationTrace v)

data ListValidationTrace (v :: Validity) where
  ListValidationDone :: ListValidationTrace IsValid
  ListValidationLeftoverTerms ::
    NonEmpty Term ->
    Maybe (CTree ValidatorPhaseSimple, ValidationTrace IsInvalid) ->
    ListValidationTrace IsInvalid
  ListValidationUnappliedRule ::
    CTree ValidatorPhaseSimple ->
    ListValidationTrace IsInvalid
  ListValidationConsume ::
    CTree ValidatorPhaseSimple ->
    ValidationTrace IsValid ->
    ListValidationTrace v ->
    ListValidationTrace v
  ListValidationMissingRequired ::
    CTree ValidatorPhaseSimple ->
    ValidationTrace IsInvalid ->
    ListValidationTrace IsInvalid
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
  MapValidationLeftoverKVs ::
    (Term, Term) ->
    Maybe (CTree ValidatorPhaseSimple, MapValidationTrace IsInvalid) ->
    MapValidationTrace IsInvalid
  MapValidationUnappliedRules :: NonEmpty (CTree ValidatorPhaseSimple) -> MapValidationTrace IsInvalid
  MapValidationInvalidValue ::
    CTree ValidatorPhaseSimple ->
    ValidationTrace IsValid ->
    ValidationTrace IsInvalid ->
    MapValidationTrace IsInvalid
  MapValidationConsume ::
    CTree ValidatorPhaseSimple ->
    ValidationTrace IsValid ->
    ValidationTrace IsValid ->
    MapValidationTrace v ->
    MapValidationTrace v

deriving instance Show (MapValidationTrace v)

mapTrace :: (forall v. Show (u v)) => (forall v. t v -> u v) -> Evidenced t -> Evidenced u
mapTrace f (Evidenced v t) = Evidenced v $ f t

data Evidenced t where
  Evidenced ::
    { eValidity :: SValidity v
    , eTrace :: t v
    } ->
    Evidenced t

foldEvidenced :: (forall v. t v -> a) -> Evidenced t -> a
foldEvidenced f (Evidenced v t) =
  case v of
    SValid -> f t
    SInvalid -> f t

deriving instance (forall v. Show (t v)) => Show (Evidenced t)

instance IsValidationTrace t => Semigroup (Evidenced t) where
  x@(Evidenced SValid _) <> _ = x
  _ <> y@(Evidenced SValid _) = y
  Evidenced SInvalid x <> Evidenced SInvalid y =
    Evidenced SInvalid $ case compareInvalidProgress x y of
      GT -> x
      _ -> y

data Progress = Progress {termProgress :: !Int, refProgress :: !Int}
  deriving (Eq, Ord)

instance Semigroup Progress where
  Progress t1 r1 <> Progress t2 r2 = Progress (t1 + t2) (r1 + r2)

instance Monoid Progress where
  mempty = Progress 0 0

increaseTermProgress :: Progress -> Progress
increaseTermProgress p = p <> Progress 1 0

increaseRefProgress :: Progress -> Progress
increaseRefProgress p = p <> Progress 0 1

class IsValidationTrace (t :: Validity -> Type) where
  traceValidity :: t v -> SValidity v
  measureProgress :: t v -> Progress

instance IsValidationTrace ValidationTrace where
  traceValidity = \case
    CustomSuccess {} -> SValid
    TerminalRule {} -> SValid
    ControlTrace {} -> SValid
    ChoiceBranch {} -> SValid
    UnapplicableRule {} -> SInvalid
    CustomFailure {} -> SInvalid
    UnsatisfiedControl {} -> SInvalid
    InvalidTag {} -> SInvalid
    ReferenceRule _ x -> traceValidity x
    ListTrace x -> traceValidity x
    MapTrace x -> traceValidity x
    TagTrace _ x -> traceValidity x

  measureProgress = \case
    TerminalRule {} -> Progress 1 0
    CustomSuccess -> Progress 1 0
    ControlTrace _ x -> measureProgress x
    ChoiceBranch _ x -> measureProgress x
    UnapplicableRule {} -> mempty
    CustomFailure {} -> mempty
    UnsatisfiedControl {} -> mempty
    InvalidTag {} -> mempty
    ReferenceRule _ x -> increaseRefProgress $ measureProgress x
    ListTrace x -> measureProgress x
    MapTrace x -> measureProgress x
    TagTrace _ x -> increaseTermProgress $ measureProgress x

instance IsValidationTrace ListValidationTrace where
  traceValidity = \case
    ListValidationDone -> SValid
    ListValidationLeftoverTerms {} -> SInvalid
    ListValidationUnappliedRule {} -> SInvalid
    ListValidationMissingRequired {} -> SInvalid
    ListValidationBadGroup _ _ -> SInvalid
    ListValidationConsume _ _ x -> traceValidity x
    ListValidationConsumeGroup _ _ x -> traceValidity x

  measureProgress = \case
    ListValidationDone -> Progress 1 0
    ListValidationConsume _ _ x -> increaseTermProgress $ measureProgress x
    ListValidationLeftoverTerms _ Nothing -> mempty
    ListValidationLeftoverTerms _ (Just (_, trc)) -> measureProgress trc
    ListValidationMissingRequired _ _ -> mempty
    ListValidationUnappliedRule _ -> mempty
    ListValidationConsumeGroup rs g x -> Progress 0 (length rs) <> measureProgress g <> measureProgress x
    ListValidationBadGroup rs x -> Progress 0 (length rs) <> measureProgress x

instance IsValidationTrace MapValidationTrace where
  traceValidity = \case
    MapValidationDone -> SValid
    MapValidationLeftoverKVs _ _ -> SInvalid
    MapValidationUnappliedRules _ -> SInvalid
    MapValidationInvalidValue {} -> SInvalid
    MapValidationConsume _ _ _ x -> traceValidity x

  measureProgress = \case
    MapValidationDone -> Progress 1 0
    MapValidationLeftoverKVs _ Nothing -> mempty
    MapValidationLeftoverKVs _ (Just (_, trc)) -> measureProgress trc
    MapValidationUnappliedRules _ -> mempty
    MapValidationConsume _ kTrc vTrc x ->
      measureProgress kTrc <> measureProgress vTrc <> measureProgress x
    MapValidationInvalidValue _ kTrc vTrc ->
      measureProgress kTrc <> measureProgress vTrc

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
nestContainer = hang 2 . ("└ " <>)

newtype TraceOptions = TraceOptions
  { toFoldValid :: Bool
  }

defaultTraceOptions :: TraceOptions
defaultTraceOptions = TraceOptions {toFoldValid = False}

prettyListValidationResult :: TraceOptions -> ListValidationTrace v -> Doc AnsiStyle
prettyListValidationResult opts@TraceOptions {..} = \case
  ListValidationDone -> mempty
  ListValidationLeftoverTerms _ Nothing ->
    annotate (color Red) "leftover elements not matched by any rule"
  ListValidationLeftoverTerms _ (Just (rule, trc)) ->
    hang 2 $
      vsep
        [ annotate (color Red) "leftover elements, closest matching rule:"
        , annotate (color Green) $ pretty rule
        , nestContainer $ prettyValidationTrace opts trc
        ]
  ListValidationUnappliedRule r ->
    hang 2 $
      vsep
        [ annotate (color Red) "required rule not satisfied:"
        , pretty r
        ]
  ListValidationConsume _ t c
    | toFoldValid -> foldValid 1 c
    | otherwise -> vsep $ continue c [prettyValidationTrace opts t]
  ListValidationMissingRequired _ t ->
    vsep
      [ annotate (color Red) "failed to apply required rule:"
      , nestContainer $ prettyValidationTrace opts t
      ]
  ListValidationConsumeGroup refs x c -> vsep . continue c $ prettyGroup SValid refs x
  ListValidationBadGroup refs x -> vsep $ prettyGroup SInvalid refs x
  where
    -- This prevents unnecessary empty lines
    continue ListValidationDone l = l
    continue x l = l <> [prettyListValidationResult opts x]

    prettyGroup :: SValidity v -> [Name] -> ListValidationTrace v -> [Doc AnsiStyle]
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
          , nestContainer $ prettyListValidationResult opts x
          ]

    foldValid !n (ListValidationConsume _ _ c) = foldValid (n + 1) c
    foldValid !n t =
      vsep $ continue t [annotate italicized $ pretty (n :: Int) <> " valid elements"]

prettyMapValidationResult :: TraceOptions -> MapValidationTrace v -> Doc AnsiStyle
prettyMapValidationResult opts@TraceOptions {..} = \case
  MapValidationDone -> mempty
  MapValidationLeftoverKVs _ Nothing ->
    annotate (color Red) "leftover key-value pairs not matched by any rule"
  MapValidationLeftoverKVs _ (Just (rule, trc)) ->
    hang 2 $
      vsep
        [ annotate (color Red) "leftover key-value pairs, closest matching rule:"
        , annotate (color Green) $ pretty rule
        , nestContainer $ prettyMapValidationResult opts trc
        ]
  MapValidationUnappliedRules rs ->
    hang 2 $
      vsep
        [ annotate (color Red) "not all required rules have been applied:"
        , vsep (toList $ pretty <$> rs)
        ]
  MapValidationConsume _ k v c
    | toFoldValid -> foldValid 1 c
    | otherwise ->
        vsep $
          continue
            c
            [ "key:"
            , nestContainer $ prettyValidationTrace opts k
            , "value:"
            , nestContainer $ prettyValidationTrace opts v
            ]
  MapValidationInvalidValue _ k v ->
    vsep
      [ "key:"
      , nestContainer $ prettyValidationTrace opts k
      , "value:" <+> annotate (color Red) "(fail)"
      , nestContainer $ prettyValidationTrace opts v
      ]
  where
    foldValid !n (MapValidationConsume _ _ _ c) = foldValid (n + 1) c
    foldValid !n t =
      vsep . continue t $
        [ annotate italicized $ pretty (n :: Int) <> " valid key-value pairs"
        , "..."
        ]

    -- This prevents unnecessary empty lines
    continue MapValidationDone l = l
    continue x l = l <> [prettyMapValidationResult opts x]

prettyValidationTrace :: TraceOptions -> ValidationTrace v -> Doc AnsiStyle
prettyValidationTrace opts = \case
  UnapplicableRule x -> annotate (color Red) $ vsep ["failed to apply: ", indent 2 $ pretty x]
  TerminalRule x -> "app: " <> annotate (color Green) (pretty x)
  ControlTrace ci x ->
    vsep
      [ prettyValidationTrace opts x
      , nestContainer $ "ctrl:" <+> annotate (color Yellow) ("." <> pretty ci)
      ]
  ChoiceBranch i c ->
    vsep
      [ "choice (idx: " <> pretty i <> ")"
      , nestContainer $ prettyValidationTrace opts c
      ]
  ReferenceRule (Name n) x ->
    vsep
      [ "ref: " <> annotate (color Blue) (pretty n)
      , nestContainer $ prettyValidationTrace opts x
      ]
  CustomFailure e -> annotate (color Red) $ pretty e
  CustomSuccess -> "<custom validator>"
  UnsatisfiedControl op ctl ->
    annotate (color Red) $
      vsep
        [ "unsatisfied control:"
        , indent 2 $ "." <> pretty op <+> pretty ctl
        ]
  ListTrace l ->
    vsep
      [ "array"
      , nestContainer $ prettyListValidationResult opts l
      ]
  MapTrace m ->
    vsep
      [ "map"
      , nestContainer $ prettyMapValidationResult opts m
      ]
  TagTrace t x ->
    vsep
      [ "tag:" <+> annotate (color Green) ("#6." <> pretty t)
      , nestContainer $ prettyValidationTrace opts x
      ]
  InvalidTag expected actual ->
    "expected tag #6." <> pretty expected <> ", got #6." <> pretty actual

showValidationTrace :: ValidationTrace v -> String
showValidationTrace =
  T.unpack
    . Ansi.renderStrict
    . layoutPretty defaultLayoutOptions
    . prettyValidationTrace defaultTraceOptions
