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
  ValidatorStage,
  ValidatorStageSimple,
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

import Codec.CBOR.Cuddle.CDDL (Name (..), OccurrenceIndicator (..), RangeBound (..), XTerm)
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
import Data.Text qualified as T
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import Data.Word (Word64)
import Prettyprinter (
  Doc,
  Pretty (..),
  annotate,
  defaultLayoutOptions,
  encloseSep,
  group,
  hang,
  indent,
  layoutPretty,
  list,
  punctuate,
  tupled,
  vsep,
  (<+>),
 )
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, italicized)
import Prettyprinter.Render.Terminal qualified as Ansi

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

instance Pretty (CTree ValidatorStageSimple) where
  pretty = \case
    Literal v -> pretty v
    Postlude v -> pretty v
    Range lo hi ClOpen -> pretty lo <+> "..." <+> pretty hi
    Range lo hi Closed -> pretty lo <+> ".." <+> pretty hi
    KV k v _ -> pretty k <+> "==>" <+> pretty v
    CTreeE (VRuleRefSimple (Name n)) -> pretty n
    Occur v oi ->
      case oi of
        OIOptional -> "?" <+> pretty v
        OIZeroOrMore -> "*" <+> pretty v
        OIOneOrMore -> "+" <+> pretty v
        OIBounded lo hi -> foldMap pretty lo <> "*" <> foldMap pretty hi <+> pretty v
    Map m -> encloseSep "{" "}" "," $ pretty <$> m
    Array e -> list $ pretty <$> e
    Choice c -> group . vsep . punctuate "//" $ pretty <$> toList c
    Group g -> tupled $ pretty <$> g
    Control op tgt ctl -> pretty tgt <+> pretty op <+> pretty ctl
    Enum e -> "&" <> pretty e
    Unwrap x -> "~" <> pretty x
    Tag t x -> "#6." <> pretty t <> "(" <> pretty x <> ")"

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

data ControlInfo = ControlInfo
  { ciOp :: CtlOp
  , ciRule :: CTree ValidatorStageSimple
  }
  deriving (Show)

instance Pretty ControlInfo where
  pretty ControlInfo {..} = pretty ciOp <+> pretty ciRule

data ValidationTrace (v :: Validity) where
  UnapplicableRule :: CTree ValidatorStageSimple -> ValidationTrace IsInvalid
  TerminalRule :: Maybe ControlInfo -> CTree ValidatorStageSimple -> ValidationTrace IsValid
  ReferenceRule :: Name -> ValidationTrace v -> ValidationTrace v
  CustomFailure :: Text -> ValidationTrace IsInvalid
  CustomSuccess :: ValidationTrace IsValid
  UnsatisfiedControl :: CtlOp -> CTree ValidatorStageSimple -> ValidationTrace IsInvalid
  ChoiceBranch :: Int -> ValidationTrace IsValid -> ValidationTrace IsValid
  ListTrace :: ListValidationTrace v -> ValidationTrace v
  MapTrace :: MapValidationTrace v -> ValidationTrace v
  TagTrace :: Word64 -> ValidationTrace v -> ValidationTrace v
  InvalidTag :: Word64 -> ValidationTrace IsInvalid

deriving instance Show (ValidationTrace v)

data ListValidationTrace (v :: Validity) where
  ListValidationDone :: ListValidationTrace IsValid
  ListValidationLeftoverTerms ::
    NonEmpty Term ->
    Maybe (CTree ValidatorStageSimple, ValidationTrace IsInvalid) ->
    ListValidationTrace IsInvalid
<<<<<<< HEAD
  ListValidationUnappliedRule ::
    CTree ValidatorStageSimple ->
=======
  ListValidationUnappliedRules ::
    NonEmpty (CTree ValidatorStageSimple) ->
>>>>>>> d212208 (Improve list leftover elements validation trace)
    ListValidationTrace IsInvalid
  ListValidationConsume ::
    CTree ValidatorStageSimple ->
    ValidationTrace IsValid ->
    ListValidationTrace v ->
    ListValidationTrace v
  ListValidationMissingRequired ::
    CTree ValidatorStageSimple ->
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
    Maybe (CTree ValidatorStageSimple, MapValidationTrace IsInvalid) ->
    MapValidationTrace IsInvalid
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
    InvalidTag {} -> SInvalid
    ReferenceRule _ x -> traceValidity x
    ListTrace x -> traceValidity x
    MapTrace x -> traceValidity x
    TagTrace _ x -> traceValidity x

  measureProgress = \case
    TerminalRule {} -> 1
    CustomSuccess -> 1
    ChoiceBranch {} -> 1
    UnapplicableRule {} -> 0
    CustomFailure {} -> 0
    UnsatisfiedControl {} -> 0
    InvalidTag {} -> 0
    ReferenceRule _ x -> succ $ measureProgress x
    ListTrace x -> measureProgress x
    MapTrace x -> measureProgress x
    TagTrace _ x -> succ $ measureProgress x

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
    ListValidationDone -> 10
    ListValidationConsume _ _ x -> succ $ measureProgress x
    ListValidationLeftoverTerms _ Nothing -> 0
    ListValidationLeftoverTerms _ (Just (_, trc)) -> measureProgress trc
    ListValidationMissingRequired _ _ -> 0
    ListValidationUnappliedRule _ -> 0
    ListValidationConsumeGroup _ g x -> measureProgress g + measureProgress x
    ListValidationBadGroup _ x -> measureProgress x

instance IsValidationTrace MapValidationTrace where
  traceValidity = \case
    MapValidationDone -> SValid
    MapValidationLeftoverKVs _ _ -> SInvalid
    MapValidationUnappliedRules _ -> SInvalid
    MapValidationInvalidValue {} -> SInvalid
    MapValidationConsume _ _ _ x -> traceValidity x

  measureProgress = \case
    MapValidationDone -> 10
    MapValidationLeftoverKVs _ Nothing -> 0
    MapValidationLeftoverKVs _ (Just (_, trc)) -> measureProgress trc
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
  TerminalRule mCi x -> case mCi of
    Just ci ->
      vsep
        [ appMsg
        , nestContainer $ "ctrl:" <+> annotate (color Yellow) ("." <> pretty ci)
        ]
    Nothing -> appMsg
    where
      appMsg = "app: " <> annotate (color Green) (pretty x)
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
  InvalidTag t ->
    "expected tag #6." <> pretty t

showValidationTrace :: ValidationTrace v -> String
showValidationTrace =
  T.unpack
    . Ansi.renderStrict
    . layoutPretty defaultLayoutOptions
    . prettyValidationTrace defaultTraceOptions
