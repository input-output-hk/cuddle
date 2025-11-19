{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.CBOR.Cuddle.Pretty where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Codec.CBOR.Cuddle.Comments (CollectComments (..), Comment (..), HasComment (..), unComment)
import Codec.CBOR.Cuddle.Pretty.Columnar (
  Cell (..),
  CellAlign (..),
  Columnar (..),
  Row (..),
  cellL,
  columnarListing,
  columnarSepBy,
  emptyCell,
  prettyColumnar,
  singletonRow,
 )
import Codec.CBOR.Cuddle.Pretty.Utils (renderedLen, softspace)
import Data.ByteString.Char8 qualified as BS
import Data.Default.Class (Default)
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty qualified as NE
import Data.String (IsString, fromString)
import Data.Text qualified as T
import Data.TreeDiff (ToExpr)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Optics.Core ((^.))
import Prettyprinter

type data PrettyStage

newtype instance XXTopLevel PrettyStage = PrettyXXTopLevel Comment
  deriving (Generic, CollectComments, ToExpr, Show, Eq)

newtype instance XXType2 PrettyStage = PrettyXXType2 Void
  deriving (Generic, CollectComments, ToExpr, Show, Eq)

newtype instance XTerm PrettyStage = PrettyXTerm {unPrettyXTerm :: Comment}
  deriving (Generic, CollectComments, Semigroup, Monoid, IsString, ToExpr, Show, Eq)

newtype instance XCddl PrettyStage = PrettyXCddl [Comment]
  deriving (Generic, CollectComments, ToExpr, Show, Eq)

newtype instance XRule PrettyStage = PrettyXRule {unPrettyXRule :: Comment}
  deriving (Generic, CollectComments, ToExpr, Show, Eq)
  deriving newtype (Default)

instance HasComment (XTerm PrettyStage) where
  commentL = #unPrettyXTerm

instance HasComment (XRule PrettyStage) where
  commentL = #unPrettyXRule

instance Pretty (CDDL PrettyStage) where
  pretty = vsep . fmap pretty . NE.toList . cddlTopLevel

instance Pretty (TopLevel PrettyStage) where
  pretty (XXTopLevel (PrettyXXTopLevel cmt)) = pretty cmt
  pretty (TopLevelRule x) = pretty x <> hardline

instance Pretty Name where
  pretty (Name name) = pretty name

data CommentRender
  = PreComment
  | PostComment

prettyCommentNoBreak :: Comment -> Doc ann
prettyCommentNoBreak = align . vsep . toList . fmap (pretty . ("; " <>)) . unComment

prettyCommentNoBreakWS :: Comment -> Doc ann
prettyCommentNoBreakWS cmt
  | cmt == mempty = mempty
  | otherwise = space <> prettyCommentNoBreak cmt

instance Pretty Comment where
  pretty (Comment "") = mempty
  pretty c = prettyCommentNoBreak c <> hardline

type0Def :: Type0 PrettyStage -> Doc ann
type0Def t = nest 2 $ line' <> pretty t

instance Pretty (Rule PrettyStage) where
  pretty (Rule n mgen assign tog cmt) =
    pretty (cmt ^. commentL)
      <> groupIfNoComments
        tog
        ( pretty n <> pretty mgen <+> case tog of
            TOGType t -> ppAssignT <> softline <> type0Def t
            TOGGroup g -> ppAssignG <> softline <> nest 2 (line' <> pretty g)
        )
    where
      ppAssignT = case assign of
        AssignEq -> "="
        AssignExt -> "/="
      ppAssignG = case assign of
        AssignEq -> "="
        AssignExt -> "//="

instance Pretty (GenericArg PrettyStage) where
  pretty (GenericArg (NE.toList -> l))
    | null (collectComments l) = group . cEncloseSep "<" ">" "," $ fmap pretty l
    | otherwise = columnarListing "<" ">" "," . Columnar $ singletonRow . pretty <$> l

instance Pretty (GenericParameter PrettyStage) where
  pretty (GenericParameter n (PrettyXTerm c)) = pretty n <> prettyCommentNoBreakWS c

instance Pretty (GenericParameters PrettyStage) where
  pretty (GenericParameters (NE.toList -> l))
    | null (collectComments l) = group . cEncloseSep "<" ">" "," $ pretty <$> l
    | otherwise = columnarListing "<" ">" "," . Columnar $ singletonRow . pretty <$> l

instance Pretty (Type0 PrettyStage) where
  pretty t0@(Type0 (NE.toList -> l)) =
    groupIfNoComments t0 $ columnarSepBy "/" . Columnar $ type1ToRow <$> l
    where
      type1ToRow (Type1 t2 tyOp (PrettyXTerm cmt)) =
        let
          valCell = case tyOp of
            Nothing -> cellL t2
            Just (to, t2') -> Cell (pretty t2 <+> pretty to <+> pretty t2') LeftAlign
         in
          Row [valCell, Cell (prettyCommentNoBreakWS cmt) LeftAlign]

instance Pretty CtlOp where
  pretty = pretty . T.toLower . T.pack . show

instance Pretty TyOp where
  pretty (RangeOp ClOpen) = "..."
  pretty (RangeOp Closed) = ".."
  pretty (CtrlOp n) = "." <> pretty n

instance Pretty (Type1 PrettyStage) where
  pretty (Type1 t2 Nothing (PrettyXTerm cmt)) = groupIfNoComments t2 (pretty t2) <> prettyCommentNoBreakWS cmt
  pretty (Type1 t2 (Just (tyop, t2')) (PrettyXTerm cmt)) =
    groupIfNoComments t2 (pretty t2)
      <+> pretty tyop
      <+> groupIfNoComments t2' (pretty t2')
      <> prettyCommentNoBreakWS cmt

instance Pretty (Type2 PrettyStage) where
  pretty (T2Value v) = pretty v
  pretty (T2Name n mg) = pretty n <> pretty mg
  pretty (T2Group g) = cEncloseSep "(" ")" mempty [pretty g]
  pretty (T2Map g) = prettyGroup AsMap g
  pretty (T2Array g) = prettyGroup AsArray g
  pretty (T2Unwrapped n mg) = "~" <+> pretty n <> pretty mg
  pretty (T2Enum g) = "&" <+> prettyGroup AsGroup g
  pretty (T2EnumRef g mg) = "&" <+> pretty g <> pretty mg
  pretty (T2Tag minor t) = "#6" <> min' <> enclose "(" ")" (group $ nest 2 (line' <> pretty t) <> line')
    where
      min' = case minor of
        Nothing -> mempty
        Just m -> "." <> pretty m
  pretty (T2DataItem major mminor) =
    "#" <> pretty major <> case mminor of
      Nothing -> mempty
      Just minor -> "." <> pretty minor
  pretty T2Any = "#"
  pretty (XXType2 (PrettyXXType2 v)) = absurd v

instance Pretty OccurrenceIndicator where
  pretty OIOptional = "?"
  pretty OIZeroOrMore = "*"
  pretty OIOneOrMore = "+"
  pretty (OIBounded ml mh) = pretty ml <> "*" <> pretty mh

-- | Control how to render a group
data GroupRender
  = AsMap
  | AsArray
  | AsGroup

memberKeySep :: MemberKey i -> Doc ann
memberKeySep MKType {} = " => "
memberKeySep _ = " : "

cEncloseSep :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
cEncloseSep lEnc rEnc _ [] = lEnc <> rEnc
cEncloseSep lEnc rEnc _ [x] =
  group . align . vcat $
    [ lEnc <> softspace <> x
    , rEnc
    ]
cEncloseSep lEnc rEnc s (h : tl) =
  group . align $ vsep ((lEnc <> lSpaces <> group h) : fmap ((s <> space) <>) tl) <> line' <> rEnc
  where
    lSpaces = mconcat $ replicate (renderedLen s) softspace

groupIfNoComments :: CollectComments a => a -> Doc ann -> Doc ann
groupIfNoComments x
  | not (any (mempty /=) $ collectComments x) = group
  | otherwise = id

columnarGroupChoice :: GrpChoice PrettyStage -> Columnar ann
columnarGroupChoice (GrpChoice ges _cmt) = Columnar grpEntryRows
  where
    groupEntryRow (GroupEntry oi gev (PrettyXTerm cmt)) =
      Row $
        [maybe emptyCell (\x -> Cell (pretty x <> space) LeftAlign) oi]
          <> groupEntryVariantCells gev
          <> [Cell (prettyCommentNoBreakWS cmt) LeftAlign]
    groupEntryVariantCells (GERef n ga) = [Cell (pretty n <> pretty ga) LeftAlign]
    groupEntryVariantCells (GEType (Just mk) t0) = [cellL mk, Cell (memberKeySep mk <> groupIfNoComments t0 (align $ pretty t0)) LeftAlign]
    groupEntryVariantCells (GEType Nothing t0) = [cellL t0]
    groupEntryVariantCells (GEGroup g) = [Cell (prettyGroup AsGroup g) LeftAlign, emptyCell]
    grpEntryRows = groupEntryRow <$> ges

prettyGroup :: GroupRender -> Group PrettyStage -> Doc ann
prettyGroup gr g@(Group (toList -> xs)) =
  groupIfNoComments g . columnarListing (lEnc <> softspace) rEnc "// " . Columnar $
    (\x -> singletonRow . groupIfNoComments x . columnarSepBy "," $ columnarGroupChoice x) <$> xs
  where
    (lEnc, rEnc) = case gr of
      AsMap -> ("{", "}")
      AsArray -> ("[", "]")
      AsGroup -> ("(", ")")

instance Pretty (GroupEntry PrettyStage) where
  pretty ge = prettyColumnar . columnarGroupChoice $ GrpChoice [ge] mempty

instance Pretty (MemberKey PrettyStage) where
  pretty (MKType t1) = pretty t1
  pretty (MKBareword n) = pretty n
  pretty (MKValue v) = pretty v

instance Pretty Value where
  pretty (Value v cmt) = pretty v <> prettyCommentNoBreakWS cmt

instance Pretty ValueVariant where
  pretty (VUInt i) = pretty i
  pretty (VNInt i) = "-" <> pretty i
  pretty (VBignum i) = pretty i
  pretty (VFloat16 i) = pretty i
  pretty (VFloat32 i) = pretty i
  pretty (VFloat64 i) = pretty i
  pretty (VText t) = enclose "\"" "\"" $ pretty t
  pretty (VBytes b) = fromString $ "h" <> "'" <> BS.unpack b <> "'"
  pretty (VBool True) = "true"
  pretty (VBool False) = "false"
