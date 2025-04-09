{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.CBOR.Cuddle.Pretty where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Codec.CBOR.Cuddle.Comments (Comment (..))
import Codec.CBOR.Cuddle.Pretty.Columnar (Columnar (..), prettyColumnar, Row (..), cellR, cellL, Cell (..), CellAlign (..), emptyCell)
import Codec.CBOR.Cuddle.Pretty.Utils (renderedLen, softspace)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty qualified as NE
import Data.String (fromString)
import Data.Text qualified as T
import Prettyprinter

instance Pretty CDDL where
  pretty = vsep . fmap pretty . NE.toList . cddlTopLevel

instance Pretty TopLevel where
  pretty (TopLevelComment cmt) = pretty cmt
  pretty (TopLevelRule x) = pretty x <> hardline

instance Pretty Name where
  pretty (Name name cmt) = pretty name <> pretty cmt

data CommentRender
  = PreComment
  | PostComment

prettyCommentNoBreak :: Comment -> Doc ann
prettyCommentNoBreak = align . vsep . toList . fmap ((";" <>) . pretty) . unComment

prettyCommentNoBreakWS :: Comment -> Doc ann
prettyCommentNoBreakWS cmt
  | cmt == mempty = mempty
  | otherwise = space <> prettyCommentNoBreak cmt

instance Pretty Comment where
  pretty (Comment []) = mempty
  pretty c@(Comment _) = (<> hardline) $ prettyCommentNoBreak c

type0Def :: Type0 -> Doc ann
type0Def t = nest 2 $ line' <> pretty t

instance Pretty Rule where
  pretty (Rule n mgen assign tog cmt) =
    pretty cmt
      <> group
        ( pretty n <> pretty mgen <+> case tog of
            TOGType t -> ppAssignT <+> type0Def t
            TOGGroup g -> ppAssignG <+> nest 2 (line' <> pretty g)
        )
    where
      ppAssignT = case assign of
        AssignEq -> "="
        AssignExt -> "/="
      ppAssignG = case assign of
        AssignEq -> "="
        AssignExt -> "//="

instance Pretty GenericArg where
  pretty (GenericArg (NE.toList -> l)) = cEncloseSep "<" ">" "," $ fmap pretty l

instance Pretty GenericParam where
  pretty (GenericParam (NE.toList -> l)) = cEncloseSep "<" ">" "," $ fmap pretty l

instance Pretty Type0 where
  pretty (Type0 (NE.toList -> l)) = align . sep . punctuate (space <> "/") $ fmap pretty l

instance Pretty CtlOp where
  pretty = pretty . T.toLower . T.pack . show

instance Pretty Type1 where
  pretty (Type1 t2 Nothing cmt) = pretty t2 <> prettyCommentNoBreakWS cmt
  pretty (Type1 t2 (Just (tyop, t2')) cmt) =
    pretty t2
      <+> ( case tyop of
              RangeOp ClOpen -> "..."
              RangeOp Closed -> ".."
              CtrlOp n -> "." <> pretty n
          )
      <+> pretty t2'
      <> prettyCommentNoBreakWS cmt

instance Pretty Type2 where
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

memberKeySep :: MemberKey -> Doc ann
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

columnarGroupChoice :: GrpChoice -> Columnar ann
columnarGroupChoice (GrpChoice ges _cmt) = Columnar grpEntryRows
  where
    groupEntryRow (GroupEntry oi _ gev) = Row $ [cellR oi] <> groupEntryVariantCells gev
    groupEntryVariantCells (GERef n ga) = [Cell (pretty n <> pretty ga) LeftAlign]
    groupEntryVariantCells (GEType (Just mk) t0) = [Cell (pretty mk <+> ":") LeftAlign, cellL t0]
    groupEntryVariantCells (GEType Nothing t0) = [emptyCell, cellL t0]
    groupEntryVariantCells (GEGroup g) = [Cell (prettyGroup AsGroup g) LeftAlign, emptyCell]
    grpEntryRows = groupEntryRow <$> ges

prettyGroup :: GroupRender -> Group -> Doc ann
prettyGroup gr (Group (toList -> xs)) =
  cEncloseSep lEnc rEnc "//" $ 
    group . prettyColumnar . columnarGroupChoice <$> xs
  where
    (lEnc, rEnc) = case gr of
      AsMap -> ("{", "}")
      AsArray -> ("[", "]")
      AsGroup -> ("(", ")")

instance Pretty GroupEntry where
  pretty ge = prettyColumnar . columnarGroupChoice $ GrpChoice [ge] mempty

instance Pretty MemberKey where
  pretty (MKType t1) = pretty t1
  pretty (MKBareword n) = pretty n
  pretty (MKValue v) = pretty v

instance Pretty Value where
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
