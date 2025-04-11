{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.CBOR.Cuddle.Pretty where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Data.String (fromString)
import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

instance Pretty CDDL where
  pretty (CDDL (NE.toList -> xs)) = vsep $ fmap pretty xs

instance Pretty TopLevel where
  pretty (TopLevelComment cmt) = pretty cmt
  pretty (TopLevelRule pre x post) = pretty pre <> pretty x <> post' <> hardline
    where
      post' =
        case post of
          Nothing -> mempty
          Just cmt -> hardline <> pretty cmt

deriving newtype instance Pretty Name

data CommentRender
  = PreComment
  | PostComment

prettyWithComments :: Pretty a => CommentRender -> WithComments a -> Doc ann
prettyWithComments _ (WithComments a Nothing) = pretty a
prettyWithComments PostComment (WithComments a (Just cmt)) = pretty a <> softline <> pretty cmt
prettyWithComments PreComment (WithComments a (Just cmt)) = align $ pretty cmt <> pretty a

prettyCommentNoBreak :: Comment -> Doc ann
prettyCommentNoBreak = align . vsep . toList . fmap ((";" <>) . pretty) . unComment

instance Pretty Comment where
  pretty = (<> hardline) . prettyCommentNoBreak

type0Def :: Type0 -> Doc ann
type0Def t = nest 2 $ line' <> pretty t

instance Pretty Rule where
  pretty (Rule n mgen assign tog) =
    group $
      pretty n <> pretty mgen <+> case tog of
        TOGType t -> ppAssignT <+> type0Def t
        TOGGroup g -> ppAssignG <+> nest 2 (line' <> pretty g)
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
  pretty (Type1 t2 Nothing) = pretty t2
  pretty (Type1 t2 (Just (tyop, t2'))) =
    pretty t2
      <+> ( case tyop of
              RangeOp ClOpen -> "..."
              RangeOp Closed -> ".."
              CtrlOp n -> "." <> pretty n
          )
      <+> pretty t2'

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

renderedLen :: Doc ann -> Int
renderedLen = T.length . renderStrict . layoutCompact

softspace :: Doc ann
softspace = flatAlt space mempty

spaces :: Int -> Doc ann
spaces i = mconcat $ replicate i softspace

fillLeft :: Int -> Doc ann -> Doc ann
fillLeft len doc = spaces (len - renderedLen doc) <> doc

fillRight :: Int -> Doc ann -> Doc ann
fillRight len doc = doc <> spaces (len - renderedLen doc)

memberKeySep :: MemberKey -> Doc ann
memberKeySep MKType {} = " => "
memberKeySep _ = " : "

columnarGroupChoice :: GrpChoice -> Doc ann
columnarGroupChoice [] = mempty
columnarGroupChoice groupEntries@(x : xs) =
  groupIfNoComments (columnar x : fmap (\a -> "," <+> columnar a) xs)
  where
    occurrenceIndicatorLength (WithComments ge _) =
      renderedLen . pretty $ groupEntryOccurrenceIndicator ge

    longestOccurrenceIndicator = maximum $ occurrenceIndicatorLength <$> groupEntries

    memberKeyLength (WithComments (GEType _ (Just mk) _) _) = renderedLen $ pretty mk
    memberKeyLength _ = 0

    longestMemberKey = maximum $ memberKeyLength <$> groupEntries

    oiDoc oi =
      fillLeft longestOccurrenceIndicator (pretty oi)
        <> if longestOccurrenceIndicator > 0 then space else mempty

    memberKeyDoc mmk = fillRight longestMemberKey (pretty mmk) <> fillLeft longestKeySep (memberKeySep mmk)

    memberKeySepLen (WithComments (GEType _ mmk _) _) = renderedLen $ maybe mempty memberKeySep mmk
    memberKeySepLen _ = 0

    longestKeySep = maximum $ memberKeySepLen <$> groupEntries

    longestLineColumnar =
      maximum $ renderedLen . columnar' . stripComment <$> groupEntries

    columnar (WithComments ge Nothing) = columnar' ge
    columnar (WithComments ge (Just cmt)) = fillRight longestLineColumnar (columnar' ge) <+> prettyCommentNoBreak cmt

    columnar' (GEType oi mmk t0) = oiDoc oi <> maybe mempty memberKeyDoc mmk <> pretty t0
    columnar' (GEGroup oi g) = oiDoc oi <> prettyGroup AsGroup g
    columnar' (GERef oi n mga) = oiDoc oi <> pretty n <> pretty mga

    mapInit _ [] = []
    mapInit _ [a] = [a]
    mapInit f (a : as) = f a : mapInit f as

    groupIfNoComments
      | any (\(WithComments _ cmt) -> isJust cmt) groupEntries = mconcat . mapInit (<> hardline)
      | otherwise = vcat

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

prettyGroup :: GroupRender -> Group -> Doc ann
prettyGroup gr (Group (toList -> xs)) =
  cEncloseSep lEnc rEnc "//" $ fmap (group . columnarGroupChoice) xs
  where
    (lEnc, rEnc) = case gr of
      AsMap -> ("{", "}")
      AsArray -> ("[", "]")
      AsGroup -> ("(", ")")

instance Pretty GroupEntry where
  pretty ge = columnarGroupChoice [noComment ge]

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
