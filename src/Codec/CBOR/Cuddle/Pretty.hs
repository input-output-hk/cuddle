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
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text qualified as T
import Prettyprinter

instance Pretty CDDL where
  pretty (CDDL (NE.toList -> xs)) =
    vsep . punctuate hardline $
      fmap (prettyWithComments PreComment) xs

deriving newtype instance Pretty Name

data CommentRender
  = PreComment
  | PostComment

prettyWithComments :: Pretty a => CommentRender -> WithComments a -> Doc ann
prettyWithComments _ (WithComments a Nothing) = pretty a
prettyWithComments PostComment (WithComments a (Just cmt)) = pretty a <> space <> pretty cmt
prettyWithComments PreComment (WithComments a (Just cmt)) = pretty cmt <> hardline <> pretty a

instance Pretty Comment where
  pretty = align . vsep . toList . fmap (("; " <>) . pretty) . unComment

instance Pretty Rule where
  pretty (Rule n mgen assign tog) =
    pretty n <> pretty mgen <+> case tog of
      TOGType t -> ppAssignT <+> pretty t
      TOGGroup g -> ppAssignG <+> pretty g
    where
      ppAssignT = case assign of
        AssignEq -> "="
        AssignExt -> "/="
      ppAssignG = case assign of
        AssignEq -> "="
        AssignExt -> "//="

instance Pretty GenericArg where
  pretty (GenericArg (NE.toList -> l)) = align . encloseSep "<" ">" ", " $ fmap pretty l

instance Pretty GenericParam where
  pretty (GenericParam (NE.toList -> l)) = align . encloseSep "<" ">" ", " $ fmap pretty l

instance Pretty Type0 where
  pretty (Type0 (NE.toList -> l)) = align . nest (-2) . cEncloseSep mempty mempty "/ " $ fmap (prettyWithComments PostComment) l

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
  pretty (T2Group g) = enclose "(" ")" . align $ pretty g
  pretty (T2Map g) = prettyGroup AsMap g
  pretty (T2Array g) = prettyGroup AsArray g
  pretty (T2Unwrapped n mg) = "~" <+> pretty n <> pretty mg
  pretty (T2Enum g) = "&" <+> prettyGroup AsGroup g
  pretty (T2EnumRef g mg) = "&" <+> pretty g <> pretty mg
  pretty (T2Tag minor t) = "#6" <> min' <> enclose "(" ")" (pretty t)
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

cEncloseSep :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
cEncloseSep l r _ [] = l <> r
cEncloseSep l r _ [x] = l <> x <> r
cEncloseSep l r s (x : xs) =
  group $
    vsep ((l <> x) : map (s <>) xs <> [r])

cEncloseSepAlt :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
cEncloseSepAlt l = cEncloseSep lAlt
  where
    lAlt = flatAlt (l <> space) l
    --rAlt = flatAlt (space <> r) r

prettyGroup :: GroupRender -> Group -> Doc ann
prettyGroup gr (Group (NE.toList -> xs)) =
  nest 1 $ enclose' "// " (fmap prettyGrpChoice xs)
  where
    prettyGrpChoice = nest 1 . cEncloseSep mempty mempty ", " . fmap (prettyWithComments PostComment)
    enclose' = case gr of
      AsMap -> cEncloseSepAlt "{" "}"
      AsArray -> cEncloseSepAlt "[" "]"
      AsGroup -> cEncloseSepAlt "(" ")"

instance Pretty GroupEntry where
  pretty (GEType moi mmk t) =
    hsep $
      catMaybes
        [fmap pretty moi, fmap pretty mmk, Just $ pretty t]
  pretty (GERef moi n mga) =
    hsep (catMaybes [fmap pretty moi, Just $ pretty n])
      <> pretty mga
  pretty (GEGroup moi g) =
    hsep $
      catMaybes [fmap pretty moi, Just $ prettyGroup AsGroup g]

instance Pretty MemberKey where
  pretty (MKType t1) = pretty t1 <+> "=>"
  pretty (MKBareword n) = pretty n <+> ":"
  pretty (MKValue v) = pretty v <+> ":"

instance Pretty Value where
  pretty (VUInt i) = pretty i
  pretty (VNInt i) = "-" <> pretty i
  pretty (VBignum i) = pretty i
  pretty (VFloat16 i) = pretty i
  pretty (VFloat32 i) = pretty i
  pretty (VFloat64 i) = pretty i
  pretty (VText t) = enclose "\"" "\"" $ pretty t
  pretty (VBytes b) = fromString $ "h" <> "'" <> BS.unpack b <> "'"
