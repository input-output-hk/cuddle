{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.CBOR.Cuddle.Pretty where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Data.ByteString.Char8 qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text qualified as T
import Prettyprinter

instance Pretty CDDL where
  pretty (CDDL (NE.toList -> xs)) = vsep . punctuate hardline $ fmap pretty xs

deriving newtype instance Pretty Name

instance (Pretty a) => Pretty (WithComments a) where
  pretty (WithComments a Nothing) = pretty a
  pretty (WithComments a (Just cmt)) = pretty cmt <> hardline <> pretty a

instance Pretty Comment where
  pretty (Comment t) =
    let clines = T.splitOn "\n" t
     in vsep $ fmap (("; " <>) . pretty) clines

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
  pretty (Type0 (NE.toList -> l)) = align . encloseSep mempty mempty " / " $ fmap pretty l

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
  pretty (T2Group g) = align $ enclose "(" ")" $ pretty g
  pretty (T2Map g) = align $ enclose "{" "}" $ pretty g
  pretty (T2Array g) = brackets $ pretty g
  pretty (T2Unwrapped n mg) = "~" <+> pretty n <> pretty mg
  pretty (T2Enum g) = "&" <+> enclose "(" ")" (pretty g)
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

instance Pretty Group where
  pretty (Group (NE.toList -> xs)) =
    align . vsep . punctuate " // " $ fmap prettyGrpChoice xs
    where
      prettyGrpChoice = sep . punctuate "," . fmap pretty

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
      catMaybes [fmap pretty moi, Just $ enclose "(" ")" (pretty g)]

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
