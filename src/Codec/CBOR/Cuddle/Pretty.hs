{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.CBOR.Cuddle.Pretty where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Prettyprinter

instance Pretty CDDL where
  pretty (CDDL (NE.toList -> xs)) = vsep . punctuate hardline $ fmap pretty xs

deriving newtype instance Pretty Name

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
  pretty (GenericArg (NE.toList -> l)) = encloseSep "<" ">" ", " $ fmap pretty l

instance Pretty GenericParam where
  pretty (GenericParam (NE.toList -> l)) = encloseSep "<" ">" ", " $ fmap pretty l

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
  pretty (T2Group g) = enclose "(" ")" $ pretty g
  pretty (T2Map g) = enclose "{" "}" $ pretty g
  pretty (T2Array g) = enclose "[" "]" $ pretty g
  pretty (T2Unwrapped n mg) = "~" <+> pretty n <> pretty mg
  pretty (T2Enum g) = "&" <+> enclose "(" ")" (pretty g)
  pretty (T2EnumRef g mg) = "&" <+> pretty g <+> pretty mg
  pretty (T2Tag minor t) = "#6." <> pretty minor <+> enclose "(" ")" (pretty t)
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
    align . encloseSep mempty mempty " // " $ fmap prettyGrpChoice xs
    where
      prettyGrpChoice = align . vsep . punctuate "," . fmap pretty

instance Pretty GroupEntry where
  pretty (GEType moi mmk t) = pretty moi <+> pretty mmk <+> pretty t
  pretty (GERef moi n mga) = pretty moi <+> pretty n <+> pretty mga
  pretty (GEGroup moi g) = pretty moi <+> enclose "(" ")" (pretty g)

instance Pretty MemberKey where
  pretty (MKType t1) = pretty t1 <+> "=>"
  pretty (MKBareword n) = pretty n <+> ":"
  pretty (MKValue v) = pretty v <+> ":"

instance Pretty Value where
  pretty (VNum i) = pretty i
  pretty (VText t) = enclose "\"" "\"" $ pretty t
  pretty (VBytes _b) = "some bytes"
