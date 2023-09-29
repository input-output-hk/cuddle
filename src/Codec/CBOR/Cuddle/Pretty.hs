{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.CBOR.Cuddle.Pretty where

import Codec.CBOR.Cuddle.CDDL
import Data.List.NonEmpty qualified as NE
import Prettyprinter

deriving newtype instance Pretty Name

instance Pretty Rule where
  pretty (Rule n assign tog) =
    pretty n <+> case tog of
      TOGType t -> ppAssignT <+> pretty t
      TOGGroup g -> ppAssignG <+> pretty g
    where
      ppAssignT = case assign of
        AssignEq -> "="
        AssignExt -> "/="
      ppAssignG = case assign of
        AssignEq -> "="
        AssignExt -> "//="

instance Pretty Type0 where
  pretty (Type0 (NE.toList -> l)) = encloseSep mempty mempty " / " $ fmap pretty l

instance Pretty Type1 where
  pretty (Type1 t2 Nothing) = pretty t2
  pretty (Type1 t2 (Just (tyop, t2'))) =
    pretty t2
      <+> ( case tyop of
              RangeOp ClOpen -> "..."
              RangeOp Closed -> ".."
              CtrlOp n -> "." <+> pretty n
          )
      <+> pretty t2'

instance Pretty Type2 where
  pretty (T2Value v) = pretty v
  pretty (T2Name n) = pretty n
  pretty (T2Group g) = enclose "(" ")" $ pretty g
  pretty (T2Map g) = enclose "{" "}" $ pretty g
  pretty (T2Array g) = enclose "[" "]" $ pretty g
  pretty (T2Unwrapped n) = "~" <+> pretty n
  pretty (T2Enum g) = "&" <+> pretty g
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
    encloseSep mempty mempty " // " $ fmap pretty xs

instance Pretty GroupEntry where
  pretty (GroupEntry moi mmk t) = pretty moi <+> pretty mmk <+> pretty t

instance Pretty MemberKey where
  pretty (MKType t1) = pretty t1 <+> "=>"
  pretty (MKBareword n) = pretty n <+> ":"
  pretty (MKValue v) = pretty v <+> ":"

instance Pretty Value where
  pretty (VNum i) = pretty i
  pretty (VText t) = enclose "\"" "\"" $ pretty t
  pretty (VBytes _b) = "some bytes"