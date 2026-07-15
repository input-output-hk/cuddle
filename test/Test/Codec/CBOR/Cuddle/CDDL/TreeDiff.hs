{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Codec.CBOR.Cuddle.CDDL.TreeDiff () where

import Codec.CBOR.Cuddle.CBOR.NInt (NInt, fromNInt)
import Codec.CBOR.Cuddle.CDDL (
  Assign,
  CDDL,
  ForAllExtensions,
  GenericArg,
  GenericParameter,
  GenericParameters,
  Group,
  GroupEntry,
  GroupEntryVariant,
  GrpChoice,
  MemberKey,
  Name,
  OccurrenceIndicator,
  RangeBound,
  Rule,
  TopLevel,
  TyOp,
  Type0,
  Type1,
  Type2,
  TypeOrGroup,
  Value,
  ValueVariant,
  XCddl,
 )
import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Codec.CBOR.Cuddle.Comments (Comment)
import Codec.CBOR.Cuddle.Pretty (PrettyStage, XRule, XTerm, XXTopLevel, XXType2)
import Data.TreeDiff (ToExpr (..))
import Numeric.Half (Half, fromHalf)

instance ToExpr Name

instance ForAllExtensions p ToExpr => ToExpr (GenericParameter p)

instance ForAllExtensions p ToExpr => ToExpr (GenericParameters p)

instance ForAllExtensions p ToExpr => ToExpr (GenericArg p)

instance ToExpr Comment

instance ToExpr NInt where
  toExpr = toExpr . fromNInt

instance ToExpr Half where
  toExpr = toExpr . fromHalf

instance ToExpr ValueVariant

instance ToExpr Value

instance ForAllExtensions p ToExpr => ToExpr (MemberKey p)

instance ForAllExtensions p ToExpr => ToExpr (GroupEntryVariant p)

instance ToExpr OccurrenceIndicator

instance ToExpr CtlOp

instance ToExpr RangeBound

instance ToExpr TyOp

instance ToExpr Assign

instance ForAllExtensions p ToExpr => ToExpr (GroupEntry p)

instance ForAllExtensions p ToExpr => ToExpr (GrpChoice p)

instance ForAllExtensions p ToExpr => ToExpr (Group p)

instance ForAllExtensions p ToExpr => ToExpr (Type2 p)

instance ForAllExtensions p ToExpr => ToExpr (Type1 p)

instance ForAllExtensions p ToExpr => ToExpr (Type0 p)

instance ForAllExtensions p ToExpr => ToExpr (TypeOrGroup p)

instance ForAllExtensions p ToExpr => ToExpr (Rule p)

instance ForAllExtensions p ToExpr => ToExpr (TopLevel p)

instance ForAllExtensions p ToExpr => ToExpr (CDDL p)

instance ToExpr (XCddl PrettyStage)

instance ToExpr (XTerm PrettyStage)

instance ToExpr (XRule PrettyStage)

instance ToExpr (XXTopLevel PrettyStage)

instance ToExpr (XXType2 PrettyStage)
