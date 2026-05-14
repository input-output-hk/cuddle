{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Codec.CBOR.Cuddle.TreeDiff () where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Codec.CBOR.Cuddle.Comments (Comment)
import Codec.CBOR.Cuddle.Pretty (PrettyPhase)
import Data.TreeDiff (ToExpr)

instance ToExpr Name

instance ForAllExtensions p ToExpr => ToExpr (GenericParameter p)

instance ForAllExtensions p ToExpr => ToExpr (GenericParameters p)

instance ForAllExtensions p ToExpr => ToExpr (GenericArg p)

instance ToExpr Comment

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

instance ToExpr (XCddl PrettyPhase)

instance ToExpr (XTerm PrettyPhase)

instance ToExpr (XRule PrettyPhase)

instance ToExpr (XXTopLevel PrettyPhase)

instance ToExpr (XXType2 PrettyPhase)
