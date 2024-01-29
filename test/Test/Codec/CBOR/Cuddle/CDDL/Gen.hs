-- | Hedgehog generators for CDDL
module Test.Codec.CBOR.Cuddle.CDDL.Gen (genCDDL, genRule, genName) where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genCDDL :: (MonadGen m) => m CDDL
genCDDL = CDDL <$> Gen.nonEmpty (Range.constant 1 15) genRule

-- TODO Expand the range of names generated
genName :: (MonadGen m) => m Name
genName = Name <$> Gen.text (Range.constant 1 10) Gen.alpha

genAssign :: (MonadGen m) => m Assign
genAssign = Gen.element [AssignEq, AssignExt]

genGenericParams :: (MonadGen m) => m GenericParam
genGenericParams = GenericParam <$> Gen.nonEmpty (Range.constant 1 5) genName

genGenericArg :: (MonadGen m) => m GenericArg
genGenericArg = GenericArg <$> Gen.nonEmpty (Range.constant 1 5) genType1

genRule :: (MonadGen m) => m Rule
genRule =
  Rule
    <$> genName
    <*> Gen.maybe genGenericParams
    <*> genAssign
    <*> genTypeOrGroup

genRangeBound :: (MonadGen m) => m RangeBound
genRangeBound = Gen.element [ClOpen, Closed]

genTyOp :: (MonadGen m) => m TyOp
genTyOp =
  Gen.choice
    [ RangeOp <$> genRangeBound,
      CtrlOp <$> genCtlOp
    ]

genTypeOrGroup :: (MonadGen m) => m TypeOrGroup
genTypeOrGroup =
  Gen.choice
    [ TOGGroup <$> genGroupEntry,
      TOGType <$> genType0
    ]

genType0 :: (MonadGen m) => m Type0
genType0 = Type0 <$> Gen.nonEmpty (Range.constant 1 4) genType1

genType1 :: (MonadGen m) => m Type1
genType1 = Type1 <$> genType2 <*> Gen.maybe ((,) <$> genTyOp <*> genType2)

genType2 :: (MonadGen m) => m Type2
genType2 =
  Gen.recursive
    Gen.choice
    [ T2Value <$> genValue,
      T2Name <$> genName <*> Gen.maybe genGenericArg,
      T2Map <$> genGroup,
      T2Array <$> genGroup,
      T2Unwrapped <$> genName <*> Gen.maybe genGenericArg,
      T2Enum <$> genGroup,
      T2EnumRef <$> genName <*> Gen.maybe genGenericArg,
      T2DataItem
        <$> Gen.int (Range.constant 0 10)
        <*> Gen.maybe (Gen.int (Range.constant 0 10)),
      pure T2Any
    ]
    [ T2Group <$> genType0,
      T2Tag <$> Gen.maybe (Gen.int (Range.constant 0 10)) <*> genType0
    ]

genOccurrenceIndicator :: (MonadGen m) => m OccurrenceIndicator
genOccurrenceIndicator =
  Gen.choice
    [ pure OIOptional,
      pure OIZeroOrMore,
      pure OIOneOrMore,
      OIBounded
        <$> Gen.maybe (Gen.int (Range.constant 0 5))
        <*> Gen.maybe (Gen.int (Range.constant 5 10))
    ]

genGroup :: (MonadGen m) => m Group
genGroup = Group <$> Gen.nonEmpty (Range.constant 1 10) genGrpChoice

genGrpChoice :: (MonadGen m) => m GrpChoice
genGrpChoice = Gen.list (Range.constant 1 10) genGroupEntry

genGroupEntry :: (MonadGen m) => m GroupEntry
genGroupEntry =
  Gen.recursive
    Gen.choice
    [ GERef
        <$> Gen.maybe genOccurrenceIndicator
        <*> genName
        <*> Gen.maybe genGenericArg
    ]
    [ GEType
        <$> Gen.maybe genOccurrenceIndicator
        <*> Gen.maybe genMemberKey
        <*> genType0,
      GEGroup <$> Gen.maybe genOccurrenceIndicator <*> genGroup
    ]

genMemberKey :: (MonadGen m) => m MemberKey
genMemberKey =
  Gen.recursive
    Gen.choice
    [ MKBareword <$> genName,
      MKValue <$> genValue
    ]
    [ MKType <$> genType1
    ]

genValue :: (MonadGen m) => m Value
genValue =
  Gen.choice
    [ VNum <$> Gen.int (Range.constant 0 255),
      VText <$> Gen.text (Range.constant 0 1000) Gen.unicode,
      VBytes <$> Gen.bytes (Range.constant 0 1100)
    ]

genCtlOp :: (MonadGen m) => m CtlOp
genCtlOp =
  Gen.element
    [ Size,
      Bits,
      Regexp,
      Cbor,
      Cborseq,
      Within,
      And,
      Lt,
      Le,
      Gt,
      Ge,
      Eq,
      Ne,
      Default
    ]
