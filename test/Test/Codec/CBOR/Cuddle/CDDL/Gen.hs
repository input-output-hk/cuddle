-- | Hedgehog generators for CDDL
module Test.Codec.CBOR.Cuddle.CDDL.Gen
  ( genCDDL,
    genRule,
    genName,
    genValue,
  )
where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Hedgehog (MonadGen)
import Hedgehog.Gen (sized)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genCDDL :: (MonadGen m) => m CDDL
genCDDL = CDDL . fmap noComment <$> Gen.nonEmpty (Range.linear 1 15) genRule

-- TODO Expand the range of names generated
genName :: (MonadGen m) => m Name
genName = Name <$> Gen.text (Range.linear 1 10) Gen.alpha

genAssign :: (MonadGen m) => m Assign
genAssign = Gen.element [AssignEq, AssignExt]

genGenericParams :: (MonadGen m) => m GenericParam
genGenericParams = GenericParam <$> Gen.nonEmpty (Range.linear 1 5) genName

genGenericArg :: (MonadGen m) => m GenericArg
genGenericArg = GenericArg <$> Gen.nonEmpty (Range.linear 1 5) genType1

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
genType0 = Type0 <$> Gen.nonEmpty (Range.linear 1 4) genType1

genType1 :: (MonadGen m) => m Type1
genType1 = Type1 <$> genType2 <*> Gen.maybe ((,) <$> genTyOp <*> genType2)

genType2 :: (MonadGen m) => m Type2
genType2 =
  Gen.recursive
    Gen.choice
    [ T2Value <$> genValue,
      T2Map <$> genGroup,
      T2Array <$> genGroup,
      T2Enum <$> genGroup,
      T2DataItem
        <$> Gen.int (Range.linear 0 10)
        <*> Gen.maybe (Gen.int (Range.linear 0 10)),
      T2Name <$> genName <*> maybeRec genGenericArg,
      T2Unwrapped <$> genName <*> maybeRec genGenericArg,
      T2EnumRef <$> genName <*> maybeRec genGenericArg,
      pure T2Any
    ]
    [ T2Group <$> genType0,
      T2Tag <$> Gen.maybe (Gen.int (Range.linear 0 10)) <*> genType0
    ]

genOccurrenceIndicator :: (MonadGen m) => m OccurrenceIndicator
genOccurrenceIndicator =
  Gen.choice
    [ pure OIOptional,
      pure OIZeroOrMore,
      pure OIOneOrMore,
      OIBounded
        <$> Gen.maybe (Gen.int (Range.linear 0 5))
        <*> Gen.maybe (Gen.int (Range.linear 5 10))
    ]

genGroup :: (MonadGen m) => m Group
genGroup = Group <$> Gen.nonEmpty (Range.linear 1 5) genGrpChoice

genGrpChoice :: (MonadGen m) => m GrpChoice
genGrpChoice = Gen.list (Range.linear 1 10) genGroupEntry

genGroupEntry :: (MonadGen m) => m GroupEntry
genGroupEntry =
  Gen.recursive
    Gen.choice
    [ GERef
        <$> Gen.maybe genOccurrenceIndicator
        <*> genName
        <*> maybeRec genGenericArg
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
    [ VNum <$> Gen.int (Range.linear 0 255),
      VText <$> Gen.text (Range.linear 0 1000) Gen.alphaNum
      -- VBytes <$> Gen.bytes (Range.linear 0 1100)
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

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

-- | Variant on maybe which shrinks the size whenever it selects the 'Just'
-- option. When the size gets to one or less, the Just constructor is no longer
-- called, ensuring termination.
maybeRec :: (MonadGen m) => m a -> m (Maybe a)
maybeRec gen =
  sized $ \n ->
    if n <= 1
      then pure Nothing
      else
        Gen.frequency
          [ (2, pure Nothing),
            (1 + fromIntegral n, Just <$> Gen.small gen)
          ]
