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
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Test.QuickCheck
import Test.QuickCheck qualified as Gen
import Prelude hiding (maybe)

genCDDL :: Gen CDDL
genCDDL = CDDL . fmap noComment <$> nonEmpty genRule

genName :: Gen Name
genName =
  let endChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['@', '_', '$']
      midChars = ['1' .. '9'] <> ['-', '.']
   in do
        fstChar <- elements endChars
        midChar <- listOf . elements $ endChars <> midChars
        lastChar <- elements $ endChars <> ['1' .. '9']
        pure $ Name . T.pack $ fstChar : midChar <> [lastChar]

genAssign :: Gen Assign
genAssign = Gen.elements [AssignEq, AssignExt]

genGenericParams :: Gen GenericParam
genGenericParams = GenericParam <$> nonEmpty genName

genGenericArg :: Gen GenericArg
genGenericArg = GenericArg <$> nonEmpty genType1

genRule :: Gen Rule
genRule =
  Rule
    <$> genName
    <*> maybe genGenericParams
    <*> genAssign
    <*> genTypeOrGroup

genRangeBound :: Gen RangeBound
genRangeBound = Gen.elements [ClOpen, Closed]

genTyOp :: Gen TyOp
genTyOp =
  Gen.oneof
    [ RangeOp <$> genRangeBound,
      CtrlOp <$> genCtlOp
    ]

genTypeOrGroup :: Gen TypeOrGroup
genTypeOrGroup =
  Gen.oneof
    [ TOGGroup <$> genGroupEntry,
      TOGType <$> genType0
    ]

genType0 :: Gen Type0
genType0 = Type0 <$> nonEmpty genType1

genType1 :: Gen Type1
genType1 = Type1 <$> genType2 <*> maybe ((,) <$> genTyOp <*> genType2)

genType2 :: Gen Type2
genType2 =
  recursive
    Gen.oneof
    [ T2Value <$> genValue,
      T2Map <$> genGroup,
      T2Array <$> genGroup,
      T2Enum <$> genGroup,
      T2DataItem
        <$> arbitrary
        <*> maybe arbitrary,
      T2Name <$> genName <*> maybeRec genGenericArg,
      T2Unwrapped <$> genName <*> maybeRec genGenericArg,
      T2EnumRef <$> genName <*> maybeRec genGenericArg,
      pure T2Any
    ]
    [ T2Group <$> genType0,
      T2Tag <$> maybe arbitrary <*> genType0
    ]

genOccurrenceIndicator :: Gen OccurrenceIndicator
genOccurrenceIndicator =
  Gen.oneof
    [ pure OIOptional,
      pure OIZeroOrMore,
      pure OIOneOrMore,
      OIBounded
        <$> maybe arbitrary
        <*> maybe arbitrary
    ]

genGroup :: Gen Group
genGroup = Group <$> nonEmpty genGrpChoice

genGrpChoice :: Gen GrpChoice
genGrpChoice = Gen.listOf genGroupEntry

genGroupEntry :: Gen GroupEntry
genGroupEntry =
  recursive
    Gen.oneof
    [ GERef
        <$> maybe genOccurrenceIndicator
        <*> genName
        <*> maybeRec genGenericArg
    ]
    [ GEType
        <$> maybe genOccurrenceIndicator
        <*> maybe genMemberKey
        <*> genType0,
      GEGroup <$> maybe genOccurrenceIndicator <*> genGroup
    ]

genMemberKey :: Gen MemberKey
genMemberKey =
  recursive
    Gen.oneof
    [ MKBareword <$> genName,
      MKValue <$> genValue
    ]
    [ MKType <$> genType1
    ]

genValue :: Gen Value
genValue =
  Gen.oneof
    [ VUInt <$> arbitrary,
      VNInt <$> arbitrary,
      VFloat16 <$> arbitrary,
      VFloat32 <$> arbitrary,
      VFloat64 <$> arbitrary,
      VText . T.pack <$> listOf (elements $ ['a' .. 'z'] <> ['0' .. '9'] <> [' '])
      -- VBytes <$> Gen.bytes (Range.linear 0 1100)
    ]

genCtlOp :: Gen CtlOp
genCtlOp =
  Gen.elements
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

-- | Generate a non-empty list, whose maximum length depends on the size
-- parameter.
nonEmpty :: Gen a -> Gen (NE.NonEmpty a)
nonEmpty f = do
  sing <- f
  n <- getSize
  k <- choose (0, n)
  (sing NE.:|) <$> vectorOf k f

-- | Generates 'Nothing' some of the time
maybe :: Gen a -> Gen (Maybe a)
maybe f = Gen.oneof [Just <$> f, pure Nothing]

-- | Variant on maybe which shrinks the size whenever it selects the 'Just'
-- option. When the size gets to one or less, the Just constructor is no longer
-- called, ensuring termination.
maybeRec :: Gen a -> Gen (Maybe a)
maybeRec gen =
  sized $ \n ->
    if n <= 1
      then pure Nothing
      else
        Gen.frequency
          [ (2, pure Nothing),
            (1 + fromIntegral n, Just <$> Gen.scale golden gen)
          ]

-- | Choose from a set of non-recursive generators and a set of recursive
-- generators, decreasing the size parameter whenever we pick one of the
-- recursive generators.
recursive :: ([Gen a] -> Gen a) -> [Gen a] -> [Gen a] -> Gen a
recursive f nonrec rec = sized $ \n ->
  if n <= 1
    then f nonrec
    else f $ nonrec ++ fmap (scale golden) rec

golden :: Int -> Int
golden x = round (fromIntegral x * 0.61803398875 :: Double)
