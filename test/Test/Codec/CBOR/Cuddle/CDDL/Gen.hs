{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Hedgehog generators for CDDL
module Test.Codec.CBOR.Cuddle.CDDL.Gen (
    genCDDL,
    genRule,
    genName,
    genValue,
    --
    genType0,
    genGroupEntry,
)
where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Data.List (inits)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
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
        veryShortListOf = resize 3 . listOf
     in do
            fstChar <- elements endChars
            midChar <- veryShortListOf . elements $ endChars <> midChars
            lastChar <- elements $ endChars <> ['1' .. '9']
            pure $ Name . T.pack $ fstChar : midChar <> [lastChar]

instance Arbitrary Name where
    arbitrary = genName

    shrink (Name (T.unpack -> t)) = case t of
        [_] -> []
        xs -> Name . T.pack <$> drop 1 (inits xs)

genAssign :: Gen Assign
genAssign = Gen.elements [AssignEq, AssignExt]

instance Arbitrary Assign where
    arbitrary = genAssign
    shrink = genericShrink

genGenericParams :: Gen GenericParam
genGenericParams = GenericParam <$> nonEmpty genName

instance Arbitrary GenericParam where
    arbitrary = genGenericParams
    shrink (GenericParam neName) = GenericParam <$> shrinkNE neName

genGenericArg :: Gen GenericArg
genGenericArg = GenericArg <$> nonEmpty genType1

instance Arbitrary GenericArg where
    arbitrary = genGenericArg
    shrink (GenericArg neArg) = GenericArg <$> shrinkNE neArg

genRule :: Gen Rule
genRule =
    Rule
        <$> genName
        <*> maybe genGenericParams
        <*> genAssign
        <*> genTypeOrGroup

instance Arbitrary Rule where
    arbitrary = genRule
    shrink = genericShrink

genRangeBound :: Gen RangeBound
genRangeBound = Gen.elements [ClOpen, Closed]

instance Arbitrary RangeBound where
    arbitrary = genRangeBound
    shrink = genericShrink

genTyOp :: Gen TyOp
genTyOp =
    Gen.oneof
        [ RangeOp <$> genRangeBound
        , CtrlOp <$> genCtlOp
        ]

instance Arbitrary TyOp where
    arbitrary = genTyOp
    shrink = genericShrink

genTypeOrGroup :: Gen TypeOrGroup
genTypeOrGroup =
    Gen.oneof
        [ TOGGroup <$> genGroupEntry
        , TOGType <$> genType0
        ]

instance Arbitrary TypeOrGroup where
    arbitrary = genTypeOrGroup
    shrink = genericShrink

genType0 :: Gen Type0
genType0 = Type0 <$> nonEmpty genType1

instance Arbitrary Type0 where
    arbitrary = genType0
    shrink (Type0 neType1) = Type0 <$> shrinkNE neType1

genType1 :: Gen Type1
genType1 = Type1 <$> genType2 <*> maybe ((,) <$> genTyOp <*> genType2)

instance Arbitrary Type1 where
    arbitrary = genType1
    shrink = genericShrink

genType2 :: Gen Type2
genType2 =
    recursive
        Gen.oneof
        [ T2Value <$> genValue
        , T2Map <$> genGroup
        , T2Array <$> genGroup
        , T2Enum <$> genGroup
        , T2DataItem
            <$> elements [0 .. 7]
            <*> maybe Gen.arbitrarySizedBoundedIntegral
        , T2Name <$> genName <*> maybeRec genGenericArg
        , T2Unwrapped <$> genName <*> maybeRec genGenericArg
        , T2EnumRef <$> genName <*> maybeRec genGenericArg
        , pure T2Any
        ]
        [ T2Group <$> genType0
        , T2Tag <$> maybe arbitrary <*> genType0
        ]

instance Arbitrary Type2 where
    arbitrary = genType2
    shrink = genericShrink

genOccurrenceIndicator :: Gen OccurrenceIndicator
genOccurrenceIndicator =
    Gen.oneof
        [ pure OIOptional
        , pure OIZeroOrMore
        , pure OIOneOrMore
        , OIBounded
            <$> maybe arbitrary
            <*> maybe arbitrary
        ]

instance Arbitrary OccurrenceIndicator where
    arbitrary = genOccurrenceIndicator
    shrink = genericShrink

genGroup :: Gen Group
genGroup = Group <$> nonEmpty genGrpChoice

instance Arbitrary Group where
    arbitrary = genGroup
    shrink (Group gr) = Group <$> shrinkNE gr

genGrpChoice :: Gen GrpChoice
genGrpChoice = listOf' (noComment <$> genGroupEntry)

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
            <*> genType0
        , GEGroup <$> maybe genOccurrenceIndicator <*> genGroup
        ]

instance Arbitrary GroupEntry where
    arbitrary = genGroupEntry
    shrink = genericShrink

genMemberKey :: Gen MemberKey
genMemberKey =
    recursive
        Gen.oneof
        [ MKBareword <$> genName
        , MKValue <$> genValue
        ]
        [ MKType <$> genType1
        ]

instance Arbitrary MemberKey where
    arbitrary = genMemberKey
    shrink = genericShrink

genValue :: Gen Value
genValue =
    Gen.oneof
        [ VUInt <$> arbitrary
        , VNInt <$> arbitrary
        , VFloat16 <$> arbitrary
        , VFloat32 <$> arbitrary
        , VFloat64 <$> arbitrary
        , VText . T.pack <$> listOf (elements $ ['a' .. 'z'] <> ['0' .. '9'] <> [' '])
        -- VBytes <$> Gen.bytes (Range.linear 0 1100)
        ]

instance Arbitrary Value where
    arbitrary = genValue

genCtlOp :: Gen CtlOp
genCtlOp =
    Gen.elements
        [ Size
        , Bits
        , Regexp
        , Cbor
        , Cborseq
        , Within
        , And
        , Lt
        , Le
        , Gt
        , Ge
        , Eq
        , Ne
        , Default
        ]

instance Arbitrary CtlOp where
    arbitrary = genCtlOp
    shrink = genericShrink

instance (Arbitrary a) => Arbitrary (WithComments a) where
    arbitrary = noComment <$> arbitrary
    shrink (WithComments x _) = noComment <$> shrink x

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

{- | Generate a non-empty list. This function applies similar recursive scaling
to @listOf'@ - see the comment there for details.
-}
nonEmpty :: Gen a -> Gen (NE.NonEmpty a)
nonEmpty f = do
    sing <- f
    n <- getSize
    k <- choose (0, n)
    (sing NE.:|) <$> vectorOf k (scale (scaleBy k) f)

-- | Generates 'Nothing' some of the time
maybe :: Gen a -> Gen (Maybe a)
maybe f = Gen.oneof [Just <$> f, pure Nothing]

{- | Variant on maybe which shrinks the size whenever it selects the 'Just'
option. When the size gets to five or less, the Just constructor is no longer
called, ensuring termination.
-}
maybeRec :: Gen a -> Gen (Maybe a)
maybeRec gen = sized $
    \n ->
        if n <= 5
            then pure Nothing
            else
                Gen.frequency
                    [ (2, pure Nothing)
                    , (1 + fromIntegral n, Just <$> Gen.scale golden gen)
                    ]

{- | Choose from a set of non-recursive generators and a set of recursive
generators, decreasing the size parameter whenever we pick one of the
recursive generators.
-}
recursive :: ([Gen a] -> Gen a) -> [Gen a] -> [Gen a] -> Gen a
recursive f nonrec rec = sized $ \n ->
    if n <= 5
        then f nonrec
        else f $ nonrec ++ fmap (scale golden) rec

golden :: Int -> Int
golden x = round (fromIntegral x * 0.61803398875 :: Double)

scaleBy :: Int -> Int -> Int
scaleBy k x = round (fromIntegral x * ((1 :: Double) / fromIntegral k))

shrinkNE :: (Arbitrary a) => NE.NonEmpty a -> [NE.NonEmpty a]
shrinkNE (NE.toList -> l) = mapMaybe NE.nonEmpty (shrink l)

{- | Variant on 'listOf' that tries to constrain the ultimate size of the
generated tree by scaling recursive generators according to the size of the
generated list - that is, short lists will result in minimal size scaling,
whereas long lists will give significant scaling. Overall, the flattened size
should therefore remain roughly constant.
-}
listOf' :: Gen a -> Gen [a]
listOf' f = do
    n <- getSize
    k <- choose (0, n)
    vectorOf k $ scale (scaleBy k) f
