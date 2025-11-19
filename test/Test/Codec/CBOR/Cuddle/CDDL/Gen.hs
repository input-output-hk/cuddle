{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Hedgehog generators for CDDL
module Test.Codec.CBOR.Cuddle.CDDL.Gen () where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.Comments (Comment (..))
import Codec.CBOR.Cuddle.Parser (ParserStage, XTerm (..))
import Codec.CBOR.Cuddle.Pretty (PrettyStage, XRule (..), XTerm (..), XXTopLevel (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Test.QuickCheck
import Test.QuickCheck qualified as Gen

instance Arbitrary (CDDL PrettyStage) where
  arbitrary = CDDL <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

deriving newtype instance Arbitrary (XXTopLevel PrettyStage)

deriving newtype instance Arbitrary (XTerm PrettyStage)

deriving newtype instance Arbitrary (XRule PrettyStage)

instance Arbitrary (TopLevel PrettyStage) where
  arbitrary =
    Gen.oneof
      [ XXTopLevel <$> arbitrary
      , TopLevelRule <$> arbitrary
      ]
  shrink = genericShrink

deriving newtype instance Arbitrary (XTerm ParserStage)

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink = fmap T.pack . shrink . T.unpack

instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary
  shrink = fmap BS.pack . shrink . BS.unpack

instance Arbitrary Comment where
  arbitrary = Comment <$> arbitrary
  shrink = genericShrink

nameFstChars :: [Char]
nameFstChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['@', '_', '$']

nameMidChars :: [Char]
nameMidChars = nameFstChars <> ['1' .. '9'] <> ['-', '.']

nameEndChars :: [Char]
nameEndChars = nameFstChars <> ['1' .. '9']

instance Arbitrary Name where
  arbitrary =
    let veryShortListOf = resize 3 . listOf
     in do
          fstChar <- elements nameFstChars
          midChar <- veryShortListOf . elements $ nameMidChars
          lastChar <- elements nameEndChars
          pure $ Name (T.pack $ fstChar : midChar <> [lastChar])

  shrink (Name xs) =
    Name <$> fmap T.pack (filter isValidName (shrink $ T.unpack xs))
    where
      isValidName [] = False
      isValidName (h : tl) = h `elem` nameFstChars && isValidNameTail tl

      isValidNameTail [] = True
      isValidNameTail [x] = x `elem` nameEndChars
      isValidNameTail (h : tl) = h `elem` nameMidChars && isValidNameTail tl

instance Arbitrary Assign where
  arbitrary = Gen.elements [AssignEq, AssignExt]
  shrink = genericShrink

instance Arbitrary (XTerm i) => Arbitrary (GenericParameter i) where
  arbitrary = GenericParameter <$> arbitrary <*> arbitrary

instance (Monoid (XTerm i), Arbitrary (XTerm i)) => Arbitrary (GenericParameters i) where
  arbitrary = GenericParameters <$> nonEmpty arbitrary
  shrink (GenericParameters neName) = GenericParameters <$> shrinkNE neName

instance (Arbitrary (XTerm i), Monoid (XTerm i)) => Arbitrary (GenericArg i) where
  arbitrary = GenericArg <$> nonEmpty arbitrary
  shrink (GenericArg neArg) = GenericArg <$> shrinkNE neArg

instance
  ( Monoid (XTerm i)
  , Arbitrary (XTerm i)
  , Arbitrary (XRule i)
  ) =>
  Arbitrary (Rule i)
  where
  arbitrary =
    Rule
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink = genericShrink

instance Arbitrary RangeBound where
  arbitrary = Gen.elements [ClOpen, Closed]
  shrink = genericShrink

instance Arbitrary TyOp where
  arbitrary =
    Gen.oneof
      [ RangeOp <$> arbitrary
      , CtrlOp <$> arbitrary
      ]
  shrink = genericShrink

instance
  ( Arbitrary (XTerm i)
  , Monoid (XTerm i)
  ) =>
  Arbitrary (TypeOrGroup i)
  where
  arbitrary =
    Gen.oneof
      [ TOGGroup <$> arbitrary
      , TOGType <$> arbitrary
      ]
  shrink = genericShrink

instance (Arbitrary (XTerm i), Monoid (XTerm i)) => Arbitrary (Type0 i) where
  arbitrary = Type0 <$> nonEmpty arbitrary
  shrink (Type0 neType1) = Type0 <$> shrinkNE neType1

instance (Arbitrary (XTerm i), Monoid (XTerm i)) => Arbitrary (Type1 i) where
  arbitrary = Type1 <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Monoid (XTerm i), Arbitrary (XTerm i)) => Arbitrary (Type2 i) where
  arbitrary =
    recursive
      Gen.oneof
      [ T2Value <$> arbitrary
      , T2Map <$> arbitrary
      , T2Array <$> arbitrary
      , T2Enum <$> arbitrary
      , T2DataItem
          <$> elements [0 .. 7]
          <*> genMaybe Gen.arbitrarySizedBoundedIntegral
      , T2Name <$> arbitrary <*> maybeRec arbitrary
      , T2Unwrapped <$> arbitrary <*> maybeRec arbitrary
      , T2EnumRef <$> arbitrary <*> maybeRec arbitrary
      , pure T2Any
      ]
      [ T2Group <$> arbitrary
      , T2Tag <$> arbitrary <*> arbitrary
      ]

instance Arbitrary OccurrenceIndicator where
  arbitrary =
    Gen.oneof
      [ pure OIOptional
      , pure OIZeroOrMore
      , pure OIOneOrMore
      , OIBounded
          <$> arbitrary
          <*> arbitrary
      ]

  shrink = genericShrink

instance (Monoid (XTerm i), Arbitrary (XTerm i)) => Arbitrary (Group i) where
  arbitrary = Group <$> nonEmpty arbitrary
  shrink (Group gr) = Group <$> shrinkNE gr

instance (Monoid (XTerm i), Arbitrary (XTerm i)) => Arbitrary (GrpChoice i) where
  arbitrary = GrpChoice <$> listOf' arbitrary <*> pure mempty
  shrink = genericShrink

instance (Monoid (XTerm i), Arbitrary (XTerm i)) => Arbitrary (GroupEntryVariant i) where
  arbitrary =
    recursive
      Gen.oneof
      [ GERef
          <$> arbitrary
          <*> maybeRec arbitrary
      ]
      [ GEType
          <$> arbitrary
          <*> arbitrary
      , GEGroup <$> arbitrary
      ]
  shrink = genericShrink

instance
  ( Arbitrary (XTerm i)
  , Monoid (XTerm i)
  ) =>
  Arbitrary (GroupEntry i)
  where
  arbitrary =
    GroupEntry
      <$> arbitrary
      <*> arbitrary
      <*> pure mempty
  shrink = genericShrink

instance (Monoid (XTerm i), Arbitrary (XTerm i)) => Arbitrary (MemberKey i) where
  arbitrary =
    recursive
      Gen.oneof
      [ MKBareword <$> arbitrary
      , MKValue <$> arbitrary
      ]
      [ MKType <$> arbitrary
      ]

  shrink = genericShrink

instance Arbitrary Value where
  arbitrary = Value <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary ValueVariant where
  arbitrary =
    Gen.oneof
      [ VUInt <$> arbitrary
      , VNInt <$> arbitrary
      , VFloat16 <$> arbitrary
      , VFloat32 <$> arbitrary
      , VFloat64 <$> arbitrary
      , VText . T.pack <$> listOf (elements $ ['a' .. 'z'] <> ['0' .. '9'] <> [' '])
      -- VBytes <$> Gen.bytes (Range.linear 0 1100)
      ]
  shrink = genericShrink

instance Arbitrary CtlOp where
  arbitrary =
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
  shrink = genericShrink

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

-- | Generate a non-empty list. This function applies similar recursive scaling
-- to @listOf'@ - see the comment there for details.
nonEmpty :: Gen a -> Gen (NE.NonEmpty a)
nonEmpty f = do
  sing <- f
  n <- getSize
  k <- choose (0, n)
  (sing NE.:|) <$> vectorOf k (scale (scaleBy k) f)

-- | Generates 'Nothing' some of the time
genMaybe :: Gen a -> Gen (Maybe a)
genMaybe f = Gen.oneof [Just <$> f, pure Nothing]

-- | Variant on maybe which shrinks the size whenever it selects the 'Just'
-- option. When the size gets to five or less, the Just constructor is no longer
-- called, ensuring termination.
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

-- | Choose from a set of non-recursive generators and a set of recursive
-- generators, decreasing the size parameter whenever we pick one of the
-- recursive generators.
recursive :: ([Gen a] -> Gen a) -> [Gen a] -> [Gen a] -> Gen a
recursive f nonrec rec = sized $ \n ->
  if n <= 5
    then f nonrec
    else f $ nonrec ++ fmap (scale golden) rec

golden :: Int -> Int
golden x = round (fromIntegral x * 0.61803398875 :: Double)

scaleBy :: Int -> Int -> Int
scaleBy k x = round (fromIntegral x * ((1 :: Double) / fromIntegral k))

shrinkNE :: Arbitrary a => NE.NonEmpty a -> [NE.NonEmpty a]
shrinkNE (NE.toList -> l) = mapMaybe NE.nonEmpty (shrink l)

-- | Variant on 'listOf' that tries to constrain the ultimate size of the
-- generated tree by scaling recursive generators according to the size of the
-- generated list - that is, short lists will result in minimal size scaling,
-- whereas long lists will give significant scaling. Overall, the flattened size
-- should therefore remain roughly constant.
listOf' :: Gen a -> Gen [a]
listOf' f = do
  n <- getSize
  k <- choose (0, n)
  vectorOf k $ scale (scaleBy k) f
