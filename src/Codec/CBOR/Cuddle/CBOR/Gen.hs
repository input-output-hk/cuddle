{-# LANGUAGE CPP #-}
#if MIN_VERSION_random(1,3,0)
{-# OPTIONS_GHC -Wno-deprecations #-} -- Due to usage of `split`
#endif
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Generate example CBOR given a CDDL specification
module Codec.CBOR.Cuddle.CBOR.Gen (
  generateFromName,
  GenPhase,
  GenSimple,
  XXCTree (..),
) where

#if MIN_VERSION_random(1,3,0)
#endif
import Codec.CBOR.Cuddle.CDDL (
  Name (..),
  OccurrenceIndicator (..),
  RangeBound (..),
  Value (..),
  ValueVariant (..),
 )
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (CBORGenerator (..), WrappedTerm (..))
import Codec.CBOR.Cuddle.CDDL.CTree (CTree (..), CTreeRoot (..), PTerm (..), foldCTree)
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp qualified as CtlOp
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, XXCTree (..))
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Codec.CBOR.Term (Term (..))
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Char (chr)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Internal.Encoding.Utf8 (utf8Length)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder qualified as LB
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Numeric.Half (Half (..), fromHalf)
import System.Random.Stateful (Random, StatefulGen (..), runStateGen_, uniformByteStringM)
import Test.AntiGen (
  AntiGen,
  antiChoose,
  antiNonNegative,
  antiNonPositive,
  faultyNum,
  runAntiGen,
  (|!),
 )
import Test.QuickCheck (
  Arbitrary,
  NonNegative (..),
 )
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Gen (Gen (..), getSize)
import Test.QuickCheck.GenT (MonadGen (..), elements, listOf, oneof, suchThat, vectorOf)

type data GenPhase

data instance XXCTree GenPhase
  = GenRef Name
  | GenGenerator CBORGenerator (CTree GenPhase)

instance IndexMappable CTree MonoReferenced GenPhase where
  mapIndex = foldCTree mapExt mapIndex
    where
      mapExt (MRuleRef n) = CTreeE $ GenRef n
      mapExt (MGenerator g x) = CTreeE . GenGenerator g $ mapIndex x
      mapExt (MValidator _ x) = mapIndex x

instance IndexMappable CTreeRoot MonoReferenced GenPhase where
  mapIndex (CTreeRoot m) = CTreeRoot $ mapIndex <$> m

-- TODO remove this once QuickCheck gets QC
data QC = QC

instance StatefulGen QC Gen where
  uniformWord32 QC = MkGen (\r _n -> runStateGen_ r uniformWord32)
  uniformWord64 QC = MkGen (\r _n -> runStateGen_ r uniformWord64)
#if MIN_VERSION_random(1,3,0)
  uniformByteArrayM pinned sz QC = 
    MkGen (\r _n -> runStateGen_ r (uniformByteArrayM pinned sz))
#else
  uniformShortByteString k QC =
    MkGen (\r _n -> runStateGen_ r (uniformShortByteString k))
#endif

--------------------------------------------------------------------------------
-- Lifted MonadGen utils
--------------------------------------------------------------------------------

arbitrary :: (Arbitrary a, MonadGen m) => m a
arbitrary = liftGen QC.arbitrary

scale :: MonadGen m => (Int -> Int) -> m a -> m a
scale f m = sized $ \n -> resize (f n) m

--------------------------------------------------------------------------------
-- MonoSimple
--------------------------------------------------------------------------------

type data GenSimple

newtype instance XXCTree GenSimple = GenSimpleRef Name
  deriving (Show)

instance IndexMappable CTree GenPhase GenSimple where
  mapIndex = foldCTree mapExt mapIndex
    where
      mapExt (GenRef n) = CTreeE $ GenSimpleRef n
      mapExt (GenGenerator _ x) = mapIndex x

--------------------------------------------------------------------------------
-- Generator infrastructure
--------------------------------------------------------------------------------

-- | Generator context, parametrised over the type of the random seed
newtype GenEnv = GenEnv
  { cddl :: CTreeRoot GenPhase
  }
  deriving (Generic)

--------------------------------------------------------------------------------
-- Postlude
--------------------------------------------------------------------------------

-- | Simple values that are either unassigned or don't have a specialized type already
simple :: [Word8]
simple =
  -- TODO add the other values once they are supported
  -- [0 .. 19] ++ [23] ++ [32 ..]
  [23]

genHalf :: MonadGen m => m Float
genHalf = do
  half <- Half <$> arbitrary
  if isInfinite half || isDenormalized half || isNaN half
    then genHalf
    else pure $ fromHalf half

genTerm :: AntiGen Term
genTerm =
  oneof
    [ TInt <$> choose (minBound, maxBound)
    , TInteger
        <$> oneof
          [ choose (toInteger (maxBound :: Int) + 1, toInteger (maxBound :: Word64))
          , choose (negate (toInteger (maxBound :: Word64)), toInteger (minBound :: Int) - 1)
          ]
    , TBytes <$> (genNBytes . getNonNegative =<< arbitrary)
    , TBytesI <$> (genNLazyBytes . getNonNegative =<< arbitrary)
    , TString . T.pack <$> arbitrary
    , TStringI . TL.pack <$> arbitrary
    , TList <$> listOf smallerTerm
    , TListI <$> listOf smallerTerm
    , TMap <$> listOf ((,) <$> smallerTerm <*> smallerTerm)
    , TMapI <$> listOf ((,) <$> smallerTerm <*> smallerTerm)
    , TTagged <$> choose (6, maxBound :: Word64) <*> smallerTerm
    , TBool <$> arbitrary
    , pure TNull
    , TSimple <$> elements simple
    , THalf <$> genHalf
    , TFloat <$> arbitrary
    , TDouble <$> arbitrary
    ]
  where
    smallerTerm :: AntiGen Term
    smallerTerm = scale (`div` 5) genTerm

genNBytes :: MonadGen m => Int -> m ByteString
genNBytes n = liftGen (uniformByteStringM n QC)

genBytes :: MonadGen m => m ByteString
genBytes = sized $ \sz -> do
  numElems <- choose (0, sz)
  genNBytes numElems

genNLazyBytes :: MonadGen m => Int -> m BSL.ByteString
genNLazyBytes n = BSL.fromStrict <$> genNBytes n

genCharAtMostBytes :: MonadGen m => Int -> m Char
genCharAtMostBytes 1 = chr <$> choose (0x00, 0x7F)
genCharAtMostBytes 2 = chr <$> choose (0x00, 0x7FF)
genCharAtMostBytes 3 = chr <$> oneof [choose (0x00, 0xD7FF), choose (0xE000, 0xFFFF)]
genCharAtMostBytes n
  | n <= 0 = error "expected positive number"
  | otherwise = chr <$> oneof [choose (0x00, 0xD7FF), choose (0xE000, 0x10FFFF)]

genNBytesText :: MonadGen m => Int -> m Text
genNBytesText n = do
  let
    go 0 = pure mempty
    go m = do
      c <- genCharAtMostBytes m
      rest <- go (m - utf8Length c)
      pure $ LB.singleton c <> rest
  builder <- go n
  pure . LT.toStrict $ toLazyText builder

-- genNBytesText n = do
--  x <- arbitrary
--  let len = utf8Length x
--  if len <= n
--    then do
--      xs <- genNBytesText $ n - len
--      pure $ T.cons x xs
--    else genNBytesText n

genText :: MonadGen m => m Text
genText = sized $ \sz -> genNBytesText =<< choose (0, sz)

-- | Primitive types defined by the CDDL specification, with their generators
genPostlude :: PTerm -> AntiGen Term
genPostlude pt = case pt of
  PTBool -> TBool <$> arbitrary
  PTUInt -> TInteger <$> antiNonNegative
  PTNInt -> TInteger <$> antiNonPositive
  PTInt -> TInteger . fromIntegral <$> choose (minBound :: Int, maxBound)
  PTHalf -> THalf <$> choose (-65504, 65504)
  PTFloat -> TFloat <$> arbitrary
  PTDouble -> TDouble <$> arbitrary
  PTBytes -> TBytes <$> genBytes
  PTText -> TString <$> genText
  PTAny -> genTerm
  PTNil -> pure TNull
  PTUndefined -> pure $ TSimple 23

--------------------------------------------------------------------------------
-- Kinds of terms
--------------------------------------------------------------------------------

-- | Recursively flatten wrapped list. That is, expand any groups out to their
-- individual entries.
flattenWrappedList :: [WrappedTerm] -> [WrappedTerm]
flattenWrappedList [] = []
flattenWrappedList (G xxs : xs) =
  flattenWrappedList xxs <> flattenWrappedList xs
flattenWrappedList (y : xs) = y : flattenWrappedList xs

-- | Convert a list of wrapped terms to a list of terms. If any 'PairTerm's are
-- present, we just take their "value" part.
singleTermList :: [WrappedTerm] -> Maybe [Term]
singleTermList [] = Just []
singleTermList (S x : xs) = (x :) <$> singleTermList xs
singleTermList (P _ y : xs) = (y :) <$> singleTermList xs
singleTermList _ = Nothing

-- | Convert a list of wrapped terms to a list of pairs of terms, or fail if any
-- 'SingleTerm's are present.
pairTermList :: [WrappedTerm] -> Maybe [(Term, Term)]
pairTermList [] = Just []
pairTermList (P x y : xs) = ((x, y) :) <$> pairTermList xs
pairTermList _ = Nothing

showSimple :: CTree GenPhase -> String
showSimple = show . mapIndex @_ @_ @GenSimple

-- | Drop all negative generators
dropNegativeGen :: AntiGen a -> AntiGen a
dropNegativeGen = liftGen . runAntiGen

--------------------------------------------------------------------------------
-- Generator functions
--------------------------------------------------------------------------------

genSized :: HasCallStack => Word64 -> CTree i -> AntiGen WrappedTerm
genSized s target = do
  case target of
    CTree.Postlude PTText -> S . TString <$> genNBytesText (fromIntegral s)
    CTree.Postlude PTBytes -> S . TBytes <$> genNBytes (fromIntegral s)
    CTree.Postlude PTUInt -> S . TInteger <$> choose (0, 256 ^ s - 1)
    _ -> error "Cannot apply size operator to target "

range :: Enum a => RangeBound -> a -> a -> (a, a)
range ClOpen x y = (x, pred y)
range Closed x y = (x, y)

genBetween :: (Integral a, Random a) => (a, a) -> AntiGen a
genBetween rng = do
  size <- liftGen getSize
  let
    sizeBounds :: Integral a => (a, a)
    sizeBounds = (0, fromIntegral size)
  antiChoose rng sizeBounds

genForCTree ::
  HasCallStack => CTreeRoot GenPhase -> CTree GenPhase -> AntiGen WrappedTerm
genForCTree _ (CTree.Literal v) = pure . S $ valueToTerm v
genForCTree _ (CTree.Postlude pt) = S <$> genPostlude pt
genForCTree cddl (CTree.Map nodes) = do
  items <- pairTermList . flattenWrappedList <$> traverse (genForCTree cddl) nodes
  case items of
    Just ts ->
      let
        -- De-duplicate keys in the map.
        -- Per RFC7049:
        -- >> A map that has duplicate keys may be well-formed, but it is not
        -- >> valid, and thus it causes indeterminate decoding
        tsNodup = nubOrdOn fst ts
       in
        pure . S $ TMap tsNodup
    Nothing -> error "Single terms in map context"
genForCTree cddl (CTree.Array nodes) = do
  items <- singleTermList . flattenWrappedList <$> traverse (genForCTree cddl) nodes
  case items of
    Just ts -> pure . S $ TList ts
    Nothing -> error "Something weird happened which shouldn't be possible"
genForCTree cddl (CTree.Choice (NE.toList -> nodes)) = do
  ix <- choose (0, length nodes - 1)
  genForCTree cddl $ nodes !! ix
genForCTree cddl (CTree.Group nodes) = G <$> traverse (genForCTree cddl) nodes
genForCTree cddl (CTree.KV key value _cut) = do
  kg <- genForCTree cddl key
  vg <- genForCTree cddl value
  case (kg, vg) of
    (S k, S v) -> pure $ P k v
    _ ->
      error $
        "Non single-term generated outside of group context: "
          <> showSimple key
          <> " => "
          <> showSimple value
genForCTree cddl (CTree.Occur item occurs) =
  applyOccurenceIndicator occurs (genForCTree cddl item)
genForCTree cddl (CTree.Range from to bounds) = do
  term1 <- dropNegativeGen $ genForCTree cddl from
  term2 <- dropNegativeGen $ genForCTree cddl to
  case (term1, term2) of
    (S (TInt a), S (TInt b))
      | a <= b -> genBetween (range bounds a b) <&> S . TInt
    (S (TInt a), S (TInteger b))
      | fromIntegral a <= b ->
          genBetween (range bounds (fromIntegral a) b) <&> S . TInteger
    (S (TInteger a), S (TInteger b))
      | a <= b -> genBetween (range bounds a b) <&> S . TInteger
    (S (THalf a), S (THalf b))
      | a <= b -> choose (range bounds a b) <&> S . THalf
    (S (TFloat a), S (TFloat b))
      | a <= b -> choose (range bounds a b) <&> S . TFloat
    (S (TDouble a), S (TDouble b))
      | a <= b -> choose (range bounds a b) <&> S . TDouble
    (a, b) -> error $ "invalid range (a = " <> show a <> ", b = " <> show b <> ")"
genForCTree cddl (CTree.Control op target controller) = do
  resolvedController <- case controller of
    CTreeE (GenRef n) -> dropNegativeGen $ resolveRef cddl n
    x -> pure x
  case (op, resolvedController) of
    (CtlOp.Le, CTree.Literal (Value (VUInt n) _)) -> case target of
      CTree.Postlude PTUInt -> S . TInteger <$> choose (0, fromIntegral n)
      _ -> error "Cannot apply le operator to target"
    (CtlOp.Le, _) -> error $ "Invalid controller for .le operator: " <> showSimple controller
    (CtlOp.Lt, CTree.Literal (Value (VUInt n) _)) -> case target of
      CTree.Postlude PTUInt -> S . TInteger <$> choose (0, fromIntegral n - 1)
      _ -> error "Cannot apply lt operator to target"
    (CtlOp.Lt, _) -> error $ "Invalid controller for .lt operator: " <> showSimple controller
    (CtlOp.Size, CTree.Literal (Value (VUInt s) _)) -> do
      s' <- pure s |! sized (\sz -> choose (0, fromIntegral sz) `suchThat` (/= s))
      genSized s' target
    (CtlOp.Size, CTree.Range {CTree.from, CTree.to}) -> do
      case (from, to) of
        (CTree.Literal (Value (VUInt f) _), CTree.Literal (Value (VUInt t) _)) -> do
          s <- sized $ \sz ->
            antiChoose
              (fromIntegral f, fromIntegral t)
              (0, max (succ t) $ fromIntegral sz)
          genSized s target
        _ ->
          error $
            "Invalid controller for .size operator: "
              <> showSimple controller
    (CtlOp.Size, _) ->
      error $
        "Invalid controller for .size operator: "
          <> showSimple controller
    (CtlOp.Cbor, _) -> do
      enc <- genForCTree cddl controller
      case enc of
        S x -> pure . S . TBytes . CBOR.toStrictByteString $ CBOR.encodeTerm x
        _ -> error "Controller does not correspond to a single term"
    (c, _) -> error $ "Controller not yet implemented: " <> show c
genForCTree cddl (CTree.Enum tree) = do
  case tree of
    CTree.Group trees -> do
      ix <- choose (0, length trees - 1)
      genForCTree cddl $ trees !! ix
    _ -> error "Attempt to form an enum from something other than a group"
genForCTree cddl (CTree.Unwrap node) = genForCTree cddl node
genForCTree cddl (CTree.Tag t node) = do
  tag <- faultyNum t
  enc <- genForCTree cddl node
  case enc of
    S x -> pure $ S $ TTagged tag x
    _ -> error "Tag controller does not correspond to a single term"
genForCTree cddl (CTree.CTreeE (GenRef n)) = genForNode cddl n
genForCTree _ (CTree.CTreeE (GenGenerator (CBORGenerator gen) _)) = gen

genForNode :: HasCallStack => CTreeRoot GenPhase -> Name -> AntiGen WrappedTerm
genForNode cddl = genForCTree cddl <=< resolveRef cddl

-- | Take a reference and resolve it to the relevant Tree, following multiple
-- links if necessary.
resolveRef :: MonadGen m => CTreeRoot GenPhase -> Name -> m (CTree GenPhase)
resolveRef (CTreeRoot cddl) n = do
  -- Since we follow a reference, we decrease the 'size' of the Gen monad.
  scale (\x -> max 0 $ x - 1) $
    case Map.lookup n cddl of
      Nothing -> error $ "Unbound reference: " <> show n
      Just val -> pure val

-- | Generate a CBOR Term corresponding to a top-level name.
--
-- Since we apply this to a monomorphised CTree, the names must be monomorphic
-- terms in the original CDDL.
--
-- This will throw an error if the generated item does not correspond to a
-- single CBOR term (e.g. if the name resolves to a group, which cannot be
-- generated outside a context).
generateFromName :: HasCallStack => CTreeRoot GenPhase -> Name -> AntiGen Term
generateFromName root@(CTreeRoot cddl) n = do
  case Map.lookup n cddl of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val ->
      genForCTree root val >>= \case
        S x -> pure x
        _ ->
          error $
            "Tried to generate a top-level term for "
              <> show n
              <> ", but it does not correspond to a single term."

sizeBiasedBool :: MonadGen m => m Bool
sizeBiasedBool = sized $ \sz -> (> 1) <$> choose (0, sz)

listOfScaled :: MonadGen m => m a -> m [a]
listOfScaled g = sized $ \sz -> do
  i <- choose (0, sz)
  vectorOf i $ scale (`div` (i + 1)) g

listOfScaled1 :: AntiGen a -> AntiGen [a]
listOfScaled1 g = sized $ \sz -> do
  i <- choose (1, max sz 1) |! pure 0
  vectorOf i $ scale (`div` (i + 1)) g

-- | Apply an occurence indicator to a group entry
applyOccurenceIndicator ::
  OccurrenceIndicator ->
  AntiGen WrappedTerm ->
  AntiGen WrappedTerm
applyOccurenceIndicator OIOptional oldGen =
  sizeBiasedBool >>= \case
    False -> pure $ G mempty
    True -> oldGen
applyOccurenceIndicator OIZeroOrMore oldGen =
  G <$> listOfScaled oldGen
applyOccurenceIndicator OIOneOrMore oldGen =
  G <$> listOfScaled1 oldGen
applyOccurenceIndicator (OIBounded mlb mub) oldGen =
  sized $ \sz -> do
    let
      lb = fromMaybe 0 mlb
      ub = fromMaybe (lb + fromIntegral sz) mub
    i <- fromIntegral <$> genBetween (lb, ub)
    G <$> vectorOf i (scale (`div` (i + 1)) oldGen)

valueToTerm :: Value -> Term
valueToTerm (Value x _) = valueVariantToTerm x

valueVariantToTerm :: ValueVariant -> Term
valueVariantToTerm (VUInt i)
  | toInteger i <= toInteger (maxBound :: Int) = TInt (fromIntegral i)
  | otherwise = TInteger (fromIntegral i)
valueVariantToTerm (VNInt i)
  | -toInteger i >= toInteger (minBound :: Int) = TInt (-fromIntegral i)
  | otherwise = TInteger (-fromIntegral i)
valueVariantToTerm (VBignum i) = TInteger i
valueVariantToTerm (VFloat16 i) = THalf i
valueVariantToTerm (VFloat32 i) = TFloat i
valueVariantToTerm (VFloat64 i) = TDouble i
valueVariantToTerm (VText t) = TString t
valueVariantToTerm (VBytes b) = TBytes b
valueVariantToTerm (VBool b) = TBool b
