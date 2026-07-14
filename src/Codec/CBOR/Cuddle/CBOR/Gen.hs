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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Generate example CBOR given a CDDL specification
module Codec.CBOR.Cuddle.CBOR.Gen (
  generateFromName,
  generateFromGRef,
  GenPhase,
  XXCTree (..),
) where

#if MIN_VERSION_random(1,3,0)
#endif
import Codec.CBOR.Cuddle.CBOR.Canonical (toCanonical)
import Codec.CBOR.Cuddle.CBOR.Term (
  CBORTerm (..),
  encodeCBORTerm,
  toNInt,
  unsignedToBytes,
 )
import Codec.CBOR.Cuddle.CDDL (
  GRef (..),
  Name (..),
  OccurrenceIndicator (..),
  RangeBound (..),
  Value (..),
  ValueVariant (..),
 )
import Codec.CBOR.Cuddle.CDDL.CTree (
  CTree (..),
  PTerm (..),
  Range (..),
  unFloatLiteral,
  unIntLiteral,
 )
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp qualified as CtlOp
import Codec.CBOR.Cuddle.CDDL.Custom.Core (MonadCddl (..), RuleTerm (..))
import Codec.CBOR.Cuddle.CDDL.Custom.Generator (
  CBORGen,
  GenConfig (..),
  GenEnv (..),
  GenPhase,
  disableTwiddle,
  liftAntiGen,
  withAntiGen,
 )
import Codec.CBOR.Cuddle.CDDL.Resolve (XXCTree (..), showSimple)
import Codec.CBOR.Write qualified as CBOR
import Control.Monad (zipWithM, (<=<))
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (chr)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Internal.Encoding.Utf8 (utf8Length)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder qualified as LB
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Prettyprinter (Pretty (..), layoutCompact)
import Prettyprinter.Render.Text (renderStrict)
import System.Random.Stateful (Random, StatefulGen (..), runStateGen_, uniformByteStringM)
import Test.AntiGen (
  AntiGen,
  antiChoose,
  faultyBool,
  faultyNum,
  reweigh,
  runAntiGen,
  withAnnotation,
  (|!),
 )
import Test.QuickCheck (
  Arbitrary,
  NonNegative (..),
 )
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Gen (Gen (..), getSize)
import Test.QuickCheck.GenT (MonadGen (..), elements, frequency, listOf, oneof, suchThat, vectorOf)

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
-- Generator infrastructure
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Postlude
--------------------------------------------------------------------------------

-- | Split a list into a random number of non-empty contiguous sublists, such
-- that @concat \<$\> genSublists xs@ always yields @xs@ again.
genSublists :: [a] -> CBORGen [[a]]
genSublists [] = pure []
genSublists xs = do
  n <- choose (1, length xs)
  let (chunk, rest) = splitAt n xs
  (chunk :) <$> genSublists rest

twiddleString :: Text -> CBORGen CBORTerm
twiddleString t = do
  twiddle <- asks (gcTwiddle . geConfig)
  let splitText = fmap (fmap LT.pack) . genSublists . T.unpack
  if twiddle
    then oneof [pure $ TermString t, TermStringI <$> splitText t]
    else pure $ TermString t

twiddleList :: [CBORTerm] -> CBORGen CBORTerm
twiddleList t = do
  twiddle <- asks (gcTwiddle . geConfig)
  if twiddle
    then elements [TermArray t, TermArrayI t]
    else pure $ TermArray t

twiddleBytes :: ByteString -> CBORGen CBORTerm
twiddleBytes t = do
  twiddle <- asks (gcTwiddle . geConfig)
  let splitBytes = fmap (fmap LBS.pack) . genSublists . BS.unpack
  if twiddle
    then oneof [pure $ TermBytes t, TermBytesI <$> splitBytes t]
    else pure $ TermBytes t

twiddleMap :: [(CBORTerm, CBORTerm)] -> CBORGen CBORTerm
twiddleMap t = do
  twiddle <- asks (gcTwiddle . geConfig)
  if twiddle
    then elements [TermMap t, TermMapI t]
    else pure $ TermMap t

genTerm :: CBORGen CBORTerm
genTerm =
  oneof
    [ TermUInt <$> arbitrary
    , TermNInt <$> arbitrary
    , twiddleBytes =<< genNBytes . getNonNegative =<< arbitrary
    , twiddleString . T.pack =<< arbitrary
    , twiddleList =<< listOf smallerTerm
    , twiddleMap =<< listOf ((,) <$> smallerTerm <*> smallerTerm)
    , TermTag <$> choose (6, maxBound :: Word64) <*> smallerTerm
    , TermSimple <$> elements [20 .. 23]
    , TermHalf <$> arbitrary
    , TermFloat <$> arbitrary
    , TermDouble <$> arbitrary
    ]
  where
    smallerTerm :: CBORGen CBORTerm
    smallerTerm = scale (`div` 5) genTerm

genNBytes :: MonadGen m => Int -> m ByteString
genNBytes n = liftGen (uniformByteStringM n QC)

genBytes :: MonadGen m => m ByteString
genBytes = sized $ \sz -> do
  numElems <- choose (0, sz)
  genNBytes numElems

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
    go m !acc
      | m <= 0 = pure acc
      | otherwise = do
          c <- genCharAtMostBytes m
          go (m - utf8Length c) (acc <> LB.singleton c)
  builder <- go n mempty
  pure . LT.toStrict $ toLazyText builder

genText :: MonadGen m => m Text
genText = sized $ \sz -> genNBytesText =<< choose (0, sz)

-- | Primitive types defined by the CDDL specification, with their generators
genPostlude :: PTerm -> CBORGen CBORTerm
genPostlude pt = genPTerm =<< liftAntiGen (faultyPTerm pt)
  where
    genExcluding ls =
      elements (filter (`notElem` ls) [minBound .. maxBound])
    nonPInteger p =
      pure p |! genExcluding [PTUInt, PTNInt, PTInt, PTAny]
    -- \| Introduces a decision point that can change the type of the CBOR term that is generated
    faultyPTerm t = reweigh 0.1 $
      case t of
        PTAny -> pure PTAny
        p@PTUInt -> nonPInteger p
        p@PTNInt -> nonPInteger p
        p@PTInt -> nonPInteger p
        p -> pure p |! genExcluding [PTAny, p]
    -- \| Given a CDDL postlude type, generates a value of that type
    genPTerm = \case
      PTBool -> TermSimple <$> elements [20 .. 21]
      -- For integers, introduce decision points that sometimes generate
      -- out-of-bounds values
      PTUInt -> TermUInt <$> arbitrary
      PTNInt -> TermNInt <$> arbitrary
      PTInt -> oneof [TermNInt <$> arbitrary, TermUInt <$> arbitrary]
      PTHalf -> TermHalf <$> arbitrary
      PTFloat -> TermFloat <$> arbitrary
      PTDouble -> TermDouble <$> arbitrary
      PTBytes -> twiddleBytes =<< genBytes
      PTText -> twiddleString =<< genText
      PTAny -> genTerm
      PTNil -> pure $ TermSimple 22
      PTUndefined -> pure $ TermSimple 23

--------------------------------------------------------------------------------
-- Kinds of terms
--------------------------------------------------------------------------------

-- | Recursively flatten wrapped list. That is, expand any groups out to their
-- individual entries.
flattenWrappedList :: [RuleTerm] -> [RuleTerm]
flattenWrappedList [] = []
flattenWrappedList (GroupTerm xxs : xs) =
  flattenWrappedList xxs <> flattenWrappedList xs
flattenWrappedList (y : xs) = y : flattenWrappedList xs

-- | Convert a list of wrapped terms to a list of terms. If any 'PairTerm's are
-- present, we just take their "value" part.
singleTermList :: [RuleTerm] -> Maybe [CBORTerm]
singleTermList [] = Just []
singleTermList (SingleTerm x : xs) = (x :) <$> singleTermList xs
singleTermList (PairTerm _ y : xs) = (y :) <$> singleTermList xs
singleTermList _ = Nothing

-- | Remove all negative generators from the `AntiGen`.
dropNegativeGen :: AntiGen a -> AntiGen a
dropNegativeGen = liftGen . runAntiGen

--------------------------------------------------------------------------------
-- Generator functions
--------------------------------------------------------------------------------

genSized :: HasCallStack => Word64 -> CTree GenPhase -> CBORGen RuleTerm
genSized s target = annotateTerm target $ case target of
  CTree.Postlude PTText -> fmap SingleTerm . twiddleString =<< genNBytesText (fromIntegral s)
  CTree.Postlude PTBytes -> fmap SingleTerm . twiddleBytes =<< genNBytes (fromIntegral s)
  CTree.Postlude PTUInt -> SingleTerm . TermUInt <$> choose (0, 256 ^ s - 1)
  _ -> error "Cannot apply size operator to target "

genBetween :: (Integral a, Random a) => (a, a) -> CBORGen a
genBetween rng = liftAntiGen $ do
  size <- liftGen getSize
  let
    sizeBounds :: Integral a => (a, a)
    sizeBounds = (0, fromIntegral size)
  antiChoose rng sizeBounds

annotate :: Text -> CBORGen b -> CBORGen b
annotate ann = withAntiGen $ withAnnotation ann

-- | Annotate a generator with the label derived from a CTree node.
-- Control nodes are transparent — the annotation comes from the target inside.
annotateTerm :: CTree GenPhase -> CBORGen a -> CBORGen a
annotateTerm = \case
  CTree.Literal _ -> annotate "literal"
  CTree.Postlude pt -> annotate $ renderStrict . layoutCompact $ pretty pt
  CTree.Map _ -> annotate "map"
  CTree.Array _ -> annotate "array"
  CTree.Choice _ -> id
  CTree.Group _ -> annotate "group"
  CTree.KV {} -> annotate "kv"
  CTree.Occur {} -> id
  CTree.CRange {} -> annotate "range"
  CTree.Control {} -> id
  CTree.Enum _ -> annotate "enum"
  CTree.Unwrap _ -> annotate "unwrap"
  CTree.Tag {} -> annotate "tag"
  CTree.CTreeE (GenRef n) -> annotate (unName n)
  CTree.CTreeE (GenGenerator _ _) -> annotate "custom_generator"

genForCTree :: HasCallStack => CTree GenPhase -> CBORGen RuleTerm
genForCTree node = annotateTerm node $ case node of
  CTree.Literal v -> SingleTerm <$> valueToTerm v
  CTree.Postlude pt -> SingleTerm <$> genPostlude pt
  CTree.Map nodes -> genMap nodes
  CTree.Array nodes -> genArray nodes
  CTree.Choice (NE.toList -> nodes) -> do
    ix <- choose (0, length nodes - 1)
    genForCTree $ nodes !! ix
  CTree.Group nodes -> GroupTerm <$> traverse genForCTree nodes
  CTree.KV key value _cut -> genKV key value
  CTree.Occur item occurs ->
    applyOccurenceIndicator occurs (genForCTree item)
  CTree.CRange (Range from to bounds) -> genRange from to bounds
  CTree.Control op target controller -> genControl op target controller
  CTree.Enum tree -> genEnum tree
  CTree.Unwrap n -> genForCTree n
  CTree.Tag t n -> genTag t n
  CTree.CTreeE (GenRef n) -> genForNode n
  CTree.CTreeE (GenGenerator gen _) -> gen

-- | Generate a map from a list of nodes
genMap ::
  HasCallStack => [CTree GenPhase] -> CBORGen RuleTerm
genMap nodes = do
  let
    elemsNeeded KV {} = 1
    elemsNeeded (Occur _ OIOneOrMore) = 1
    elemsNeeded (Occur _ (OIBounded (Just lo) _)) = lo
    elemsNeeded _ = 0

    tryGenKV ::
      Int ->
      Map.Map CBORTerm a ->
      CTree GenPhase ->
      CTree GenPhase ->
      CBORGen (Maybe (CBORTerm, CBORTerm))
    tryGenKV nTries m kNode vNode = go nTries
      where
        canonicalKeys = toCanonical <$> Map.keys m
        unS (SingleTerm x) = x
        unS x = error $ "Expected single, got " <> show x
        go !n
          | n > 0 = do
              k <- unS <$> scale (`div` 2) (withAntiGen (withAnnotation "key") $ genForCTree kNode)
              if toCanonical k `notElem` canonicalKeys
                then do
                  v <- unS <$> scale (`div` 2) (withAntiGen (withAnnotation "value") $ genForCTree vNode)
                  pure $ Just (k, v)
                else go (n - 1)
          | otherwise = pure Nothing

    genNodes ::
      Int -> Map.Map CBORTerm CBORTerm -> [CTree GenPhase] -> CBORGen (Maybe (Map.Map CBORTerm CBORTerm))
    genNodes _ m [] = pure $ Just m
    genNodes !i !m (n : ns) =
      let
        ann = withAntiGen (withAnnotation (T.pack $ show i))
        -- A term was produced, advance the index
        next x y = scale (\s -> max 0 (s - 1)) $ genNodes (i + 1) x y
        -- No term was produced, keep the index
        same x y = scale (\s -> max 0 (s - 1)) $ genNodes i x y
        optGenKV kNode vNode = sized $ \sz ->
          frequency [(1, pure Nothing), (max 0 sz, ann $ tryGenKV 10 m kNode vNode)]
       in
        case n of
          KV kNode vNode _ -> do
            mKV <- ann $ tryGenKV 100 m kNode vNode
            case mKV of
              Just (k, v) -> next (Map.insert k v m) ns
              Nothing -> pure Nothing
          Occur kv@(KV kNode vNode _) oi -> case oi of
            OIOptional -> do
              mt <- optGenKV kNode vNode
              case mt of
                Just (k, v) -> next (Map.insert k v m) ns
                Nothing -> same m ns
            OIZeroOrMore -> do
              mt <- optGenKV kNode vNode
              case mt of
                Just (k, v) -> next (Map.insert k v m) (n : ns)
                Nothing -> same m ns
            OIOneOrMore -> genNodes i m (kv : Occur kv OIZeroOrMore : ns)
            OIBounded mlb mub -> do
              let
                clampedPred 0 = 0
                clampedPred x = x - 1
                newLow = clampedPred <$> mlb
                newHigh = clampedPred <$> mub
                res
                  | maybe False (> 0) mlb = genNodes i m (kv : Occur kv (OIBounded newLow newHigh) : ns)
                  | maybe False (< 1) mub = same m ns
                  | otherwise = do
                      mt <- optGenKV kNode vNode
                      case mt of
                        Just (k, v) -> next (Map.insert k v m) (Occur kv (OIBounded newLow newHigh) : ns)
                        Nothing -> same m ns
              res
          node -> error $ "Unexpected node: " <> showSimple node
  mItems <- genNodes 0 Map.empty $ sortOn (Down . elemsNeeded) nodes
  case mItems of
    Just items -> SingleTerm <$> twiddleMap (Map.toList items)
    Nothing -> error "Failed to generate unique keys for map after max retries"

-- | Generate an array from a list of nodes
genArray :: HasCallStack => [CTree GenPhase] -> CBORGen RuleTerm
genArray nodes = do
  items <-
    singleTermList . flattenWrappedList
      <$> zipWithM
        (\i node -> withAntiGen (withAnnotation (T.pack $ show i)) $ genForCTree node)
        [0 :: Int ..]
        nodes
  case items of
    Just ts -> SingleTerm <$> twiddleList ts
    Nothing -> error "Something weird happened which shouldn't be possible"

-- | Generate a key-value pair
genKV :: HasCallStack => CTree GenPhase -> CTree GenPhase -> CBORGen RuleTerm
genKV key value = do
  kg <- withAntiGen (withAnnotation "key") $ genForCTree key
  vg <- withAntiGen (withAnnotation "value") $ genForCTree value
  case (kg, vg) of
    (SingleTerm k, SingleTerm v) -> pure $ PairTerm k v
    _ ->
      error $
        "Non single-term generated outside of group context: "
          <> showSimple key
          <> " => "
          <> showSimple value

-- | Generate a value from a range
genRange ::
  HasCallStack =>
  CTree GenPhase ->
  CTree GenPhase ->
  RangeBound ->
  CBORGen RuleTerm
genRange from to bounds
  | CTreeE (GenRef n) <- from = do
      from' <- resolveRef n
      genRange from' to bounds
  | CTreeE (GenRef n) <- to = do
      to' <- resolveRef n
      genRange from to' bounds
  | Just lo <- unIntLiteral from
  , Just hi <- unIntLiteral to
  , lo <= hi = do
      val <-
        case bounds of
          ClOpen -> genBetween (lo, pred hi)
          Closed -> genBetween (lo, hi)
      pure . SingleTerm $
        if val >= 0
          then TermUInt $ fromInteger val
          else TermNInt . fromJust $ toNInt val
  | Just lo <- unFloatLiteral from
  , Just hi <- unFloatLiteral to
  , lo <= hi = do
      val <- choose (lo, hi)
      SingleTerm
        <$> elements
          [ TermHalf $ realToFrac val
          , TermFloat $ realToFrac val
          , TermDouble $ realToFrac val
          ]
  | otherwise = error "Encountered range with invalid boundary values"

-- | Generate a value with a control operator applied
genControl ::
  HasCallStack =>
  CtlOp.CtlOp ->
  CTree GenPhase ->
  CTree GenPhase ->
  CBORGen RuleTerm
genControl op target controller = annotateTerm target $ do
  resolvedController <- case controller of
    CTreeE (GenRef n) -> withAntiGen dropNegativeGen $ resolveRef n
    x -> pure x
  case (op, resolvedController) of
    (CtlOp.Le, CTree.Literal (Value (VUInt n) _)) -> case target of
      CTree.Postlude PTUInt -> SingleTerm . TermUInt <$> choose (0, fromIntegral n)
      _ -> error "Cannot apply le operator to target"
    (CtlOp.Le, _) -> error $ "Invalid controller for .le operator: " <> showSimple controller
    (CtlOp.Lt, CTree.Literal (Value (VUInt n) _)) -> case target of
      CTree.Postlude PTUInt -> SingleTerm . TermUInt <$> choose (0, fromIntegral n - 1)
      _ -> error "Cannot apply lt operator to target"
    (CtlOp.Lt, _) -> error $ "Invalid controller for .lt operator: " <> showSimple controller
    (CtlOp.Size, CTree.Literal (Value (VUInt s) _)) -> do
      s' <- liftAntiGen $ pure s |! sized (\sz -> choose (0, fromIntegral sz) `suchThat` (/= s))
      genSized s' target
    (CtlOp.Size, CTree.CRange Range {rangeFrom, rangeTo}) ->
      case (rangeFrom, rangeTo) of
        (CTree.Literal (Value (VUInt f) _), CTree.Literal (Value (VUInt t) _)) -> do
          s <- liftAntiGen $ sized $ \sz ->
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
      enc <- genForCTree controller
      case enc of
        SingleTerm x -> fmap SingleTerm . twiddleBytes $ CBOR.toStrictByteString (encodeCBORTerm x)
        _ -> error "Controller does not correspond to a single term"
    (c, _) -> error $ "Controller not yet implemented: " <> show c

-- | Generate a value from an enum
genEnum :: HasCallStack => CTree GenPhase -> CBORGen RuleTerm
genEnum tree = case tree of
  CTree.Group trees -> do
    ix <- choose (0, length trees - 1)
    genForCTree $ trees !! ix
  _ -> error "Attempt to form an enum from something other than a group"

-- | Generate a tagged value
genTag :: HasCallStack => Word64 -> CTree GenPhase -> CBORGen RuleTerm
genTag t node = do
  omitTag <- liftAntiGen $ faultyBool False
  if omitTag
    then genForCTree node
    else do
      tag <- liftAntiGen $ faultyNum t
      enc <- case tag of
        n
          | n == 2 || n == 3 ->
              -- TODO remove this once `cborg` can decode indefinite bytes in bignums
              disableTwiddle $ genForCTree node
        _ -> genForCTree node
      case enc of
        SingleTerm x -> pure $ SingleTerm $ TermTag tag x
        _ -> error "Tag controller does not correspond to a single term"

genForNode :: HasCallStack => Name -> CBORGen RuleTerm
genForNode = genForCTree <=< resolveRef

-- | Take a reference and resolve it to the relevant Tree, following multiple
-- links if necessary.
resolveRef :: Name -> CBORGen (CTree GenPhase)
resolveRef n = do
  mRule <- lookupCddl n
  -- Since we follow a reference, we decrease the 'size' of the Gen monad.
  scale (\x -> max 0 $ x - 1) $
    case mRule of
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
generateFromName :: HasCallStack => Name -> CBORGen CBORTerm
generateFromName n = do
  mRule <- lookupCddl n
  case mRule of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val ->
      genForCTree val >>= \case
        SingleTerm x -> pure x
        _ ->
          error $
            "Tried to generate a top-level term for "
              <> show n
              <> ", but it does not correspond to a single term."

-- | Generate a 'RuleTerm' for the type bound to the given generic
-- parameter at the enclosing rule. Use this from inside a custom generator
-- attached to a generic rule.
generateFromGRef :: HasCallStack => GRef -> CBORGen RuleTerm
generateFromGRef ref = do
  mRule <- lookupGRef ref
  case mRule of
    Nothing -> error $ "Unbound generic reference: " <> show ref
    Just val -> genForCTree val

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
  CBORGen RuleTerm ->
  CBORGen RuleTerm
applyOccurenceIndicator OIOptional oldGen =
  sizeBiasedBool >>= \case
    False -> pure $ GroupTerm mempty
    True -> oldGen
applyOccurenceIndicator OIZeroOrMore oldGen =
  GroupTerm <$> listOfScaled oldGen
applyOccurenceIndicator OIOneOrMore oldGen =
  withAntiGen (fmap GroupTerm . listOfScaled1) oldGen
applyOccurenceIndicator (OIBounded mlb mub) oldGen =
  sized $ \sz -> do
    let
      lb = fromMaybe 0 mlb
      ub = fromMaybe (lb + fromIntegral sz) mub
    i <- fromIntegral <$> genBetween (lb, ub)
    GroupTerm <$> vectorOf i (scale (`div` (i + 1)) oldGen)

valueToTerm :: Value -> CBORGen CBORTerm
valueToTerm (Value x _) = valueVariantToTerm x

valueVariantToTerm :: ValueVariant -> CBORGen CBORTerm
valueVariantToTerm (VUInt i) = pure $ TermUInt i
valueVariantToTerm (VNInt i) = pure $ TermNInt i
valueVariantToTerm (VBignum i)
  | i >= 0 = TermTag 2 <$> twiddleBytes (unsignedToBytes i)
  -- RFC 8949 §3.4.3: tag 3 content n denotes -1 - n
  | otherwise = TermTag 3 <$> twiddleBytes (unsignedToBytes (-1 - i))
valueVariantToTerm (VFloat16 i) = pure $ TermHalf i
valueVariantToTerm (VFloat32 i) = pure $ TermFloat i
valueVariantToTerm (VFloat64 i) = pure $ TermDouble i
valueVariantToTerm (VText t) = twiddleString t
valueVariantToTerm (VBytes b) = twiddleBytes b
valueVariantToTerm (VBool False) = pure $ TermSimple 20
valueVariantToTerm (VBool True) = pure $ TermSimple 21
