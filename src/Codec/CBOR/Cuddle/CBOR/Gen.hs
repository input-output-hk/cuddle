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
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (
  CBORGen (..),
  GenEnv (..),
  GenPhase,
  WrappedTerm (..),
  XXCTree (..),
  liftAntiGen,
  lookupCddl,
  runCBORGen,
  withAntiGen,
  withTwiddle,
 )
import Codec.CBOR.Cuddle.CDDL.CTree (CTree (..), CTreeRoot (..), PTerm (..), foldCTree)
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp qualified as CtlOp
import Codec.CBOR.Cuddle.CDDL.Resolve (XXCTree (..))
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Codec.CBOR.Term (Term (..))
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad ((<=<))
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (chr)
import Data.Functor ((<&>))
import Data.List (sortOn)
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
import GHC.Stack (HasCallStack)
import Numeric.Half (Half (..), fromHalf)
import Prettyprinter (Pretty (..), layoutCompact)
import Prettyprinter.Render.Text (renderStrict)
import System.Random.Stateful (Random, StatefulGen (..), runStateGen_, uniformByteStringM)
import Test.AntiGen (
  AntiGen,
  antiChoose,
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
  if isDenormalized half
    then genHalf
    else pure $ fromHalf half

twiddleString :: Text -> CBORGen Term
twiddleString t = do
  twiddle <- asks geTwiddle
  if twiddle
    then ($ t) <$> elements [TString, TStringI . TL.fromStrict]
    else pure $ TString t

twiddleList :: [Term] -> CBORGen Term
twiddleList t = do
  twiddle <- asks geTwiddle
  if twiddle
    then ($ t) <$> elements [TList, TListI]
    else pure $ TList t

twiddleBytes :: ByteString -> CBORGen Term
twiddleBytes t = do
  twiddle <- asks geTwiddle
  if twiddle
    then ($ t) <$> elements [TBytes, TBytesI . LBS.fromStrict]
    else pure $ TBytes t

twiddleMap :: [(Term, Term)] -> CBORGen Term
twiddleMap t = do
  twiddle <- asks geTwiddle
  if twiddle
    then ($ t) <$> elements [TMap, TMapI]
    else pure $ TMap t

genTerm :: CBORGen Term
genTerm =
  oneof
    [ TInt <$> choose (minBound, maxBound)
    , TInteger
        <$> oneof
          [ choose (toInteger (maxBound :: Int) + 1, toInteger (maxBound :: Word64))
          , choose (negate (toInteger (maxBound :: Word64)), toInteger (minBound :: Int) - 1)
          ]
    , twiddleBytes =<< genNBytes . getNonNegative =<< arbitrary
    , twiddleString . T.pack =<< arbitrary
    , twiddleList =<< listOf smallerTerm
    , twiddleMap =<< listOf ((,) <$> smallerTerm <*> smallerTerm)
    , TTagged <$> choose (6, maxBound :: Word64) <*> smallerTerm
    , TBool <$> arbitrary
    , pure TNull
    , TSimple <$> elements simple
    , THalf <$> genHalf
    , TFloat <$> arbitrary
    , TDouble <$> arbitrary
    ]
  where
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
genPostlude :: PTerm -> CBORGen Term
genPostlude pt = genPTerm =<< liftAntiGen (faultyPTerm pt)
  where
    genExcluding ls =
      elements (filter (`notElem` ls) [minBound .. maxBound])
    nonPInteger p =
      pure p |! genExcluding [PTUInt, PTNInt, PTInt, PTAny]
    -- \| Introduces a decision point that can change the type of the CBOR term that is generated
    faultyPTerm t = reweigh 0.1 . withAnnotation (renderStrict . layoutCompact $ pretty t) $
      case t of
        PTAny -> pure PTAny
        p@PTUInt -> nonPInteger p
        p@PTNInt -> nonPInteger p
        p@PTInt -> nonPInteger p
        p -> pure p |! genExcluding [PTAny, p]
    maxUInt = 2 ^ (64 :: Integer) - 1
    minNInt = -(2 ^ (64 :: Integer))
    genUInt = choose (0, maxUInt)
    genNInt = choose (minNInt, -1)
    genAboveUInt = choose (maxUInt + 1, 2 * maxUInt)
    genBelowNInt = choose (2 * minNInt, minNInt - 1)
    -- \| Given a CDDL postlude type, generates a value of that type
    genPTerm = \case
      PTBool -> TBool <$> arbitrary
      -- For integers, introduce decision points that sometimes generate
      -- out-of-bounds values
      PTUInt -> TInteger <$> liftAntiGen (genUInt |! oneof [genNInt, genBelowNInt, genAboveUInt])
      PTNInt -> TInteger <$> liftAntiGen (genNInt |! oneof [genUInt, genBelowNInt, genAboveUInt])
      PTInt -> TInteger <$> choose (minNInt, maxUInt)
      PTHalf -> THalf <$> genHalf
      PTFloat -> TFloat <$> arbitrary
      PTDouble -> TDouble <$> arbitrary
      PTBytes -> twiddleBytes =<< genBytes
      PTText -> twiddleString =<< genText
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

showSimple :: CTree GenPhase -> String
showSimple = show . mapIndex @_ @_ @GenSimple

-- | Remove all negative generators from the `AntiGen`.
dropNegativeGen :: AntiGen a -> AntiGen a
dropNegativeGen = liftGen . runAntiGen

--------------------------------------------------------------------------------
-- Generator functions
--------------------------------------------------------------------------------

genSized :: HasCallStack => Word64 -> CTree i -> CBORGen WrappedTerm
genSized s target = do
  case target of
    CTree.Postlude PTText -> fmap S . twiddleString =<< genNBytesText (fromIntegral s)
    CTree.Postlude PTBytes -> fmap S . twiddleBytes =<< genNBytes (fromIntegral s)
    CTree.Postlude PTUInt -> S . TInteger <$> choose (0, 256 ^ s - 1)
    _ -> error "Cannot apply size operator to target "

range :: Enum a => RangeBound -> a -> a -> (a, a)
range ClOpen x y = (x, pred y)
range Closed x y = (x, y)

genBetween :: (Integral a, Random a) => (a, a) -> CBORGen a
genBetween rng = liftAntiGen . withAnnotation "genBetween" $ do
  size <- liftGen getSize
  let
    sizeBounds :: Integral a => (a, a)
    sizeBounds = (0, fromIntegral size)
  antiChoose rng sizeBounds

annotate :: Text -> CBORGen b -> CBORGen b
annotate ann = withAntiGen $ withAnnotation ann

genForCTree :: HasCallStack => CTree GenPhase -> CBORGen WrappedTerm
genForCTree = \case
  CTree.Literal v -> annotate "literal" $ S <$> valueToTerm v
  CTree.Postlude pt -> annotate "postlude" $ S <$> genPostlude pt
  CTree.Map nodes -> annotate "map" $ genMap nodes
  CTree.Array nodes -> annotate "array" $ genArray nodes
  CTree.Choice (NE.toList -> nodes) -> annotate "choice" $ do
    ix <- choose (0, length nodes - 1)
    genForCTree $ nodes !! ix
  CTree.Group nodes -> annotate "group" $ G <$> traverse genForCTree nodes
  CTree.KV key value _cut -> annotate "kv" $ genKV key value
  CTree.Occur item occurs ->
    annotate "occur" $
      applyOccurenceIndicator occurs (genForCTree item)
  CTree.Range from to bounds -> annotate "range" $ genRange from to bounds
  CTree.Control op target controller -> annotate "control" $ genControl op target controller
  CTree.Enum tree -> annotate "enum" $ genEnum tree
  CTree.Unwrap node -> annotate "unwrap" $ genForCTree node
  CTree.Tag t node -> annotate "tag" $ genTag t node
  CTree.CTreeE (GenRef n) -> annotate (unName n) $ genForNode n
  CTree.CTreeE (GenGenerator gen _) -> annotate "custom_generator" gen

-- | Generate a map from a list of nodes
genMap ::
  HasCallStack => [CTree GenPhase] -> CBORGen WrappedTerm
genMap nodes = do
  let
    elemsNeeded KV {} = 1
    elemsNeeded (Occur _ OIOneOrMore) = 1
    elemsNeeded (Occur _ (OIBounded (Just lo) _)) = lo
    elemsNeeded _ = 0

    tryGenKV (0 :: Int) _ _ _ = pure Nothing
    tryGenKV nTries m kNode vNode = do
      let
        unS (S x) = x
        unS x = error $ "Expected single, got " <> show x
      k <- unS <$> scale (`div` 2) (withAntiGen (withAnnotation "key") $ genForCTree kNode)
      if Map.notMember k m
        then do
          v <- unS <$> scale (`div` 2) (withAntiGen (withAnnotation "value") $ genForCTree vNode)
          pure . Just $ (k, v)
        else tryGenKV (nTries - 1) m kNode vNode

    genNodes :: Int -> Map.Map Term Term -> [CTree GenPhase] -> CBORGen (Maybe (Map.Map Term Term))
    genNodes _ m [] = pure $ Just m
    genNodes !i !m (n : ns) =
      let
        ann = withAntiGen (withAnnotation (T.pack $ show i))
        cont x y = scale (\s -> max 0 (s - 1)) $ genNodes (i + 1) x y
        optGenKV kNode vNode = sized $ \sz ->
          frequency [(100, pure Nothing), (max 0 sz, ann $ tryGenKV 10 m kNode vNode)]
       in
        case n of
          KV kNode vNode _ -> do
            mKV <- ann $ tryGenKV 100 m kNode vNode
            case mKV of
              Just (k, v) -> cont (Map.insert k v m) ns
              Nothing -> pure Nothing
          Occur kv@(KV kNode vNode _) oi -> case oi of
            OIOptional -> do
              mt <- optGenKV kNode vNode
              case mt of
                Just (k, v) -> cont (Map.insert k v m) ns
                Nothing -> cont m ns
            OIZeroOrMore -> do
              mt <- optGenKV kNode vNode
              case mt of
                Just (k, v) -> cont (Map.insert k v m) (n : ns)
                Nothing -> cont m ns
            OIOneOrMore -> genNodes i m (kv : Occur kv OIZeroOrMore : ns)
            OIBounded mlb mub -> do
              let
                clampedPred 0 = 0
                clampedPred x = x - 1
                newLow = clampedPred <$> mlb
                newHigh = clampedPred <$> mub
                res
                  | maybe False (> 0) mlb = genNodes i m (kv : Occur kv (OIBounded newLow newHigh) : ns)
                  | maybe False (< 1) mub = genNodes i m ns
                  | otherwise = do
                      mt <- optGenKV kNode vNode
                      case mt of
                        Just (k, v) -> cont (Map.insert k v m) (Occur kv (OIBounded newLow newHigh) : ns)
                        Nothing -> genNodes i m ns
              res
          node -> error $ "Unexpected node: " <> showSimple node
  mItems <- genNodes 0 Map.empty $ sortOn (negate . elemsNeeded) nodes
  case mItems of
    Just items -> pure . S . TMap $ Map.toList items
    Nothing -> error "Failed to generate unique keys for map after max retries"

-- | Generate an array from a list of nodes
genArray :: HasCallStack => [CTree GenPhase] -> CBORGen WrappedTerm
genArray nodes = do
  items <-
    singleTermList . flattenWrappedList
      <$> traverse
        (\(i, node) -> withAntiGen (withAnnotation (T.pack $ show i)) $ genForCTree node)
        ([0 :: Int ..] `zip` nodes)
  case items of
    Just ts -> S <$> twiddleList ts
    Nothing -> error "Something weird happened which shouldn't be possible"

-- | Generate a key-value pair
genKV :: HasCallStack => CTree GenPhase -> CTree GenPhase -> CBORGen WrappedTerm
genKV key value = do
  kg <- withAntiGen (withAnnotation "key") $ genForCTree key
  vg <- withAntiGen (withAnnotation "value") $ genForCTree value
  case (kg, vg) of
    (S k, S v) -> pure $ P k v
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
  CBORGen WrappedTerm
genRange from to bounds = do
  term1 <- withAntiGen dropNegativeGen $ genForCTree from
  term2 <- withAntiGen dropNegativeGen $ genForCTree to
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

-- | Generate a value with a control operator applied
genControl ::
  HasCallStack =>
  CtlOp.CtlOp ->
  CTree GenPhase ->
  CTree GenPhase ->
  CBORGen WrappedTerm
genControl op target controller = do
  resolvedController <- case controller of
    CTreeE (GenRef n) -> withAntiGen dropNegativeGen $ resolveRef n
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
      s' <- liftAntiGen $ pure s |! sized (\sz -> choose (0, fromIntegral sz) `suchThat` (/= s))
      genSized s' target
    (CtlOp.Size, CTree.Range {CTree.from, CTree.to}) -> do
      case (from, to) of
        (CTree.Literal (Value (VUInt f) _), CTree.Literal (Value (VUInt t) _)) -> do
          s <- sized $ \sz ->
            liftAntiGen $
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
        S x -> pure . S . TBytes . CBOR.toStrictByteString $ CBOR.encodeTerm x
        _ -> error "Controller does not correspond to a single term"
    (c, _) -> error $ "Controller not yet implemented: " <> show c

-- | Generate a value from an enum
genEnum ::
  HasCallStack => CTree GenPhase -> CBORGen WrappedTerm
genEnum tree = case tree of
  CTree.Group trees -> do
    ix <- choose (0, length trees - 1)
    genForCTree $ trees !! ix
  _ -> error "Attempt to form an enum from something other than a group"

-- | Generate a tagged value
genTag ::
  HasCallStack => Word64 -> CTree GenPhase -> CBORGen WrappedTerm
genTag t node = do
  tag <- liftAntiGen $ faultyNum t
  enc <- case tag of
    n
      | n == 2 || n == 3 ->
          -- TODO remove this once `cborg` can decode indefinite bytes in bignums
          withTwiddle False $ genForCTree node
    _ -> genForCTree node
  case enc of
    S x -> pure $ S $ TTagged tag x
    _ -> error "Tag controller does not correspond to a single term"

genForNode :: HasCallStack => Name -> CBORGen WrappedTerm
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
generateFromName :: HasCallStack => CTreeRoot GenPhase -> Name -> AntiGen Term
generateFromName root@(CTreeRoot cddlMap) n = do
  let env = GenEnv {geRoot = root, geTwiddle = True}
  case Map.lookup n cddlMap of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val ->
      runCBORGen env (genForCTree val) >>= \case
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
  CBORGen WrappedTerm ->
  CBORGen WrappedTerm
applyOccurenceIndicator OIOptional oldGen =
  sizeBiasedBool >>= \case
    False -> pure $ G mempty
    True -> oldGen
applyOccurenceIndicator OIZeroOrMore oldGen =
  G <$> listOfScaled oldGen
applyOccurenceIndicator OIOneOrMore oldGen =
  withAntiGen (\g -> G <$> listOfScaled1 g) oldGen
applyOccurenceIndicator (OIBounded mlb mub) oldGen =
  sized $ \sz -> do
    let
      lb = fromMaybe 0 mlb
      ub = fromMaybe (lb + fromIntegral sz) mub
    i <- fromIntegral <$> genBetween (lb, ub)
    G <$> vectorOf i (scale (`div` (i + 1)) oldGen)

valueToTerm :: Value -> CBORGen Term
valueToTerm (Value x _) = valueVariantToTerm x

valueVariantToTerm :: ValueVariant -> CBORGen Term
valueVariantToTerm (VUInt i)
  | toInteger i <= toInteger (maxBound :: Int) = pure $ TInt (fromIntegral i)
  | otherwise = pure $ TInteger (fromIntegral i)
valueVariantToTerm (VNInt i)
  | -toInteger i >= toInteger (minBound :: Int) = pure $ TInt (-fromIntegral i)
  | otherwise = pure $ TInteger (-fromIntegral i)
valueVariantToTerm (VBignum i) = pure $ TInteger i
valueVariantToTerm (VFloat16 i) = pure $ THalf i
valueVariantToTerm (VFloat32 i) = pure $ TFloat i
valueVariantToTerm (VFloat64 i) = pure $ TDouble i
valueVariantToTerm (VText t) = twiddleString t
valueVariantToTerm (VBytes b) = twiddleBytes b
valueVariantToTerm (VBool b) = pure $ TBool b
