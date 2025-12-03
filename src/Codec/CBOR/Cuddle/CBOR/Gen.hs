{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE ViewPatterns #-}

#if MIN_VERSION_random(1,3,0)
{-# OPTIONS_GHC -Wno-deprecations #-} -- Due to usage of `split`
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
#endif
-- | Generate example CBOR given a CDDL specification
module Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm, generateCBORTerm') where

import Capability.Reader
import Capability.Sink (HasSink)
import Capability.Source (HasSource, MonadState (..))
import Capability.State (HasState, get, modify)
import Codec.CBOR.Cuddle.CDDL (
  Name (..),
  OccurrenceIndicator (..),
  Value (..),
  ValueVariant (..),
 )
import Codec.CBOR.Cuddle.CDDL.CTree (CTree (..), CTreeRoot (..), PTerm (..), foldCTree)
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp qualified as CtlOp
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, XXCTree (..))
import Codec.CBOR.Term (Term (..))
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad (join, replicateM, (<=<))
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State.Strict (StateT, runStateT)
import Control.Monad.State.Strict qualified as MTL
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import GHC.Generics (Generic)
import System.Random.Stateful (
  Random,
  RandomGen (..),
  StateGenM (..),
  UniformRange (uniformRM),
  randomM,
  uniformByteStringM,
 )
#if MIN_VERSION_random(1,3,0)
import System.Random.Stateful (
  SplitGen (..)
  )
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (WrappedTerm (..), CBORGenerator (..))
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import GHC.Stack (HasCallStack)
#endif

type data MonoDropGen

newtype instance XXCTree MonoDropGen = MDGRef Name
  deriving (Show)

instance IndexMappable CTree MonoReferenced MonoDropGen where
  mapIndex = foldCTree mapExt mapIndex
    where
      mapExt (MRuleRef n) = CTreeE $ MDGRef n
      mapExt (MGenerator _ x) = mapIndex x

--------------------------------------------------------------------------------
-- Generator infrastructure
--------------------------------------------------------------------------------

-- | Generator context, parametrised over the type of the random seed
newtype GenEnv = GenEnv
  { cddl :: CTreeRoot MonoReferenced
  }
  deriving (Generic)

data GenState g = GenState
  { randomSeed :: g
  -- ^ Actual seed
  , depth :: Int
  -- ^ Depth of the generator. This measures the number of references we
  -- follow. As we go deeper into the tree, we try to reduce the likelihood of
  -- following recursive paths, and generate shorter lists where allowed by
  -- the occurrence bounds.
  }
  deriving (Generic)

instance RandomGen g => RandomGen (GenState g) where
  genWord8 = withRandomSeed genWord8
  genWord16 = withRandomSeed genWord16
  genWord32 = withRandomSeed genWord32
  genWord64 = withRandomSeed genWord64
  split = splitGenStateWith split

#if MIN_VERSION_random(1,3,0)
instance SplitGen g => SplitGen (GenState g) where
  splitGen = splitGenStateWith splitGen
#endif

splitGenStateWith :: (g -> (g, g)) -> GenState g -> (GenState g, GenState g)
splitGenStateWith f s =
  case f (randomSeed s) of
    (gen', gen) -> (s {randomSeed = gen'}, s {randomSeed = gen})

withRandomSeed :: (t -> (a, g)) -> GenState t -> (a, GenState g)
withRandomSeed f s =
  case f (randomSeed s) of
    (r, gen) -> (r, s {randomSeed = gen})

newtype M g a = M {runM :: StateT (GenState g) (Reader GenEnv) a}
  deriving (Functor, Applicative, Monad, MTL.MonadState (GenState g))
  deriving
    (HasSource "randomSeed" g, HasSink "randomSeed" g, HasState "randomSeed" g)
    via Field
          "randomSeed"
          ()
          (MonadState (StateT (GenState g) (Reader GenEnv)))
  deriving
    (HasSource "depth" Int, HasSink "depth" Int, HasState "depth" Int)
    via Field
          "depth"
          ()
          (MonadState (StateT (GenState g) (Reader GenEnv)))
  deriving
    ( HasSource "cddl" (CTreeRoot MonoReferenced)
    , HasReader "cddl" (CTreeRoot MonoReferenced)
    )
    via Field
          "cddl"
          ()
          (Lift (StateT (GenState g) (MonadReader (Reader GenEnv))))

runGen :: M g a -> GenEnv -> GenState g -> (a, GenState g)
runGen m env st = runReader (runStateT (runM m) st) env

evalGen :: M g a -> GenEnv -> GenState g -> a
evalGen m env = fst . runGen m env

--------------------------------------------------------------------------------
-- Wrappers around some Random function in Gen
--------------------------------------------------------------------------------

genUniformRM :: forall a g. (UniformRange a, RandomGen g) => (a, a) -> M g a
genUniformRM r = uniformRM r (StateGenM @(GenState g))

-- | Generate a random number in a given range, biased increasingly towards the
-- lower end as the depth parameter increases.
genDepthBiasedRM ::
  forall a g.
  (Ord a, UniformRange a, RandomGen g) =>
  (a, a) ->
  M g a
genDepthBiasedRM bounds = do
  d <- get @"depth"
  samples <- replicateM d (genUniformRM bounds)
  pure $ minimum samples

-- | Generates a bool, increasingly likely to be 'False' as the depth increases.
genDepthBiasedBool :: forall g. RandomGen g => M g Bool
genDepthBiasedBool = do
  d <- get @"depth"
  and <$> replicateM d genRandomM

genRandomM :: forall g a. (Random a, RandomGen g) => M g a
genRandomM = randomM (StateGenM @(GenState g))

genBytes :: forall g. RandomGen g => Int -> M g ByteString
genBytes n = uniformByteStringM n (StateGenM @(GenState g))

genText :: forall g. RandomGen g => Int -> M g Text
genText n = pure $ T.pack . take n . join $ repeat ['a' .. 'z']

--------------------------------------------------------------------------------
-- Postlude
--------------------------------------------------------------------------------

-- | Primitive types defined by the CDDL specification, with their generators
genPostlude :: RandomGen g => PTerm -> M g Term
genPostlude pt = case pt of
  PTBool ->
    genRandomM
      <&> TBool
  PTUInt ->
    genUniformRM (minBound :: Word32, maxBound)
      <&> TInteger
        . fromIntegral
  PTNInt ->
    genUniformRM
      (minBound :: Int, 0)
      <&> TInteger
        . fromIntegral
  PTInt ->
    genUniformRM (minBound :: Int, maxBound)
      <&> TInteger
        . fromIntegral
  PTHalf ->
    genUniformRM (-65504, 65504)
      <&> THalf
  PTFloat ->
    genRandomM
      <&> TFloat
  PTDouble ->
    genRandomM
      <&> TDouble
  PTBytes -> TBytes <$> genBytes 30
  PTText -> TString <$> genText 30
  PTAny -> pure $ TString "Any"
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

showDropGen :: CTree MonoReferenced -> String
showDropGen = show . mapIndex @_ @_ @MonoDropGen

--------------------------------------------------------------------------------
-- Generator functions
--------------------------------------------------------------------------------

genForCTree :: (HasCallStack, RandomGen g) => CTree MonoReferenced -> M g WrappedTerm
genForCTree (CTree.Literal v) = S <$> genValue v
genForCTree (CTree.Postlude pt) = S <$> genPostlude pt
genForCTree (CTree.Map nodes) = do
  items <- pairTermList . flattenWrappedList <$> traverse genForCTree nodes
  case items of
    Just ts ->
      let
        -- De-duplicate keys in the map.
        -- Per RFC7049:
        -- >> A map that has duplicate keys may be well-formed, but it is not
        -- >> valid, and thus it causes indeterminate decoding
        tsNodup = Map.toList $ Map.fromList ts
       in
        pure . S $ TMap tsNodup
    Nothing -> error "Single terms in map context"
genForCTree (CTree.Array nodes) = do
  items <- singleTermList . flattenWrappedList <$> traverse genForCTree nodes
  case items of
    Just ts -> pure . S $ TList ts
    Nothing -> error "Something weird happened which shouldn't be possible"
genForCTree (CTree.Choice (NE.toList -> nodes)) = do
  ix <- genUniformRM (0, length nodes - 1)
  genForCTree $ nodes !! ix
genForCTree (CTree.Group nodes) = G <$> traverse genForCTree nodes
genForCTree (CTree.KV key value _cut) = do
  kg <- genForCTree key
  vg <- genForCTree value
  case (kg, vg) of
    (S k, S v) -> pure $ P k v
    _ ->
      error $
        "Non single-term generated outside of group context: "
          <> showDropGen key
          <> " => "
          <> showDropGen value
genForCTree (CTree.Occur item occurs) =
  applyOccurenceIndicator occurs (genForCTree item)
genForCTree (CTree.Range from to _bounds) = do
  -- TODO Handle bounds correctly
  term1 <- genForCTree from
  term2 <- genForCTree to
  case (term1, term2) of
    (S (TInt a), S (TInt b))
      | a <= b -> genUniformRM (a, b) <&> S . TInt
      | otherwise -> error $ "invalid range, a > b\na = " <> show a <> "\nb = " <> show b
    (S (TInt a), S (TInteger b))
      | fromIntegral a <= b -> genUniformRM (fromIntegral a, b) <&> S . TInteger
      | otherwise -> error $ "invalid range, a > b\na = " <> show a <> "\nb = " <> show b
    (S (TInteger a), S (TInteger b))
      | a <= b -> genUniformRM (a, b) <&> S . TInteger
      | otherwise -> error $ "invalid range, a > b\na = " <> show a <> "\nb = " <> show b
    (S (THalf a), S (THalf b))
      | a <= b -> genUniformRM (a, b) <&> S . THalf
      | otherwise -> error $ "invalid range, a > b\na = " <> show a <> "\nb = " <> show b
    (S (TFloat a), S (TFloat b))
      | a <= b -> genUniformRM (a, b) <&> S . TFloat
      | otherwise -> error $ "invalid range, a > b\na = " <> show a <> "\nb = " <> show b
    (S (TDouble a), S (TDouble b))
      | a <= b -> genUniformRM (a, b) <&> S . TDouble
      | otherwise -> error $ "invalid range, a > b\na = " <> show a <> "\nb = " <> show b
    x -> error $ "Cannot apply range operator to non-numeric types: " <> show x
genForCTree (CTree.Control op target controller) = do
  case (op, controller) of
    (CtlOp.Le, CTree.Literal (Value (VUInt n) _)) -> case target of
      CTree.Postlude PTUInt -> S . TInteger <$> genUniformRM (0, fromIntegral n)
      _ -> error "Cannot apply le operator to target"
    (CtlOp.Le, _) -> error $ "Invalid controller for .le operator: " <> showDropGen controller
    (CtlOp.Lt, CTree.Literal (Value (VUInt n) _)) -> case target of
      CTree.Postlude PTUInt -> S . TInteger <$> genUniformRM (0, fromIntegral n - 1)
      _ -> error "Cannot apply lt operator to target"
    (CtlOp.Lt, _) -> error $ "Invalid controller for .lt operator: " <> showDropGen controller
    (CtlOp.Size, CTree.Literal (Value (VUInt n) _)) -> case target of
      CTree.Postlude PTText -> S . TString <$> genText (fromIntegral n)
      CTree.Postlude PTBytes -> S . TBytes <$> genBytes (fromIntegral n)
      CTree.Postlude PTUInt -> S . TInteger <$> genUniformRM (0, 2 ^ n - 1)
      _ -> error "Cannot apply size operator to target "
    (CtlOp.Size, CTree.Range {CTree.from, CTree.to}) -> do
      case (from, to) of
        (CTree.Literal (Value (VUInt f1) _), CTree.Literal (Value (VUInt t1) _)) -> case target of
          CTree.Postlude PTText ->
            genUniformRM (fromIntegral f1, fromIntegral t1)
              >>= (fmap (S . TString) . genText)
          CTree.Postlude PTBytes ->
            genUniformRM (fromIntegral f1, fromIntegral t1)
              >>= (fmap (S . TBytes) . genBytes)
          CTree.Postlude PTUInt ->
            S . TInteger
              <$> genUniformRM (fromIntegral f1, fromIntegral t1)
          _ -> error $ "Cannot apply size operator to target: " <> showDropGen target
        _ ->
          error $
            "Invalid controller for .size operator: "
              <> showDropGen controller
    (CtlOp.Size, _) ->
      error $
        "Invalid controller for .size operator: "
          <> showDropGen controller
    (CtlOp.Cbor, _) -> do
      enc <- genForCTree controller
      case enc of
        S x -> pure . S . TBytes . CBOR.toStrictByteString $ CBOR.encodeTerm x
        _ -> error "Controller does not correspond to a single term"
    _ -> genForCTree target
genForCTree (CTree.Enum tree) = do
  case tree of
    CTree.Group trees -> do
      ix <- genUniformRM (0, length trees)
      genForCTree $ trees !! ix
    _ -> error "Attempt to form an enum from something other than a group"
genForCTree (CTree.Unwrap node) = genForCTree node
genForCTree (CTree.Tag tag node) = do
  enc <- genForCTree node
  case enc of
    S x -> pure $ S $ TTagged tag x
    _ -> error "Tag controller does not correspond to a single term"
genForCTree (CTree.CTreeE (MRuleRef n)) = genForNode n
genForCTree (CTree.CTreeE (MGenerator (CBORGenerator gen) _)) = gen StateGenM

genForNode :: (HasCallStack, RandomGen g) => Name -> M g WrappedTerm
genForNode = genForCTree <=< resolveRef

-- | Take a reference and resolve it to the relevant Tree, following multiple
-- links if necessary.
resolveRef :: RandomGen g => Name -> M g (CTree MonoReferenced)
resolveRef n = do
  (CTreeRoot cddl) <- ask @"cddl"
  -- Since we follow a reference, we increase the 'depth' of the gen monad.
  modify @"depth" (+ 1)
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
genForName :: (HasCallStack, RandomGen g) => Name -> M g Term
genForName n = do
  (CTreeRoot cddl) <- ask @"cddl"
  case Map.lookup n cddl of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val ->
      genForCTree val >>= \case
        S x -> pure x
        _ ->
          error $
            "Tried to generate a top-level term for "
              <> show n
              <> ", but it does not correspond to a single term."

-- | Apply an occurence indicator to a group entry
applyOccurenceIndicator ::
  RandomGen g =>
  OccurrenceIndicator ->
  M g WrappedTerm ->
  M g WrappedTerm
applyOccurenceIndicator OIOptional oldGen =
  genDepthBiasedBool >>= \case
    False -> pure $ G mempty
    True -> oldGen
applyOccurenceIndicator OIZeroOrMore oldGen =
  genDepthBiasedRM (0 :: Int, 10) >>= \i ->
    G <$> replicateM i oldGen
applyOccurenceIndicator OIOneOrMore oldGen =
  genDepthBiasedRM (1 :: Int, 10) >>= \i ->
    G <$> replicateM i oldGen
applyOccurenceIndicator (OIBounded mlb mub) oldGen =
  genDepthBiasedRM (lo, fromMaybe (max 10 lo) mub)
    >>= \i -> G <$> replicateM (fromIntegral i) oldGen
  where
    lo = fromMaybe 0 mlb

genValue :: RandomGen g => Value -> M g Term
genValue (Value x _) = genValueVariant x

genValueVariant :: RandomGen g => ValueVariant -> M g Term
genValueVariant (VUInt i) = pure . TInt $ fromIntegral i
genValueVariant (VNInt i) = pure . TInt $ fromIntegral (-i)
genValueVariant (VBignum i) = pure $ TInteger i
genValueVariant (VFloat16 i) = pure . THalf $ i
genValueVariant (VFloat32 i) = pure . TFloat $ i
genValueVariant (VFloat64 i) = pure . TDouble $ i
genValueVariant (VText t) = pure $ TString t
genValueVariant (VBytes b) = pure $ TBytes b
genValueVariant (VBool b) = pure $ TBool b

--------------------------------------------------------------------------------
-- Generator functions
--------------------------------------------------------------------------------

generateCBORTerm :: (HasCallStack, RandomGen g) => CTreeRoot MonoReferenced -> Name -> g -> Term
generateCBORTerm cddl n stdGen =
  let genEnv = GenEnv {cddl}
      genState = GenState {randomSeed = stdGen, depth = 1}
   in evalGen (genForName n) genEnv genState

generateCBORTerm' ::
  (HasCallStack, RandomGen g) => CTreeRoot MonoReferenced -> Name -> g -> (Term, g)
generateCBORTerm' cddl n stdGen =
  let genEnv = GenEnv {cddl}
      genState = GenState {randomSeed = stdGen, depth = 1}
   in second randomSeed $ runGen (genForName n) genEnv genState
