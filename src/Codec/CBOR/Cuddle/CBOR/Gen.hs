{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

-- | Generate example CBOR given a CDDL specification
module Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm, generateCBORTerm') where

import Capability.Reader
import Capability.Sink (HasSink)
import Capability.Source (HasSource, MonadState (..))
import Capability.State (HasState, get, modify, state)
import Codec.CBOR.Cuddle.CDDL (
  Name (..),
  OccurrenceIndicator (..),
  Value (..),
  ValueVariant (..),
 )
import Codec.CBOR.Cuddle.CDDL.CTree (CTree, CTreeRoot' (..))
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp qualified as CtlOp
import Codec.CBOR.Cuddle.CDDL.Postlude (PTerm (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoRef (..))
import Codec.CBOR.Term (Term (..))
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad (join, replicateM, (<=<))
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State.Strict (StateT, runStateT)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (runIdentity))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import System.Random.Stateful (
  Random,
  RandomGen (genShortByteString, genWord32, genWord64),
  RandomGenM,
  StatefulGen (..),
  UniformRange (uniformRM),
  applyRandomGenM,
  randomM,
  uniformByteStringM,
 )

--------------------------------------------------------------------------------
-- Generator infrastructure
--------------------------------------------------------------------------------

-- | Generator context, parametrised over the type of the random seed
data GenEnv g = GenEnv
  { cddl :: CTreeRoot' Identity MonoRef
  , fakeSeed :: CapGenM g
  -- ^ Access the "fake" seed, necessary to recursively call generators
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

newtype M g a = M {runM :: StateT (GenState g) (Reader (GenEnv g)) a}
  deriving (Functor, Applicative, Monad)
  deriving
    (HasSource "randomSeed" g, HasSink "randomSeed" g, HasState "randomSeed" g)
    via Field
          "randomSeed"
          ()
          (MonadState (StateT (GenState g) (Reader (GenEnv g))))
  deriving
    (HasSource "depth" Int, HasSink "depth" Int, HasState "depth" Int)
    via Field
          "depth"
          ()
          (MonadState (StateT (GenState g) (Reader (GenEnv g))))
  deriving
    ( HasSource "cddl" (CTreeRoot' Identity MonoRef)
    , HasReader "cddl" (CTreeRoot' Identity MonoRef)
    )
    via Field
          "cddl"
          ()
          (Lift (StateT (GenState g) (MonadReader (Reader (GenEnv g)))))
  deriving
    (HasSource "fakeSeed" (CapGenM g), HasReader "fakeSeed" (CapGenM g))
    via Field
          "fakeSeed"
          ()
          (Lift (StateT (GenState g) (MonadReader (Reader (GenEnv g)))))

-- | Opaque type carrying the type of a pure PRNG inside a capability-style
-- state monad.
data CapGenM g = CapGenM

instance RandomGen g => StatefulGen (CapGenM g) (M g) where
  uniformWord64 _ = state @"randomSeed" genWord64
  uniformWord32 _ = state @"randomSeed" genWord32

  uniformShortByteString n _ = state @"randomSeed" (genShortByteString n)

instance RandomGen r => RandomGenM (CapGenM r) r (M r) where
  applyRandomGenM f _ = state @"randomSeed" f

runGen :: M g a -> GenEnv g -> GenState g -> (a, GenState g)
runGen m env st = runReader (runStateT (runM m) st) env

evalGen :: M g a -> GenEnv g -> GenState g -> a
evalGen m env = fst . runGen m env

asksM :: forall tag r m a. HasReader tag r m => (r -> m a) -> m a
asksM f = f =<< ask @tag

--------------------------------------------------------------------------------
-- Wrappers around some Random function in Gen
--------------------------------------------------------------------------------

genUniformRM :: forall a g. (UniformRange a, RandomGen g) => (a, a) -> M g a
genUniformRM = asksM @"fakeSeed" . uniformRM

-- | Generate a random number in a given range, biased increasingly towards the
-- lower end as the depth parameter increases.
genDepthBiasedRM ::
  forall a g.
  (Ord a, UniformRange a, RandomGen g) =>
  (a, a) ->
  M g a
genDepthBiasedRM bounds = do
  fs <- ask @"fakeSeed"
  d <- get @"depth"
  samples <- replicateM d (uniformRM bounds fs)
  pure $ minimum samples

-- | Generates a bool, increasingly likely to be 'False' as the depth increases.
genDepthBiasedBool :: forall g. RandomGen g => M g Bool
genDepthBiasedBool = do
  d <- get @"depth"
  and <$> replicateM d genRandomM

genRandomM :: forall g a. (Random a, RandomGen g) => M g a
genRandomM = asksM @"fakeSeed" randomM

genBytes :: forall g. RandomGen g => Int -> M g ByteString
genBytes n = asksM @"fakeSeed" $ uniformByteStringM n

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

data WrappedTerm
  = SingleTerm Term
  | PairTerm Term Term
  | GroupTerm [WrappedTerm]
  deriving (Eq, Show)

-- | Recursively flatten wrapped list. That is, expand any groups out to their
-- individual entries.
flattenWrappedList :: [WrappedTerm] -> [WrappedTerm]
flattenWrappedList [] = []
flattenWrappedList (GroupTerm xxs : xs) =
  flattenWrappedList xxs <> flattenWrappedList xs
flattenWrappedList (y : xs) = y : flattenWrappedList xs

pattern S :: Term -> WrappedTerm
pattern S t = SingleTerm t

-- | Convert a list of wrapped terms to a list of terms. If any 'PairTerm's are
-- present, we just take their "value" part.
singleTermList :: [WrappedTerm] -> Maybe [Term]
singleTermList [] = Just []
singleTermList (S x : xs) = (x :) <$> singleTermList xs
singleTermList (P _ y : xs) = (y :) <$> singleTermList xs
singleTermList _ = Nothing

pattern P :: Term -> Term -> WrappedTerm
pattern P t1 t2 = PairTerm t1 t2

-- | Convert a list of wrapped terms to a list of pairs of terms, or fail if any
-- 'SingleTerm's are present.
pairTermList :: [WrappedTerm] -> Maybe [(Term, Term)]
pairTermList [] = Just []
pairTermList (P x y : xs) = ((x, y) :) <$> pairTermList xs
pairTermList _ = Nothing

pattern G :: [WrappedTerm] -> WrappedTerm
pattern G xs = GroupTerm xs

--------------------------------------------------------------------------------
-- Generator functions
--------------------------------------------------------------------------------

genForCTree :: RandomGen g => CTree MonoRef -> M g WrappedTerm
genForCTree (CTree.Literal v) = S <$> genValue v
genForCTree (CTree.Postlude pt) = S <$> genPostlude pt
genForCTree (CTree.Map nodes) = do
  items <- pairTermList . flattenWrappedList <$> traverse genForNode nodes
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
  items <- singleTermList . flattenWrappedList <$> traverse genForNode nodes
  case items of
    Just ts -> pure . S $ TList ts
    Nothing -> error "Something weird happened which shouldn't be possible"
genForCTree (CTree.Choice (NE.toList -> nodes)) = do
  ix <- genUniformRM (0, length nodes - 1)
  genForNode $ nodes !! ix
genForCTree (CTree.Group nodes) = G <$> traverse genForNode nodes
genForCTree (CTree.KV key value _cut) = do
  kg <- genForNode key
  vg <- genForNode value
  case (kg, vg) of
    (S k, S v) -> pure $ P k v
    _ ->
      error $
        "Non single-term generated outside of group context: "
          <> show key
          <> " => "
          <> show value
genForCTree (CTree.Occur item occurs) =
  applyOccurenceIndicator occurs (genForNode item)
genForCTree (CTree.Range from to _bounds) = do
  -- TODO Handle bounds correctly
  term1 <- genForNode from
  term2 <- genForNode to
  case (term1, term2) of
    (S (TInt a), S (TInt b)) -> genUniformRM (a, b) <&> S . TInt
    (S (TInt a), S (TInteger b)) -> genUniformRM (fromIntegral a, b) <&> S . TInteger
    (S (TInteger a), S (TInteger b)) -> genUniformRM (a, b) <&> S . TInteger
    (S (THalf a), S (THalf b)) -> genUniformRM (a, b) <&> S . THalf
    (S (TFloat a), S (TFloat b)) -> genUniformRM (a, b) <&> S . TFloat
    (S (TDouble a), S (TDouble b)) -> genUniformRM (a, b) <&> S . TDouble
    x -> error $ "Cannot apply range operator to non-numeric types: " <> show x
genForCTree (CTree.Control op target controller) = do
  tt <- resolveIfRef target
  ct <- resolveIfRef controller
  case (op, ct) of
    (CtlOp.Le, CTree.Literal (Value (VUInt n) _)) -> case tt of
      CTree.Postlude PTUInt -> S . TInteger <$> genUniformRM (0, fromIntegral n)
      _ -> error "Cannot apply le operator to target"
    (CtlOp.Le, _) -> error $ "Invalid controller for .le operator: " <> show controller
    (CtlOp.Lt, CTree.Literal (Value (VUInt n) _)) -> case tt of
      CTree.Postlude PTUInt -> S . TInteger <$> genUniformRM (0, fromIntegral n - 1)
      _ -> error "Cannot apply lt operator to target"
    (CtlOp.Lt, _) -> error $ "Invalid controller for .lt operator: " <> show controller
    (CtlOp.Size, CTree.Literal (Value (VUInt n) _)) -> case tt of
      CTree.Postlude PTText -> S . TString <$> genText (fromIntegral n)
      CTree.Postlude PTBytes -> S . TBytes <$> genBytes (fromIntegral n)
      CTree.Postlude PTUInt -> S . TInteger <$> genUniformRM (0, 2 ^ n - 1)
      _ -> error "Cannot apply size operator to target "
    (CtlOp.Size, CTree.Range {CTree.from, CTree.to}) -> do
      f <- resolveIfRef from
      t <- resolveIfRef to
      case (f, t) of
        (CTree.Literal (Value (VUInt f1) _), CTree.Literal (Value (VUInt t1) _)) -> case tt of
          CTree.Postlude PTText ->
            genUniformRM (fromIntegral f1, fromIntegral t1)
              >>= (fmap (S . TString) . genText)
          CTree.Postlude PTBytes ->
            genUniformRM (fromIntegral f1, fromIntegral t1)
              >>= (fmap (S . TBytes) . genBytes)
          CTree.Postlude PTUInt ->
            S . TInteger
              <$> genUniformRM (fromIntegral f1, fromIntegral t1)
          _ -> error $ "Cannot apply size operator to target: " <> show tt
        _ ->
          error $
            "Invalid controller for .size operator: "
              <> show controller
    (CtlOp.Size, _) ->
      error $
        "Invalid controller for .size operator: "
          <> show controller
    (CtlOp.Cbor, _) -> do
      enc <- genForCTree ct
      case enc of
        S x -> pure . S . TBytes . CBOR.toStrictByteString $ CBOR.encodeTerm x
        _ -> error "Controller does not correspond to a single term"
    _ -> genForNode target
genForCTree (CTree.Enum node) = do
  tree <- resolveIfRef node
  case tree of
    CTree.Group nodes -> do
      ix <- genUniformRM (0, length nodes)
      genForNode $ nodes !! ix
    _ -> error "Attempt to form an enum from something other than a group"
genForCTree (CTree.Unwrap node) = genForCTree =<< resolveIfRef node
genForCTree (CTree.Tag tag node) = do
  enc <- genForNode node
  case enc of
    S x -> pure $ S $ TTagged tag x
    _ -> error "Tag controller does not correspond to a single term"

genForNode :: RandomGen g => CTree.Node MonoRef -> M g WrappedTerm
genForNode = genForCTree <=< resolveIfRef

-- | Take something which might be a reference and resolve it to the relevant
-- Tree, following multiple links if necessary.
resolveIfRef :: RandomGen g => CTree.Node MonoRef -> M g (CTree MonoRef)
resolveIfRef (MIt a) = pure a
resolveIfRef (MRuleRef n) = do
  (CTreeRoot cddl) <- ask @"cddl"
  -- Since we follow a reference, we increase the 'depth' of the gen monad.
  modify @"depth" (+ 1)
  case Map.lookup n cddl of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val -> resolveIfRef $ runIdentity val

-- | Generate a CBOR Term corresponding to a top-level name.
--
-- Since we apply this to a monomorphised CTree, the names must be monomorphic
-- terms in the original CDDL.
--
-- This will throw an error if the generated item does not correspond to a
-- single CBOR term (e.g. if the name resolves to a group, which cannot be
-- generated outside a context).
genForName :: RandomGen g => Name -> M g Term
genForName n = do
  (CTreeRoot cddl) <- ask @"cddl"
  case Map.lookup n cddl of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val ->
      genForNode (runIdentity val) >>= \case
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
  genDepthBiasedRM (fromMaybe 0 mlb :: Word64, fromMaybe 10 mub)
    >>= \i -> G <$> replicateM (fromIntegral i) oldGen

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
genValueVariant (VBytes b) = case Base16.decode b of
  Right bHex -> pure $ TBytes bHex
  Left err -> error $ "Unable to parse hex encoded bytestring: " <> err
genValueVariant (VBool b) = pure $ TBool b

--------------------------------------------------------------------------------
-- Generator functions
--------------------------------------------------------------------------------

generateCBORTerm :: RandomGen g => CTreeRoot' Identity MonoRef -> Name -> g -> Term
generateCBORTerm cddl n stdGen =
  let genEnv = GenEnv {cddl, fakeSeed = CapGenM}
      genState = GenState {randomSeed = stdGen, depth = 1}
   in evalGen (genForName n) genEnv genState

generateCBORTerm' :: RandomGen g => CTreeRoot' Identity MonoRef -> Name -> g -> (Term, g)
generateCBORTerm' cddl n stdGen =
  let genEnv = GenEnv {cddl, fakeSeed = CapGenM}
      genState = GenState {randomSeed = stdGen, depth = 1}
   in second randomSeed $ runGen (genForName n) genEnv genState
