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
module Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm) where

import Capability.Reader
import Capability.Sink (HasSink)
import Capability.Source (HasSource, MonadState (..))
import Capability.State (HasState, state)
import Codec.CBOR.Cuddle.CDDL
  ( Name (..),
    OccurrenceIndicator (..),
    Value (..),
  )
import Codec.CBOR.Cuddle.CDDL.CTree (CTree, CTreeRoot' (..))
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp qualified as CtlOp
import Codec.CBOR.Cuddle.CDDL.Postlude (PTerm (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoRef (..))
import Codec.CBOR.Term (Term (..))
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad (replicateM, (<=<))
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State.Strict (StateT, runStateT)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (runIdentity))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Random.Stateful
  ( Random,
    RandomGen (genShortByteString, genWord32, genWord64),
    RandomGenM,
    StatefulGen (..),
    StdGen,
    UniformRange (uniformRM),
    applyRandomGenM,
    randomM,
    uniformByteStringM,
  )
import qualified Data.ByteString.Base16 as Base16

--------------------------------------------------------------------------------
-- Generator infrastructure
--------------------------------------------------------------------------------

-- | Generator context, parametrised over the type of the random seed
data GenEnv g = GenEnv
  { cddl :: CTreeRoot' Identity MonoRef,
    -- | Access the "fake" seed, necessary to recursively call generators
    fakeSeed :: CapGenM g
  }
  deriving (Generic)

newtype GenState g = GenState
  { -- | Actual seed
    randomSeed :: g
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
    ( HasSource "cddl" (CTreeRoot' Identity MonoRef),
      HasReader "cddl" (CTreeRoot' Identity MonoRef)
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

instance (RandomGen g) => StatefulGen (CapGenM g) (M g) where
  uniformWord64 _ = state @"randomSeed" genWord64
  uniformWord32 _ = state @"randomSeed" genWord32

  uniformShortByteString n _ = state @"randomSeed" (genShortByteString n)

instance (RandomGen r) => RandomGenM (CapGenM r) r (M r) where
  applyRandomGenM f _ = state @"randomSeed" f

type Gen = M StdGen

runGen :: M g a -> GenEnv g -> GenState g -> (a, GenState g)
runGen (M m) env st = runReader (runStateT m st) env

evalGen :: M g a -> GenEnv g -> GenState g -> a
evalGen m env = fst . runGen m env

asksM :: forall tag r m a. (HasReader tag r m) => (r -> m a) -> m a
asksM f = f =<< ask @tag

--------------------------------------------------------------------------------
-- Wrappers around some Random function in Gen
--------------------------------------------------------------------------------

genUniformRM :: forall a g. (UniformRange a, RandomGen g) => (a, a) -> M g a
genUniformRM = asksM @"fakeSeed" . uniformRM

genRandomM :: forall g a. (Random a, RandomGen g) => M g a
genRandomM = asksM @"fakeSeed" randomM

genBytes :: forall g. (RandomGen g) => Int -> M g ByteString
genBytes n = asksM @"fakeSeed" $ uniformByteStringM n

genText :: forall g. (RandomGen g) => Int -> M g Text
genText n = pure $ T.pack $ take n ['a' ..]

--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

choose :: [a] -> Gen a
choose xs = genUniformRM (0, length xs) >>= \i -> pure $ xs !! i

oneOf :: [Gen a] -> Gen a
oneOf xs = genUniformRM (0, length xs) >>= \i -> xs !! i

oneOfGenerated :: Gen [a] -> Gen a
oneOfGenerated genXs = genXs >>= choose

--------------------------------------------------------------------------------
-- Postlude
--------------------------------------------------------------------------------

-- | Primitive types defined by the CDDL specification, with their generators
genPostlude :: PTerm -> Gen Term
genPostlude pt = case pt of
  PTBool ->
    genRandomM
      <&> TBool
  PTUInt ->
    genUniformRM (minBound :: Word, maxBound)
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
  PTText -> TBytes <$> genBytes 30
  PTAny -> pure $ TString "Any"
  PTNil -> pure TNull

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

genForCTree :: CTree MonoRef -> Gen WrappedTerm
genForCTree (CTree.Literal v) = S <$> genValue v
genForCTree (CTree.Postlude pt) = S <$> genPostlude pt
genForCTree (CTree.Map nodes) = do
  items <- pairTermList . flattenWrappedList <$> traverse genForNode nodes
  case items of
    Just ts -> pure . S $ TMap ts
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
    (CtlOp.Size, CTree.Literal (VUInt n)) -> case tt of
      CTree.Postlude PTText -> S . TString <$> genText (fromIntegral n)
      CTree.Postlude PTBytes -> S . TBytes <$> genBytes (fromIntegral n)
      CTree.Postlude PTUInt -> S . TInteger <$> genUniformRM (0, 2 ^ n - 1)
      _ -> error "Cannot apply size operator to target "
    (CtlOp.Size, CTree.Range {CTree.from, CTree.to}) -> do
      f <- resolveIfRef from
      t <- resolveIfRef to
      case (f, t) of
        (CTree.Literal (VUInt f1), CTree.Literal (VUInt t1)) -> case tt of
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

genForNode :: CTree.Node MonoRef -> Gen WrappedTerm
genForNode = genForCTree <=< resolveIfRef

-- | Take something which might be a reference and resolve it to the relevant
-- Tree, following multiple links if necessary.
resolveIfRef :: CTree.Node MonoRef -> Gen (CTree MonoRef)
resolveIfRef (MIt a) = pure a
resolveIfRef (MRuleRef n) = do
  (CTreeRoot cddl) <- ask @"cddl"
  case Map.lookup n cddl of
    Nothing -> error "Unbound reference"
    Just val -> resolveIfRef $ runIdentity val

-- | Generate a CBOR Term corresponding to a top-level name.
--
-- Since we apply this to a monomorphised CTree, the names must be monomorphic
-- terms in the original CDDL.
--
-- This will throw an error if the generated item does not correspond to a
-- single CBOR term (e.g. if the name resolves to a group, which cannot be
-- generated outside a context).
genForName :: Name -> Gen Term
genForName n = do
  (CTreeRoot cddl) <- ask @"cddl"
  case Map.lookup n cddl of
    Nothing -> error "Unbound reference"
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
  OccurrenceIndicator ->
  Gen WrappedTerm ->
  Gen WrappedTerm
applyOccurenceIndicator OIOptional oldGen =
  genRandomM >>= \case
    False -> pure $ G mempty
    True -> oldGen
applyOccurenceIndicator OIZeroOrMore oldGen =
  genUniformRM (0 :: Int, 10) >>= \i ->
    G <$> replicateM i oldGen
applyOccurenceIndicator OIOneOrMore oldGen =
  genUniformRM (0 :: Int, 10) >>= \i ->
    G <$> replicateM i oldGen
applyOccurenceIndicator (OIBounded mlb mub) oldGen =
  genUniformRM (fromMaybe 0 mlb :: Word64, fromMaybe 10 mub)
    >>= \i -> G <$> replicateM (fromIntegral i) oldGen

genValue :: Value -> Gen Term
genValue (VUInt i) = pure . TInt $ fromIntegral i
genValue (VNInt i) = pure . TInt $ fromIntegral (-i)
genValue (VBignum i) = pure $ TInteger i
genValue (VFloat16 i) = pure . THalf $ i
genValue (VFloat32 i) = pure . TFloat $ i
genValue (VFloat64 i) = pure . TDouble $ i
genValue (VText t) = pure $ TString t
genValue (VBytes b) = case Base16.decode b of 
  Right bHex -> pure $ TBytes bHex
  Left err -> error $ "Unable to parse hex encoded bytestring: " <> err

--------------------------------------------------------------------------------
-- Generator functions
--------------------------------------------------------------------------------

generateCBORTerm :: CTreeRoot' Identity MonoRef -> Name -> StdGen -> Term
generateCBORTerm cddl n stdGen =
  let genEnv = GenEnv {cddl, fakeSeed = CapGenM}
      genState = GenState {randomSeed = stdGen}
   in evalGen (genForName n) genEnv genState
