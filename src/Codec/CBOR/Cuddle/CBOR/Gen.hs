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
module Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTermM) where

#if MIN_VERSION_random(1,3,0)
import System.Random.Stateful (
  StatefulGen (..), Uniform (..)
  )
#endif
import Codec.CBOR.Cuddle.CDDL (
  Name (..),
  OccurrenceIndicator (..),
  Value (..),
  ValueVariant (..),
 )
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (
  CBORGenerator (..),
  GenPhase,
  MonadCBORGen (..),
  WrappedTerm (..),
  XXCTree (..),
  getDepth,
  modifyDepth,
 )
import Codec.CBOR.Cuddle.CDDL.CTree (CTree (..), CTreeRoot (..), PTerm (..), foldCTree)
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp qualified as CtlOp
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Codec.CBOR.Term (Term (..))
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad (replicateM)
import Control.Monad.Reader (MonadReader (..), MonadTrans (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..), evalStateT)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.Random.Stateful (
  UniformRange (uniformRM),
  uniformByteStringM,
 )

type data GenSimple

newtype instance XXCTree GenSimple = MDGRef Name
  deriving (Show)

instance IndexMappable CTree GenPhase GenSimple where
  mapIndex = foldCTree mapExt mapIndex
    where
      mapExt (GenRef n) = CTreeE $ MDGRef n
      mapExt (GenCustom _ x) = mapIndex x

--------------------------------------------------------------------------------
-- Generator infrastructure
--------------------------------------------------------------------------------

-- | Generator context, parametrised over the type of the random seed
newtype GenEnv = GenEnv
  { cddl :: CTreeRoot GenPhase
  }
  deriving (Generic)

newtype CBORGenT m a = CBORGenT (ReaderT (CTreeRoot GenPhase) (StateT Int m) a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans CBORGenT where
  lift m = CBORGenT . ReaderT $ \_ -> StateT (\i -> (,i) <$> m)

instance Monad m => MonadCBORGen (CBORGenT m) where
  stateDepth = CBORGenT . state
  askCDDL = CBORGenT ask

newtype CGen g = CGen {unCGen :: g}

instance StatefulGen g m => StatefulGen (CGen g) (CBORGenT m) where
  uniformWord32R x (CGen g) = lift $ uniformWord32R x g
  uniformWord64R x (CGen g) = lift $ uniformWord64R x g
  uniformWord8 = lift . uniformWord8 . unCGen
  uniformWord16 = lift . uniformWord16 . unCGen
  uniformWord32 = lift . uniformWord32 . unCGen
  uniformWord64 = lift . uniformWord64 . unCGen
  uniformByteArrayM b s = lift . uniformByteArrayM b s . unCGen
  uniformShortByteString s = lift . uniformShortByteString s . unCGen

runCBORGenT :: Monad m => CTreeRoot GenPhase -> CBORGenT m a -> m a
runCBORGenT cddl (CBORGenT m) = evalStateT (runReaderT m cddl) 1

--------------------------------------------------------------------------------
-- Wrappers around some Random function in Gen
--------------------------------------------------------------------------------

-- | Generate a random number in a given range, biased increasingly towards the
-- lower end as the depth parameter increases.
genDepthBiasedRM ::
  ( StatefulGen g m
  , MonadCBORGen m
  , UniformRange a
  , Ord a
  ) =>
  (a, a) ->
  g ->
  m a
genDepthBiasedRM bounds g = do
  d <- getDepth
  samples <- replicateM d $ uniformRM bounds g
  pure $ minimum samples

-- | Generates a bool, increasingly likely to be 'False' as the depth increases.
genDepthBiasedBool :: (StatefulGen g m, MonadCBORGen m) => g -> m Bool
genDepthBiasedBool g = do
  d <- getDepth
  and <$> replicateM d (uniformM g)

--------------------------------------------------------------------------------
-- Postlude
--------------------------------------------------------------------------------

-- | Primitive types defined by the CDDL specification, with their generators
genPostlude :: StatefulGen g m => PTerm -> g -> m Term
genPostlude pt g = case pt of
  PTBool -> uniformM g <&> TBool
  PTUInt -> uniformRM (minBound :: Word32, maxBound) g <&> TInteger . fromIntegral
  PTNInt -> uniformRM (minBound :: Int, 0) g <&> TInteger . fromIntegral
  PTInt -> uniformRM (minBound :: Int, maxBound) g <&> TInteger . fromIntegral
  PTHalf -> uniformRM (-65504, 65504) g <&> THalf
  PTFloat -> uniformRM (-1e9, 1e9) g <&> TFloat
  PTDouble -> uniformRM (-1e9, 1e9) g <&> TDouble
  PTBytes -> do
    len <- uniformRM (0, 200) g
    TBytes <$> uniformByteStringM len g
  PTText -> do
    len <- uniformRM (0, 200) g
    TString <$> genText len g
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

showSimple :: CTree GenPhase -> String
showSimple = show . mapIndex @_ @_ @GenSimple

--------------------------------------------------------------------------------
-- Generator functions
--------------------------------------------------------------------------------

genText :: StatefulGen g m => Int -> g -> m Text
genText n g = decodeUtf8With (\_ _ -> Just '\xfffd') <$> uniformByteStringM n g

genForCTree ::
  (HasCallStack, StatefulGen g m, MonadCBORGen m) => CTree GenPhase -> g -> m WrappedTerm
genForCTree (CTree.Literal v) _ = pure . S $ genValue v
genForCTree (CTree.Postlude pt) g = S <$> genPostlude pt g
genForCTree (CTree.Map nodes) g = do
  items <- pairTermList . flattenWrappedList <$> traverse (`genForCTree` g) nodes
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
genForCTree (CTree.Array nodes) g = do
  items <- singleTermList . flattenWrappedList <$> traverse (`genForCTree` g) nodes
  case items of
    Just ts -> pure . S $ TList ts
    Nothing -> error "Something weird happened which shouldn't be possible"
genForCTree (CTree.Choice (NE.toList -> nodes)) g = do
  ix <- uniformRM (0, length nodes - 1) g
  genForCTree (nodes !! ix) g
genForCTree (CTree.Group nodes) g = G <$> traverse (`genForCTree` g) nodes
genForCTree (CTree.KV key value _cut) g = do
  kg <- genForCTree key g
  vg <- genForCTree value g
  case (kg, vg) of
    (S k, S v) -> pure $ P k v
    _ ->
      error $
        "Non single-term generated outside of group context: "
          <> showSimple key
          <> " => "
          <> showSimple value
genForCTree (CTree.Occur item occurs) g =
  applyOccurenceIndicator occurs g (genForCTree item g)
genForCTree (CTree.Range from to _bounds) g = do
  -- TODO Handle bounds correctly
  term1 <- genForCTree from g
  term2 <- genForCTree to g
  case (term1, term2) of
    (S (TInt a), S (TInt b))
      | a <= b -> uniformRM (a, b) g <&> S . TInt
    (S (TInt a), S (TInteger b))
      | fromIntegral a <= b -> uniformRM (fromIntegral a, b) g <&> S . TInteger
    (S (TInteger a), S (TInteger b))
      | a <= b -> uniformRM (a, b) g <&> S . TInteger
    (S (THalf a), S (THalf b))
      | a <= b -> uniformRM (a, b) g <&> S . THalf
    (S (TFloat a), S (TFloat b))
      | a <= b -> uniformRM (a, b) g <&> S . TFloat
    (S (TDouble a), S (TDouble b))
      | a <= b -> uniformRM (a, b) g <&> S . TDouble
    (a, b) -> error $ "invalid range (a = " <> show a <> ", b = " <> show b <> ")"
genForCTree (CTree.Control op target controller) g = do
  resolvedController <- case controller of
    CTreeE (GenRef n) -> resolveRef n
    x -> pure x
  case (op, resolvedController) of
    (CtlOp.Le, CTree.Literal (Value (VUInt n) _)) -> case target of
      CTree.Postlude PTUInt -> S . TInteger <$> uniformRM (0, fromIntegral n) g
      _ -> error "Cannot apply le operator to target"
    (CtlOp.Le, _) -> error $ "Invalid controller for .le operator: " <> showSimple controller
    (CtlOp.Lt, CTree.Literal (Value (VUInt n) _)) -> case target of
      CTree.Postlude PTUInt -> S . TInteger <$> uniformRM (0, fromIntegral n - 1) g
      _ -> error "Cannot apply lt operator to target"
    (CtlOp.Lt, _) -> error $ "Invalid controller for .lt operator: " <> showSimple controller
    (CtlOp.Size, CTree.Literal (Value (VUInt n) _)) -> case target of
      CTree.Postlude PTText -> S . TString <$> genText (fromIntegral n) g
      CTree.Postlude PTBytes -> S . TBytes <$> uniformByteStringM (fromIntegral n) g
      CTree.Postlude PTUInt -> S . TInteger <$> uniformRM (0, 2 ^ n - 1) g
      _ -> error "Cannot apply size operator to target "
    (CtlOp.Size, CTree.Range {CTree.from, CTree.to}) -> do
      case (from, to) of
        (CTree.Literal (Value (VUInt f1) _), CTree.Literal (Value (VUInt t1) _)) -> case target of
          CTree.Postlude PTText ->
            uniformRM (fromIntegral f1, fromIntegral t1) g
              >>= (fmap (S . TString) . (`genText` g))
          CTree.Postlude PTBytes ->
            uniformRM (fromIntegral f1, fromIntegral t1) g
              >>= (fmap (S . TBytes) . (`uniformByteStringM` g))
          CTree.Postlude PTUInt ->
            S . TInteger
              <$> uniformRM (fromIntegral f1, fromIntegral t1) g
          _ -> error $ "Cannot apply size operator to target: " <> showSimple target
        _ ->
          error $
            "Invalid controller for .size operator: "
              <> showSimple controller
    (CtlOp.Size, _) ->
      error $
        "Invalid controller for .size operator: "
          <> showSimple controller
    (CtlOp.Cbor, _) -> do
      enc <- genForCTree controller g
      case enc of
        S x -> pure . S . TBytes . CBOR.toStrictByteString $ CBOR.encodeTerm x
        _ -> error "Controller does not correspond to a single term"
    _ -> genForCTree target g
genForCTree (CTree.Enum tree) g = do
  case tree of
    CTree.Group trees -> do
      ix <- uniformRM (0, length trees - 1) g
      genForCTree (trees !! ix) g
    _ -> error "Attempt to form an enum from something other than a group"
genForCTree (CTree.Unwrap node) g = genForCTree node g
genForCTree (CTree.Tag tag node) g = do
  enc <- genForCTree node g
  case enc of
    S x -> pure $ S $ TTagged tag x
    _ -> error "Tag controller does not correspond to a single term"
genForCTree (CTree.CTreeE (GenRef n)) g = genForNode n g
genForCTree (CTree.CTreeE (GenCustom (CBORGenerator gen) _)) g = gen g

genForNode ::
  ( HasCallStack
  , StatefulGen g m
  , MonadCBORGen m
  ) =>
  Name -> g -> m WrappedTerm
genForNode n g = do
  ct <- resolveRef n
  genForCTree ct g

-- | Take a reference and resolve it to the relevant Tree, following multiple
-- links if necessary.
resolveRef ::
  MonadCBORGen m =>
  Name -> m (CTree GenPhase)
resolveRef n = do
  (CTreeRoot cddl) <- askCDDL
  -- Since we follow a reference, we increase the 'depth' of the gen monad.
  modifyDepth (+ 1)
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
genForName ::
  ( HasCallStack
  , StatefulGen g m
  , MonadCBORGen m
  ) =>
  Name ->
  g ->
  m Term
genForName n g = do
  (CTreeRoot cddl) <- askCDDL
  case Map.lookup n cddl of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val ->
      genForCTree val g >>= \case
        S x -> pure x
        _ ->
          error $
            "Tried to generate a top-level term for "
              <> show n
              <> ", but it does not correspond to a single term."

-- | Apply an occurence indicator to a group entry
applyOccurenceIndicator ::
  (StatefulGen g m, MonadCBORGen m) =>
  OccurrenceIndicator ->
  g ->
  m WrappedTerm ->
  m WrappedTerm
applyOccurenceIndicator OIOptional g oldGen =
  genDepthBiasedBool g >>= \case
    False -> pure $ G mempty
    True -> oldGen
applyOccurenceIndicator OIZeroOrMore g oldGen =
  genDepthBiasedRM (0 :: Int, 10) g >>= \i ->
    G <$> replicateM i oldGen
applyOccurenceIndicator OIOneOrMore g oldGen =
  genDepthBiasedRM (1 :: Int, 10) g >>= \i ->
    G <$> replicateM i oldGen
applyOccurenceIndicator (OIBounded mlb mub) g oldGen =
  genDepthBiasedRM (lo, fromMaybe (max 10 lo) mub) g
    >>= \i -> G <$> replicateM (fromIntegral i) oldGen
  where
    lo = fromMaybe 0 mlb

genValue :: Value -> Term
genValue (Value x _) = genValueVariant x

genValueVariant :: ValueVariant -> Term
genValueVariant (VUInt i) = TInt $ fromIntegral i
genValueVariant (VNInt i) = TInt $ fromIntegral (-i)
genValueVariant (VBignum i) = TInteger i
genValueVariant (VFloat16 i) = THalf i
genValueVariant (VFloat32 i) = TFloat i
genValueVariant (VFloat64 i) = TDouble i
genValueVariant (VText t) = TString t
genValueVariant (VBytes b) = TBytes b
genValueVariant (VBool b) = TBool b

--------------------------------------------------------------------------------
-- Generator functions
--------------------------------------------------------------------------------

generateCBORTermM ::
  (Monad m, StatefulGen g m) =>
  CTreeRoot GenPhase -> Name -> g -> m Term
generateCBORTermM cddl n g = runCBORGenT cddl (genForName n $ CGen g)
