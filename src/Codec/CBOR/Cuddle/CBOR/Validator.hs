{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.CBOR.Cuddle.CBOR.Validator (
  validateCBOR,
  validateFromName,
  validateFromGRef,
  ValidatorPhase,
  ValidatorInputError (..),
  displayValidatorInputError,
  ValidatorError (..),
  displayValidatorError,
) where

import Codec.CBOR.Cuddle.CBOR.Validator.Trace (
  ControlInfo (..),
  Evidenced (..),
  IsValidationTrace (..),
  ListValidationTrace (..),
  MapValidationTrace (..),
  SValidity (..),
  ValidationTrace (..),
  evidence,
  isValid,
  mapTrace,
 )
import Codec.CBOR.Cuddle.CBOR.Validator.Trace qualified as Trace
import Codec.CBOR.Cuddle.CDDL hiding (CDDL, Group, Rule)
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.CDDL.Custom.Core (MonadCddl (..), RuleTerm (..))
import Codec.CBOR.Cuddle.CDDL.Custom.Validator (
  CustomValidatorResult (..),
  TermValidator,
  ValidateEnv (..),
  Validator,
  ValidatorError (..),
  ValidatorPhase,
  XXCTree (..),
  displayValidatorError,
  runValidator,
  runValidatorM,
 )
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import Codec.CBOR.Term
import Control.Monad (guard)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Bifunctor (Bifunctor (..))
import Data.Bits hiding (And)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Function (on)
import Data.IntSet qualified as IS
import Data.List (maximumBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Word
import GHC.Float
import GHC.Stack (HasCallStack)
import Text.Regex.TDFA

--------------------------------------------------------------------------------
-- Main entry point

-- | Errors that prevent validation from running at all: the input could not be
-- decoded as CBOR, decoded successfully but had trailing garbage, the caller
-- asked us to validate against a top-level rule name not defined in the CDDL,
-- or the CDDL itself was malformed in a way the validator could not handle.
-- These are distinct from schema-level mismatches, which are reported through
-- 'Evidenced' 'ValidationTrace'.
data ValidatorInputError
  = CBORDecodeFailure DeserialiseFailure
  | LeftoverBytesAfterTerm BS.ByteString
  | UnknownTopLevelRule Name
  | MalformedCDDL ValidatorError
  deriving (Show)

-- | Human-readable rendering of a 'ValidatorInputError'.
displayValidatorInputError :: ValidatorInputError -> String
displayValidatorInputError = \case
  CBORDecodeFailure e -> "Could not decode input as CBOR: " <> show e
  LeftoverBytesAfterTerm rest -> "Leftover bytes after CBOR term: " <> show rest
  UnknownTopLevelRule (Name n) -> "No such top-level rule in the CDDL: " <> T.unpack n
  MalformedCDDL e -> "Malformed CDDL: " <> displayValidatorError e

validateCBOR ::
  BS.ByteString ->
  Name ->
  CTreeRoot ValidatorPhase ->
  Either ValidatorInputError (Evidenced ValidationTrace)
validateCBOR bs rule cddl@(CTreeRoot tree) =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict bs) of
    Left e -> Left $ CBORDecodeFailure e
    Right (rest, _)
      | not (BSL.null rest) -> Left . LeftoverBytesAfterTerm $ BSL.toStrict rest
    Right (_, term) ->
      case Map.lookup rule tree of
        Nothing -> Left $ UnknownTopLevelRule rule
        Just r -> first MalformedCDDL $ runValidatorM (validateTerm term r) cddl

-- | Validate a CBOR 'Term' against a top-level rule from inside a custom
-- validator.
validateFromName ::
  HasCallStack => Name -> Term -> Validator ()
validateFromName n term = do
  mRule <- lookupCddl n
  case mRule of
    Nothing -> throwError $ InvalidReference n
    Just rule -> validateAgainst term rule

-- | Validate a CBOR 'Term' against the type bound to the given generic
-- parameter at the enclosing rule. Use this from inside a custom validator
-- attached to a generic rule.
validateFromGRef ::
  HasCallStack => GRef -> Term -> Validator ()
validateFromGRef ref term = do
  mRule <- lookupGRef ref
  case mRule of
    Nothing -> throwError $ InvalidGenericReference ref
    Just rule -> validateAgainst term rule

validateAgainst :: Term -> CTree ValidatorPhase -> Validator ()
validateAgainst term rule = do
  res <- validateTerm term rule
  if isValid res
    then pure ()
    else fail $ "Validation failed for term: " <> show term

--------------------------------------------------------------------------------
-- Terms

-- | Core function that validates a CBOR term to a particular rule of the CDDL
-- spec
validateTerm ::
  Term ->
  CTree ValidatorPhase ->
  Validator (Evidenced ValidationTrace)
validateTerm term rule
  | CTreeE (VRuleRef n) <- rule =
      dereferenceAndValidate n (validateTerm term)
  | CTreeE (VValidator v _) <- rule =
      runCustomValidator (SingleTerm term) v
  | otherwise =
      case term of
        TInt i -> validateInteger (fromIntegral i) rule
        TInteger i -> validateInteger i rule
        TBytes b -> validateBytes b rule
        TBytesI b -> validateBytes (BSL.toStrict b) rule
        TString s -> validateText s rule
        TStringI s -> validateText (TL.toStrict s) rule
        TList ts -> validateList ts rule
        TListI ts -> validateList ts rule
        TMap ts -> validateMap ts rule
        TMapI ts -> validateMap ts rule
        TTagged w t -> validateTagged w t rule
        TBool b -> validateBool b rule
        TNull -> validateNull rule
        TSimple s -> validateSimple s rule
        THalf h -> validateHalf h rule
        TFloat h -> validateFloat h rule
        TDouble d -> validateDouble d rule

terminal :: CTree ValidatorPhase -> Evidenced ValidationTrace
terminal = evidence . TerminalRule . mapIndex

-- | Run a custom validator on a term. This is used by type-specific validators
-- when they encounter a 'VValidator' node after dereferencing a rule reference.
runCustomValidator ::
  RuleTerm ->
  TermValidator ->
  Validator (Evidenced ValidationTrace)
runCustomValidator term validator = do
  root <- asks veRoot
  case runValidator (validator term) root of
    CustomValidatorSuccess -> pure $ evidence Trace.CustomSuccess
    CustomValidatorFailure err -> pure $ evidence $ Trace.CustomFailure err

unapplicable :: CTree ValidatorPhase -> Evidenced ValidationTrace
unapplicable = evidence . UnapplicableRule . mapIndex

--------------------------------------------------------------------------------
-- Ints and integers

-- | Validation of an Int or Integer. CBOR categorizes every integral in `TInt`
-- or `TInteger` but it can be the case that we are decoding something that is
-- expected to be a `Word64` even if we get a `TInt`.
--
-- > ghci> encodeWord64 15
-- > [TkInt 15]
-- > ghci> encodeWord64 maxBound
-- > [TkInteger 18446744073709551615]
--
-- For this reason, we cannot assume that bounds or literals are going to be
-- Ints, so we convert everything to Integer.
validateInteger ::
  HasCallStack =>
  Integer ->
  CTree ValidatorPhase ->
  Validator (Evidenced ValidationTrace)
validateInteger i rule =
  case rule of
    CTreeE (VRuleRef n) -> dereferenceAndValidate n $ validateInteger i
    CTreeE (VValidator v _) -> runCustomValidator (SingleTerm $ TInteger i) v
    -- a = any
    Postlude PTAny -> pure $ terminal rule
    -- a = int
    Postlude PTInt
      | i >= nintMin && i <= uintMax -> pure $ terminal rule
    -- a = uint
    Postlude PTUInt
      | i >= 0 && i <= uintMax -> pure $ terminal rule
    -- a = nint
    Postlude PTNInt
      | i >= nintMin && i <= -1 -> pure $ terminal rule
    -- a = x
    Literal (Value (VUInt i') _) | i == fromIntegral i' -> pure $ terminal rule
    -- a = -x
    Literal (Value (VNInt i') _) | -i == fromIntegral i' -> pure $ terminal rule
    -- a = <big number>
    Literal (Value (VBignum i') _) | i == i' -> pure $ terminal rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateInteger i) op tgt ctrl (controlInteger i)
    -- a = foo / bar
    Choice opts -> validateChoice (validateInteger i) opts
    -- a = x..y
    Range low high bound ->
      case (low, high) of
        (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _))
          | n <= i && range bound i m -> pure $ terminal rule
          | otherwise -> pure $ unapplicable rule
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _))
          | -n <= i && range bound i m -> pure $ terminal rule
          | otherwise -> pure $ unapplicable rule
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _))
          | -n <= i && range bound i (-m) -> pure $ terminal rule
          | otherwise -> pure $ unapplicable rule
        (Literal (Value (VBignum n) _), Literal (Value (VUInt (fromIntegral -> m)) _))
          | n <= i && range bound i m -> pure $ terminal rule
          | otherwise -> pure $ unapplicable rule
        (Literal (Value (VBignum n) _), Literal (Value (VNInt (fromIntegral -> m)) _))
          | n <= i && range bound i (-m) -> pure $ terminal rule
          | otherwise -> pure $ unapplicable rule
        (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _))
          | n <= i && range bound i m -> pure $ terminal rule
          | otherwise -> pure $ unapplicable rule
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _))
          | (-n) <= i && range bound i m -> pure $ terminal rule
          | otherwise -> pure $ unapplicable rule
        (CTreeE (VRuleRef n), _) ->
          dereferenceAndValidate n $ \lo -> validateInteger i (Range lo high bound)
        (_, CTreeE (VRuleRef n)) ->
          dereferenceAndValidate n $ \hi -> validateInteger i (Range low hi bound)
        _ -> throwError IncompatibleRangeTypes
    -- a = &(x, y, z)
    Enum g ->
      case g of
        CTreeE (VRuleRef n) -> dereferenceAndValidate n $ validateInteger i
        Group g' -> validateInteger i (Choice (NE.fromList g'))
        _ -> throwError InvalidType
    -- a = x: y
    -- Note KV cannot appear on its own, but we will use this when validating
    -- lists.
    KV _ v _ -> validateInteger i v
    Tag 2 x -> validateBigInt x
    Tag 3 x -> validateBigInt x
    _ -> pure $ unapplicable rule
  where
    validateBigInt x = case x of
      CTreeE (VRuleRef n) -> dereferenceAndValidate n validateBigInt
      Postlude PTBytes -> pure $ terminal rule
      Control op tgt@(Postlude PTBytes) ctrl ->
        ctrlDispatch (validateBytes bs) op tgt ctrl (controlBytes bs)
        where
          -- TODO figure out a way to turn Integer into bytes or figure out why
          -- tagged bigints are decoded as integers in the first place
          bs = mempty
      _ -> pure $ unapplicable rule

-- | Controls for an Integer
controlInteger ::
  HasCallStack =>
  Integer ->
  CtlOp ->
  CTree ValidatorPhase ->
  Validator Bool
controlInteger i op (CTreeE (VRuleRef n)) =
  controlInteger i op =<< dereference n
controlInteger i Size ctrl =
  pure $ case ctrl of
    Literal (Value (VUInt sz) _) -> 0 <= i && i < 256 ^ sz
    _ -> False
controlInteger i Bits ctrl = do
  indices <- case ctrl of
    Literal (Value (VUInt i') _) -> pure [i']
    Choice nodes -> getIndicesOfChoice nodes
    Range ff tt incl -> getIndicesOfRange ff tt incl
    Enum g -> getIndicesOfEnum g
    _ -> throwError $ InvalidController Bits
  pure $ go (IS.fromList (map fromIntegral indices)) i 0
  where
    go _ 0 _ = True
    go indices n idx =
      let bitSet = testBit n 0
          allowed = not bitSet || IS.member idx indices
       in allowed && go indices (shiftR n 1) (idx + 1)
controlInteger i Lt ctrl =
  pure $ case ctrl of
    Literal (Value (VUInt i') _) -> i < fromIntegral i'
    Literal (Value (VNInt i') _) -> i < -fromIntegral i'
    Literal (Value (VBignum i') _) -> i < i'
    _ -> False
controlInteger i Gt ctrl =
  pure $ case ctrl of
    Literal (Value (VUInt i') _) -> i > fromIntegral i'
    Literal (Value (VNInt i') _) -> i > -fromIntegral i'
    Literal (Value (VBignum i') _) -> i > i'
    _ -> False
controlInteger i Le ctrl =
  pure $ case ctrl of
    Literal (Value (VUInt i') _) -> i <= fromIntegral i'
    Literal (Value (VNInt i') _) -> i <= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i <= i'
    _ -> False
controlInteger i Ge ctrl =
  pure $ case ctrl of
    Literal (Value (VUInt i') _) -> i >= fromIntegral i'
    Literal (Value (VNInt i') _) -> i >= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i >= i'
    _ -> False
controlInteger i Eq ctrl =
  pure $ case ctrl of
    Literal (Value (VUInt i') _) -> i == fromIntegral i'
    Literal (Value (VNInt i') _) -> i == -fromIntegral i'
    Literal (Value (VBignum i') _) -> i == i'
    _ -> False
controlInteger i Ne ctrl =
  pure $ case ctrl of
    Literal (Value (VUInt i') _) -> i /= fromIntegral i'
    Literal (Value (VNInt i') _) -> i /= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i /= i'
    _ -> False
controlInteger _ op _ = throwError $ UnsupportedControlOperator op

--------------------------------------------------------------------------------
-- Floating point (Float16, Float32, Float64)
--
-- As opposed to Integral types, there seems to be no ambiguity when encoding
-- and decoding floating-point numbers.

-- | Validating a `Float16`
validateHalf ::
  HasCallStack =>
  Float ->
  CTree ValidatorPhase ->
  Validator (Evidenced ValidationTrace)
validateHalf f (CTreeE (VRuleRef n)) = dereferenceAndValidate n $ validateHalf f
validateHalf f (CTreeE (VValidator v _)) = runCustomValidator (SingleTerm $ THalf f) v
validateHalf f rule =
  case rule of
    Postlude PTAny -> pure $ terminal rule
    Postlude PTHalf -> pure $ terminal rule
    Literal (Value (VFloat16 f') _) | f == f' -> pure $ terminal rule
    Choice opts -> validateChoice (validateHalf f) opts
    Control op tgt ctrl -> ctrlDispatch (validateHalf f) op tgt ctrl (controlHalf f)
    Range (CTreeE (VRuleRef n)) high bound ->
      dereferenceAndValidate n $ \lo -> validateHalf f $ Range lo high bound
    Range low (CTreeE (VRuleRef n)) bound ->
      dereferenceAndValidate n $ \hi -> validateHalf f $ Range low hi bound
    Range (Literal (Value (VFloat16 n) _)) (Literal (Value (VFloat16 m) _)) bound
      | n <= f && range bound f m -> pure $ terminal rule
    _ -> pure $ unapplicable rule

-- | Controls for `Float16`
controlHalf ::
  HasCallStack =>
  Float ->
  CtlOp ->
  CTree ValidatorPhase ->
  Validator Bool
controlHalf f op (CTreeE (VRuleRef n)) =
  controlHalf f op =<< dereference n
controlHalf f Eq ctrl =
  pure $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f == f'
    _ -> False
controlHalf f Ne ctrl =
  pure $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f /= f'
    _ -> False
controlHalf _ op _ = throwError $ InvalidController op

-- | Validating a `Float32`
validateFloat ::
  HasCallStack =>
  Float ->
  CTree ValidatorPhase ->
  Validator (Evidenced ValidationTrace)
validateFloat f (CTreeE (VRuleRef n)) =
  dereferenceAndValidate n $ validateFloat f
validateFloat f (CTreeE (VValidator v _)) = runCustomValidator (SingleTerm $ TFloat f) v
validateFloat f rule =
  case rule of
    Postlude PTAny -> pure $ terminal rule
    Postlude PTFloat -> pure $ terminal rule
    Literal (Value (VFloat32 f') _) | f == f' -> pure $ terminal rule
    Choice opts -> validateChoice (validateFloat f) opts
    Control op tgt ctrl -> ctrlDispatch (validateFloat f) op tgt ctrl (controlFloat f)
    Range (CTreeE (VRuleRef n)) high bound ->
      dereferenceAndValidate n $ \lo -> validateFloat f $ Range lo high bound
    Range low (CTreeE (VRuleRef n)) bound ->
      dereferenceAndValidate n $ \hi -> validateFloat f $ Range low hi bound
    Range (Literal (Value nv _)) (Literal (Value mv _)) bound
      | VFloat16 n <- nv
      , VFloat16 m <- mv
      , n <= f && range bound f m ->
          pure $ terminal rule
      | VFloat32 n <- nv
      , VFloat32 m <- mv
      , n <= f && range bound f m ->
          pure $ terminal rule
    _ -> pure $ unapplicable rule

-- | Controls for `Float32`
controlFloat ::
  HasCallStack =>
  Float ->
  CtlOp ->
  CTree ValidatorPhase ->
  Validator Bool
controlFloat f op (CTreeE (VRuleRef n)) =
  controlFloat f op =<< dereference n
controlFloat f Eq ctrl =
  pure $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f == f'
    Literal (Value (VFloat32 f') _) -> f == f'
    _ -> False
controlFloat f Ne ctrl =
  pure $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f /= f'
    Literal (Value (VFloat32 f') _) -> f /= f'
    _ -> False
controlFloat _ op _ = throwError $ InvalidController op

-- | Validating a `Float64`
validateDouble ::
  HasCallStack =>
  Double ->
  CTree ValidatorPhase ->
  Validator (Evidenced ValidationTrace)
validateDouble f (CTreeE (VRuleRef n)) =
  dereferenceAndValidate n $ validateDouble f
validateDouble f (CTreeE (VValidator v _)) = runCustomValidator (SingleTerm $ TDouble f) v
validateDouble f rule =
  case rule of
    Postlude PTAny -> pure $ terminal rule
    Postlude PTDouble -> pure $ terminal rule
    Literal (Value (VFloat64 f') _) | f == f' -> pure $ terminal rule
    Choice opts -> validateChoice (validateDouble f) opts
    Control op tgt ctrl -> ctrlDispatch (validateDouble f) op tgt ctrl (controlDouble f)
    Range (CTreeE (VRuleRef n)) high bound ->
      dereferenceAndValidate n $ \lo -> validateDouble f $ Range lo high bound
    Range low (CTreeE (VRuleRef n)) bound ->
      dereferenceAndValidate n $ \hi -> validateDouble f $ Range low hi bound
    Range (Literal (Value nv _)) (Literal (Value mv _)) bound
      | VFloat16 n <- nv
      , VFloat16 m <- mv
      , float2Double n <= f && range bound f (float2Double m) ->
          pure $ terminal rule
      | VFloat32 n <- nv
      , VFloat32 m <- mv
      , float2Double n <= f && range bound f (float2Double m) ->
          pure $ terminal rule
      | VFloat64 n <- nv
      , VFloat64 m <- mv
      , n <= f && range bound f m ->
          pure $ terminal rule
    _ -> pure $ unapplicable rule

-- | Controls for `Float64`
controlDouble ::
  HasCallStack =>
  Double ->
  CtlOp ->
  CTree ValidatorPhase ->
  Validator Bool
controlDouble f op (CTreeE (VRuleRef n)) =
  controlDouble f op =<< dereference n
controlDouble f Eq ctrl =
  pure $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f == float2Double f'
    Literal (Value (VFloat32 f') _) -> f == float2Double f'
    Literal (Value (VFloat64 f') _) -> f == f'
    _ -> False
controlDouble f Ne ctrl =
  pure $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f /= float2Double f'
    Literal (Value (VFloat32 f') _) -> f /= float2Double f'
    Literal (Value (VFloat64 f') _) -> f /= f'
    _ -> False
controlDouble _ op _ = throwError $ InvalidController op

--------------------------------------------------------------------------------
-- Bool

-- | Validating a boolean
validateBool ::
  Bool ->
  CTree ValidatorPhase ->
  Validator (Evidenced ValidationTrace)
validateBool b (CTreeE (VRuleRef n)) =
  dereferenceAndValidate n $ validateBool b
validateBool b (CTreeE (VValidator v _)) = runCustomValidator (SingleTerm $ TBool b) v
validateBool b rule =
  case rule of
    Postlude PTAny -> pure $ terminal rule
    Postlude PTBool -> pure $ terminal rule
    Literal (Value (VBool b') _) | b == b' -> pure $ terminal rule
    Control op tgt ctrl -> ctrlDispatch (validateBool b) op tgt ctrl (controlBool b)
    Choice opts -> validateChoice (validateBool b) opts
    _ -> pure $ unapplicable rule

-- | Controls for `Bool`
controlBool ::
  HasCallStack =>
  Bool ->
  CtlOp ->
  CTree ValidatorPhase ->
  Validator Bool
controlBool b op (CTreeE (VRuleRef n)) =
  controlBool b op =<< dereference n
controlBool b Eq ctrl =
  pure $ case ctrl of
    Literal (Value (VBool b') _) -> b == b'
    _ -> False
controlBool b Ne ctrl =
  pure $ case ctrl of
    Literal (Value (VBool b') _) -> b /= b'
    _ -> False
controlBool _ op _ = throwError $ InvalidController op

--------------------------------------------------------------------------------
-- Simple

-- | Validating a `TSimple`. It is unclear if this is used for anything else than `undefined`.
validateSimple ::
  Word8 ->
  CTree ValidatorPhase ->
  Validator (Evidenced ValidationTrace)
validateSimple i (CTreeE (VRuleRef n)) =
  dereferenceAndValidate n $ validateSimple i
validateSimple i (CTreeE (VValidator v _)) = runCustomValidator (SingleTerm $ TSimple i) v
validateSimple 23 rule =
  case rule of
    Postlude PTAny -> pure $ terminal rule
    Postlude PTUndefined -> pure $ terminal rule
    Choice opts -> validateChoice (validateSimple 23) opts
    _ -> pure $ unapplicable rule
validateSimple n _ =
  error $ "Found simple different to 23! please report this somewhere! Found: " <> show n

--------------------------------------------------------------------------------
-- Null/nil

-- | Validating nil
validateNull :: CTree ValidatorPhase -> Validator (Evidenced ValidationTrace)
validateNull (CTreeE (VRuleRef n)) =
  dereferenceAndValidate n validateNull
validateNull (CTreeE (VValidator v _)) = runCustomValidator (SingleTerm TNull) v
validateNull rule =
  case rule of
    Postlude PTAny -> pure $ terminal rule
    Postlude PTNil -> pure $ terminal rule
    Choice opts -> validateChoice validateNull opts
    _ -> pure $ unapplicable rule

--------------------------------------------------------------------------------
-- Bytes

-- | Validating a byte sequence
validateBytes :: BS.ByteString -> CTree ValidatorPhase -> Validator (Evidenced ValidationTrace)
validateBytes bs (CTreeE (VRuleRef n)) =
  dereferenceAndValidate n $ validateBytes bs
validateBytes bs (CTreeE (VValidator v _)) = runCustomValidator (SingleTerm $ TBytes bs) v
validateBytes bs rule =
  case rule of
    Postlude PTAny -> pure $ terminal rule
    Postlude PTBytes -> pure $ terminal rule
    Literal (Value (VBytes bs') _) | bs == bs' -> pure $ terminal rule
    Control op tgt ctrl -> ctrlDispatch (validateBytes bs) op tgt ctrl (controlBytes bs)
    Choice opts -> validateChoice (validateBytes bs) opts
    _ -> pure $ unapplicable rule

-- | Controls for byte strings
controlBytes ::
  HasCallStack =>
  BS.ByteString ->
  CtlOp ->
  CTree ValidatorPhase ->
  Validator Bool
controlBytes bs op (CTreeE (VRuleRef n)) =
  controlBytes bs op =<< dereference n
controlBytes bs op@Size ctrl =
  case ctrl of
    Literal (Value (VUInt sz) _) -> pure $ fromIntegral (BS.length bs) == sz
    Range (CTreeE (VRuleRef n)) high bound -> do
      lo <- dereference n
      controlBytes bs op $ Range lo high bound
    Range low (CTreeE (VRuleRef n)) bound -> do
      hi <- dereference n
      controlBytes bs op $ Range low hi bound
    Range low high bound ->
      let i = BS.length bs
       in pure $ case (low, high) of
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) ->
              boundPlacement i (Just n, Just m) bound == WithinBounds
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) ->
              boundPlacement i (Just $ -n, Just m) bound == WithinBounds
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) ->
              boundPlacement i (Just $ -n, Just $ -m) bound == WithinBounds
            _ -> False
    _ -> pure False
controlBytes bs Bits ctrl = do
  indices <- case ctrl of
    Literal (Value (VUInt i') _) -> pure [i']
    Choice nodes -> getIndicesOfChoice nodes
    Range ff tt incl -> getIndicesOfRange ff tt incl
    Enum g -> getIndicesOfEnum g
    _ -> throwError $ InvalidController Bits
  pure $ bitsControlCheck (map fromIntegral indices)
  where
    bitsControlCheck :: [Int] -> Bool
    bitsControlCheck allowedBits =
      let allowedSet = IS.fromList allowedBits
          totalBits = BS.length bs * 8
          isAllowedBit n =
            let byteIndex = n `shiftR` 3
                bitIndex = n .&. 7
             in case BS.indexMaybe bs byteIndex of
                  Just byte -> not (testBit byte bitIndex) || IS.member n allowedSet
                  Nothing -> True
       in all isAllowedBit [0 .. totalBits - 1]
controlBytes bs Cbor ctrl =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict bs) of
    Right (BSL.null -> True, term) -> isValid <$> validateTerm term ctrl
    _ -> pure False
controlBytes bs Cborseq ctrl =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict (BS.snoc (BS.cons 0x9f bs) 0xff)) of
    Right (BSL.null -> True, TListI terms) ->
      isValid <$> validateTerm (TList terms) (Array [Occur ctrl OIZeroOrMore])
    _ -> pure False
controlBytes _ op _ = throwError $ InvalidController op

--------------------------------------------------------------------------------
-- Text

-- | Validating text strings
validateText ::
  T.Text ->
  CTree ValidatorPhase ->
  Validator (Evidenced ValidationTrace)
validateText txt (CTreeE (VRuleRef n)) =
  dereferenceAndValidate n $ validateText txt
validateText txt (CTreeE (VValidator v _)) = runCustomValidator (SingleTerm $ TString txt) v
validateText txt rule =
  case rule of
    Postlude PTAny -> pure $ terminal rule
    Postlude PTText -> pure $ terminal rule
    Literal (Value (VText txt') _) | txt == txt' -> pure $ terminal rule
    Control op tgt ctrl -> ctrlDispatch (validateText txt) op tgt ctrl (controlText txt)
    Choice opts -> validateChoice (validateText txt) opts
    _ -> pure $ unapplicable rule

-- | Controls for text strings
controlText ::
  HasCallStack =>
  T.Text ->
  CtlOp ->
  CTree ValidatorPhase ->
  Validator Bool
controlText bs op (CTreeE (VRuleRef n)) =
  controlText bs op =<< dereference n
controlText bs op@Size ctrl =
  let bsSize = BS.length $ encodeUtf8 bs
   in case ctrl of
        Literal (Value (VUInt (fromIntegral -> sz)) _) -> pure $ bsSize == sz
        Range (CTreeE (VRuleRef n)) high bound -> do
          lo <- dereference n
          controlText bs op $ Range lo high bound
        Range low (CTreeE (VRuleRef n)) bound -> do
          hi <- dereference n
          controlText bs op $ Range low hi bound
        Range ff tt bound ->
          pure $ case (ff, tt) of
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) ->
              n <= T.length bs && range bound bsSize m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) ->
              -n <= T.length bs && range bound bsSize m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) ->
              -n <= T.length bs && range bound bsSize (-m)
            _ -> False
        _ -> throwError $ InvalidController op
controlText s Regexp ctrl =
  case ctrl of
    Literal (Value (VText rxp) _) -> case s =~ rxp :: (T.Text, T.Text, T.Text) of
      ("", s', "") -> pure $ s == s'
      _ -> pure False
    _ -> throwError $ InvalidController Regexp
controlText _ op _ = throwError $ UnsupportedControlOperator op

--------------------------------------------------------------------------------
-- Tagged values

-- | Validating a `TTagged`
validateTagged ::
  Word64 -> Term -> CTree ValidatorPhase -> Validator (Evidenced ValidationTrace)
validateTagged tag term (CTreeE (VRuleRef n)) =
  dereferenceAndValidate n $ validateTagged tag term
validateTagged tag term (CTreeE (VValidator v _)) = runCustomValidator (SingleTerm $ TTagged tag term) v
validateTagged tag term rule =
  case rule of
    Postlude PTAny -> pure $ terminal rule
    Tag tag' rule' ->
      if tag == tag'
        then mapTrace (TagTrace tag) <$> validateTerm term rule'
        else pure $ evidence $ InvalidTag tag' tag
    Choice opts -> validateChoice (validateTagged tag term) opts
    _ -> pure $ unapplicable rule

--------------------------------------------------------------------------------
-- Lists

isWithinBoundsInclusive :: Ord a => a -> Maybe a -> Maybe a -> Bool
isWithinBoundsInclusive x lb ub = maybe True (x >=) lb && maybe True (x <=) ub

isOptional :: CTree i -> Bool
isOptional (Occur _ oi) = case oi of
  OIOptional -> True
  OIZeroOrMore -> True
  OIBounded lb ub -> isWithinBoundsInclusive 0 lb ub
  _ -> False
isOptional _ = False

data BoundPlacement
  = BelowBounds
  | WithinBounds
  | AboveBounds
  deriving (Eq, Ord, Show)

boundPlacement :: Ord a => a -> (Maybe a, Maybe a) -> RangeBound -> BoundPlacement
boundPlacement _ (Nothing, Nothing) _ = WithinBounds
boundPlacement x (Just lo, mHi) bounds
  | lo > x = BelowBounds
  | otherwise = boundPlacement x (Nothing, mHi) bounds
boundPlacement x (Nothing, Just hi) bounds
  | range bounds x hi = WithinBounds
  | otherwise = AboveBounds

decrementBounds :: (Maybe Word64, Maybe Word64) -> OccurrenceIndicator
decrementBounds (lb, ub) = OIBounded (clampedPred <$> lb) (clampedPred <$> ub)
  where
    clampedPred 0 = 0
    clampedPred x = pred x

validateList ::
  [Term] ->
  CTree ValidatorPhase ->
  Validator (Evidenced ValidationTrace)
validateList terms (CTreeE (VRuleRef n)) =
  dereferenceAndValidate n $ validateList terms
validateList terms (CTreeE (VValidator v _)) = runCustomValidator (SingleTerm $ TList terms) v
validateList terms rule =
  case rule of
    Postlude PTAny -> pure $ terminal rule
    Array rules -> do
      (ev, _) <- validate finalize [] terms rules
      pure $ mapTrace ListTrace ev
      where
        finalize _ [] = pure $ evidence ListValidationDone
        finalize skipped (x : xs) = do
          let tryRule r = runMaybeT $ do
                Evidenced SInvalid trc <- lift $ validateTerm x (unwrapOccur r)
                guard $ measureProgress trc > mempty
                pure (mapIndex r, trc)
          bestAttempt <- bestBy (measureProgress . snd) <$> traverse tryRule skipped
          pure $ evidence $ ListValidationLeftoverTerms (x :| xs) bestAttempt
    Choice opts -> validateChoice (validateList terms) opts
    _ -> pure $ unapplicable rule
  where
    validate ::
      ([CTree ValidatorPhase] -> [Term] -> Validator (Evidenced ListValidationTrace)) ->
      [CTree ValidatorPhase] ->
      [Term] ->
      [CTree ValidatorPhase] ->
      Validator (Evidenced ListValidationTrace, [Term])
    validate f skipped tss [] = do
      ev <- f skipped tss
      pure (ev, tss)
    validate f skipped [] (r : rs)
      | isOptional r = validate f skipped [] rs
      | otherwise = pure (evidence . ListValidationUnappliedRule $ mapIndex r, [])
    validate f skipped tss@(t : ts) (r : rs) =
      validateRule r
      where
        consumeTerm ct g = do
          res <- validateTerm t ct
          case res of
            Evidenced SValid trc ->
              first (mapTrace $ ListValidationConsume (mapIndex r) trc) <$> g ts
            Evidenced SInvalid trc ->
              pure (evidence $ ListValidationMissingRequired (mapIndex ct) trc, tss)

        consumeGroup ns gp g = do
          (ev, leftover) <- validate (\_ _ -> pure $ evidence ListValidationDone) [] tss gp
          case ev of
            Evidenced SValid trc ->
              first (mapTrace $ ListValidationConsumeGroup (reverse ns) trc) <$> g leftover
            Evidenced SInvalid trc ->
              pure (evidence $ ListValidationBadGroup (reverse ns) trc, leftover)

        consume ct@(CTreeE (VRuleRef n)) g = goRef n []
          where
            goRef name ns = do
              deref <- dereference name
              case deref of
                Group gp -> consumeGroup (name : ns) gp g
                CTreeE (VRuleRef newName) -> goRef newName (name : ns)
                _ -> consumeTerm ct g
        consume (Group gp) g = consumeGroup [] gp g
        consume (KV _ v _) g = consume v g
        consume ct g = consumeTerm ct g
        continue l = validate f skipped l rs
        skipRule = validate f (r : skipped) tss rs
        rewriteRule newRule l = validate f skipped l (newRule : rs)
        validateRule =
          \case
            Occur ct oi -> case oi of
              OIOptional -> liftA2 (<>) (consume ct continue) skipRule
              OIZeroOrMore -> liftA2 (<>) (consume ct (rewriteRule r)) skipRule
              OIOneOrMore -> consume ct (rewriteRule (Occur ct OIZeroOrMore))
              OIBounded lb ub ->
                let bounds = (lb, ub)
                 in case boundPlacement 1 bounds Closed of
                      BelowBounds -> consume ct (rewriteRule (Occur ct $ decrementBounds bounds))
                      WithinBounds ->
                        liftA2
                          (<>)
                          (consume ct (rewriteRule (Occur ct $ decrementBounds bounds)))
                          skipRule
                      AboveBounds
                        | boundPlacement 0 (lb, ub) Closed == WithinBounds -> skipRule
                        | otherwise -> throwError InvalidRangeBounds
            _ -> consume r continue

--------------------------------------------------------------------------------
-- Maps

validateMap ::
  HasCallStack =>
  [(Term, Term)] ->
  CTree ValidatorPhase ->
  Validator (Evidenced ValidationTrace)
validateMap terms (CTreeE (VRuleRef n)) =
  dereferenceAndValidate n $ validateMap terms
validateMap terms (CTreeE (VValidator v _)) = runCustomValidator (SingleTerm $ TMap terms) v
validateMap terms rule =
  case rule of
    Postlude PTAny -> pure $ terminal rule
    Map rules -> mapTrace MapTrace <$> validate [] terms rules
    Choice opts -> validateChoice (validateMap terms) opts
    _ -> pure $ unapplicable rule
  where
    validate ::
      [CTree ValidatorPhase] ->
      [(Term, Term)] ->
      [CTree ValidatorPhase] ->
      Validator (Evidenced MapValidationTrace)
    validate _ [] [] = pure $ evidence MapValidationDone
    validate exhausted (kv : _) [] = do
      let tryRule r = runMaybeT $ do
            KV k v _ <- pure $ unwrapOccur r
            Evidenced SValid kTrc <- lift $ validateTerm (fst kv) k
            Evidenced SInvalid vTrc <- lift $ validateTerm (snd kv) v
            let trc = MapValidationInvalidValue (mapIndex r) kTrc vTrc
            guard $ measureProgress trc > mempty
            pure (mapIndex r, trc)
      bestAttempt <- bestBy (measureProgress . snd) <$> traverse tryRule exhausted
      pure $ evidence $ MapValidationLeftoverKVs kv bestAttempt
    validate exhausted [] rs =
      case NE.nonEmpty $ filter (not . isOptional) (exhausted <> rs) of
        Nothing -> pure $ evidence MapValidationDone
        Just requiredRules ->
          pure $ evidence $ MapValidationUnappliedRules (mapIndex <$> requiredRules)
    validate exhausted kvs@((tk, tv) : leftover) (r : rs) =
      let
        consume (KV k v _) f = do
          kRes <- validateTerm tk k
          case kRes of
            Evidenced SValid kTrc -> do
              vRes <- validateTerm tv v
              case vRes of
                Evidenced SValid vTrc ->
                  mapTrace (MapValidationConsume (mapIndex r) kTrc vTrc) <$> f leftover
                Evidenced SInvalid vTrc ->
                  pure $ evidence $ MapValidationInvalidValue (mapIndex r) kTrc vTrc
            Evidenced SInvalid _ ->
              pure $ evidence $ MapValidationUnappliedRules (NE.singleton $ mapIndex r)
        consume _ _ = throwError InvalidMapElement
        postponeRule l = validate (r : exhausted) l rs
        dropRule l = validate exhausted l rs
        resetDropRule l = validate [] l (exhausted <> rs)
        rewriteRule newRule l = validate [] l (newRule : exhausted <> rs)
       in
        case r of
          Occur ct oi ->
            case oi of
              OIOptional ->
                liftA2 (<>) (consume ct resetDropRule) (postponeRule kvs)
              OIZeroOrMore ->
                liftA2 (<>) (consume ct (rewriteRule r)) (postponeRule kvs)
              OIOneOrMore ->
                liftA2 (<>) (consume ct (rewriteRule (Occur ct OIZeroOrMore))) (postponeRule kvs)
              OIBounded mlb mub
                | Just lb <- mlb, Just ub <- mub, lb > ub -> throwError InvalidRangeBounds
                | otherwise -> case compare 0 <$> mub of
                    Just EQ -> dropRule kvs
                    Just GT -> throwError InvalidRangeBounds
                    _ ->
                      liftA2
                        (<>)
                        (consume ct (rewriteRule (Occur ct $ decrementBounds (mlb, mub))))
                        (postponeRule kvs)
          _ -> liftA2 (<>) (consume r resetDropRule) (postponeRule kvs)

--------------------------------------------------------------------------------
-- Choices

validateChoice ::
  (CTree ValidatorPhase -> Validator (Evidenced ValidationTrace)) ->
  NE.NonEmpty (CTree ValidatorPhase) ->
  Validator (Evidenced ValidationTrace)
validateChoice v = go 0
  where
    go :: Int -> NE.NonEmpty (CTree ValidatorPhase) -> Validator (Evidenced ValidationTrace)
    go i (choice NE.:| xs) = do
      res <- v choice
      case res of
        Evidenced SValid trc -> pure . evidence $ ChoiceBranch i trc
        err -> case xs of
          [] -> pure err
          y : ys -> do
            rest <- go (succ i) (y NE.:| ys)
            pure $ err <> rest

--------------------------------------------------------------------------------
-- Control helpers

-- | Validate both rules
ctrlAnd ::
  (CTree ValidatorPhase -> Validator (Evidenced ValidationTrace)) ->
  CTree ValidatorPhase ->
  CTree ValidatorPhase ->
  Validator (Evidenced ValidationTrace)
ctrlAnd v tgt ctrl = do
  res <- v tgt
  case res of
    (isValid -> True) -> v ctrl
    err -> pure err

-- | Dispatch to the appropriate control
ctrlDispatch ::
  (CTree ValidatorPhase -> Validator (Evidenced ValidationTrace)) ->
  CtlOp ->
  CTree ValidatorPhase ->
  CTree ValidatorPhase ->
  (CtlOp -> CTree ValidatorPhase -> Validator Bool) ->
  Validator (Evidenced ValidationTrace)
ctrlDispatch v And tgt ctrl _ = ctrlAnd v tgt ctrl
ctrlDispatch v Within tgt ctrl _ = ctrlAnd v tgt ctrl
ctrlDispatch v op tgt ctrl vctrl = do
  lhs <- v tgt
  rhs <- vctrl op ctrl
  pure $ case lhs of
    Evidenced SValid trc
      | rhs -> evidence $ ControlTrace (ControlInfo op (mapIndex ctrl)) trc
      | otherwise -> evidence $ UnsatisfiedControl op (mapIndex ctrl)
    err -> err

--------------------------------------------------------------------------------
-- Bits control

getIndicesOfChoice :: NE.NonEmpty (CTree ValidatorPhase) -> Validator [Word64]
getIndicesOfChoice = fmap concat . traverse go
  where
    go :: CTree ValidatorPhase -> Validator [Word64]
    go = \case
      Literal (Value (VUInt v) _) -> pure [fromIntegral v]
      KV _ v _ ->
        case v of
          CTreeE (VRuleRef n) -> go =<< dereference n
          Literal (Value (VUInt v') _) -> pure [fromIntegral v']
          _ -> throwError $ InvalidController Bits
      Range ff tt incl -> getIndicesOfRange ff tt incl
      Enum g -> getIndicesOfEnum g
      _ -> throwError $ InvalidController Bits

getIndicesOfRange ::
  CTree ValidatorPhase -> CTree ValidatorPhase -> RangeBound -> Validator [Word64]
getIndicesOfRange (CTreeE (VRuleRef n)) tt incl = do
  ff <- dereference n
  getIndicesOfRange ff tt incl
getIndicesOfRange ff (CTreeE (VRuleRef n)) incl = do
  tt <- dereference n
  getIndicesOfRange ff tt incl
getIndicesOfRange ff tt incl =
  case (ff, tt) of
    (Literal (Value (VUInt ff') _), Literal (Value (VUInt tt') _)) ->
      pure $ case incl of
        ClOpen -> init rng
        Closed -> rng
      where
        rng = [ff' .. tt']
    _ -> throwError $ InvalidController Bits

getIndicesOfEnum :: CTree ValidatorPhase -> Validator [Word64]
getIndicesOfEnum (CTreeE (VRuleRef n)) = getIndicesOfEnum =<< dereference n
getIndicesOfEnum g =
  case g of
    Group g' -> getIndicesOfChoice (fromJust $ NE.nonEmpty g')
    _ -> throwError $ InvalidController Bits

dereference ::
  Name ->
  Validator (CTree ValidatorPhase)
dereference n = do
  res <- lookupCddl n
  case res of
    Just r -> pure r
    Nothing -> throwError $ InvalidReference n

dereferenceAndValidate ::
  Name ->
  (CTree ValidatorPhase -> Validator (Evidenced ValidationTrace)) ->
  Validator (Evidenced ValidationTrace)
dereferenceAndValidate n f = do
  res <- dereference n
  mapTrace (ReferenceRule n) <$> f res

--------------------------------------------------------------------------------
-- Utils

range :: Ord a => RangeBound -> a -> a -> Bool
range Closed = (<=)
range ClOpen = (<)

-- | Strip a single layer of 'Occur' wrapping, if any.
unwrapOccur :: CTree i -> CTree i
unwrapOccur (Occur ct _) = ct
unwrapOccur ct = ct

-- | Pick the entry with the largest measure, or 'Nothing' if the input is empty.
bestBy :: Ord b => (a -> b) -> [Maybe a] -> Maybe a
bestBy measure = fmap (maximumBy (compare `on` measure)) . NE.nonEmpty . catMaybes
