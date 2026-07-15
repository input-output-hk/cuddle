{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.CBOR.Cuddle.CBOR.Validator (
  validateCBOR,
  validateFromName,
  validateFromGRef,
  ValidatorPhase,
  ValidateCBORError (..),
) where

import Codec.CBOR.Cuddle.CBOR.Canonical (CanonicalTerm, toCanonical)
import Codec.CBOR.Cuddle.CBOR.Term (
  CBORTerm (..),
  NInt,
  bytesToUnsigned,
  decodeCBORTerm,
  fromNInt,
  mkTermArray,
  mkTermBytes,
  mkTermMap,
  mkTermNInt,
  mkTermString,
  mkTermTag,
  mkTermUInt,
  unwrapBytes,
 )
import Codec.CBOR.Cuddle.CBOR.Validator.Trace (
  ControlInfo (..),
  Evidenced (..),
  IsValidationTrace (..),
  ListValidationTrace (..),
  MapValidationTrace (..),
  SValidity (..),
  ValidationTrace (..),
  compareEvidencedProgress,
  evidence,
  isValid,
  mapTrace,
 )
import Codec.CBOR.Cuddle.CDDL hiding (CDDL, Group, Rule)
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.CDDL.Custom.Core (MonadCddl (..), RuleTerm (..))
import Codec.CBOR.Cuddle.CDDL.Custom.Validator (
  CustomValidatorResult (..),
  TermValidator,
  ValidateEnv (..),
  Validator,
  ValidatorPhase,
  XXCTree (..),
  runValidator,
 )
import Codec.CBOR.Cuddle.CDDL.Resolve (showSimple)
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Codec.CBOR.Read
import Control.Monad.Reader (asks)
import Data.Bifunctor (Bifunctor (..))
import Data.Bits hiding (And)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Function (on, (&))
import Data.IntSet qualified as IS
import Data.List (maximumBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Word
import GHC.Stack (HasCallStack)
import Numeric.Half (Half)
import Text.Regex.TDFA

data ValidateCBORError
  = DecodingFailed DeserialiseFailure
  | LeftoverBytes BSL.ByteString
  | RuleDoesNotExist Name
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Main entry point
validateCBOR ::
  HasCallStack =>
  BS.ByteString ->
  Name ->
  CTreeRoot ValidatorPhase ->
  Either ValidateCBORError (Evidenced ValidationTrace)
validateCBOR bs ruleName cddl@(CTreeRoot tree) =
  case deserialiseFromBytes decodeCBORTerm (BSL.fromStrict bs) of
    Left e -> Left $ DecodingFailed e
    Right (rest, term)
      | BSL.null rest -> case tree Map.!? ruleName of
          Just rule -> Right $ validateTerm cddl term rule
          Nothing -> Left $ RuleDoesNotExist ruleName
      | otherwise -> Left $ LeftoverBytes rest

-- | Validate a CBOR 'Term' against a top-level rule from inside a custom
-- validator.
validateFromName ::
  HasCallStack => Name -> CBORTerm -> Validator ()
validateFromName n term = do
  mRule <- lookupCddl n
  case mRule of
    Nothing -> fail $ "Unbound name: " <> show n
    Just rule -> validateAgainst term rule

-- | Validate a CBOR 'Term' against the type bound to the given generic
-- parameter at the enclosing rule. Use this from inside a custom validator
-- attached to a generic rule.
validateFromGRef ::
  HasCallStack => GRef -> CBORTerm -> Validator ()
validateFromGRef ref term = do
  mRule <- lookupGRef ref
  case mRule of
    Nothing -> fail $ "Unbound generic reference: " <> show ref
    Just rule -> validateAgainst term rule

validateAgainst :: CBORTerm -> CTree ValidatorPhase -> Validator ()
validateAgainst term rule = do
  cddl <- asks veRoot
  let res = validateTerm cddl term rule
  if isValid res
    then pure ()
    else fail $ "Validation failed for term: " <> show term

--------------------------------------------------------------------------------
-- Terms

-- | Core function that validates a CBOR term to a particular rule of the CDDL
-- spec
validateTerm ::
  CTreeRoot ValidatorPhase ->
  CBORTerm ->
  CTree ValidatorPhase ->
  Evidenced ValidationTrace
validateTerm cddl term rule
  | CTreeE (VRuleRef n) <- rule =
      dereferenceAndValidate cddl n (validateTerm cddl term)
  | CTreeE (VValidator v _) <- rule =
      runCustomValidator cddl (SingleTerm term) v
  | otherwise =
      case term of
        TermUInt i -> validateUInt cddl i rule
        TermNInt i -> validateNInt cddl i rule
        TermBytes bs -> validateBytes cddl bs rule
        TermBytesI bss -> validateBytes cddl (mconcat $ BSL.toStrict <$> bss) rule
        TermString t -> validateText cddl t rule
        TermStringI ts -> validateText cddl (mconcat $ TL.toStrict <$> ts) rule
        TermArray xs -> validateList cddl xs rule
        TermArrayI xs -> validateList cddl xs rule
        TermMap xs -> validateMap cddl xs rule
        TermMapI xs -> validateMap cddl xs rule
        TermTag i t -> validateTagged cddl i t rule
        TermSimple i -> validateSimple cddl i rule
        TermHalf x -> validateHalf cddl x rule
        TermFloat x -> validateFloat cddl x rule
        TermDouble x -> validateDouble cddl x rule

terminal :: CTree ValidatorPhase -> Evidenced ValidationTrace
terminal = evidence . TerminalRule . mapIndex

-- | Run a custom validator on a term. This is used by type-specific validators
-- when they encounter a 'VValidator' node after dereferencing a rule reference.
runCustomValidator ::
  CTreeRoot ValidatorPhase ->
  RuleTerm ->
  TermValidator ->
  Evidenced ValidationTrace
runCustomValidator cddl term validator =
  case runValidator (validator term) cddl of
    CustomValidatorSuccess -> evidence CustomSuccess
    CustomValidatorFailure err -> evidence $ CustomFailure err

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
validateUInt ::
  HasCallStack =>
  CTreeRoot ValidatorPhase ->
  Word64 ->
  CTree ValidatorPhase ->
  Evidenced ValidationTrace
validateUInt cddl i rule =
  case rule of
    CTreeE (VRuleRef n) -> dereferenceAndValidate cddl n $ validateUInt cddl i
    CTreeE (VValidator v _) -> runCustomValidator cddl (SingleTerm $ mkTermUInt i) v
    Postlude PTAny -> terminal rule
    Postlude PTInt -> terminal rule
    Postlude PTUInt -> terminal rule
    Literal (Value (VUInt j) _) | i' == toInteger j -> terminal rule
    Control op tgt ctrl -> ctrlDispatch (validateUInt cddl i) op tgt ctrl (controlInteger cddl i')
    Choice opts -> validateChoice (validateUInt cddl i) opts
    CRange (Range low high bound) ->
      case (low, high) of
        (Literal (Value (VUInt (toInteger -> n)) _), Literal (Value (VUInt (toInteger -> m)) _))
          | i' `isInRange` Range n m bound -> terminal rule
          | otherwise -> unapplicable rule
        (Literal (Value (VNInt (fromNInt -> n)) _), Literal (Value (VUInt (toInteger -> m)) _))
          | i' `isInRange` Range n m bound -> terminal rule
          | otherwise -> unapplicable rule
        (Literal (Value (VNInt _) _), Literal (Value (VNInt _) _)) -> unapplicable rule
        (Literal (Value VUInt {} _), Literal (Value VNInt {} _)) -> error "range types mismatch"
        (Literal (Value (VBignum n) _), Literal (Value (VUInt (toInteger -> m)) _))
          | i' `isInRange` Range n m bound -> terminal rule
          | otherwise -> unapplicable rule
        (Literal (Value (VBignum _) _), Literal (Value (VNInt _) _)) -> unapplicable rule
        (Literal (Value (VUInt (toInteger -> n)) _), Literal (Value (VBignum m) _))
          | i' `isInRange` Range n m bound -> terminal rule
          | otherwise -> unapplicable rule
        (Literal (Value (VNInt (fromNInt -> n)) _), Literal (Value (VBignum m) _))
          | i' `isInRange` Range n m bound -> terminal rule
          | otherwise -> unapplicable rule
        (CTreeE (VRuleRef n), _) ->
          dereferenceAndValidate cddl n $ \lo -> validateUInt cddl i (CRange (Range lo high bound))
        (_, CTreeE (VRuleRef n)) ->
          dereferenceAndValidate cddl n $ \hi -> validateUInt cddl i (CRange (Range low hi bound))
        (lo, hi) -> error $ "Unable to validate range: (" <> showSimple lo <> ", " <> showSimple hi <> ")"
    Enum g ->
      case g of
        CTreeE (VRuleRef n) -> dereferenceAndValidate cddl n $ validateUInt cddl i
        Group g' -> validateUInt cddl i (Choice (NE.fromList g'))
        _ -> error "Not yet implemented"
    KV _ v _ -> validateUInt cddl i v
    _ -> unapplicable rule
  where
    i' = toInteger i

validateNInt ::
  HasCallStack =>
  CTreeRoot ValidatorPhase ->
  NInt ->
  CTree ValidatorPhase ->
  Evidenced ValidationTrace
validateNInt cddl i rule =
  case rule of
    CTreeE (VRuleRef n) -> dereferenceAndValidate cddl n $ validateNInt cddl i
    CTreeE (VValidator v _) -> runCustomValidator cddl (SingleTerm $ mkTermNInt i) v
    Postlude PTAny -> terminal rule
    Postlude PTInt -> terminal rule
    Postlude PTNInt -> terminal rule
    Literal (Value (VNInt j) _) | i' == fromNInt j -> terminal rule
    Control op tgt ctrl -> ctrlDispatch (validateNInt cddl i) op tgt ctrl (controlInteger cddl i')
    Choice opts -> validateChoice (validateNInt cddl i) opts
    CRange (Range low high bound) ->
      case (low, high) of
        (Literal (Value (VUInt _) _), Literal (Value (VUInt _) _)) -> unapplicable rule
        (Literal (Value (VNInt (fromNInt -> n)) _), Literal (Value (VUInt (toInteger -> m)) _))
          | i' `isInRange` Range n m bound -> terminal rule
          | otherwise -> unapplicable rule
        (Literal (Value (VNInt (fromNInt -> n)) _), Literal (Value (VNInt (fromNInt -> m)) _))
          | i' `isInRange` Range n m bound -> terminal rule
          | otherwise -> unapplicable rule
        (Literal (Value VUInt {} _), Literal (Value VNInt {} _)) -> error "range types mismatch"
        (Literal (Value (VBignum n) _), Literal (Value (VUInt (toInteger -> m)) _))
          | i' `isInRange` Range n m bound -> terminal rule
          | otherwise -> unapplicable rule
        (Literal (Value (VBignum n) _), Literal (Value (VNInt (fromNInt -> m)) _))
          | i' `isInRange` Range n m bound -> terminal rule
          | otherwise -> unapplicable rule
        (Literal (Value (VUInt _) _), Literal (Value (VBignum _) _)) -> unapplicable rule
        (Literal (Value (VNInt (fromNInt -> n)) _), Literal (Value (VBignum m) _))
          | i' `isInRange` Range n m bound -> terminal rule
          | otherwise -> unapplicable rule
        (CTreeE (VRuleRef n), _) ->
          dereferenceAndValidate cddl n $ \lo -> validateNInt cddl i (CRange (Range lo high bound))
        (_, CTreeE (VRuleRef n)) ->
          dereferenceAndValidate cddl n $ \hi -> validateNInt cddl i (CRange (Range low hi bound))
        (lo, hi) -> error $ "Unable to validate range: (" <> showSimple lo <> ", " <> showSimple hi <> ")"
    Enum g ->
      case g of
        CTreeE (VRuleRef n) -> dereferenceAndValidate cddl n $ validateNInt cddl i
        Group g' -> validateNInt cddl i (Choice (NE.fromList g'))
        _ -> error "Not yet implemented"
    KV _ v _ -> validateNInt cddl i v
    _ -> unapplicable rule
  where
    i' = fromNInt i

-- | Controls for an Integer
controlInteger ::
  HasCallStack =>
  CTreeRoot ValidatorPhase ->
  Integer ->
  CtlOp ->
  CTree ValidatorPhase ->
  Bool
controlInteger cddl i op (CTreeE (VRuleRef n)) =
  controlInteger cddl i op $ dereference cddl n
controlInteger _ i Size ctrl =
  case ctrl of
    Literal (Value (VUInt sz) _) -> 0 <= i && i < 256 ^ sz
    _ -> False
controlInteger cddl i Bits ctrl = do
  let
    indices = case ctrl of
      Literal (Value (VUInt i') _) -> [i']
      Choice nodes -> getIndicesOfChoice cddl nodes
      CRange (Range ff tt incl) -> getIndicesOfRange cddl ff tt incl
      Enum g -> getIndicesOfEnum cddl g
      _ -> error "Not yet implemented"
  go (IS.fromList (map fromIntegral indices)) i 0
  where
    go _ 0 _ = True
    go indices n idx =
      let bitSet = testBit n 0
          allowed = not bitSet || IS.member idx indices
       in allowed && go indices (shiftR n 1) (idx + 1)
controlInteger _ i Lt ctrl
  | Just i' <- unIntLiteral ctrl = i < i'
  | otherwise = False
controlInteger _ i Gt ctrl
  | Just i' <- unIntLiteral ctrl = i > i'
  | otherwise = False
controlInteger _ i Le ctrl
  | Just i' <- unIntLiteral ctrl = i <= i'
  | otherwise = False
controlInteger _ i Ge ctrl
  | Just i' <- unIntLiteral ctrl = i >= i'
  | otherwise = False
controlInteger _ i Eq ctrl
  | Just i' <- unIntLiteral ctrl = i == i'
  | otherwise = False
controlInteger _ i Ne ctrl
  | Just i' <- unIntLiteral ctrl = i /= i'
  | otherwise = False
controlInteger _ _ _ ctrl = error $ "unexpected control: " <> showSimple ctrl

--------------------------------------------------------------------------------
-- Floating point (Float16, Float32, Float64)
--
-- As opposed to Integral types, there seems to be no ambiguity when encoding
-- and decoding floating-point numbers.

-- | Validating a `Float16`
validateHalf ::
  HasCallStack =>
  CTreeRoot ValidatorPhase ->
  Half ->
  CTree ValidatorPhase ->
  Evidenced ValidationTrace
validateHalf cddl f (CTreeE (VRuleRef n)) = dereferenceAndValidate cddl n $ validateHalf cddl f
validateHalf cddl f (CTreeE (VValidator v _)) = runCustomValidator cddl (SingleTerm $ TermHalf f) v
validateHalf cddl f rule =
  case rule of
    -- a = any
    Postlude PTAny -> terminal rule
    -- a = float16
    Postlude PTHalf -> terminal rule
    -- a = 0.5
    Literal (Value (VFloat16 f') _) | f == realToFrac f' -> terminal rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateHalf cddl f) opts
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateHalf cddl f) op tgt ctrl (controlHalf cddl f)
    -- a = x..y
    CRange (Range (CTreeE (VRuleRef n)) high bound) ->
      dereferenceAndValidate cddl n $ \lo -> validateHalf cddl f . CRange $ Range lo high bound
    CRange (Range low (CTreeE (VRuleRef n)) bound) ->
      dereferenceAndValidate cddl n $ \hi -> validateHalf cddl f . CRange $ Range low hi bound
    CRange (Range (unFloatLiteral -> Just n) (unFloatLiteral -> Just m) bound)
      | realToFrac f `isInRange` Range n m bound -> terminal rule
    _ -> unapplicable rule

-- | Controls for `Float16`
controlHalf ::
  HasCallStack =>
  CTreeRoot ValidatorPhase ->
  Half ->
  CtlOp ->
  CTree ValidatorPhase ->
  Bool
controlHalf cddl f op (CTreeE (VRuleRef n)) =
  controlHalf cddl f op $ dereference cddl n
controlHalf _ f Eq ctrl =
  case ctrl of
    Literal (Value (VFloat16 f') _) -> f == realToFrac f'
    _ -> False
controlHalf _ f Ne ctrl =
  case ctrl of
    Literal (Value (VFloat16 f') _) -> f /= realToFrac f'
    _ -> False
controlHalf _ _ op _ = error $ "Not yet implemented for half: " <> show op

-- | Validating a `Float32`
validateFloat ::
  HasCallStack =>
  CTreeRoot ValidatorPhase ->
  Float ->
  CTree ValidatorPhase ->
  Evidenced ValidationTrace
validateFloat cddl f (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateFloat cddl f
validateFloat cddl f (CTreeE (VValidator v _)) = runCustomValidator cddl (SingleTerm $ TermFloat f) v
validateFloat cddl f rule =
  case rule of
    -- a = any
    Postlude PTAny -> terminal rule
    -- a = float32
    Postlude PTFloat -> terminal rule
    -- a = 0.000000005
    -- TODO: it is unclear if smaller floats should also validate
    Literal (Value (VFloat32 f') _) | f == f' -> terminal rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateFloat cddl f) opts
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateFloat cddl f) op tgt ctrl (controlFloat cddl f)
    -- a = x..y
    -- TODO it is unclear if this should mix floating point types too
    CRange (Range (CTreeE (VRuleRef n)) high bound) ->
      dereferenceAndValidate cddl n $ \lo -> validateFloat cddl f . CRange $ Range lo high bound
    CRange (Range low (CTreeE (VRuleRef n)) bound) ->
      dereferenceAndValidate cddl n $ \hi -> validateFloat cddl f . CRange $ Range low hi bound
    CRange (Range (unFloatLiteral -> Just low) (unFloatLiteral -> Just high) bound)
      | realToFrac f `isInRange` Range low high bound -> terminal rule
    _ -> unapplicable rule

-- | Controls for `Float32`
controlFloat ::
  HasCallStack =>
  CTreeRoot ValidatorPhase ->
  Float ->
  CtlOp ->
  CTree ValidatorPhase ->
  Bool
controlFloat cddl f op (CTreeE (VRuleRef n)) =
  controlFloat cddl f op $ dereference cddl n
controlFloat _ f Eq ctrl
  | Just f' <- unFloatLiteral ctrl = realToFrac f == f'
  | otherwise = False
controlFloat _ f Ne ctrl
  | Just f' <- unFloatLiteral ctrl = realToFrac f /= f'
  | otherwise = False
controlFloat _ _ op _ = error $ "Not yet implemented for float: " <> show op

-- | Validating a `Float64`
validateDouble ::
  HasCallStack =>
  CTreeRoot ValidatorPhase ->
  Double ->
  CTree ValidatorPhase ->
  Evidenced ValidationTrace
validateDouble cddl f (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateDouble cddl f
validateDouble cddl f (CTreeE (VValidator v _)) = runCustomValidator cddl (SingleTerm $ TermDouble f) v
validateDouble cddl f rule =
  case rule of
    -- a = any
    Postlude PTAny -> terminal rule
    -- a = float64
    Postlude PTDouble -> terminal rule
    -- a = 0.0000000000000000000000000000000000000000000005
    -- TODO: it is unclear if smaller floats should also validate
    Literal (Value (VFloat64 f') _) | f == f' -> terminal rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateDouble cddl f) opts
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateDouble cddl f) op tgt ctrl (controlDouble cddl f)
    -- a = x..y
    -- TODO it is unclear if this should mix floating point types too
    CRange (Range (CTreeE (VRuleRef n)) high bound) ->
      dereferenceAndValidate cddl n $ \lo -> validateDouble cddl f . CRange $ Range lo high bound
    CRange (Range low (CTreeE (VRuleRef n)) bound) ->
      dereferenceAndValidate cddl n $ \hi -> validateDouble cddl f . CRange $ Range low hi bound
    CRange (Range (unFloatLiteral -> Just n) (unFloatLiteral -> Just m) bound)
      | f `isInRange` Range (realToFrac n) (realToFrac m) bound -> terminal rule
    _ -> unapplicable rule

-- | Controls for `Float64`
controlDouble ::
  HasCallStack =>
  CTreeRoot ValidatorPhase ->
  Double ->
  CtlOp ->
  CTree ValidatorPhase ->
  Bool
controlDouble cddl f op (CTreeE (VRuleRef n)) =
  controlDouble cddl f op $ dereference cddl n
controlDouble _ f Eq ctrl
  | Just f' <- unFloatLiteral ctrl = f == f'
  | otherwise = False
controlDouble _ f Ne ctrl
  | Just f' <- unFloatLiteral ctrl = f /= f'
  | otherwise = False
controlDouble _ _ op _ = error $ "Not yet implmented for double: " <> show op

--------------------------------------------------------------------------------
-- Bool

-- | Controls for `Bool`
controlBool ::
  HasCallStack =>
  CTreeRoot ValidatorPhase ->
  Bool ->
  CtlOp ->
  CTree ValidatorPhase ->
  Bool
controlBool cddl b op (CTreeE (VRuleRef n)) =
  controlBool cddl b op $ dereference cddl n
controlBool _ b Eq ctrl =
  case ctrl of
    Literal (Value (VBool b') _) -> b == b'
    _ -> False
controlBool _ b Ne ctrl =
  case ctrl of
    Literal (Value (VBool b') _) -> b /= b'
    _ -> False
controlBool _ _ op _ = error $ "Not yet implemented for bool: " <> show op

--------------------------------------------------------------------------------
-- Simple

validateSimple ::
  CTreeRoot ValidatorPhase ->
  Word8 ->
  CTree ValidatorPhase ->
  Evidenced ValidationTrace
validateSimple cddl i (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateSimple cddl i
validateSimple cddl i (CTreeE (VValidator v _)) = runCustomValidator cddl (SingleTerm $ TermSimple i) v
validateSimple cddl i rule =
  case rule of
    -- a = any
    Postlude PTAny -> terminal rule
    Postlude PTBool | isJust boolVal -> terminal rule
    Literal (Value (VBool b) _) | boolVal == Just b -> terminal rule
    Control op tgt ctrl | Just b <- boolVal -> ctrlDispatch (validateSimple cddl i) op tgt ctrl (controlBool cddl b)
    Postlude PTNil | i == 22 -> terminal rule
    -- a = undefined
    Postlude PTUndefined | i == 23 -> terminal rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateSimple cddl i) opts
    _ -> unapplicable rule
  where
    boolVal
      | i == 20 = Just False
      | i == 21 = Just True
      | otherwise = Nothing

--------------------------------------------------------------------------------
-- Bytes

-- | Validating a byte sequence
validateBytes ::
  CTreeRoot ValidatorPhase -> BS.ByteString -> CTree ValidatorPhase -> Evidenced ValidationTrace
validateBytes cddl bs (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateBytes cddl bs
validateBytes cddl bs (CTreeE (VValidator v _)) = runCustomValidator cddl (SingleTerm $ mkTermBytes bs) v
validateBytes cddl bs rule =
  case rule of
    -- a = any
    Postlude PTAny -> terminal rule
    -- a = bytes
    Postlude PTBytes -> terminal rule
    -- a = h'123456'
    Literal (Value (VBytes bs') _) | bs == bs' -> terminal rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateBytes cddl bs) op tgt ctrl (controlBytes cddl bs)
    -- a = foo / bar
    Choice opts -> validateChoice (validateBytes cddl bs) opts
    _ -> unapplicable rule

-- | Controls for byte strings
controlBytes ::
  HasCallStack =>
  CTreeRoot ValidatorPhase ->
  BS.ByteString ->
  CtlOp ->
  CTree ValidatorPhase ->
  Bool
controlBytes cddl bs op (CTreeE (VRuleRef n)) =
  controlBytes cddl bs op $ dereference cddl n
controlBytes cddl bs op@Size ctrl =
  case ctrl of
    Literal (Value (VUInt sz) _) -> fromIntegral (BS.length bs) == sz
    CRange (Range (CTreeE (VRuleRef n)) high bound) ->
      dereference cddl n & \lo -> controlBytes cddl bs op . CRange $ Range lo high bound
    CRange (Range low (CTreeE (VRuleRef n)) bound) ->
      dereference cddl n & \hi -> controlBytes cddl bs op . CRange $ Range low hi bound
    CRange (Range (unIntLiteral -> Just n) (unIntLiteral -> Just m) bound) ->
      let i = toInteger $ BS.length bs
       in boundPlacement i (Just n, Just m) bound == WithinBounds
    _ -> False
controlBytes cddl bs Bits ctrl = do
  let
    indices =
      case ctrl of
        Literal (Value (VUInt i') _) -> [i']
        Choice nodes -> getIndicesOfChoice cddl nodes
        CRange (Range ff tt incl) -> getIndicesOfRange cddl ff tt incl
        Enum g -> getIndicesOfEnum cddl g
        _ -> error "Not yet implemented"
  bitsControlCheck (map fromIntegral indices)
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
controlBytes cddl bs Cbor ctrl =
  case deserialiseFromBytes decodeCBORTerm (BSL.fromStrict bs) of
    Right (BSL.null -> True, term) -> isValid $ validateTerm cddl term ctrl
    _ -> False
controlBytes cddl bs Cborseq ctrl =
  case deserialiseFromBytes decodeCBORTerm (BSL.fromStrict (BS.snoc (BS.cons 0x9f bs) 0xff)) of
    Right (BSL.null -> True, TermArrayI terms) -> isValid $ validateTerm cddl (mkTermArray terms) (Array [Occur ctrl OIZeroOrMore])
    _ -> False
controlBytes _ _ op _ = error $ "Not yet implmented for bytes: " <> show op

--------------------------------------------------------------------------------
-- Text

-- | Validating text strings
validateText ::
  CTreeRoot ValidatorPhase ->
  T.Text ->
  CTree ValidatorPhase ->
  Evidenced ValidationTrace
validateText cddl txt (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateText cddl txt
validateText cddl txt (CTreeE (VValidator v _)) = runCustomValidator cddl (SingleTerm $ mkTermString txt) v
validateText cddl txt rule =
  case rule of
    -- a = any
    Postlude PTAny -> terminal rule
    -- a = text
    Postlude PTText -> terminal rule
    -- a = "foo"
    Literal (Value (VText txt') _) | txt == txt' -> terminal rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateText cddl txt) op tgt ctrl (controlText cddl txt)
    -- a = foo / bar
    Choice opts -> validateChoice (validateText cddl txt) opts
    _ -> unapplicable rule

-- | Controls for text strings
controlText ::
  HasCallStack =>
  CTreeRoot ValidatorPhase ->
  T.Text ->
  CtlOp ->
  CTree ValidatorPhase ->
  Bool
controlText cddl bs op (CTreeE (VRuleRef n)) =
  controlText cddl bs op $ dereference cddl n
controlText cddl bs op@Size ctrl =
  let bsSize = toInteger . BS.length $ encodeUtf8 bs
   in case ctrl of
        Literal (Value (VUInt (fromIntegral -> sz)) _) -> bsSize == sz
        CRange (Range (CTreeE (VRuleRef n)) high bound) ->
          dereference cddl n & \lo -> controlText cddl bs op . CRange $ Range lo high bound
        CRange (Range low (CTreeE (VRuleRef n)) bound) ->
          dereference cddl n & \hi -> controlText cddl bs op . CRange $ Range low hi bound
        CRange (Range (unIntLiteral -> Just n) (unIntLiteral -> Just m) bound) ->
          bsSize `isInRange` Range n m bound
        _ -> error "Invalid control value in .size"
controlText _ s Regexp ctrl =
  case ctrl of
    Literal (Value (VText rxp) _) -> case s =~ rxp :: (T.Text, T.Text, T.Text) of
      ("", s', "") -> s == s'
      _ -> False
    _ -> error "Invalid control value in .regexp"
controlText _ _ op _ = error $ "Not yet implemented for text: " <> show op

--------------------------------------------------------------------------------
-- Tagged values

-- | Validating a `TTagged`
validateTagged ::
  CTreeRoot ValidatorPhase -> Word64 -> CBORTerm -> CTree ValidatorPhase -> Evidenced ValidationTrace
validateTagged cddl tag term (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateTagged cddl tag term
validateTagged cddl tag term (CTreeE (VValidator v _)) = runCustomValidator cddl (SingleTerm $ mkTermTag tag term) v
validateTagged cddl tag term rule =
  case rule of
    Postlude PTAny -> terminal rule
    Literal (Value (VBignum i) _)
      | i >= 0
      , tag == 2
      , Just bs <- unwrapBytes term
      , bytesToUnsigned bs == i ->
          terminal rule
      | i < 0
      , tag == 3
      , Just bs <- unwrapBytes term
      , -- RFC 8949 §3.4.3: tag 3 content n denotes -1 - n
        -1 - bytesToUnsigned bs == i ->
          terminal rule
    Tag tag' rule' ->
      -- If the tag does not match, this is a direct fail
      if tag == tag'
        then mapTrace (TagTrace tag) $ validateTerm cddl term rule'
        else evidence $ InvalidTag tag' tag
    Choice opts -> validateChoice (validateTagged cddl tag term) opts
    _ -> unapplicable rule

-- --------------------------------------------------------------------------------
-- -- Lists

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
  where
    range ClOpen a upper = a < upper
    range Closed a upper = a <= upper

decrementBounds :: (Maybe Word64, Maybe Word64) -> OccurrenceIndicator
decrementBounds (lb, ub) = OIBounded (clampedPred <$> lb) (clampedPred <$> ub)
  where
    clampedPred 0 = 0
    clampedPred x = pred x

validateList ::
  CTreeRoot ValidatorPhase ->
  [CBORTerm] ->
  CTree ValidatorPhase ->
  Evidenced ValidationTrace
validateList cddl terms (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateList cddl terms
validateList cddl terms (CTreeE (VValidator v _)) = runCustomValidator cddl (SingleTerm $ mkTermArray terms) v
validateList cddl terms rule =
  case rule of
    Postlude PTAny -> terminal rule
    Array rules -> mapTrace ListTrace . fst $ validate finalize [] terms rules
      where
        finalize _ [] = evidence ListValidationDone
        finalize skipped (x : xs) =
          let leftovers = x :| xs
              unwrapOccur (Occur ct _) = ct
              unwrapOccur ct = ct
              attempts =
                [ (mapIndex r, trc)
                | r <- skipped
                , Evidenced SInvalid trc <- [validateTerm cddl x (unwrapOccur r)]
                , measureProgress trc > mempty
                ]
              bestAttempt = case attempts of
                [] -> Nothing
                _ -> Just $ maximumBy (compare `on` (measureProgress . snd)) attempts
           in evidence $ ListValidationLeftoverTerms leftovers bestAttempt
    Choice opts -> validateChoice (validateList cddl terms) opts
    _ -> unapplicable rule
  where
    validate ::
      ([CTree ValidatorPhase] -> [CBORTerm] -> Evidenced ListValidationTrace) ->
      [CTree ValidatorPhase] ->
      [CBORTerm] ->
      [CTree ValidatorPhase] ->
      (Evidenced ListValidationTrace, [CBORTerm])
    validate f skipped tss [] = (f skipped tss, tss)
    validate f skipped [] (r : rs)
      | isOptional r = validate f skipped [] rs
      | otherwise = (evidence . ListValidationUnappliedRule $ mapIndex r, [])
    validate f skipped tss@(t : ts) (r : rs) =
      let
        orElse x@(Evidenced SValid _, _) _ = x
        orElse x@(ex, _) y@(ey, _) =
          case compareEvidencedProgress ex ey of
            GT -> x
            _ -> y

        consumeTerm ct g = case validateTerm cddl t ct of
          Evidenced SValid trc -> first (mapTrace $ ListValidationConsume (mapIndex r) trc) $ g ts
          Evidenced SInvalid trc -> (evidence $ ListValidationMissingRequired (mapIndex ct) trc, tss)

        consumeGroup ns gp g = case validate (\_ _ -> evidence ListValidationDone) [] tss gp of
          (Evidenced SValid trc, leftover) -> first (mapTrace $ ListValidationConsumeGroup (reverse ns) trc) $ g leftover
          (Evidenced SInvalid trc, leftover) -> (evidence $ ListValidationBadGroup (reverse ns) trc, leftover)

        consume ct@(CTreeE (VRuleRef n)) g = go n []
          where
            go name ns =
              case dereference cddl name of
                Group gp -> consumeGroup (name : ns) gp g
                CTreeE (VRuleRef newName) -> go newName (name : ns)
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
              OIOptional -> consume ct continue `orElse` skipRule
              OIZeroOrMore -> consume ct (rewriteRule r) `orElse` skipRule
              OIOneOrMore -> consume ct (rewriteRule (Occur ct OIZeroOrMore))
              OIBounded lb ub ->
                let bounds = (lb, ub)
                 in case boundPlacement 1 bounds Closed of
                      BelowBounds -> consume ct (rewriteRule (Occur ct $ decrementBounds bounds))
                      WithinBounds -> consume ct (rewriteRule (Occur ct $ decrementBounds bounds)) `orElse` skipRule
                      AboveBounds
                        | boundPlacement 0 (lb, ub) Closed == WithinBounds -> skipRule
                        | otherwise -> error "Negative upper bound"
            _ -> consume r continue
       in
        validateRule r

--------------------------------------------------------------------------------
-- Maps

validateMap ::
  HasCallStack =>
  CTreeRoot ValidatorPhase ->
  [(CBORTerm, CBORTerm)] ->
  CTree ValidatorPhase ->
  Evidenced ValidationTrace
validateMap cddl terms (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateMap cddl terms
validateMap cddl terms (CTreeE (VValidator v _)) = runCustomValidator cddl (SingleTerm $ mkTermMap terms) v
validateMap cddl terms rule =
  case rule of
    Postlude PTAny -> terminal rule
    Map rules -> mapTrace MapTrace $ validate [] terms rules mempty
    Choice opts -> validateChoice (validateMap cddl terms) opts
    _ -> unapplicable rule
  where
    validate ::
      [CTree ValidatorPhase] ->
      [(CBORTerm, CBORTerm)] ->
      [CTree ValidatorPhase] ->
      Set CanonicalTerm ->
      Evidenced MapValidationTrace
    validate _ [] [] _ = evidence MapValidationDone
    validate exhausted (kv : _) [] _ =
      let
        unwrapOccur (Occur ct _) = ct
        unwrapOccur ct = ct
        attempts =
          [ (mapIndex r, MapValidationInvalidValue (mapIndex r) kTrc vTrc)
          | r <- exhausted
          , KV k v _ <- [unwrapOccur r]
          , Evidenced SValid kTrc <- [validateTerm cddl (fst kv) k]
          , Evidenced SInvalid vTrc <- [validateTerm cddl (snd kv) v]
          , measureProgress (MapValidationInvalidValue (mapIndex r) kTrc vTrc) > mempty
          ]
        bestAttempt = case attempts of
          [] -> Nothing
          _ -> Just $ maximumBy (compare `on` (measureProgress . snd)) attempts
       in
        evidence $ MapValidationLeftoverKVs kv bestAttempt
    validate [] [] rs _ =
      case NE.nonEmpty $ filter (not . isOptional) rs of
        Nothing -> evidence MapValidationDone
        Just requiredRules -> evidence $ MapValidationUnappliedRules (mapIndex <$> requiredRules)
    validate exhausted kvs (r : rs) seen =
      let
        consume (KV k v _) f = case kvs of
          ((tk, tv) : leftover) ->
            let
              cKey = toCanonical tk
             in
              case validateTerm cddl tk k of
                Evidenced SValid kTrc
                  | cKey `Set.notMember` seen ->
                      case validateTerm cddl tv v of
                        Evidenced SValid vTrc -> mapTrace (MapValidationConsume (mapIndex r) kTrc vTrc) $ f leftover (Set.insert cKey seen)
                        Evidenced SInvalid vTrc -> evidence $ MapValidationInvalidValue (mapIndex r) kTrc vTrc
                  | otherwise -> evidence $ MapValidationDuplicateKeys (mapIndex r) cKey kTrc
                Evidenced SInvalid _ -> evidence $ MapValidationUnappliedRules (NE.singleton $ mapIndex r)
          [] -> error "No remaining KV pairs"
        consume x _ = error $ "Unexpected value in map: " <> showSimple x
        postponeRule l = validate (r : exhausted) l rs
        dropRule l = validate exhausted l rs
        resetDropRule l = validate [] l (exhausted <> rs)
        rewriteRule newRule l = validate [] l (newRule : exhausted <> rs)
       in
        case r of
          Occur ct oi ->
            case oi of
              OIOptional ->
                consume ct resetDropRule <> postponeRule kvs seen
              OIZeroOrMore ->
                consume ct (rewriteRule r) <> postponeRule kvs seen
              OIOneOrMore ->
                consume ct (rewriteRule (Occur ct OIZeroOrMore)) <> postponeRule kvs seen
              OIBounded mlb mub
                | Just lb <- mlb, Just ub <- mub, lb > ub -> error "Unsatisfiable range encountered"
                | otherwise -> case compare 0 <$> mub of
                    Just EQ -> dropRule kvs seen
                    Just GT -> error "Unsatisfiable range encountered"
                    _ ->
                      consume ct (rewriteRule (Occur ct $ decrementBounds (mlb, mub)))
                        <> postponeRule kvs seen
          _ -> consume r resetDropRule <> postponeRule kvs seen

--------------------------------------------------------------------------------
-- Choices

validateChoice ::
  (CTree ValidatorPhase -> Evidenced ValidationTrace) ->
  NE.NonEmpty (CTree ValidatorPhase) ->
  Evidenced ValidationTrace
validateChoice v = go 0
  where
    go :: Int -> NE.NonEmpty (CTree ValidatorPhase) -> Evidenced ValidationTrace
    go i (choice NE.:| xs) = do
      case v choice of
        Evidenced SValid trc -> evidence $ ChoiceBranch i trc
        err -> case xs of
          [] -> err
          y : ys -> err <> go (succ i) (y NE.:| ys)

--------------------------------------------------------------------------------
-- Control helpers

-- | Validate both rules
ctrlAnd ::
  (CTree ValidatorPhase -> Evidenced ValidationTrace) ->
  CTree ValidatorPhase ->
  CTree ValidatorPhase ->
  Evidenced ValidationTrace
ctrlAnd v tgt ctrl =
  case v tgt of
    (isValid -> True) -> v ctrl
    err -> err

-- | Dispatch to the appropriate control
ctrlDispatch ::
  (CTree ValidatorPhase -> Evidenced ValidationTrace) ->
  CtlOp ->
  CTree ValidatorPhase ->
  CTree ValidatorPhase ->
  (CtlOp -> CTree ValidatorPhase -> Bool) ->
  Evidenced ValidationTrace
ctrlDispatch v And tgt ctrl _ = ctrlAnd v tgt ctrl
ctrlDispatch v Within tgt ctrl _ = ctrlAnd v tgt ctrl
ctrlDispatch v op tgt ctrl vctrl =
  case v tgt of
    Evidenced SValid trc
      | vctrl op ctrl ->
          evidence $ ControlTrace (ControlInfo op (mapIndex ctrl)) trc
      | otherwise ->
          evidence $ UnsatisfiedControl op (mapIndex ctrl)
    err -> err

--------------------------------------------------------------------------------
-- Bits control

getIndicesOfChoice :: CTreeRoot ValidatorPhase -> NE.NonEmpty (CTree ValidatorPhase) -> [Word64]
getIndicesOfChoice cddl = concatMap go
  where
    go = \case
      Literal (Value (VUInt v) _) -> [fromIntegral v]
      KV _ v _ ->
        case v of
          CTreeE (VRuleRef n) -> go $ dereference cddl n
          Literal (Value (VUInt v') _) -> [fromIntegral v']
          somethingElse ->
            error $
              "Malformed value in KV in choice in .bits: "
                <> showSimple somethingElse
      CRange (Range ff tt incl) -> getIndicesOfRange cddl ff tt incl
      Enum g -> getIndicesOfEnum cddl g
      somethingElse ->
        error $
          "Malformed alternative in choice in .bits: "
            <> showSimple somethingElse

getIndicesOfRange ::
  CTreeRoot ValidatorPhase -> CTree ValidatorPhase -> CTree ValidatorPhase -> RangeBound -> [Word64]
getIndicesOfRange cddl (CTreeE (VRuleRef n)) tt incl =
  dereference cddl n & \ff -> getIndicesOfRange cddl ff tt incl
getIndicesOfRange cddl ff (CTreeE (VRuleRef n)) incl =
  dereference cddl n & \tt -> getIndicesOfRange cddl ff tt incl
getIndicesOfRange _ ff tt incl =
  case (ff, tt) of
    (Literal (Value (VUInt ff') _), Literal (Value (VUInt tt') _)) ->
      case incl of
        ClOpen -> init rng
        Closed -> rng
      where
        rng = [ff' .. tt']
    (lo, hi) -> error $ "Malformed range in .bits: (" <> showSimple lo <> ", " <> showSimple hi <> ")"

getIndicesOfEnum :: CTreeRoot ValidatorPhase -> CTree ValidatorPhase -> [Word64]
getIndicesOfEnum cddl (CTreeE (VRuleRef n)) = getIndicesOfEnum cddl $ dereference cddl n
getIndicesOfEnum cddl g =
  case g of
    Group g' -> getIndicesOfChoice cddl (fromJust $ NE.nonEmpty g')
    somethingElse -> error $ "Malformed enum in .bits: " <> showSimple somethingElse

dereference ::
  CTreeRoot ValidatorPhase ->
  Name ->
  CTree ValidatorPhase
dereference (CTreeRoot m) n =
  case Map.lookup n m of
    Just r -> r
    Nothing -> error $ "Nonexistent rule referenced: " <> T.unpack (unName n)

dereferenceAndValidate ::
  CTreeRoot ValidatorPhase ->
  Name ->
  (CTree ValidatorPhase -> Evidenced ValidationTrace) ->
  Evidenced ValidationTrace
dereferenceAndValidate cddl n f =
  mapTrace (ReferenceRule n) . f $ dereference cddl n
