{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.CBOR.Cuddle.CBOR.Validator (
  validateCBOR,
  ValidatorStage,
) where

import Codec.CBOR.Cuddle.CBOR.Validator.Trace (
  ControlInfo (..),
  Evidenced (..),
  ListValidationTrace (..),
  MapValidationTrace (..),
  SValidity (..),
  ValidationTrace (..),
  ValidatorStage,
  XXCTree (..),
  evidence,
  isValid,
  mapTrace,
  showSimple,
  showValidationTrace,
 )
import Codec.CBOR.Cuddle.CDDL hiding (CDDL, Group, Rule)
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (CBORValidator (..), CustomValidatorResult (..))
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Codec.CBOR.Read
import Codec.CBOR.Term
import Data.Bifunctor (Bifunctor (..))
import Data.Bits hiding (And)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Function ((&))
import Data.IntSet qualified as IS
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Word
import GHC.Float
import GHC.Stack (HasCallStack)
import Text.Regex.TDFA

--------------------------------------------------------------------------------
-- Main entry point

validateCBOR ::
  HasCallStack =>
  BS.ByteString ->
  Name ->
  CTreeRoot ValidatorStage ->
  Evidenced ValidationTrace
validateCBOR bs rule cddl@(CTreeRoot tree) =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict bs) of
    Left e -> error $ show e
    Right (rest, term)
      | BSL.null rest -> validateTerm cddl term (tree Map.! rule)
      | otherwise -> error $ "Leftover bytes in CBOR: " <> show rest

--------------------------------------------------------------------------------
-- Terms

-- | Core function that validates a CBOR term to a particular rule of the CDDL
-- spec
validateTerm ::
  CTreeRoot ValidatorStage ->
  Term ->
  CTree ValidatorStage ->
  Evidenced ValidationTrace
validateTerm cddl term rule
  | CTreeE (VRuleRef n) <- rule =
      dereferenceAndValidate cddl n (validateTerm cddl term)
  | CTreeE (VValidator (CBORValidator validator) _) <- rule =
      case validator term of
        CustomValidatorSuccess -> evidence CustomSuccess
        CustomValidatorFailure err -> evidence $ CustomFailure err
  | otherwise =
      case term of
        TInt i -> validateInteger cddl (fromIntegral i) rule
        TInteger i -> validateInteger cddl i rule
        TBytes b -> validateBytes cddl b rule
        TBytesI b -> validateBytes cddl (BSL.toStrict b) rule
        TString s -> validateText cddl s rule
        TStringI s -> validateText cddl (TL.toStrict s) rule
        TList ts -> validateList cddl ts rule
        TListI ts -> validateList cddl ts rule
        TMap ts -> validateMap cddl ts rule
        TMapI ts -> validateMap cddl ts rule
        TTagged w t -> validateTagged cddl w t rule
        TBool b -> validateBool cddl b rule
        TNull -> validateNull cddl rule
        TSimple s -> validateSimple cddl s rule
        THalf h -> validateHalf cddl h rule
        TFloat h -> validateFloat cddl h rule
        TDouble d -> validateDouble cddl d rule

terminal :: CTree ValidatorStage -> Evidenced ValidationTrace
terminal = evidence . TerminalRule Nothing . mapIndex

unapplicable :: CTree ValidatorStage -> Evidenced ValidationTrace
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
  CTreeRoot ValidatorStage ->
  Integer ->
  CTree ValidatorStage ->
  Evidenced ValidationTrace
validateInteger cddl i rule =
  case rule of
    CTreeE (VRuleRef n) -> dereferenceAndValidate cddl n $ validateInteger cddl i
    -- echo "C24101" | xxd -r -p - example.cbor
    -- echo "foo = int" > a.cddl
    -- cddl a.cddl validate example.cbor
    --
    -- but
    --
    -- echo "C249010000000000000000"| xxd -r -p - example.cbor
    -- echo "foo = int" > a.cddl
    -- cddl a.cddl validate example.cbor
    --
    -- and they are both bigints?

    -- a = any
    Postlude PTAny -> terminal rule
    -- a = int
    Postlude PTInt
      | i >= -(2 ^ (64 :: Int)) && i <= 2 ^ (64 :: Int) - 1 -> terminal rule
    -- a = uint
    Postlude PTUInt
      | i >= 0 && i <= 2 ^ (64 :: Int) - 1 -> terminal rule
    -- a = nint
    Postlude PTNInt
      | i >= -(2 ^ (64 :: Int)) && i <= -1 -> terminal rule
    -- a = x
    Literal (Value (VUInt i') _) | i == fromIntegral i' -> terminal rule
    -- a = -x
    Literal (Value (VNInt i') _) | -i == fromIntegral i' -> terminal rule
    -- a = <big number>
    Literal (Value (VBignum i') _) | i == i' -> terminal rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateInteger cddl i) op tgt ctrl (controlInteger cddl i)
    -- a = foo / bar
    Choice opts -> validateChoice (validateInteger cddl i) opts
    -- a = x..y
    Range low high bound ->
      case (low, high) of
        (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _))
          | n <= i && range bound i m -> terminal rule
          | otherwise -> unapplicable rule
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _))
          | -n <= i && range bound i m -> terminal rule
          | otherwise -> unapplicable rule
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _))
          | -n <= i && range bound i (-m) -> terminal rule
          | otherwise -> unapplicable rule
        (Literal (Value VUInt {} _), Literal (Value VNInt {} _)) -> error "range types mismatch"
        (Literal (Value (VBignum n) _), Literal (Value (VUInt (fromIntegral -> m)) _))
          | n <= i && range bound i m -> terminal rule
          | otherwise -> unapplicable rule
        (Literal (Value (VBignum n) _), Literal (Value (VNInt (fromIntegral -> m)) _))
          | n <= i && range bound i (-m) -> terminal rule
          | otherwise -> unapplicable rule
        (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _))
          | n <= i && range bound i m -> terminal rule
          | otherwise -> unapplicable rule
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _))
          | (-n) <= i && range bound i m -> terminal rule
          | otherwise -> unapplicable rule
        (CTreeE (VRuleRef n), _) ->
          dereferenceAndValidate cddl n $ \lo -> validateInteger cddl i (Range lo high bound)
        (_, CTreeE (VRuleRef n)) ->
          dereferenceAndValidate cddl n $ \hi -> validateInteger cddl i (Range low hi bound)
        (lo, hi) -> error $ "Unable to validate range: (" <> showSimple lo <> ", " <> showSimple hi <> ")"
    -- a = &(x, y, z)
    Enum g ->
      case g of
        CTreeE (VRuleRef n) -> dereferenceAndValidate cddl n $ validateInteger cddl i
        Group g' -> validateInteger cddl i (Choice (NE.fromList g'))
        _ -> error "Not yet implemented"
    -- a = x: y
    -- Note KV cannot appear on its own, but we will use this when validating
    -- lists.
    KV _ v _ -> validateInteger cddl i v
    Tag 2 x -> validateBigInt x
    Tag 3 x -> validateBigInt x
    _ -> unapplicable rule
  where
    validateBigInt x = case x of
      CTreeE (VRuleRef n) -> dereferenceAndValidate cddl n validateBigInt
      Postlude PTBytes -> terminal rule
      Control op tgt@(Postlude PTBytes) ctrl ->
        ctrlDispatch (validateBytes cddl bs) op tgt ctrl (controlBytes cddl bs)
        where
          -- TODO figure out a way to turn Integer into bytes or figure out why
          -- tagged bigints are decoded as integers in the first place
          bs = mempty
      _ -> unapplicable rule

-- | Controls for an Integer
controlInteger ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Integer ->
  CtlOp ->
  CTree ValidatorStage ->
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
      Range ff tt incl -> getIndicesOfRange cddl ff tt incl
      Enum g -> getIndicesOfEnum cddl g
      _ -> error "Not yet implemented"
  go (IS.fromList (map fromIntegral indices)) i 0
  where
    go _ 0 _ = True
    go indices n idx =
      let bitSet = testBit n 0
          allowed = not bitSet || IS.member idx indices
       in allowed && go indices (shiftR n 1) (idx + 1)
controlInteger _ i Lt ctrl =
  case ctrl of
    Literal (Value (VUInt i') _) -> i < fromIntegral i'
    Literal (Value (VNInt i') _) -> i < -fromIntegral i'
    Literal (Value (VBignum i') _) -> i < i'
    _ -> False
controlInteger _ i Gt ctrl =
  case ctrl of
    Literal (Value (VUInt i') _) -> i > fromIntegral i'
    Literal (Value (VNInt i') _) -> i > -fromIntegral i'
    Literal (Value (VBignum i') _) -> i > i'
    _ -> False
controlInteger _ i Le ctrl =
  case ctrl of
    Literal (Value (VUInt i') _) -> i <= fromIntegral i'
    Literal (Value (VNInt i') _) -> i <= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i <= i'
    _ -> False
controlInteger _ i Ge ctrl =
  case ctrl of
    Literal (Value (VUInt i') _) -> i >= fromIntegral i'
    Literal (Value (VNInt i') _) -> i >= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i >= i'
    _ -> False
controlInteger _ i Eq ctrl =
  case ctrl of
    Literal (Value (VUInt i') _) -> i == fromIntegral i'
    Literal (Value (VNInt i') _) -> i == -fromIntegral i'
    Literal (Value (VBignum i') _) -> i == i'
    _ -> False
controlInteger _ i Ne ctrl =
  case ctrl of
    Literal (Value (VUInt i') _) -> i /= fromIntegral i'
    Literal (Value (VNInt i') _) -> i /= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i /= i'
    _ -> False
controlInteger _ _ _ ctrl = error $ "unexpected control: " <> showSimple ctrl

--------------------------------------------------------------------------------
-- Floating point (Float16, Float32, Float64)
--
-- As opposed to Integral types, there seems to be no ambiguity when encoding
-- and decoding floating-point numbers.

-- | Validating a `Float16`
validateHalf ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Float ->
  CTree ValidatorStage ->
  Evidenced ValidationTrace
validateHalf cddl f (CTreeE (VRuleRef n)) = dereferenceAndValidate cddl n $ validateHalf cddl f
validateHalf cddl f rule =
  case rule of
    -- a = any
    Postlude PTAny -> terminal rule
    -- a = float16
    Postlude PTHalf -> terminal rule
    -- a = 0.5
    Literal (Value (VFloat16 f') _) | f == f' -> terminal rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateHalf cddl f) opts
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateHalf cddl f) op tgt ctrl (controlHalf cddl f)
    -- a = x..y
    Range (CTreeE (VRuleRef n)) high bound ->
      dereferenceAndValidate cddl n $ \lo -> validateHalf cddl f $ Range lo high bound
    Range low (CTreeE (VRuleRef n)) bound ->
      dereferenceAndValidate cddl n $ \hi -> validateHalf cddl f $ Range low hi bound
    Range (Literal (Value (VFloat16 n) _)) (Literal (Value (VFloat16 m) _)) bound
      | n <= f && range bound f m -> terminal rule
    _ -> unapplicable rule

-- | Controls for `Float16`
controlHalf ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Float ->
  CtlOp ->
  CTree ValidatorStage ->
  Bool
controlHalf cddl f op (CTreeE (VRuleRef n)) =
  controlHalf cddl f op $ dereference cddl n
controlHalf _ f Eq ctrl =
  case ctrl of
    Literal (Value (VFloat16 f') _) -> f == f'
    _ -> False
controlHalf _ f Ne ctrl =
  case ctrl of
    Literal (Value (VFloat16 f') _) -> f /= f'
    _ -> False
controlHalf _ _ op _ = error $ "Not yet implemented for half: " <> show op

-- | Validating a `Float32`
validateFloat ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Float ->
  CTree ValidatorStage ->
  Evidenced ValidationTrace
validateFloat cddl f (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateFloat cddl f
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
    Range (CTreeE (VRuleRef n)) high bound ->
      dereferenceAndValidate cddl n $ \lo -> validateFloat cddl f $ Range lo high bound
    Range low (CTreeE (VRuleRef n)) bound ->
      dereferenceAndValidate cddl n $ \hi -> validateFloat cddl f $ Range low hi bound
    Range (Literal (Value nv _)) (Literal (Value mv _)) bound
      | VFloat16 n <- nv
      , VFloat16 m <- mv
      , n <= f && range bound f m ->
          terminal rule
      | VFloat32 n <- nv
      , VFloat32 m <- mv
      , n <= f && range bound f m ->
          terminal rule
    _ -> unapplicable rule

-- | Controls for `Float32`
controlFloat ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Float ->
  CtlOp ->
  CTree ValidatorStage ->
  Bool
controlFloat cddl f op (CTreeE (VRuleRef n)) =
  controlFloat cddl f op $ dereference cddl n
controlFloat _ f Eq ctrl =
  case ctrl of
    Literal (Value (VFloat16 f') _) -> f == f'
    Literal (Value (VFloat32 f') _) -> f == f'
    _ -> False
controlFloat _ f Ne ctrl =
  case ctrl of
    Literal (Value (VFloat16 f') _) -> f /= f'
    Literal (Value (VFloat32 f') _) -> f /= f'
    _ -> False
controlFloat _ _ op _ = error $ "Not yet implemented for float: " <> show op

-- | Validating a `Float64`
validateDouble ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Double ->
  CTree ValidatorStage ->
  Evidenced ValidationTrace
validateDouble cddl f (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateDouble cddl f
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
    Range (CTreeE (VRuleRef n)) high bound ->
      dereferenceAndValidate cddl n $ \lo -> validateDouble cddl f $ Range lo high bound
    Range low (CTreeE (VRuleRef n)) bound ->
      dereferenceAndValidate cddl n $ \hi -> validateDouble cddl f $ Range low hi bound
    Range (Literal (Value nv _)) (Literal (Value mv _)) bound
      | VFloat16 n <- nv
      , VFloat16 m <- mv
      , float2Double n <= f && range bound f (float2Double m) ->
          terminal rule
      | VFloat32 n <- nv
      , VFloat32 m <- mv
      , float2Double n <= f && range bound f (float2Double m) ->
          terminal rule
      | VFloat64 n <- nv
      , VFloat64 m <- mv
      , n <= f && range bound f m ->
          terminal rule
    _ -> unapplicable rule

-- | Controls for `Float64`
controlDouble ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Double ->
  CtlOp ->
  CTree ValidatorStage ->
  Bool
controlDouble cddl f op (CTreeE (VRuleRef n)) =
  controlDouble cddl f op $ dereference cddl n
controlDouble _ f Eq ctrl =
  case ctrl of
    Literal (Value (VFloat16 f') _) -> f == float2Double f'
    Literal (Value (VFloat32 f') _) -> f == float2Double f'
    Literal (Value (VFloat64 f') _) -> f == f'
    _ -> False
controlDouble _ f Ne ctrl =
  case ctrl of
    Literal (Value (VFloat16 f') _) -> f /= float2Double f'
    Literal (Value (VFloat32 f') _) -> f /= float2Double f'
    Literal (Value (VFloat64 f') _) -> f /= f'
    _ -> False
controlDouble _ _ op _ = error $ "Not yet implmented for double: " <> show op

--------------------------------------------------------------------------------
-- Bool

-- | Validating a boolean
validateBool ::
  CTreeRoot ValidatorStage ->
  Bool ->
  CTree ValidatorStage ->
  Evidenced ValidationTrace
validateBool cddl b (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateBool cddl b
validateBool cddl b rule =
  case rule of
    -- a = any
    Postlude PTAny -> terminal rule
    -- a = bool
    Postlude PTBool -> terminal rule
    -- a = true
    Literal (Value (VBool b') _) | b == b' -> terminal rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateBool cddl b) op tgt ctrl (controlBool cddl b)
    -- a = foo / bar
    Choice opts -> validateChoice (validateBool cddl b) opts
    _ -> unapplicable rule

-- | Controls for `Bool`
controlBool ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Bool ->
  CtlOp ->
  CTree ValidatorStage ->
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

-- | Validating a `TSimple`. It is unclear if this is used for anything else than `undefined`.
validateSimple ::
  CTreeRoot ValidatorStage ->
  Word8 ->
  CTree ValidatorStage ->
  Evidenced ValidationTrace
validateSimple cddl i (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateSimple cddl i
validateSimple cddl 23 rule =
  case rule of
    -- a = any
    Postlude PTAny -> terminal rule
    -- a = undefined
    Postlude PTUndefined -> terminal rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateSimple cddl 23) opts
    _ -> unapplicable rule
validateSimple _ n _ = error $ "Found simple different to 23! please report this somewhere! Found: " <> show n

--------------------------------------------------------------------------------
-- Null/nil

-- | Validating nil
validateNull :: CTreeRoot ValidatorStage -> CTree ValidatorStage -> Evidenced ValidationTrace
validateNull cddl (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateNull cddl
validateNull cddl rule =
  case rule of
    -- a = any
    Postlude PTAny -> terminal rule
    -- a = nil
    Postlude PTNil -> terminal rule
    Choice opts -> validateChoice (validateNull cddl) opts
    _ -> unapplicable rule

--------------------------------------------------------------------------------
-- Bytes

-- | Validating a byte sequence
validateBytes ::
  CTreeRoot ValidatorStage -> BS.ByteString -> CTree ValidatorStage -> Evidenced ValidationTrace
validateBytes cddl bs (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateBytes cddl bs
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
  CTreeRoot ValidatorStage ->
  BS.ByteString ->
  CtlOp ->
  CTree ValidatorStage ->
  Bool
controlBytes cddl bs op (CTreeE (VRuleRef n)) =
  controlBytes cddl bs op $ dereference cddl n
controlBytes cddl bs op@Size ctrl =
  case ctrl of
    Literal (Value (VUInt sz) _) -> fromIntegral (BS.length bs) == sz
    Range (CTreeE (VRuleRef n)) high bound ->
      dereference cddl n & \lo -> controlBytes cddl bs op $ Range lo high bound
    Range low (CTreeE (VRuleRef n)) bound ->
      dereference cddl n & \hi -> controlBytes cddl bs op $ Range low hi bound
    Range low high bound ->
      let i = BS.length bs
       in case (low, high) of
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) ->
              boundPlacement i (Just n, Just m) bound == WithinBounds
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) ->
              boundPlacement i (Just $ -n, Just m) bound == WithinBounds
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) ->
              boundPlacement i (Just $ -n, Just $ -m) bound == WithinBounds
            _ -> False
    _ -> False
controlBytes cddl bs Bits ctrl = do
  let
    indices =
      case ctrl of
        Literal (Value (VUInt i') _) -> [i']
        Choice nodes -> getIndicesOfChoice cddl nodes
        Range ff tt incl -> getIndicesOfRange cddl ff tt incl
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
  case deserialiseFromBytes decodeTerm (BSL.fromStrict bs) of
    Right (BSL.null -> True, term) -> isValid $ validateTerm cddl term ctrl
    _ -> False
controlBytes cddl bs Cborseq ctrl =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict (BS.snoc (BS.cons 0x9f bs) 0xff)) of
    Right (BSL.null -> True, TListI terms) -> isValid $ validateTerm cddl (TList terms) (Array [Occur ctrl OIZeroOrMore])
    _ -> False
controlBytes _ _ op _ = error $ "Not yet implmented for bytes: " <> show op

--------------------------------------------------------------------------------
-- Text

-- | Validating text strings
validateText ::
  CTreeRoot ValidatorStage ->
  T.Text ->
  CTree ValidatorStage ->
  Evidenced ValidationTrace
validateText cddl txt (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateText cddl txt
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
  CTreeRoot ValidatorStage ->
  T.Text ->
  CtlOp ->
  CTree ValidatorStage ->
  Bool
controlText cddl bs op (CTreeE (VRuleRef n)) =
  controlText cddl bs op $ dereference cddl n
controlText cddl bs op@Size ctrl =
  let bsSize = BS.length $ encodeUtf8 bs
   in case ctrl of
        Literal (Value (VUInt (fromIntegral -> sz)) _) -> bsSize == sz
        Range (CTreeE (VRuleRef n)) high bound ->
          dereference cddl n & \lo -> controlText cddl bs op $ Range lo high bound
        Range low (CTreeE (VRuleRef n)) bound ->
          dereference cddl n & \hi -> controlText cddl bs op $ Range low hi bound
        Range ff tt bound ->
          case (ff, tt) of
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) ->
              n <= T.length bs && range bound bsSize m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) ->
              -n <= T.length bs && range bound bsSize m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) ->
              -n <= T.length bs && range bound bsSize (-m)
            _ -> False
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
  CTreeRoot ValidatorStage -> Word64 -> Term -> CTree ValidatorStage -> Evidenced ValidationTrace
validateTagged cddl tag term (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateTagged cddl tag term
validateTagged cddl tag term rule =
  case rule of
    Postlude PTAny -> terminal rule
    Tag tag' rule' ->
      -- If the tag does not match, this is a direct fail
      if tag == tag'
        then mapTrace (TagTrace tag) $ validateTerm cddl term rule'
        else evidence $ InvalidTag tag
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

decrementBounds :: (Maybe Word64, Maybe Word64) -> OccurrenceIndicator
decrementBounds (lb, ub) = OIBounded (clampedPred <$> lb) (clampedPred <$> ub)
  where
    clampedPred 0 = 0
    clampedPred x = pred x

validateList ::
  CTreeRoot ValidatorStage ->
  [Term] ->
  CTree ValidatorStage ->
  Evidenced ValidationTrace
validateList cddl terms (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateList cddl terms
validateList cddl terms rule =
  case rule of
    Postlude PTAny -> terminal rule
    Array rules -> mapTrace ListTrace . fst $ validate finalize terms rules
      where
        finalize [] = evidence ListValidationDone
        finalize (x : xs) = evidence . ListValidationLeftoverTerms $ x :| xs
    Choice opts -> validateChoice (validateList cddl terms) opts
    _ -> unapplicable rule
  where
    validate ::
      ([Term] -> Evidenced ListValidationTrace) ->
      [Term] ->
      [CTree ValidatorStage] ->
      (Evidenced ListValidationTrace, [Term])
    validate f tss [] = (f tss, tss)
    validate f [] (r : rs)
      | isOptional r = validate f [] rs
      | otherwise = (evidence $ ListValidationUnappliedRules (mapIndex <$> r :| rs), [])
    validate f tss@(t : ts) (r : rs) =
      let
        consumeTerm ct g = case validateTerm cddl t ct of
          Evidenced SValid trc -> first (mapTrace $ ListValidationConsume (mapIndex r) trc) $ g ts
          Evidenced SInvalid trc -> (evidence $ ListValidationMissingRequired (mapIndex ct) trc, tss)

        consumeGroup ns gp g = case validate (const $ evidence ListValidationDone) tss gp of
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
        continue l = validate f l rs
        skipRule = validate f tss rs
        rewriteRule newRule l = validate f l (newRule : rs)
        validateRule =
          \case
            Occur ct oi -> case oi of
              OIOptional -> consume ct continue <> skipRule
              OIZeroOrMore -> consume ct (rewriteRule r) <> skipRule
              OIOneOrMore -> consume ct (rewriteRule (Occur ct OIZeroOrMore))
              OIBounded lb ub ->
                let bounds = (lb, ub)
                 in case boundPlacement 1 bounds Closed of
                      BelowBounds -> consume ct (rewriteRule (Occur ct $ decrementBounds bounds))
                      WithinBounds -> consume ct (rewriteRule (Occur ct $ decrementBounds bounds)) <> skipRule
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
  CTreeRoot ValidatorStage ->
  [(Term, Term)] ->
  CTree ValidatorStage ->
  Evidenced ValidationTrace
validateMap cddl terms (CTreeE (VRuleRef n)) =
  dereferenceAndValidate cddl n $ validateMap cddl terms
validateMap cddl terms rule =
  case rule of
    Postlude PTAny -> terminal rule
    Map rules -> mapTrace MapTrace $ validate [] terms rules
    Choice opts -> validateChoice (validateMap cddl terms) opts
    _ -> unapplicable rule
  where
    validate ::
      [CTree ValidatorStage] -> [(Term, Term)] -> [CTree ValidatorStage] -> Evidenced MapValidationTrace
    validate [] [] [] = evidence MapValidationDone
    validate _ kvs [] = evidence $ MapValidationLeftoverKVs kvs
    validate [] [] rs =
      case NE.nonEmpty $ filter (not . isOptional) rs of
        Nothing -> evidence MapValidationDone
        Just requiredRules -> evidence $ MapValidationUnappliedRules (mapIndex <$> requiredRules)
    validate exhausted kvs (r : rs) =
      let
        consume (KV k v _) f = case kvs of
          ((tk, tv) : leftover) ->
            case validateTerm cddl tk k of
              Evidenced SValid kTrc ->
                case validateTerm cddl tv v of
                  Evidenced SValid vTrc -> mapTrace (MapValidationConsume (mapIndex r) kTrc vTrc) $ f leftover
                  Evidenced SInvalid vTrc -> evidence $ MapValidationInvalidValue (mapIndex r) kTrc vTrc
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
                consume ct resetDropRule <> postponeRule kvs
              OIZeroOrMore ->
                consume ct (rewriteRule r) <> postponeRule kvs
              OIOneOrMore ->
                consume ct (rewriteRule (Occur ct OIZeroOrMore)) <> postponeRule kvs
              OIBounded mlb mub
                | Just lb <- mlb, Just ub <- mub, lb > ub -> error "Unsatisfiable range encountered"
                | otherwise -> case compare 0 <$> mub of
                    Just EQ -> dropRule kvs
                    Just GT -> error "Unsatisfiable range encountered"
                    _ ->
                      consume ct (rewriteRule (Occur ct $ decrementBounds (mlb, mub)))
                        <> postponeRule kvs
          _ -> consume r resetDropRule <> postponeRule kvs

--------------------------------------------------------------------------------
-- Choices

validateChoice ::
  (CTree ValidatorStage -> Evidenced ValidationTrace) ->
  NE.NonEmpty (CTree ValidatorStage) ->
  Evidenced ValidationTrace
validateChoice v = go 0
  where
    go :: Int -> NE.NonEmpty (CTree ValidatorStage) -> Evidenced ValidationTrace
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
  (CTree ValidatorStage -> Evidenced ValidationTrace) ->
  CTree ValidatorStage ->
  CTree ValidatorStage ->
  Evidenced ValidationTrace
ctrlAnd v tgt ctrl =
  case v tgt of
    (isValid -> True) -> v ctrl
    err -> err

-- | Dispatch to the appropriate control
ctrlDispatch ::
  (CTree ValidatorStage -> Evidenced ValidationTrace) ->
  CtlOp ->
  CTree ValidatorStage ->
  CTree ValidatorStage ->
  (CtlOp -> CTree ValidatorStage -> Bool) ->
  Evidenced ValidationTrace
ctrlDispatch v And tgt ctrl _ = ctrlAnd v tgt ctrl
ctrlDispatch v Within tgt ctrl _ = ctrlAnd v tgt ctrl
ctrlDispatch v op tgt ctrl vctrl =
  case v tgt of
    Evidenced SValid (TerminalRule _ res)
      | vctrl op ctrl ->
          evidence $ TerminalRule (Just (ControlInfo op (mapIndex ctrl))) res
      | otherwise ->
          evidence $ UnsatisfiedControl op (mapIndex ctrl)
    Evidenced SValid trc ->
      error $ "Unexpected trace:\n\t" <> showValidationTrace trc
    err -> err

--------------------------------------------------------------------------------
-- Bits control

getIndicesOfChoice :: CTreeRoot ValidatorStage -> NE.NonEmpty (CTree ValidatorStage) -> [Word64]
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
      Range ff tt incl -> getIndicesOfRange cddl ff tt incl
      Enum g -> getIndicesOfEnum cddl g
      somethingElse ->
        error $
          "Malformed alternative in choice in .bits: "
            <> showSimple somethingElse

getIndicesOfRange ::
  CTreeRoot ValidatorStage -> CTree ValidatorStage -> CTree ValidatorStage -> RangeBound -> [Word64]
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

getIndicesOfEnum :: CTreeRoot ValidatorStage -> CTree ValidatorStage -> [Word64]
getIndicesOfEnum cddl (CTreeE (VRuleRef n)) = getIndicesOfEnum cddl $ dereference cddl n
getIndicesOfEnum cddl g =
  case g of
    Group g' -> getIndicesOfChoice cddl (fromJust $ NE.nonEmpty g')
    somethingElse -> error $ "Malformed enum in .bits: " <> showSimple somethingElse

dereference ::
  CTreeRoot ValidatorStage ->
  Name ->
  CTree ValidatorStage
dereference (CTreeRoot m) n =
  case Map.lookup n m of
    Just r -> r
    Nothing -> error $ "Nonexistent rule referenced: " <> T.unpack (unName n)

dereferenceAndValidate ::
  CTreeRoot ValidatorStage ->
  Name ->
  (CTree ValidatorStage -> Evidenced ValidationTrace) ->
  Evidenced ValidationTrace
dereferenceAndValidate cddl n f =
  mapTrace (ReferenceRule n) . f $ dereference cddl n

--------------------------------------------------------------------------------
-- Utils

range :: Ord a => RangeBound -> a -> a -> Bool
range Closed = (<=)
range ClOpen = (<)
