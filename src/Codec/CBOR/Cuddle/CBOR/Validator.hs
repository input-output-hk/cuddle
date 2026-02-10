{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.CBOR.Cuddle.CBOR.Validator (
  validateCBOR,
  ValidatorStage,
  ValidatorStageSimple,
  showSimple,
) where

import Codec.CBOR.Cuddle.CDDL hiding (CDDL, Group, Rule)
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (CBORValidator (..), CustomValidatorResult (..))
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, XXCTree (..))
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Codec.CBOR.Read
import Codec.CBOR.Term
import Data.Bits hiding (And)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.IntSet qualified as IS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Word
import GHC.Float
import GHC.Stack (HasCallStack)
import Text.Regex.TDFA

--------------------------------------------------------------------------------
-- Validation result

type data Validity
  = IsValid
  | IsInvalid

data SValidity (v :: Validity) where
  SValid :: SValidity IsValid
  SInvalid :: SValidity IsInvalid

data ValidationTrace (v :: Validity) where
  UnapplicableRule :: CTree ValidatorStage -> ValidationTrace IsInvalid
  TerminalRule :: CTree ValidatorStage -> ValidationTrace IsValid
  ReferenceRule :: Name -> ValidationTrace v -> ValidationTrace v
  CustomFailure :: Text -> ValidationTrace IsInvalid
  CustomSuccess :: ValidationTrace IsValid
  UnsatisfiedControl :: CtlOp -> ValidationTrace IsInvalid
  TraceInformation :: Text -> ValidationTrace v -> ValidationTrace v

data ValidationResult = forall (v :: Validity). ValidationResult
  { vrValidity :: SValidity v
  , vrTrace :: ValidationTrace v
  }

traceValidity :: ValidationTrace v -> SValidity v
traceValidity UnapplicableRule {} = SInvalid
traceValidity TerminalRule {} = SValid
traceValidity (ReferenceRule _ x) = traceValidity x
traceValidity CustomFailure {} = SInvalid
traceValidity UnsatisfiedControl {} = SInvalid
traceValidity (TraceInformation _ x) = traceValidity x
traceValidity CustomSuccess {} = SValid

toResult :: ValidationTrace v -> ValidationResult
toResult x =
  ValidationResult
    { vrValidity = traceValidity x
    , vrTrace = x
    }

isValid :: ValidationResult -> Bool
isValid ValidationResult {vrValidity = SValid} = True
isValid _ = False

--------------------------------------------------------------------------------
-- ValidatorStage

type data ValidatorStage

data instance XTerm ValidatorStage = ValidatorXTerm
  deriving (Show, Eq)

data instance XXCTree ValidatorStage
  = VRuleRef Name
  | VValidator CBORValidator (CTree ValidatorStage)

instance IndexMappable CTreeRoot MonoReferenced ValidatorStage where
  mapIndex (CTreeRoot m) = CTreeRoot $ mapIndex <$> m

instance IndexMappable CTree MonoReferenced ValidatorStage where
  mapIndex = foldCTree mapExt mapIndex
    where
      mapExt (MRuleRef n) = CTreeE $ VRuleRef n
      mapExt (MGenerator _ x) = mapIndex x
      mapExt (MValidator v x) = CTreeE . VValidator v $ mapIndex x

type data ValidatorStageSimple

newtype instance XXCTree ValidatorStageSimple = VRuleRefSimple Name

instance IndexMappable CTreeRoot ValidatorStage ValidatorStageSimple where
  mapIndex (CTreeRoot m) = CTreeRoot $ mapIndex <$> m

instance IndexMappable CTree ValidatorStage ValidatorStageSimple where
  mapIndex = foldCTree mapExt mapIndex
    where
      mapExt (VRuleRef n) = CTreeE $ VRuleRefSimple n
      mapExt (VValidator _ x) = mapIndex x

showSimple ::
  ( IndexMappable a ValidatorStage ValidatorStageSimple
  , Show (a ValidatorStageSimple)
  ) =>
  a ValidatorStage -> String
showSimple = show . mapIndex @_ @_ @ValidatorStageSimple

deriving instance Eq (Node ValidatorStageSimple)

deriving instance Show (Node ValidatorStageSimple)

--------------------------------------------------------------------------------
-- Main entry point

validateCBOR ::
  HasCallStack =>
  BS.ByteString ->
  Name ->
  CTreeRoot ValidatorStage ->
  ValidationResult
validateCBOR bs rule cddl@(CTreeRoot tree) =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict bs) of
    Left e -> error $ show e
    Right (rest, term)
      | BSL.null rest -> validateTerm cddl term (tree Map.! rule)
      | otherwise -> error $ "Leftover bytes in CBOR" <> show rest

--------------------------------------------------------------------------------
-- Terms

-- | Core function that validates a CBOR term to a particular rule of the CDDL
-- spec
validateTerm ::
  CTreeRoot ValidatorStage ->
  Term ->
  CTree ValidatorStage ->
  ValidationResult
validateTerm cddl term (resolveIfRef cddl -> rule)
  | CTreeE (VValidator (CBORValidator validator) _) <- rule =
      case validator term of
        CustomValidatorSuccess -> toResult CustomSuccess
        CustomValidatorFailure err -> toResult $ CustomFailure err
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
  ValidationResult
validateInteger cddl i (resolveIfRef cddl -> rule) =
  case rule of
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
    Postlude PTAny -> toResult $ TerminalRule rule
    -- a = int
    Postlude PTInt -> toResult $ TerminalRule rule
    -- a = uint
    Postlude PTUInt
      | i >= 0 -> toResult $ TerminalRule rule
    -- a = nint
    Postlude PTNInt | i <= 0 -> toResult $ TerminalRule rule
    -- a = x
    Literal (Value (VUInt i') _) | i == fromIntegral i' -> toResult $ TerminalRule rule
    -- a = -x
    Literal (Value (VNInt i') _) | -i == fromIntegral i' -> toResult $ TerminalRule rule
    -- a = <big number>
    Literal (Value (VBignum i') _) | i == i' -> toResult $ TerminalRule rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateInteger cddl i) op tgt ctrl (controlInteger cddl i)
    -- a = foo / bar
    Choice opts -> validateChoice (validateInteger cddl i) opts rule
    -- a = x..y
    Range low high bound ->
      case (resolveIfRef cddl low, resolveIfRef cddl high) of
        (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) | n <= i && range bound i m -> toResult $ TerminalRule rule
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) | -n <= i && range bound i m -> toResult $ TerminalRule rule
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) | -n <= i && range bound i (-m) -> toResult $ TerminalRule rule
        (Literal (Value VUInt {} _), Literal (Value VNInt {} _)) -> error "range types mismatch"
        (Literal (Value (VBignum n) _), Literal (Value (VUInt (fromIntegral -> m)) _)) | n <= i && range bound i m -> toResult $ TerminalRule rule
        (Literal (Value (VBignum n) _), Literal (Value (VNInt (fromIntegral -> m)) _)) | n <= i && range bound i (-m) -> toResult $ TerminalRule rule
        (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _)) | n <= i && range bound i m -> toResult $ TerminalRule rule
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _)) | (-n) <= i && range bound i m -> toResult $ TerminalRule rule
        (lo, hi) ->
          error $ "Unable to validate range: (" <> showSimple lo <> ", " <> showSimple hi <> ")"
    -- a = &(x, y, z)
    Enum g ->
      case resolveIfRef cddl g of
        Group g' -> validateInteger cddl i (Choice (NE.fromList g'))
        _ -> error "Not yet implemented"
    -- a = x: y
    -- Note KV cannot appear on its own, but we will use this when validating
    -- lists.
    KV _ v _ -> validateInteger cddl i v
    Tag 2 x -> validateBigInt x
    Tag 3 x -> validateBigInt x
    _ -> toResult $ UnapplicableRule rule
  where
    validateBigInt (resolveIfRef cddl -> x) = case x of
      Postlude PTBytes -> toResult $ TerminalRule rule
      Control op tgt@(Postlude PTBytes) ctrl ->
        ctrlDispatch (validateBytes cddl bs) op tgt ctrl (controlBytes cddl bs)
        where
          -- TODO figure out a way to turn Integer into bytes or figure out why
          -- tagged bigints are decoded as integers in the first place
          bs = mempty
      _ -> toResult $ UnapplicableRule rule

-- | Controls for an Integer
controlInteger ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Integer ->
  CtlOp ->
  CTree ValidatorStage ->
  ValidationResult
controlInteger cddl i op@Size (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VUInt sz) _)
      | 0 <= i && i < 256 ^ sz -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnsatisfiedControl op
controlInteger cddl i op@Bits (resolveIfRef cddl -> ctrl) = do
  let
    indices = case ctrl of
      Literal (Value (VUInt i') _) -> [i']
      Choice nodes -> getIndicesOfChoice cddl nodes
      Range ff tt incl -> getIndicesOfRange cddl ff tt incl
      Enum g -> getIndicesOfEnum cddl g
      _ -> error "Not yet implemented"
  if go (IS.fromList (map fromIntegral indices)) i 0
    then toResult $ TerminalRule ctrl
    else toResult $ UnsatisfiedControl op
  where
    go _ 0 _ = True
    go indices n idx =
      let bitSet = testBit n 0
          allowed = not bitSet || IS.member idx indices
       in allowed && go indices (shiftR n 1) (idx + 1)
controlInteger cddl i op@Lt (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VUInt i') _) | i < fromIntegral i' -> toResult $ TerminalRule ctrl
    Literal (Value (VNInt i') _) | i < -fromIntegral i' -> toResult $ TerminalRule ctrl
    Literal (Value (VBignum i') _) | i < i' -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnsatisfiedControl op
controlInteger cddl i op@Gt (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VUInt i') _) | i > fromIntegral i' -> toResult $ TerminalRule ctrl
    Literal (Value (VNInt i') _) | i > -fromIntegral i' -> toResult $ TerminalRule ctrl
    Literal (Value (VBignum i') _) | i > i' -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnsatisfiedControl op
controlInteger cddl i op@Le (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VUInt i') _) | i <= fromIntegral i' -> toResult $ TerminalRule ctrl
    Literal (Value (VNInt i') _) | i <= -fromIntegral i' -> toResult $ TerminalRule ctrl
    Literal (Value (VBignum i') _) | i <= i' -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnsatisfiedControl op
controlInteger cddl i op@Ge (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VUInt i') _) | i >= fromIntegral i' -> toResult $ TerminalRule ctrl
    Literal (Value (VNInt i') _) | i >= -fromIntegral i' -> toResult $ TerminalRule ctrl
    Literal (Value (VBignum i') _) | i >= i' -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnsatisfiedControl op
controlInteger cddl i op@Eq (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VUInt i') _) | i == fromIntegral i' -> toResult $ TerminalRule ctrl
    Literal (Value (VNInt i') _) | i == -fromIntegral i' -> toResult $ TerminalRule ctrl
    Literal (Value (VBignum i') _) | i == i' -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnsatisfiedControl op
controlInteger cddl i op@Ne (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VUInt i') _) | i /= fromIntegral i' -> toResult $ TerminalRule ctrl
    Literal (Value (VNInt i') _) | i /= -fromIntegral i' -> toResult $ TerminalRule ctrl
    Literal (Value (VBignum i') _) | i /= i' -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnsatisfiedControl op
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
  ValidationResult
validateHalf cddl f (resolveIfRef cddl -> rule) =
  case rule of
    -- a = any
    Postlude PTAny -> toResult $ TerminalRule rule
    -- a = float16
    Postlude PTHalf -> toResult $ TerminalRule rule
    -- a = 0.5
    Literal (Value (VFloat16 f') _) | f == f' -> toResult $ TerminalRule rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateHalf cddl f) opts rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateHalf cddl f) op tgt ctrl (controlHalf cddl f)
    -- a = x..y
    Range
      (resolveIfRef cddl -> Literal (Value (VFloat16 n) _))
      (resolveIfRef cddl -> Literal (Value (VFloat16 m) _))
      bound | n <= f && range bound f m -> toResult $ TerminalRule rule
    _ -> toResult $ UnapplicableRule rule

-- | Controls for `Float16`
controlHalf ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Float ->
  CtlOp ->
  CTree ValidatorStage ->
  ValidationResult
controlHalf cddl f op@Eq (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VFloat16 f') _) | f == f' -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnsatisfiedControl op
controlHalf cddl f op@Ne (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VFloat16 f') _) | f /= f' -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnsatisfiedControl op
controlHalf _ _ op _ = error $ "Not yet implemented for half: " <> show op

-- | Validating a `Float32`
validateFloat ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Float ->
  CTree ValidatorStage ->
  ValidationResult
validateFloat cddl f (resolveIfRef cddl -> rule) =
  case rule of
    -- a = any
    Postlude PTAny -> toResult $ TerminalRule rule
    -- a = float32
    Postlude PTFloat -> toResult $ TerminalRule rule
    -- a = 0.000000005
    -- TODO: it is unclear if smaller floats should also validate
    Literal (Value (VFloat32 f') _) | f == f' -> toResult $ TerminalRule rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateFloat cddl f) opts rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateFloat cddl f) op tgt ctrl (controlFloat cddl f)
    -- a = x..y
    -- TODO it is unclear if this should mix floating point types too
    Range
      (resolveIfRef cddl -> Literal (Value nv _))
      (resolveIfRef cddl -> Literal (Value mv _))
      bound
        | VFloat16 n <- nv
        , VFloat16 m <- mv
        , n <= f && range bound f m ->
            toResult $ TerminalRule rule
        | VFloat32 n <- nv
        , VFloat32 m <- mv
        , n <= f && range bound f m ->
            toResult $ TerminalRule rule
    _ -> toResult $ UnapplicableRule rule

-- | Controls for `Float32`
controlFloat ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Float ->
  CtlOp ->
  CTree ValidatorStage ->
  ValidationResult
controlFloat cddl f Eq (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VFloat16 f') _) | f == f' -> toResult $ TerminalRule ctrl
    Literal (Value (VFloat32 f') _) | f == f' -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnapplicableRule ctrl
controlFloat cddl f Ne (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VFloat16 f') _) | f /= f' -> toResult $ TerminalRule ctrl
    Literal (Value (VFloat32 f') _) | f /= f' -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnapplicableRule ctrl
controlFloat _ _ op _ = error $ "Not yet implemented for float: " <> show op

-- | Validating a `Float64`
validateDouble ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Double ->
  CTree ValidatorStage ->
  ValidationResult
validateDouble cddl f (resolveIfRef cddl -> rule) =
  case rule of
    -- a = any
    Postlude PTAny -> toResult $ TerminalRule rule
    -- a = float64
    Postlude PTDouble -> toResult $ TerminalRule rule
    -- a = 0.0000000000000000000000000000000000000000000005
    -- TODO: it is unclear if smaller floats should also validate
    Literal (Value (VFloat64 f') _) | f == f' -> toResult $ TerminalRule rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateDouble cddl f) opts rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateDouble cddl f) op tgt ctrl (controlDouble cddl f)
    -- a = x..y
    -- TODO it is unclear if this should mix floating point types too
    Range
      (resolveIfRef cddl -> Literal (Value nv _))
      (resolveIfRef cddl -> Literal (Value mv _))
      bound
        | VFloat16 n <- nv
        , VFloat16 m <- mv
        , float2Double n <= f && range bound f (float2Double m) ->
            toResult $ TerminalRule rule
        | VFloat32 n <- nv
        , VFloat32 m <- mv
        , float2Double n <= f && range bound f (float2Double m) ->
            toResult $ TerminalRule rule
        | VFloat64 n <- nv
        , VFloat64 m <- mv
        , n <= f && range bound f m ->
            toResult $ TerminalRule rule
    _ -> toResult $ UnapplicableRule rule

-- | Controls for `Float64`
controlDouble ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Double ->
  CtlOp ->
  CTree ValidatorStage ->
  ValidationResult
controlDouble cddl f op@Eq (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VFloat16 f') _) | f == float2Double f' -> toResult $ TerminalRule ctrl
    Literal (Value (VFloat32 f') _) | f == float2Double f' -> toResult $ TerminalRule ctrl
    Literal (Value (VFloat64 f') _) | f == f' -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnsatisfiedControl op
controlDouble cddl f op@Ne (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VFloat16 f') _) | f /= float2Double f' -> toResult $ TerminalRule ctrl
    Literal (Value (VFloat32 f') _) | f /= float2Double f' -> toResult $ TerminalRule ctrl
    Literal (Value (VFloat64 f') _) | f /= f' -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnsatisfiedControl op
controlDouble _ _ op _ = error $ "Not yet implmented for double: " <> show op

--------------------------------------------------------------------------------
-- Bool

-- | Validating a boolean
validateBool ::
  CTreeRoot ValidatorStage ->
  Bool ->
  CTree ValidatorStage ->
  ValidationResult
validateBool cddl b (resolveIfRef cddl -> rule) =
  case rule of
    -- a = any
    Postlude PTAny -> toResult $ TerminalRule rule
    -- a = bool
    Postlude PTBool -> toResult $ TerminalRule rule
    -- a = true
    Literal (Value (VBool b') _) | b == b' -> toResult $ TerminalRule rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateBool cddl b) op tgt ctrl (controlBool cddl b)
    -- a = foo / bar
    Choice opts -> validateChoice (validateBool cddl b) opts rule
    _ -> toResult $ UnapplicableRule rule

-- | Controls for `Bool`
controlBool ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Bool ->
  CtlOp ->
  CTree ValidatorStage ->
  ValidationResult
controlBool cddl b op@Eq (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VBool b') _) | b == b' -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnsatisfiedControl op
controlBool cddl b op@Ne (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VBool b') _) | b /= b' -> toResult $ TerminalRule ctrl
    _ -> toResult $ UnsatisfiedControl op
controlBool _ _ op _ = error $ "Not yet implemented for bool: " <> show op

--------------------------------------------------------------------------------
-- Simple

-- | Validating a `TSimple`. It is unclear if this is used for anything else than.
validateSimple ::
  CTreeRoot ValidatorStage ->
  Word8 ->
  CTree ValidatorStage ->
  ValidationResult
validateSimple cddl 23 (resolveIfRef cddl -> rule) =
  case rule of
    -- a = any
    Postlude PTAny -> toResult $ TerminalRule rule
    -- a = undefined
    Postlude PTUndefined -> toResult $ TerminalRule rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateSimple cddl 23) opts rule
    _ -> toResult $ UnapplicableRule rule
validateSimple _ n _ = error $ "Found simple different to 23! please report this somewhere! Found: " <> show n

--------------------------------------------------------------------------------
-- Null/nil

-- | Validating nil
validateNull :: CTreeRoot ValidatorStage -> CTree ValidatorStage -> ValidationResult
validateNull cddl (resolveIfRef cddl -> rule) =
  case rule of
    -- a = any
    Postlude PTAny -> toResult $ TerminalRule rule
    -- a = nil
    Postlude PTNil -> toResult $ TerminalRule rule
    Choice opts -> validateChoice (validateNull cddl) opts rule
    _ -> toResult $ UnapplicableRule rule

--------------------------------------------------------------------------------
-- Bytes

-- | Validating a byte sequence
validateBytes ::
  CTreeRoot ValidatorStage -> BS.ByteString -> CTree ValidatorStage -> ValidationResult
validateBytes cddl bs (resolveIfRef cddl -> rule) =
  case rule of
    -- a = any
    Postlude PTAny -> toResult $ TerminalRule rule
    -- a = bytes
    Postlude PTBytes -> toResult $ TerminalRule rule
    -- a = h'123456'
    Literal (Value (VBytes bs') _) | bs == bs' -> toResult $ TerminalRule rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateBytes cddl bs) op tgt ctrl (controlBytes cddl bs)
    -- a = foo / bar
    Choice opts -> validateChoice (validateBytes cddl bs) opts rule
    _ -> toResult $ UnapplicableRule rule

-- | Controls for byte strings
controlBytes ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  BS.ByteString ->
  CtlOp ->
  CTree ValidatorStage ->
  ValidationResult
controlBytes cddl bs op@Size (resolveIfRef cddl -> ctrl) =
  case ctrl of
    Literal (Value (VUInt (fromIntegral -> sz)) _) | BS.length bs == sz -> toResult $ TerminalRule ctrl
    Range low high bound ->
      let i = BS.length bs
       in case (resolveIfRef cddl low, resolveIfRef cddl high) of
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _))
              | n <= i && range bound i m -> toResult $ TerminalRule ctrl
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _))
              | -n <= i && range bound i m -> toResult $ TerminalRule ctrl
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _))
              | -n <= i && range bound i (-m) -> toResult $ TerminalRule ctrl
            _ -> toResult $ UnsatisfiedControl op
    _ -> toResult $ UnsatisfiedControl op
controlBytes cddl bs op@Bits (resolveIfRef cddl -> ctrl) = do
  let
    indices =
      case ctrl of
        Literal (Value (VUInt i') _) -> [i']
        Choice nodes -> getIndicesOfChoice cddl nodes
        Range ff tt incl -> getIndicesOfRange cddl ff tt incl
        Enum g -> getIndicesOfEnum cddl g
        _ -> error "Not yet implemented"
  if bitsControlCheck (map fromIntegral indices)
    then toResult $ TerminalRule ctrl
    else toResult $ UnsatisfiedControl op
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
    Right (BSL.null -> True, term) -> validateTerm cddl term ctrl
    _ -> error "Not yet implemented"
controlBytes cddl bs op@Cborseq ctrl =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict (BS.snoc (BS.cons 0x9f bs) 0xff)) of
    Right (BSL.null -> True, TListI terms) -> validateTerm cddl (TList terms) (Array [Occur ctrl OIZeroOrMore])
    _ -> toResult $ UnsatisfiedControl op
controlBytes _ _ op _ = error $ "Not yet implmented for bytes: " <> show op

--------------------------------------------------------------------------------
-- Text

-- | Validating text strings
validateText ::
  CTreeRoot ValidatorStage ->
  T.Text ->
  CTree ValidatorStage ->
  ValidationResult
validateText cddl txt (resolveIfRef cddl -> rule) =
  case rule of
    -- a = any
    Postlude PTAny -> toResult $ TerminalRule rule
    -- a = text
    Postlude PTText -> toResult $ TerminalRule rule
    -- a = "foo"
    Literal (Value (VText txt') _) | txt == txt' -> toResult $ TerminalRule rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateText cddl txt) op tgt ctrl (controlText cddl txt)
    -- a = foo / bar
    Choice opts -> validateChoice (validateText cddl txt) opts rule
    _ -> toResult $ UnapplicableRule rule

-- | Controls for text strings
controlText ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  T.Text ->
  CtlOp ->
  CTree ValidatorStage ->
  ValidationResult
controlText cddl bs op@Size (resolveIfRef cddl -> ctrl) =
  let bsSize = BS.length $ encodeUtf8 bs
   in case ctrl of
        Literal (Value (VUInt (fromIntegral -> sz)) _) | bsSize == sz -> toResult $ TerminalRule ctrl
        Range ff tt bound ->
          case (resolveIfRef cddl ff, resolveIfRef cddl tt) of
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) | n <= T.length bs && range bound bsSize m -> toResult $ TerminalRule ctrl
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) | -n <= T.length bs && range bound bsSize m -> toResult $ TerminalRule ctrl
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) | -n <= T.length bs && range bound bsSize (-m) -> toResult $ TerminalRule ctrl
            _ -> toResult $ UnsatisfiedControl op
        _ -> error "Invalid control value in .size"
controlText cddl s op@Regexp ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VText rxp) _) -> case s =~ rxp :: (T.Text, T.Text, T.Text) of
      ("", s', "") | s == s' -> toResult $ TerminalRule ctrl
      _ -> toResult $ UnsatisfiedControl op
    _ -> error "Invalid control value in .regexp"
controlText _ _ op _ = error $ "Not yet implemented for text: " <> show op

--------------------------------------------------------------------------------
-- Tagged values

-- | Validating a `TTagged`
validateTagged ::
  CTreeRoot ValidatorStage -> Word64 -> Term -> CTree ValidatorStage -> ValidationResult
validateTagged cddl tag term (resolveIfRef cddl -> rule) =
  case rule of
    Postlude PTAny -> toResult $ TerminalRule rule
    Tag tag' rule' ->
      -- If the tag does not match, this is a direct fail
      if tag == tag'
        then validateTerm cddl term rule'
        else
          let msg = T.pack $ "invalid tag: " <> show tag <> "\nexpected: " <> show tag'
           in toResult . TraceInformation msg $ UnapplicableRule rule
    Choice opts -> validateChoice (validateTagged cddl tag term) opts rule
    _ -> toResult $ UnapplicableRule rule

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
  ValidationResult
validateList cddl terms (resolveIfRef cddl -> rule) =
  case rule of
    Postlude PTAny -> toResult $ TerminalRule rule
    Array rules -> validate terms rules
    Choice opts -> validateChoice (validateList cddl terms) opts rule
    _ -> toResult $ UnapplicableRule rule
  where
    validate :: [Term] -> [CTree ValidatorStage] -> ValidationResult
    validate [] [] = toResult $ TerminalRule rule
    validate _ [] =
      toResult $
        TraceInformation "leftover terms after all rules have been applied" $
          UnapplicableRule rule
    validate [] (r : rs)
      | isOptional r = validate [] rs
      | otherwise =
          toResult $
            TraceInformation "leftover mandatory rules after all terms have been consumed" $
              UnapplicableRule rule
    validate (t : ts) (r : rs) = case r of
      Occur ct oi -> case oi of
        OIOptional
          | (isValid -> True, leftover) <- validateTermInList (t : ts) ct
          , res <- validate leftover rs
          , isValid res ->
              res
          | otherwise -> validate (t : ts) rs
        OIZeroOrMore
          | (isValid -> True, leftover) <- validateTermInList (t : ts) ct
          , res <- validate leftover (r : rs)
          , isValid res ->
              res
          | otherwise -> validate (t : ts) rs
        OIOneOrMore -> case validateTermInList (t : ts) ct of
          (isValid -> True, leftover) -> validate leftover (Occur ct OIZeroOrMore : rs)
          (err, _) -> err
        OIBounded lb ub -> case boundPlacement 1 (lb, ub) Closed of
          BelowBounds -> validate (t : ts) (ct : Occur ct (decrementBounds (lb, ub)) : rs)
          WithinBounds
            | (isValid -> True, leftover) <- validateTermInList (t : ts) ct
            , res <- validate leftover (Occur ct (decrementBounds (lb, ub)) : rs)
            , isValid res ->
                res
            | otherwise -> validate (t : ts) rs
          AboveBounds
            | boundPlacement 0 (lb, ub) Closed == WithinBounds -> validate (t : ts) rs
            | otherwise -> error "Negative upper bound"
      _ -> case validateTermInList (t : ts) (resolveIfRef cddl r) of
        (isValid -> True, leftover) -> validate leftover rs
        (err, _) -> err

    validateTermInList ts (KV _ v _) = validateTermInList ts v
    validateTermInList ts (Group grp) = case grp of
      (resolveIfRef cddl -> g) : gs
        | (res, leftover) <- validateTermInList ts g ->
            case res of
              (isValid -> True) -> validateTermInList leftover (Group gs)
              err -> (err, ts)
      [] -> (toResult $ TerminalRule rule, ts)
    validateTermInList (t : ts) r = (validateTerm cddl t r, ts)
    validateTermInList [] g = (validate [] [g], [])

--------------------------------------------------------------------------------
-- Maps

validateMap ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  [(Term, Term)] ->
  CTree ValidatorStage ->
  ValidationResult
validateMap cddl terms (resolveIfRef cddl -> rule) =
  case rule of
    Postlude PTAny -> toResult $ TerminalRule rule
    Map rules -> validate [] terms rules
    Choice opts -> validateChoice (validateMap cddl terms) opts rule
    _ -> toResult $ UnapplicableRule rule
  where
    validate ::
      [CTree ValidatorStage] -> [(Term, Term)] -> [CTree ValidatorStage] -> ValidationResult
    validate [] [] [] = toResult $ TerminalRule rule
    validate _ _ [] = toResult $ UnapplicableRule rule
    validate [] [] (r : rs)
      | isOptional r = validate [] [] rs
      | otherwise = toResult $ UnapplicableRule rule
    validate exhausted kvs (r : rs) = case r of
      Occur ct oi -> case oi of
        OIOptional
          | (isValid -> True, leftover) <- validateKVInMap kvs ct
          , res <- validate [] leftover (exhausted <> rs)
          , isValid res ->
              res
          | otherwise -> validate (r : exhausted) kvs rs
        OIZeroOrMore
          | (isValid -> True, leftover) <- validateKVInMap kvs ct
          , res <- validate [] leftover (r : exhausted <> rs)
          , isValid res ->
              res
          | otherwise -> validate (r : exhausted) kvs rs
        OIOneOrMore
          | (isValid -> True, leftover) <- validateKVInMap kvs ct
          , res <- validate [] leftover (Occur ct OIZeroOrMore : exhausted <> rs)
          , isValid res ->
              res
          | otherwise -> validate (r : exhausted) kvs rs
        OIBounded mlb mub
          | Just lb <- mlb, Just ub <- mub, lb > ub -> error "Unsatisfiable range encountered"
          | otherwise -> case compare 0 <$> mub of
              Just EQ -> validate exhausted kvs rs
              Just GT -> error "Unsatisfiable range encountered"
              _
                | (isValid -> True, leftover) <- validateKVInMap kvs ct
                , res <-
                    validate
                      []
                      leftover
                      (Occur ct (decrementBounds (mlb, mub)) : exhausted <> rs)
                , isValid res ->
                    res
                | otherwise -> validate (r : exhausted) kvs rs
      _ -> case validateKVInMap kvs r of
        (isValid -> True, leftover) -> validate [] leftover (exhausted <> rs)
        _ -> validate (r : exhausted) kvs rs

    validateKVInMap ((tk, tv) : ts) (KV k v _) = case (validateTerm cddl tk k, validateTerm cddl tv v) of
      (isValid -> True, res) -> (res, ts)
      (err, _) -> (err, ts)
    validateKVInMap [] _ = error "No remaining KV pairs"
    validateKVInMap _ x = error $ "Unexpected value in map: " <> showSimple x

--------------------------------------------------------------------------------
-- Choices

validateChoice ::
  (CTree ValidatorStage -> ValidationResult) ->
  NE.NonEmpty (CTree ValidatorStage) ->
  CTree ValidatorStage ->
  ValidationResult
validateChoice v rules = go rules
  where
    go :: NE.NonEmpty (CTree ValidatorStage) -> CTree ValidatorStage -> ValidationResult
    go (choice NE.:| xs) rule = do
      case v choice of
        (isValid -> True) -> toResult $ TerminalRule rule
        err -> case xs of
          [] -> err -- TODO return something more useful instead of the last failure
          y : ys -> go (y NE.:| ys) rule

--------------------------------------------------------------------------------
-- Control helpers

-- | Validate both rules
ctrlAnd ::
  (CTree ValidatorStage -> ValidationResult) ->
  CTree ValidatorStage ->
  CTree ValidatorStage ->
  ValidationResult
ctrlAnd v tgt ctrl =
  case v tgt of
    (isValid -> True) -> v ctrl
    err -> err

-- | Dispatch to the appropriate control
ctrlDispatch ::
  (CTree ValidatorStage -> ValidationResult) ->
  CtlOp ->
  CTree ValidatorStage ->
  CTree ValidatorStage ->
  (CtlOp -> CTree ValidatorStage -> ValidationResult) ->
  ValidationResult
ctrlDispatch v And tgt ctrl _ = ctrlAnd v tgt ctrl
ctrlDispatch v Within tgt ctrl _ = ctrlAnd v tgt ctrl
ctrlDispatch v op tgt ctrl vctrl =
  case v tgt of
    (isValid -> True) -> vctrl op ctrl
    err -> err

--------------------------------------------------------------------------------
-- Bits control

getIndicesOfChoice :: CTreeRoot ValidatorStage -> NE.NonEmpty (CTree ValidatorStage) -> [Word64]
getIndicesOfChoice cddl =
  concatMap $ \case
    Literal (Value (VUInt v) _) -> [fromIntegral v]
    KV _ v _ ->
      case resolveIfRef cddl v of
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
getIndicesOfRange cddl ff tt incl =
  case (resolveIfRef cddl ff, resolveIfRef cddl tt) of
    (Literal (Value (VUInt ff') _), Literal (Value (VUInt tt') _)) ->
      case incl of
        ClOpen -> init rng
        Closed -> rng
      where
        rng = [ff' .. tt']
    (lo, hi) -> error $ "Malformed range in .bits: (" <> showSimple lo <> ", " <> showSimple hi <> ")"

getIndicesOfEnum :: CTreeRoot ValidatorStage -> CTree ValidatorStage -> [Word64]
getIndicesOfEnum cddl g =
  case resolveIfRef cddl g of
    Group g' -> getIndicesOfChoice cddl (fromJust $ NE.nonEmpty g')
    somethingElse -> error $ "Malformed enum in .bits: " <> showSimple somethingElse

--------------------------------------------------------------------------------
-- Resolving rules from the CDDL spec

resolveIfRef :: CTreeRoot ValidatorStage -> CTree ValidatorStage -> CTree ValidatorStage
resolveIfRef ct@(CTreeRoot cddl) (CTreeE (VRuleRef n)) = do
  case Map.lookup n cddl of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val -> resolveIfRef ct val
resolveIfRef _ r = r

--------------------------------------------------------------------------------
-- Utils

range :: Ord a => RangeBound -> a -> a -> Bool
range Closed = (<=)
range ClOpen = (<)
