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
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (CBORValidator (..), ValidationResult (..))
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
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Word
import GHC.Float
import GHC.Stack (HasCallStack)
import Text.Regex.TDFA

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
  | CTreeE (VValidator (CBORValidator validator) _) <- rule = validator term
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
validateInteger cddl i rule =
  case resolveIfRef cddl rule of
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
    Postlude PTAny -> ValidatorSuccess
    -- a = int
    Postlude PTInt -> ValidatorSuccess
    -- a = uint
    Postlude PTUInt
      | i >= 0 -> ValidatorSuccess
    -- a = nint
    Postlude PTNInt | i <= 0 -> ValidatorSuccess
    -- a = x
    Literal (Value (VUInt i') _) | i == fromIntegral i' -> ValidatorSuccess
    -- a = -x
    Literal (Value (VNInt i') _) | -i == fromIntegral i' -> ValidatorSuccess
    -- a = <big number>
    Literal (Value (VBignum i') _) | i == i' -> ValidatorSuccess
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateInteger cddl i) op tgt ctrl (controlInteger cddl i)
    -- a = foo / bar
    Choice opts -> validateChoice (validateInteger cddl i) opts rule
    -- a = x..y
    Range low high bound ->
      case (resolveIfRef cddl low, resolveIfRef cddl high) of
        (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) | n <= i && range bound i m -> ValidatorSuccess
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) | -n <= i && range bound i m -> ValidatorSuccess
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) | -n <= i && range bound i (-m) -> ValidatorSuccess
        (Literal (Value VUInt {} _), Literal (Value VNInt {} _)) -> ValidatorFailure "range types mismatch"
        (Literal (Value (VBignum n) _), Literal (Value (VUInt (fromIntegral -> m)) _)) | n <= i && range bound i m -> ValidatorSuccess
        (Literal (Value (VBignum n) _), Literal (Value (VNInt (fromIntegral -> m)) _)) | n <= i && range bound i (-m) -> ValidatorSuccess
        (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _)) | n <= i && range bound i m -> ValidatorSuccess
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _)) | (-n) <= i && range bound i m -> ValidatorSuccess
        (lo, hi) ->
          ValidatorFailure . T.pack $
            "Unable to validate range: (" <> showSimple lo <> ", " <> showSimple hi <> ")"
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
    _ -> ValidatorFailure "unapplicable rule: validateInteger"
  where
    validateBigInt x = case resolveIfRef cddl x of
      Postlude PTBytes -> ValidatorSuccess
      Control op tgt@(Postlude PTBytes) ctrl ->
        ctrlDispatch (validateBytes cddl bs) op tgt ctrl (controlBytes cddl bs)
        where
          -- TODO figure out a way to turn Integer into bytes or figure out why
          -- tagged bigints are decoded as integers in the first place
          bs = mempty
      _ -> ValidatorFailure "Failed to validate BigInt"

-- | Controls for an Integer
controlInteger ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Integer ->
  CtlOp ->
  CTree ValidatorStage ->
  ValidationResult
controlInteger cddl i Size ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VUInt sz) _)
      | 0 <= i && i < 256 ^ sz -> ValidatorSuccess
    _ -> ValidatorFailure ".size control not satisfied"
controlInteger cddl i Bits ctrl = do
  let
    indices = case resolveIfRef cddl ctrl of
      Literal (Value (VUInt i') _) -> [i']
      Choice nodes -> getIndicesOfChoice cddl nodes
      Range ff tt incl -> getIndicesOfRange cddl ff tt incl
      Enum g -> getIndicesOfEnum cddl g
      _ -> error "Not yet implemented"
  if go (IS.fromList (map fromIntegral indices)) i 0
    then ValidatorSuccess
    else ValidatorFailure ".bits control not satisfied"
  where
    go _ 0 _ = True
    go indices n idx =
      let bitSet = testBit n 0
          allowed = not bitSet || IS.member idx indices
       in allowed && go indices (shiftR n 1) (idx + 1)
controlInteger cddl i Lt ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VUInt i') _) | i < fromIntegral i' -> ValidatorSuccess
    Literal (Value (VNInt i') _) | i < -fromIntegral i' -> ValidatorSuccess
    Literal (Value (VBignum i') _) | i < i' -> ValidatorSuccess
    _ -> ValidatorFailure ".lt control not satisfied"
controlInteger cddl i Gt ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VUInt i') _) | i > fromIntegral i' -> ValidatorSuccess
    Literal (Value (VNInt i') _) | i > -fromIntegral i' -> ValidatorSuccess
    Literal (Value (VBignum i') _) | i > i' -> ValidatorSuccess
    _ -> ValidatorFailure ".gt control not satisfied"
controlInteger cddl i Le ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VUInt i') _) | i <= fromIntegral i' -> ValidatorSuccess
    Literal (Value (VNInt i') _) | i <= -fromIntegral i' -> ValidatorSuccess
    Literal (Value (VBignum i') _) | i <= i' -> ValidatorSuccess
    _ -> ValidatorFailure ".le control not satisfied"
controlInteger cddl i Ge ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VUInt i') _) | i >= fromIntegral i' -> ValidatorSuccess
    Literal (Value (VNInt i') _) | i >= -fromIntegral i' -> ValidatorSuccess
    Literal (Value (VBignum i') _) | i >= i' -> ValidatorSuccess
    _ -> ValidatorFailure ".ge control not satisfied"
controlInteger cddl i Eq ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VUInt i') _) | i == fromIntegral i' -> ValidatorSuccess
    Literal (Value (VNInt i') _) | i == -fromIntegral i' -> ValidatorSuccess
    Literal (Value (VBignum i') _) | i == i' -> ValidatorSuccess
    _ -> ValidatorFailure ".eq control not satisfied"
controlInteger cddl i Ne ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VUInt i') _) | i /= fromIntegral i' -> ValidatorSuccess
    Literal (Value (VNInt i') _) | i /= -fromIntegral i' -> ValidatorSuccess
    Literal (Value (VBignum i') _) | i /= i' -> ValidatorSuccess
    _ -> ValidatorFailure ".ne control not satisfied"
controlInteger _ _ _ ctrl = ValidatorFailure . T.pack $ "unexpected control: " <> showSimple ctrl

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
validateHalf cddl f rule =
  case resolveIfRef cddl rule of
    -- a = any
    Postlude PTAny -> ValidatorSuccess
    -- a = float16
    Postlude PTHalf -> ValidatorSuccess
    -- a = 0.5
    Literal (Value (VFloat16 f') _) | f == f' -> ValidatorSuccess
    -- a = foo / bar
    Choice opts -> validateChoice (validateHalf cddl f) opts rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateHalf cddl f) op tgt ctrl (controlHalf cddl f)
    -- a = x..y
    Range
      (resolveIfRef cddl -> Literal (Value (VFloat16 n) _))
      (resolveIfRef cddl -> Literal (Value (VFloat16 m) _))
      bound | n <= f && range bound f m -> ValidatorSuccess
    _ -> ValidatorFailure "unapplicable rule: validateHalf"

-- | Controls for `Float16`
controlHalf ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Float ->
  CtlOp ->
  CTree ValidatorStage ->
  ValidationResult
controlHalf cddl f Eq ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VFloat16 f') _) | f == f' -> ValidatorSuccess
    _ -> error "Not yet implemented"
controlHalf cddl f Ne ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VFloat16 f') _) | f /= f' -> ValidatorSuccess
    _ -> error "Not yet implemented"
controlHalf _ _ _ _ = error "Not yet implemented"

-- | Validating a `Float32`
validateFloat ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Float ->
  CTree ValidatorStage ->
  ValidationResult
validateFloat cddl f rule =
  do
    case resolveIfRef cddl rule of
      -- a = any
      Postlude PTAny -> ValidatorSuccess
      -- a = float32
      Postlude PTFloat -> ValidatorSuccess
      -- a = 0.000000005
      -- TODO: it is unclear if smaller floats should also validate
      Literal (Value (VFloat32 f') _) | f == f' -> ValidatorSuccess
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
              ValidatorSuccess
          | VFloat32 n <- nv
          , VFloat32 m <- mv
          , n <= f && range bound f m ->
              ValidatorSuccess
      _ -> ValidatorFailure "unapplicable rule: validateFloat"

-- | Controls for `Float32`
controlFloat ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Float ->
  CtlOp ->
  CTree ValidatorStage ->
  ValidationResult
controlFloat cddl f Eq ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VFloat16 f') _) | f == f' -> ValidatorSuccess
    Literal (Value (VFloat32 f') _) | f == f' -> ValidatorSuccess
    _ -> error "Not yet implemented"
controlFloat cddl f Ne ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VFloat16 f') _) | f /= f' -> ValidatorSuccess
    Literal (Value (VFloat32 f') _) | f /= f' -> ValidatorSuccess
    _ -> error "Not yet implemented"
controlFloat _ _ _ _ = error "Not yet implemented"

-- | Validating a `Float64`
validateDouble ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Double ->
  CTree ValidatorStage ->
  ValidationResult
validateDouble cddl f rule =
  case resolveIfRef cddl rule of
    -- a = any
    Postlude PTAny -> ValidatorSuccess
    -- a = float64
    Postlude PTDouble -> ValidatorSuccess
    -- a = 0.0000000000000000000000000000000000000000000005
    -- TODO: it is unclear if smaller floats should also validate
    Literal (Value (VFloat64 f') _) | f == f' -> ValidatorSuccess
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
            ValidatorSuccess
        | VFloat32 n <- nv
        , VFloat32 m <- mv
        , float2Double n <= f && range bound f (float2Double m) ->
            ValidatorSuccess
        | VFloat64 n <- nv
        , VFloat64 m <- mv
        , n <= f && range bound f m ->
            ValidatorSuccess
    _ -> ValidatorFailure "unapplicable rule: validateDouble"

-- | Controls for `Float64`
controlDouble ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Double ->
  CtlOp ->
  CTree ValidatorStage ->
  ValidationResult
controlDouble cddl f Eq ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VFloat16 f') _) | f == float2Double f' -> ValidatorSuccess
    Literal (Value (VFloat32 f') _) | f == float2Double f' -> ValidatorSuccess
    Literal (Value (VFloat64 f') _) | f == f' -> ValidatorSuccess
    _ -> error "Not yet implemented"
controlDouble cddl f Ne ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VFloat16 f') _) | f /= float2Double f' -> ValidatorSuccess
    Literal (Value (VFloat32 f') _) | f /= float2Double f' -> ValidatorSuccess
    Literal (Value (VFloat64 f') _) | f /= f' -> ValidatorSuccess
    _ -> error "Not yet implemented"
controlDouble _ _ _ _ = error "Not yet implmented"

--------------------------------------------------------------------------------
-- Bool

-- | Validating a boolean
validateBool ::
  CTreeRoot ValidatorStage ->
  Bool ->
  CTree ValidatorStage ->
  ValidationResult
validateBool cddl b rule =
  case resolveIfRef cddl rule of
    -- a = any
    Postlude PTAny -> ValidatorSuccess
    -- a = bool
    Postlude PTBool -> ValidatorSuccess
    -- a = true
    Literal (Value (VBool b') _) | b == b' -> ValidatorSuccess
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateBool cddl b) op tgt ctrl (controlBool cddl b)
    -- a = foo / bar
    Choice opts -> validateChoice (validateBool cddl b) opts rule
    _ -> ValidatorFailure "unapplicable rule: validateBool"

-- | Controls for `Bool`
controlBool ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Bool ->
  CtlOp ->
  CTree ValidatorStage ->
  ValidationResult
controlBool cddl b Eq ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VBool b') _) | b == b' -> ValidatorSuccess
    _ -> error "Not yet implemented"
controlBool cddl b Ne ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VBool b') _) | b /= b' -> ValidatorSuccess
    _ -> error "Not yet implemented"
controlBool _ _ _ _ = error "Not yet implemented"

--------------------------------------------------------------------------------
-- Simple

-- | Validating a `TSimple`. It is unclear if this is used for anything else than undefined.
validateSimple ::
  CTreeRoot ValidatorStage ->
  Word8 ->
  CTree ValidatorStage ->
  ValidationResult
validateSimple cddl 23 rule =
  do
    case resolveIfRef cddl rule of
      -- a = any
      Postlude PTAny -> ValidatorSuccess
      -- a = undefined
      Postlude PTUndefined -> ValidatorSuccess
      -- a = foo / bar
      Choice opts -> validateChoice (validateSimple cddl 23) opts rule
      _ -> ValidatorFailure "unapplicable rule: validateSimple"
validateSimple _ n _ = error $ "Found simple different to 23! please report this somewhere! Found: " <> show n

--------------------------------------------------------------------------------
-- Null/nil

-- | Validating nil
validateNull :: CTreeRoot ValidatorStage -> CTree ValidatorStage -> ValidationResult
validateNull cddl rule =
  case resolveIfRef cddl rule of
    -- a = any
    Postlude PTAny -> ValidatorSuccess
    -- a = nil
    Postlude PTNil -> ValidatorSuccess
    Choice opts -> validateChoice (validateNull cddl) opts rule
    _ -> ValidatorFailure "unapplicable rule: validateNull"

--------------------------------------------------------------------------------
-- Bytes

-- | Validating a byte sequence
validateBytes ::
  CTreeRoot ValidatorStage -> BS.ByteString -> CTree ValidatorStage -> ValidationResult
validateBytes cddl bs rule =
  case resolveIfRef cddl rule of
    -- a = any
    Postlude PTAny -> ValidatorSuccess
    -- a = bytes
    Postlude PTBytes -> ValidatorSuccess
    -- a = h'123456'
    Literal (Value (VBytes bs') _) | bs == bs' -> ValidatorSuccess
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateBytes cddl bs) op tgt ctrl (controlBytes cddl bs)
    -- a = foo / bar
    Choice opts -> validateChoice (validateBytes cddl bs) opts rule
    _ -> ValidatorFailure "unapplicable rule: validateBytes"

-- | Controls for byte strings
controlBytes ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  BS.ByteString ->
  CtlOp ->
  CTree ValidatorStage ->
  ValidationResult
controlBytes cddl bs Size ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VUInt (fromIntegral -> sz)) _) | BS.length bs == sz -> ValidatorSuccess
    Range low high bound ->
      let i = BS.length bs
       in case (resolveIfRef cddl low, resolveIfRef cddl high) of
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) | n <= i && range bound i m -> ValidatorSuccess
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) | -n <= i && range bound i m -> ValidatorSuccess
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) | -n <= i && range bound i (-m) -> ValidatorSuccess
            _ -> error "Not yet implemented"
    _ -> ValidatorFailure ".size control not satisfied"
controlBytes cddl bs Bits ctrl = do
  let
    indices =
      case resolveIfRef cddl ctrl of
        Literal (Value (VUInt i') _) -> [i']
        Choice nodes -> getIndicesOfChoice cddl nodes
        Range ff tt incl -> getIndicesOfRange cddl ff tt incl
        Enum g -> getIndicesOfEnum cddl g
        _ -> error "Not yet implemented"
  if bitsControlCheck (map fromIntegral indices)
    then ValidatorSuccess
    else ValidatorFailure ".bits control not satisfied"
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
    Right (BSL.null -> True, term) ->
      case validateTerm cddl term ctrl of
        ValidatorSuccess -> ValidatorSuccess
        err -> err
    _ -> error "Not yet implemented"
controlBytes cddl bs Cborseq ctrl =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict (BS.snoc (BS.cons 0x9f bs) 0xff)) of
    Right (BSL.null -> True, TListI terms) ->
      case validateTerm cddl (TList terms) (Array [Occur ctrl OIZeroOrMore]) of
        ValidatorSuccess -> ValidatorSuccess
        err -> err
    _ -> error "Not yet implemented"
controlBytes _ _ _ _ = error "Not yet implmented"

--------------------------------------------------------------------------------
-- Text

-- | Validating text strings
validateText ::
  CTreeRoot ValidatorStage ->
  T.Text ->
  CTree ValidatorStage ->
  ValidationResult
validateText cddl txt rule =
  case resolveIfRef cddl rule of
    -- a = any
    Postlude PTAny -> ValidatorSuccess
    -- a = text
    Postlude PTText -> ValidatorSuccess
    -- a = "foo"
    Literal (Value (VText txt') _) | txt == txt' -> ValidatorSuccess
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateText cddl txt) op tgt ctrl (controlText cddl txt)
    -- a = foo / bar
    Choice opts -> validateChoice (validateText cddl txt) opts rule
    _ -> ValidatorFailure "unapplicable rule: validateText"

-- | Controls for text strings
controlText ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  T.Text ->
  CtlOp ->
  CTree ValidatorStage ->
  ValidationResult
controlText cddl bs Size ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VUInt (fromIntegral -> sz)) _) | T.length bs == sz -> ValidatorSuccess
    Range ff tt bound ->
      case (resolveIfRef cddl ff, resolveIfRef cddl tt) of
        (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) | n <= T.length bs && range bound (T.length bs) m -> ValidatorSuccess
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) | -n <= T.length bs && range bound (T.length bs) m -> ValidatorSuccess
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) | -n <= T.length bs && range bound (T.length bs) (-m) -> ValidatorSuccess
        _ -> error "Not yet implemented"
    _ -> error "Not yet implemented"
controlText cddl s Regexp ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VText rxp) _) -> case s =~ rxp :: (T.Text, T.Text, T.Text) of
      ("", s', "") | s == s' -> ValidatorSuccess
      _ -> error "Not yet implemented"
    _ -> error "Not yet implemented"
controlText _ _ _ _ = error "Not yet implemented"

--------------------------------------------------------------------------------
-- Tagged values

-- | Validating a `TTagged`
validateTagged ::
  CTreeRoot ValidatorStage -> Word64 -> Term -> CTree ValidatorStage -> ValidationResult
validateTagged cddl tag term rule =
  case resolveIfRef cddl rule of
    Postlude PTAny -> ValidatorSuccess
    Tag tag' rule' ->
      -- If the tag does not match, this is a direct fail
      if tag == tag'
        then case validateTerm cddl term rule' of
          ValidatorSuccess -> ValidatorSuccess
          err -> err
        else
          ValidatorFailure . T.pack $
            "invalid tag: " <> show tag <> "\nexpected: " <> show tag'
    Choice opts -> validateChoice (validateTagged cddl tag term) opts rule
    _ -> ValidatorFailure "unapplicable rule: validateTagged"

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

decrementBounds :: Maybe Word64 -> Maybe Word64 -> OccurrenceIndicator
decrementBounds lb ub = OIBounded (clampedPred <$> lb) (clampedPred <$> ub)
  where
    clampedPred 0 = 0
    clampedPred x = pred x

validateList ::
  CTreeRoot ValidatorStage ->
  [Term] ->
  CTree ValidatorStage ->
  ValidationResult
validateList cddl terms rule =
  case resolveIfRef cddl rule of
    Postlude PTAny -> ValidatorSuccess
    Array rules -> validate terms rules
    Choice opts -> validateChoice (validateList cddl terms) opts rule
    _ -> ValidatorFailure "unapplicable rule: validateList"
  where
    validate :: [Term] -> [CTree ValidatorStage] -> ValidationResult
    validate [] [] = ValidatorSuccess
    validate _ [] = ValidatorFailure "leftover terms after all rules have been applied"
    validate [] (r : rs)
      | isOptional r = validate [] rs
      | otherwise = ValidatorFailure "leftover mandatory rules after all terms have been consumed"
    validate (t : ts) (r : rs) = case r of
      Occur ct oi -> case oi of
        OIOptional
          | (ValidatorSuccess, leftover) <- validateTermInList (t : ts) ct
          , res@ValidatorSuccess <- validate leftover rs ->
              res
          | otherwise -> validate (t : ts) rs
        OIZeroOrMore
          | (ValidatorSuccess, leftover) <- validateTermInList (t : ts) ct
          , res@ValidatorSuccess <- validate leftover (r : rs) ->
              res
          | otherwise -> validate (t : ts) rs
        OIOneOrMore -> case validateTermInList (t : ts) ct of
          (ValidatorSuccess, leftover) -> validate leftover (Occur ct OIZeroOrMore : rs)
          (err, _) -> err
        OIBounded _ (Just ub) | ub < 0 -> ValidatorFailure "term count above bounds"
        OIBounded lb ub
          | (ValidatorSuccess, leftover) <- validateTermInList (t : ts) ct ->
              validate leftover (Occur ct (decrementBounds lb ub) : rs)
          | isWithinBoundsInclusive 0 lb ub ->
              validate (t : ts) rs
          | otherwise -> ValidatorFailure "term count below bounds"
      _ -> case validateTermInList (t : ts) (resolveIfRef cddl r) of
        (ValidatorSuccess, leftover) -> validate leftover rs
        (err, _) -> err

    validateTermInList ts (KV _ v _) = validateTermInList ts v
    validateTermInList ts (Group grp) = case grp of
      (resolveIfRef cddl -> g) : gs
        | (res, leftover) <- validateTermInList ts g ->
            case res of
              ValidatorSuccess -> validateTermInList leftover (Group gs)
              err -> (err, ts)
      [] -> (ValidatorSuccess, ts)
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
validateMap cddl terms rule =
  case resolveIfRef cddl rule of
    Postlude PTAny -> ValidatorSuccess
    Map rules -> validate [] terms rules
    Choice opts -> validateChoice (validateMap cddl terms) opts rule
    _ -> ValidatorFailure "unapplicable rule: validateMap"
  where
    validate ::
      [CTree ValidatorStage] -> [(Term, Term)] -> [CTree ValidatorStage] -> ValidationResult
    validate [] [] [] = ValidatorSuccess
    validate _ _ [] = ValidatorFailure ""
    validate [] [] (r : rs)
      | isOptional r = validate [] [] rs
      | otherwise = ValidatorFailure "unapplicable rule: validateMap"
    validate exhausted kvs (r : rs) = case r of
      Occur ct oi -> case oi of
        OIOptional
          | (ValidatorSuccess, leftover) <- validateKVInMap kvs ct
          , res@ValidatorSuccess <- validate [] leftover (exhausted <> rs) ->
              res
          | otherwise -> validate (r : exhausted) kvs rs
        OIZeroOrMore
          | (ValidatorSuccess, leftover) <- validateKVInMap kvs ct
          , res@ValidatorSuccess <- validate [] leftover (r : exhausted <> rs) ->
              res
          | otherwise -> validate (r : exhausted) kvs rs
        OIOneOrMore
          | (ValidatorSuccess, leftover) <- validateKVInMap kvs ct
          , res@ValidatorSuccess <- validate [] leftover (Occur ct OIZeroOrMore : exhausted <> rs) ->
              res
          | otherwise -> validate (r : exhausted) kvs rs
        OIBounded mlb mub
          | Just lb <- mlb, Just ub <- mub, lb > ub -> error "Unsatisfiable range encountered"
          | otherwise -> case compare 0 <$> mub of
              Just EQ -> validate exhausted kvs rs
              Just GT -> error "Unsatisfiable range encountered"
              _
                | (ValidatorSuccess, leftover) <- validateKVInMap kvs ct
                , res@ValidatorSuccess <-
                    validate
                      []
                      leftover
                      (Occur ct (decrementBounds mlb mub) : exhausted <> rs) ->
                    res
                | otherwise -> validate (r : exhausted) kvs rs
      _ -> case validateKVInMap kvs r of
        (ValidatorSuccess, leftover) -> validate [] leftover (exhausted <> rs)
        _ -> validate (r : exhausted) kvs rs

    validateKVInMap ((tk, tv) : ts) (KV k v _) = case (validateTerm cddl tk k, validateTerm cddl tv v) of
      (ValidatorSuccess, x@ValidatorSuccess) -> (x, ts)
      (ValidatorSuccess, err) -> (err, ts)
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
        ValidatorSuccess -> ValidatorSuccess
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
    ValidatorSuccess ->
      case v ctrl of
        ValidatorSuccess -> ValidatorSuccess
        _ -> ValidatorFailure "invalid control"
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
    ValidatorSuccess -> vctrl op ctrl
    _ -> ValidatorFailure "invalid rule"

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
