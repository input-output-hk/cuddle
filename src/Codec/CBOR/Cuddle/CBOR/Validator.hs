{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.CBOR.Cuddle.CBOR.Validator (
  validateCBOR,
  isCBORTermResultValid,
  CDDLResult (..),
  CBORTermResult (..),
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
import Data.Bifunctor (Bifunctor (..))
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
      mapExt (MGenerator _ _ x) = mapIndex x
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

instance IndexMappable AMatchedItem ValidatorStage ValidatorStageSimple where
  mapIndex (AMatchedItem x y z) = AMatchedItem x y $ mapIndex z

instance IndexMappable ANonMatchedItem ValidatorStage ValidatorStageSimple where
  mapIndex (ANonMatchedItem x y z) =
    ANonMatchedItem x y $
      bimap (bimap mapIndex mapIndex) (\(a, b, c) -> (mapIndex a, mapIndex b, mapIndex c)) <$> z

instance IndexMappable CDDLResult ValidatorStage ValidatorStageSimple where
  mapIndex (Valid x) = Valid $ mapIndex x
  mapIndex (ChoiceFail x y z) = ChoiceFail (mapIndex x) (mapIndex <$> y) (bimap mapIndex mapIndex <$> z)
  mapIndex (ListExpansionFail x y z) =
    ListExpansionFail
      (mapIndex x)
      (fmap mapIndex <$> y)
      (fmap (bimap mapIndex mapIndex) <$> z)
  mapIndex (MapExpansionFail x y z) = MapExpansionFail (mapIndex x) (fmap mapIndex <$> y) (bimap (fmap mapIndex) mapIndex <$> z)
  mapIndex (InvalidControl x y) = InvalidControl (mapIndex x) (mapIndex <$> y)
  mapIndex (InvalidRule x) = InvalidRule $ mapIndex x
  mapIndex (InvalidTagged x y) = InvalidTagged (mapIndex x) (mapIndex <$> y)
  mapIndex (UnapplicableRule x y) = UnapplicableRule x $ mapIndex y
  mapIndex (CustomValidatorFailure x y) = CustomValidatorFailure x $ mapIndex y

instance IndexMappable CBORTermResult ValidatorStage ValidatorStageSimple where
  mapIndex (CBORTermResult x y) = CBORTermResult x $ mapIndex y

showSimple ::
  ( IndexMappable a ValidatorStage ValidatorStageSimple
  , Show (a ValidatorStageSimple)
  ) =>
  a ValidatorStage -> String
showSimple = show . mapIndex @_ @_ @ValidatorStageSimple

deriving instance Eq (Node ValidatorStageSimple)

deriving instance Eq (CBORTermResult ValidatorStageSimple)

deriving instance Eq (AMatchedItem ValidatorStageSimple)

deriving instance Eq (ANonMatchedItem ValidatorStageSimple)

deriving instance Eq (CDDLResult ValidatorStageSimple)

deriving instance Show (Node ValidatorStageSimple)

deriving instance Show (CBORTermResult ValidatorStageSimple)

deriving instance Show (AMatchedItem ValidatorStageSimple)

deriving instance Show (ANonMatchedItem ValidatorStageSimple)

deriving instance Show (CDDLResult ValidatorStageSimple)

data CBORTermResult i = CBORTermResult
  { ctrTerm :: Term
  , ctrResult :: CDDLResult i
  }

data CDDLResult i
  = -- | The rule was valid
    Valid (CTree i)
  | -- | All alternatives failed
    ChoiceFail
      -- | Rule we are trying
      (CTree i)
      -- | The alternatives that arise from said rule
      (NE.NonEmpty (CTree i))
      -- | For each alternative, the result
      (NE.NonEmpty (CTree i, CDDLResult i))
  | -- | All expansions failed
    --
    -- An expansion is: Given a CBOR @TList@ of @N@ elements, we will expand the
    -- rules in a list spec to match the number of items in the list.
    ListExpansionFail
      -- | Rule we are trying
      (CTree i)
      -- | List of expansions of rules
      [[CTree i]]
      -- | For each expansion, for each of the rules in the expansion, the result
      [[(CTree i, CBORTermResult i)]]
  | -- | All expansions failed
    --
    -- An expansion is: Given a CBOR @TMap@ of @N@ elements, we will expand the
    -- rules in a map spec to match the number of items in the map.
    MapExpansionFail
      -- | Rule we are trying
      (CTree i)
      -- | List of expansions
      [[CTree i]]
      -- | A list of matched items @(key, value, rule)@ and the unmatched item
      [([AMatchedItem i], ANonMatchedItem i)]
  | -- | The rule was valid but the control failed
    InvalidControl
      -- | Control we are trying
      (CTree i)
      -- | If it is a .cbor, the result of the underlying validation
      (Maybe (CBORTermResult i))
  | InvalidRule (CTree i)
  | -- | A tagged was invalid
    InvalidTagged
      -- | Rule we are trying
      (CTree i)
      -- | Either the tag is wrong, or the contents are wrong
      (Either Word64 (CBORTermResult i))
  | -- | The rule we are trying is not applicable to the CBOR term
    UnapplicableRule
      -- | Extra information
      T.Text
      -- | Rule we are trying
      (CTree i)
  | -- | A user-provided validator failed to validate the term
    CustomValidatorFailure
      -- | Extra information
      T.Text
      -- | Rule we are trying
      (CTree i)

isCBORTermResultValid :: CBORTermResult ValidatorStage -> Bool
isCBORTermResultValid (CBORTermResult _ Valid {}) = True
isCBORTermResultValid _ = False

data ANonMatchedItem i = ANonMatchedItem
  { anmiKey :: Term
  , anmiValue :: Term
  , anmiResults :: [Either (CTree i, CDDLResult i) (CTree i, CDDLResult i, CDDLResult i)]
  -- ^ For all the tried rules, either the key failed or the key succeeded and
  -- the value failed
  }

data AMatchedItem i = AMatchedItem
  { amiKey :: Term
  , amiValue :: Term
  , amiRule :: CTree i
  }

--------------------------------------------------------------------------------
-- Main entry point

validateCBOR ::
  HasCallStack =>
  BS.ByteString ->
  Name ->
  CTreeRoot ValidatorStage ->
  CBORTermResult ValidatorStage
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
  CBORTermResult ValidatorStage
validateTerm cddl term (resolveIfRef cddl -> rule)
  | CTreeE (VValidator (CBORValidator validator) _) <- rule =
      CBORTermResult term $ case validator term of
        ValidatorSuccess -> Valid rule
        ValidatorFailure e -> CustomValidatorFailure e rule
  | otherwise =
      CBORTermResult term $ case term of
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
  CDDLResult ValidatorStage
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
    Postlude PTAny -> Valid rule
    -- a = int
    Postlude PTInt -> Valid rule
    -- a = uint
    Postlude PTUInt -> check (i >= 0) rule
    -- a = nint
    Postlude PTNInt -> check (i <= 0) rule
    -- a = x
    Literal (Value (VUInt i') _) -> check (i == fromIntegral i') rule
    -- a = -x
    Literal (Value (VNInt i') _) -> check (-i == fromIntegral i') rule
    -- a = <big number>
    Literal (Value (VBignum i') _) -> check (i == i') rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateInteger cddl i) op tgt ctrl (controlInteger cddl i) rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateInteger cddl i) opts rule
    -- a = x..y
    Range low high bound ->
      check
        ( case (resolveIfRef cddl low, resolveIfRef cddl high) of
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= i && range bound i m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> -n <= i && range bound i m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> -n <= i && range bound i (-m)
            (Literal (Value VUInt {} _), Literal (Value VNInt {} _)) -> False
            (Literal (Value (VBignum n) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= i && range bound i m
            (Literal (Value (VBignum n) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> n <= i && range bound i (-m)
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _)) -> n <= i && range bound i m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _)) -> (-n) <= i && range bound i m
            (lo, hi) -> error $ "Unable to validate range: (" <> showSimple lo <> ", " <> showSimple hi <> ")"
        )
        rule
    -- a = &(x, y, z)
    Enum g ->
      case resolveIfRef cddl g of
        Group g' -> replaceRule (validateInteger cddl i (Choice (NE.fromList g'))) rule
        _ -> error "Not yet implemented"
    -- a = x: y
    -- Note KV cannot appear on its own, but we will use this when validating
    -- lists.
    KV _ v _ -> replaceRule (validateInteger cddl i v) rule
    Tag 2 x -> validateBigInt x
    Tag 3 x -> validateBigInt x
    _ -> UnapplicableRule "validateInteger" rule
  where
    validateBigInt x = case resolveIfRef cddl x of
      Postlude PTBytes -> Valid rule
      Control op tgt@(Postlude PTBytes) ctrl ->
        ctrlDispatch (validateBytes cddl bs) op tgt ctrl (controlBytes cddl bs) rule
        where
          -- TODO figure out a way to turn Integer into bytes or figure out why
          -- tagged bigints are decoded as integers in the first place
          bs = mempty
      _ -> error "Not yet implemented"

-- | Controls for an Integer
controlInteger ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Integer ->
  CtlOp ->
  CTree ValidatorStage ->
  Either (Maybe (CBORTermResult ValidatorStage)) ()
controlInteger cddl i Size ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VUInt sz) _) ->
      boolCtrl $ 0 <= i && i < 256 ^ sz
    _ -> error "Not yet implemented"
controlInteger cddl i Bits ctrl = do
  let
    indices = case resolveIfRef cddl ctrl of
      Literal (Value (VUInt i') _) -> [i']
      Choice nodes -> getIndicesOfChoice cddl nodes
      Range ff tt incl -> getIndicesOfRange cddl ff tt incl
      Enum g -> getIndicesOfEnum cddl g
      _ -> error "Not yet implemented"
  boolCtrl $ go (IS.fromList (map fromIntegral indices)) i 0
  where
    go _ 0 _ = True
    go indices n idx =
      let bitSet = testBit n 0
          allowed = not bitSet || IS.member idx indices
       in (allowed && go indices (shiftR n 1) (idx + 1))
controlInteger cddl i Lt ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VUInt i') _) -> i < fromIntegral i'
    Literal (Value (VNInt i') _) -> i < -fromIntegral i'
    Literal (Value (VBignum i') _) -> i < i'
    _ -> error "Not yet implemented"
controlInteger cddl i Gt ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VUInt i') _) -> i > fromIntegral i'
    Literal (Value (VNInt i') _) -> i > -fromIntegral i'
    Literal (Value (VBignum i') _) -> i > i'
    _ -> error "Not yet implemented"
controlInteger cddl i Le ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VUInt i') _) -> i <= fromIntegral i'
    Literal (Value (VNInt i') _) -> i <= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i <= i'
    _ -> error "Not yet implemented"
controlInteger cddl i Ge ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VUInt i') _) -> i >= fromIntegral i'
    Literal (Value (VNInt i') _) -> i >= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i >= i'
    _ -> error "Not yet implemented"
controlInteger cddl i Eq ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VUInt i') _) -> i == fromIntegral i'
    Literal (Value (VNInt i') _) -> i == -fromIntegral i'
    Literal (Value (VBignum i') _) -> i == i'
    _ -> error "Not yet implemented"
controlInteger cddl i Ne ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VUInt i') _) -> i /= fromIntegral i'
    Literal (Value (VNInt i') _) -> i /= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i /= i'
    _ -> error "Not yet implemented"
controlInteger _ _ _ _ = error "Not yet implemented"

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
  CDDLResult ValidatorStage
validateHalf cddl f rule =
  case resolveIfRef cddl rule of
    -- a = any
    Postlude PTAny -> Valid rule
    -- a = float16
    Postlude PTHalf -> Valid rule
    -- a = 0.5
    Literal (Value (VFloat16 f') _) -> check (f == f') rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateHalf cddl f) opts rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateHalf cddl f) op tgt ctrl (controlHalf cddl f) rule
    -- a = x..y
    Range low high bound ->
      check
        ( case (resolveIfRef cddl low, resolveIfRef cddl high) of
            (Literal (Value (VFloat16 n) _), Literal (Value (VFloat16 m) _)) -> n <= f && range bound f m
            _ -> error "Not yet implemented"
        )
        rule
    _ -> UnapplicableRule "validateHalf" rule

-- | Controls for `Float16`
controlHalf ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Float ->
  CtlOp ->
  CTree ValidatorStage ->
  Either (Maybe (CBORTermResult ValidatorStage)) ()
controlHalf cddl f Eq ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VFloat16 f') _) -> f == f'
    _ -> error "Not yet implemented"
controlHalf cddl f Ne ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VFloat16 f') _) -> f /= f'
    _ -> error "Not yet implemented"
controlHalf _ _ _ _ = error "Not yet implemented"

-- | Validating a `Float32`
validateFloat ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Float ->
  CTree ValidatorStage ->
  CDDLResult ValidatorStage
validateFloat cddl f rule =
  ($ rule) $ do
    case resolveIfRef cddl rule of
      -- a = any
      Postlude PTAny -> Valid
      -- a = float32
      Postlude PTFloat -> Valid
      -- a = 0.000000005
      -- TODO: it is unclear if smaller floats should also validate
      Literal (Value (VFloat32 f') _) -> check $ f == f'
      -- a = foo / bar
      Choice opts -> validateChoice (validateFloat cddl f) opts
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateFloat cddl f) op tgt ctrl (controlFloat cddl f)
      -- a = x..y
      -- TODO it is unclear if this should mix floating point types too
      Range low high bound ->
        check $ case (resolveIfRef cddl low, resolveIfRef cddl high) of
          (Literal (Value (VFloat16 n) _), Literal (Value (VFloat16 m) _)) -> n <= f && range bound f m
          (Literal (Value (VFloat32 n) _), Literal (Value (VFloat32 m) _)) -> n <= f && range bound f m
          _ -> error "Not yet implemented"
      _ -> UnapplicableRule "validateFloat"

-- | Controls for `Float32`
controlFloat ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Float ->
  CtlOp ->
  CTree ValidatorStage ->
  Either (Maybe (CBORTermResult ValidatorStage)) ()
controlFloat cddl f Eq ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VFloat16 f') _) -> f == f'
    Literal (Value (VFloat32 f') _) -> f == f'
    _ -> error "Not yet implemented"
controlFloat cddl f Ne ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VFloat16 f') _) -> f /= f'
    Literal (Value (VFloat32 f') _) -> f /= f'
    _ -> error "Not yet implemented"
controlFloat _ _ _ _ = error "Not yet implemented"

-- | Validating a `Float64`
validateDouble ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Double ->
  CTree ValidatorStage ->
  CDDLResult ValidatorStage
validateDouble cddl f rule =
  ($ rule) $ do
    case resolveIfRef cddl rule of
      -- a = any
      Postlude PTAny -> Valid
      -- a = float64
      Postlude PTDouble -> Valid
      -- a = 0.0000000000000000000000000000000000000000000005
      -- TODO: it is unclear if smaller floats should also validate
      Literal (Value (VFloat64 f') _) -> check $ f == f'
      -- a = foo / bar
      Choice opts -> validateChoice (validateDouble cddl f) opts
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateDouble cddl f) op tgt ctrl (controlDouble cddl f)
      -- a = x..y
      -- TODO it is unclear if this should mix floating point types too
      Range low high bound ->
        check $ case (resolveIfRef cddl low, resolveIfRef cddl high) of
          (Literal (Value (VFloat16 (float2Double -> n)) _), Literal (Value (VFloat16 (float2Double -> m)) _)) -> n <= f && range bound f m
          (Literal (Value (VFloat32 (float2Double -> n)) _), Literal (Value (VFloat32 (float2Double -> m)) _)) -> n <= f && range bound f m
          (Literal (Value (VFloat64 n) _), Literal (Value (VFloat64 m) _)) -> n <= f && range bound f m
          _ -> error "Not yet implemented"
      _ -> UnapplicableRule "validateDouble"

-- | Controls for `Float64`
controlDouble ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Double ->
  CtlOp ->
  CTree ValidatorStage ->
  Either (Maybe (CBORTermResult ValidatorStage)) ()
controlDouble cddl f Eq ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VFloat16 f') _) -> f == float2Double f'
    Literal (Value (VFloat32 f') _) -> f == float2Double f'
    Literal (Value (VFloat64 f') _) -> f == f'
    _ -> error "Not yet implemented"
controlDouble cddl f Ne ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VFloat16 f') _) -> f /= float2Double f'
    Literal (Value (VFloat32 f') _) -> f /= float2Double f'
    Literal (Value (VFloat64 f') _) -> f /= f'
    _ -> error "Not yet implemented"
controlDouble _ _ _ _ = error "Not yet implmented"

--------------------------------------------------------------------------------
-- Bool

-- | Validating a boolean
validateBool ::
  CTreeRoot ValidatorStage ->
  Bool ->
  CTree ValidatorStage ->
  CDDLResult ValidatorStage
validateBool cddl b rule =
  ($ rule) $ do
    case resolveIfRef cddl rule of
      -- a = any
      Postlude PTAny -> Valid
      -- a = bool
      Postlude PTBool -> Valid
      -- a = true
      Literal (Value (VBool b') _) -> check $ b == b'
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateBool cddl b) op tgt ctrl (controlBool cddl b)
      -- a = foo / bar
      Choice opts -> validateChoice (validateBool cddl b) opts
      _ -> UnapplicableRule "validateBool"

-- | Controls for `Bool`
controlBool ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  Bool ->
  CtlOp ->
  CTree ValidatorStage ->
  Either (Maybe (CBORTermResult ValidatorStage)) ()
controlBool cddl b Eq ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VBool b') _) -> b == b'
    _ -> error "Not yet implemented"
controlBool cddl b Ne ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VBool b') _) -> b /= b'
    _ -> error "Not yet implemented"
controlBool _ _ _ _ = error "Not yet implemented"

--------------------------------------------------------------------------------
-- Simple

-- | Validating a `TSimple`. It is unclear if this is used for anything else than undefined.
validateSimple ::
  CTreeRoot ValidatorStage ->
  Word8 ->
  CTree ValidatorStage ->
  CDDLResult ValidatorStage
validateSimple cddl 23 rule =
  do
    case resolveIfRef cddl rule of
      -- a = any
      Postlude PTAny -> Valid rule
      -- a = undefined
      Postlude PTUndefined -> Valid rule
      -- a = foo / bar
      Choice opts -> validateChoice (validateSimple cddl 23) opts rule
      _ -> UnapplicableRule "validateSimple" rule
validateSimple _ n _ = error $ "Found simple different to 23! please report this somewhere! Found: " <> show n

--------------------------------------------------------------------------------
-- Null/nil

-- | Validating nil
validateNull :: CTreeRoot ValidatorStage -> CTree ValidatorStage -> CDDLResult ValidatorStage
validateNull cddl rule =
  case resolveIfRef cddl rule of
    -- a = any
    Postlude PTAny -> Valid rule
    -- a = nil
    Postlude PTNil -> Valid rule
    Choice opts -> validateChoice (validateNull cddl) opts rule
    _ -> UnapplicableRule "validateNull" rule

--------------------------------------------------------------------------------
-- Bytes

-- | Validating a byte sequence
validateBytes ::
  CTreeRoot ValidatorStage -> BS.ByteString -> CTree ValidatorStage -> CDDLResult ValidatorStage
validateBytes cddl bs rule =
  case resolveIfRef cddl rule of
    -- a = any
    Postlude PTAny -> Valid rule
    -- a = bytes
    Postlude PTBytes -> Valid rule
    -- a = h'123456'
    Literal (Value (VBytes bs') _) -> check (bs == bs') rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateBytes cddl bs) op tgt ctrl (controlBytes cddl bs) rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateBytes cddl bs) opts rule
    _ -> UnapplicableRule "validateBytes" rule

-- | Controls for byte strings
controlBytes ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  BS.ByteString ->
  CtlOp ->
  CTree ValidatorStage ->
  Either (Maybe (CBORTermResult ValidatorStage)) ()
controlBytes cddl bs Size ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VUInt (fromIntegral -> sz)) _) -> boolCtrl $ BS.length bs == sz
    Range low high bound ->
      let i = BS.length bs
       in boolCtrl $ case (resolveIfRef cddl low, resolveIfRef cddl high) of
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= i && range bound i m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> -n <= i && range bound i m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> -n <= i && range bound i (-m)
            (Literal (Value VUInt {} _), Literal (Value VNInt {} _)) -> False
            _ -> error "Not yet implemented"
    _ -> error "Not yet implemented"
controlBytes cddl bs Bits ctrl = do
  let
    indices =
      case resolveIfRef cddl ctrl of
        Literal (Value (VUInt i') _) -> [i']
        Choice nodes -> getIndicesOfChoice cddl nodes
        Range ff tt incl -> getIndicesOfRange cddl ff tt incl
        Enum g -> getIndicesOfEnum cddl g
        _ -> error "Not yet implemented"
  boolCtrl $ bitsControlCheck (map fromIntegral indices)
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
        CBORTermResult _ (Valid _) -> Right ()
        err -> Left $ Just err
    _ -> error "Not yet implemented"
controlBytes cddl bs Cborseq ctrl =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict (BS.snoc (BS.cons 0x9f bs) 0xff)) of
    Right (BSL.null -> True, TListI terms) ->
      case validateTerm cddl (TList terms) (Array [Occur ctrl OIZeroOrMore]) of
        CBORTermResult _ (Valid _) -> Right ()
        CBORTermResult _ err -> error . showSimple $ err
    _ -> error "Not yet implemented"
controlBytes _ _ _ _ = error "Not yet implmented"

--------------------------------------------------------------------------------
-- Text

-- | Validating text strings
validateText ::
  CTreeRoot ValidatorStage ->
  T.Text ->
  CTree ValidatorStage ->
  CDDLResult ValidatorStage
validateText cddl txt rule =
  case resolveIfRef cddl rule of
    -- a = any
    Postlude PTAny -> Valid rule
    -- a = text
    Postlude PTText -> Valid rule
    -- a = "foo"
    Literal (Value (VText txt') _) -> check (txt == txt') rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateText cddl txt) op tgt ctrl (controlText cddl txt) rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateText cddl txt) opts rule
    _ -> UnapplicableRule "validateText" rule

-- | Controls for text strings
controlText ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  T.Text ->
  CtlOp ->
  CTree ValidatorStage ->
  Either (Maybe (CBORTermResult ValidatorStage)) ()
controlText cddl bs Size ctrl =
  case resolveIfRef cddl ctrl of
    Literal (Value (VUInt (fromIntegral -> sz)) _) -> boolCtrl $ T.length bs == sz
    Range ff tt bound ->
      boolCtrl $ case (resolveIfRef cddl ff, resolveIfRef cddl tt) of
        (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= T.length bs && range bound (T.length bs) m
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> -n <= T.length bs && range bound (T.length bs) m
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> -n <= T.length bs && range bound (T.length bs) (-m)
        _ -> error "Not yet implemented"
    _ -> error "Not yet implemented"
controlText cddl s Regexp ctrl =
  boolCtrl $ case resolveIfRef cddl ctrl of
    Literal (Value (VText rxp) _) -> case s =~ rxp :: (T.Text, T.Text, T.Text) of
      ("", s', "") -> s == s'
      _ -> error "Not yet implemented"
    _ -> error "Not yet implemented"
controlText _ _ _ _ = error "Not yet implemented"

--------------------------------------------------------------------------------
-- Tagged values

-- | Validating a `TTagged`
validateTagged ::
  CTreeRoot ValidatorStage -> Word64 -> Term -> CTree ValidatorStage -> CDDLResult ValidatorStage
validateTagged cddl tag term rule =
  case resolveIfRef cddl rule of
    Postlude PTAny -> Valid rule
    Tag tag' rule' ->
      -- If the tag does not match, this is a direct fail
      if tag == tag'
        then case validateTerm cddl term rule' of
          CBORTermResult _ (Valid _) -> Valid rule
          err -> InvalidTagged rule (Right err)
        else InvalidTagged rule (Left tag)
    Choice opts -> validateChoice (validateTagged cddl tag term) opts rule
    _ -> UnapplicableRule "validateTagged" rule

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
  CDDLResult ValidatorStage
validateList cddl terms rule =
  case resolveIfRef cddl rule of
    Postlude PTAny -> Valid rule
    Array rules -> validate terms rules
    Choice opts -> validateChoice (validateList cddl terms) opts rule
    r -> UnapplicableRule "validateList" r
  where
    validate :: [Term] -> [CTree ValidatorStage] -> CDDLResult ValidatorStage
    validate [] [] = Valid rule
    validate _ [] = ListExpansionFail rule [] []
    validate [] (r : rs)
      | isOptional r = validate [] rs
      | otherwise = UnapplicableRule "validateList" r
    validate (t : ts) (r : rs) = case r of
      Occur ct oi -> case oi of
        OIOptional
          | (Valid {}, leftover) <- validateTermInList (t : ts) ct
          , res@Valid {} <- validate leftover rs ->
              res
          | otherwise -> validate (t : ts) rs
        OIZeroOrMore
          | (Valid {}, leftover) <- validateTermInList (t : ts) ct
          , res@Valid {} <- validate leftover (r : rs) ->
              res
          | otherwise -> validate (t : ts) rs
        OIOneOrMore -> case validateTermInList (t : ts) ct of
          (Valid {}, leftover) -> validate leftover (Occur ct OIZeroOrMore : rs)
          (err, _) -> err
        OIBounded _ (Just ub) | ub < 0 -> ListExpansionFail rule [] []
        OIBounded lb ub
          | (Valid {}, leftover) <- validateTermInList (t : ts) ct ->
              validate leftover (Occur ct (decrementBounds lb ub) : rs)
          | isWithinBoundsInclusive 0 lb ub ->
              validate (t : ts) rs
          | otherwise -> UnapplicableRule "validateList" r
      _ -> case validateTermInList (t : ts) (resolveIfRef cddl r) of
        (Valid {}, leftover) -> validate leftover rs
        (err, _) -> err

    validateTermInList ts (KV _ v _) = validateTermInList ts v
    validateTermInList ts (Group grp) = case grp of
      (resolveIfRef cddl -> g) : gs
        | (Valid {}, leftover) <- validateTermInList ts g -> validateTermInList leftover (Group gs)
        | otherwise -> (UnapplicableRule "validateTermInList group" g, ts)
      [] -> (Valid rule, ts)
    validateTermInList (t : ts) r =
      let CBORTermResult _ res = validateTerm cddl t r
       in (res, ts)
    validateTermInList [] g = (validate [] [g], [])

--------------------------------------------------------------------------------
-- Maps

validateMap ::
  HasCallStack =>
  CTreeRoot ValidatorStage ->
  [(Term, Term)] ->
  CTree ValidatorStage ->
  CDDLResult ValidatorStage
validateMap cddl terms rule =
  case resolveIfRef cddl rule of
    Postlude PTAny -> Valid rule
    Map rules -> validate [] terms rules
    Choice opts -> validateChoice (validateMap cddl terms) opts rule
    r -> UnapplicableRule "validateMap" r
  where
    validate ::
      [CTree ValidatorStage] -> [(Term, Term)] -> [CTree ValidatorStage] -> CDDLResult ValidatorStage
    validate [] [] [] = Valid rule
    validate _ _ [] = MapExpansionFail rule [] []
    validate [] [] (r : rs)
      | isOptional r = validate [] [] rs
      | otherwise = UnapplicableRule "validateMap" r
    validate exhausted kvs (r : rs) = case r of
      Occur ct oi -> case oi of
        OIOptional
          | (Valid {}, leftover) <- validateKVInMap kvs ct
          , res@Valid {} <- validate [] leftover (exhausted <> rs) ->
              res
          | otherwise -> validate (r : exhausted) kvs rs
        OIZeroOrMore
          | (Valid {}, leftover) <- validateKVInMap kvs ct
          , res@Valid {} <- validate [] leftover (r : exhausted <> rs) ->
              res
          | otherwise -> validate (r : exhausted) kvs rs
        OIOneOrMore
          | (Valid {}, leftover) <- validateKVInMap kvs ct
          , res@Valid {} <- validate [] leftover (Occur ct OIZeroOrMore : exhausted <> rs) ->
              res
          | otherwise -> validate (r : exhausted) kvs rs
        OIBounded mlb mub
          | Just lb <- mlb, Just ub <- mub, lb > ub -> error "Unsatisfiable range encountered"
          | otherwise -> case compare 0 <$> mub of
              Just EQ -> validate exhausted kvs rs
              Just GT -> error "Unsatisfiable range encountered"
              _
                | (Valid {}, leftover) <- validateKVInMap kvs ct
                , res@Valid {} <-
                    validate
                      []
                      leftover
                      (Occur ct (decrementBounds mlb mub) : exhausted <> rs) ->
                    res
                | otherwise -> validate (r : exhausted) kvs rs
      _ -> case validateKVInMap kvs r of
        (Valid {}, leftover) -> validate [] leftover (exhausted <> rs)
        _ -> validate (r : exhausted) kvs rs

    validateKVInMap ((tk, tv) : ts) (KV k v _) = case (validateTerm cddl tk k, validateTerm cddl tv v) of
      (CBORTermResult _ Valid {}, CBORTermResult _ x@Valid {}) -> (x, ts)
      (CBORTermResult _ Valid {}, CBORTermResult _ err) -> (err, ts)
      (CBORTermResult _ err, _) -> (err, ts)
    validateKVInMap [] _ = error "No remaining KV pairs"
    validateKVInMap _ x = error $ "Unexpected value in map: " <> showSimple x

--------------------------------------------------------------------------------
-- Choices

validateChoice ::
  (CTree ValidatorStage -> CDDLResult ValidatorStage) ->
  NE.NonEmpty (CTree ValidatorStage) ->
  CTree ValidatorStage ->
  CDDLResult ValidatorStage
validateChoice v rules = go rules
  where
    go :: NE.NonEmpty (CTree ValidatorStage) -> CTree ValidatorStage -> CDDLResult ValidatorStage
    go (choice NE.:| xs) rule = do
      case v choice of
        Valid _ -> Valid rule
        err -> case NE.nonEmpty xs of
          Nothing -> ChoiceFail rule rules ((choice, err) NE.:| [])
          Just choices ->
            case go choices rule of
              Valid _ -> Valid rule
              ChoiceFail _ _ errors -> ChoiceFail rule rules ((choice, err) NE.<| errors)
              _ -> error "Not yet implemented"

--------------------------------------------------------------------------------
-- Control helpers

-- | Validate both rules
ctrlAnd ::
  (CTree ValidatorStage -> CDDLResult ValidatorStage) ->
  CTree ValidatorStage ->
  CTree ValidatorStage ->
  CTree ValidatorStage ->
  CDDLResult ValidatorStage
ctrlAnd v tgt ctrl rule =
  case v tgt of
    Valid _ ->
      case v ctrl of
        Valid _ -> Valid rule
        _ -> InvalidControl rule Nothing
    _ -> InvalidRule rule

-- | Dispatch to the appropriate control
ctrlDispatch ::
  (CTree ValidatorStage -> CDDLResult ValidatorStage) ->
  CtlOp ->
  CTree ValidatorStage ->
  CTree ValidatorStage ->
  (CtlOp -> CTree ValidatorStage -> Either (Maybe (CBORTermResult ValidatorStage)) ()) ->
  CTree ValidatorStage ->
  CDDLResult ValidatorStage
ctrlDispatch v And tgt ctrl _ rule = ctrlAnd v tgt ctrl rule
ctrlDispatch v Within tgt ctrl _ rule = ctrlAnd v tgt ctrl rule
ctrlDispatch v op tgt ctrl vctrl rule =
  case v tgt of
    Valid _ ->
      case vctrl op ctrl of
        Left err -> InvalidControl rule err
        Right () -> Valid rule
    _ -> InvalidRule rule

-- | A boolean control
boolCtrl :: Bool -> Either (Maybe (CBORTermResult ValidatorStage)) ()
boolCtrl c = if c then Right () else Left Nothing

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

replaceRule :: CDDLResult ValidatorStage -> CTree ValidatorStage -> CDDLResult ValidatorStage
replaceRule (ChoiceFail _ a b) r = ChoiceFail r a b
replaceRule (ListExpansionFail _ a b) r = ListExpansionFail r a b
replaceRule (MapExpansionFail _ a b) r = MapExpansionFail r a b
replaceRule (InvalidTagged _ a) r = InvalidTagged r a
replaceRule InvalidRule {} r = InvalidRule r
replaceRule (InvalidControl _ a) r = InvalidControl r a
replaceRule (UnapplicableRule m _) r = UnapplicableRule m r
replaceRule Valid {} r = Valid r
replaceRule (CustomValidatorFailure e r) _ = CustomValidatorFailure e r

check :: Bool -> CTree ValidatorStage -> CDDLResult ValidatorStage
check c = if c then Valid else InvalidRule

range :: Ord a => RangeBound -> a -> a -> Bool
range Closed = (<=)
range ClOpen = (<)
