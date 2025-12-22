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
  ValidationTree (..),
) where

import Codec.CBOR.Cuddle.CDDL hiding (CDDL, Group, Rule)
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

newtype instance XXCTree ValidatorStage = VRuleRef Name
  deriving (Show, Eq)

instance IndexMappable CTreeRoot MonoReferenced ValidatorStage where
  mapIndex (CTreeRoot m) = CTreeRoot $ mapIndex <$> m

instance IndexMappable CTree MonoReferenced ValidatorStage where
  mapIndex = foldCTree mapExt mapIndex
    where
      mapExt (MRuleRef n) = CTreeE $ VRuleRef n
      mapExt (MGenerator _ x) = mapIndex x

type CDDL = CTreeRoot ValidatorStage
type Rule = CTree ValidatorStage

data CBORTermResult = CBORTermResult
  { ctrTerm :: Term
  , ctrResult :: CDDLResult
  }
  deriving (Show, Eq)

data ValidationTree
  = ConsumeTerm Term
  | RuleFail Rule
  | MapFail Int [Rule] [Rule]
  | ArrayFail Int [Rule] [Rule]
  deriving (Show, Eq)

data CDDLResult
  = -- | The rule was valid
    Valid ValidationTree
  | Invalid ValidationTree
  deriving (Show, Eq)

isCBORTermResultValid :: CBORTermResult -> Bool
isCBORTermResultValid (CBORTermResult _ Valid {}) = True
isCBORTermResultValid _ = False

--------------------------------------------------------------------------------
-- Main entry point

validateCBOR :: BS.ByteString -> Name -> CDDL -> CBORTermResult
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
validateTerm :: CDDL -> Term -> Rule -> CBORTermResult
validateTerm cddl term rule =
  CBORTermResult term $ case term of
    TInt i -> validateInteger cddl (fromIntegral i) rRule
    TInteger i -> validateInteger cddl i rRule
    TBytes b -> validateBytes cddl b rRule
    TBytesI b -> validateBytes cddl (BSL.toStrict b) rRule
    TString s -> validateText cddl s rRule
    TStringI s -> validateText cddl (TL.toStrict s) rRule
    TList ts -> validateList cddl ts rRule
    TListI ts -> validateList cddl ts rRule
    TMap ts -> validateMap cddl ts rRule
    TMapI ts -> validateMap cddl ts rRule
    TTagged w t -> validateTagged cddl w t rRule
    TBool b -> validateBool cddl b rRule
    TNull -> validateNull cddl rRule
    TSimple s -> validateSimple cddl s rRule
    THalf h -> validateHalf cddl h rRule
    TFloat h -> validateFloat cddl h rRule
    TDouble d -> validateDouble cddl d rRule
  where
    rRule = resolveIfRef cddl rule

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
validateInteger :: HasCallStack => CDDL -> Integer -> Rule -> CDDLResult
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
    Postlude PTAny -> Valid $ RuleMatch rule
    -- a = int
    Postlude PTInt -> Valid $ RuleMatch rule
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
            x -> error $ "Unable to validate range: " <> show x
        )
        rule
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
    _ -> Invalid $ RuleFail rule
  where
    validateBigInt x = case resolveIfRef cddl x of
      Postlude PTBytes -> Valid $ RuleMatch rule
      Control op tgt@(Postlude PTBytes) ctrl ->
        ctrlDispatch (validateBytes cddl bs) op tgt ctrl (controlBytes cddl bs) rule
        where
          -- TODO figure out a way to turn Integer into bytes or figure out why
          -- tagged bigints are decoded as integers in the first place
          bs = mempty
      e -> error $ "Not yet implemented" <> show e

-- | Controls for an Integer
controlInteger ::
  HasCallStack => CDDL -> Integer -> CtlOp -> Rule -> Either (Maybe CBORTermResult) ()
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
validateHalf :: HasCallStack => CDDL -> Float -> Rule -> CDDLResult
validateHalf cddl f rule =
  case resolveIfRef cddl rule of
    -- a = any
    Postlude PTAny -> Valid $ RuleMatch rule
    -- a = float16
    Postlude PTHalf -> Valid $ RuleMatch rule
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
    _ -> Invalid $ RuleFail rule

-- | Controls for `Float16`
controlHalf :: HasCallStack => CDDL -> Float -> CtlOp -> Rule -> Either (Maybe CBORTermResult) ()
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
validateFloat :: HasCallStack => CDDL -> Float -> Rule -> CDDLResult
validateFloat cddl f rule =
  ($ rule) $ do
    case resolveIfRef cddl rule of
      -- a = any
      Postlude PTAny -> Valid . RuleMatch
      -- a = float32
      Postlude PTFloat -> Valid . RuleMatch
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
      _ -> Invalid . RuleFail

-- | Controls for `Float32`
controlFloat :: HasCallStack => CDDL -> Float -> CtlOp -> Rule -> Either (Maybe CBORTermResult) ()
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
validateDouble :: HasCallStack => CDDL -> Double -> Rule -> CDDLResult
validateDouble cddl f rule =
  ($ rule) $ do
    case resolveIfRef cddl rule of
      -- a = any
      Postlude PTAny -> Valid . RuleMatch
      -- a = float64
      Postlude PTDouble -> Valid . RuleMatch
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
      _ -> Invalid . RuleFail

-- | Controls for `Float64`
controlDouble :: HasCallStack => CDDL -> Double -> CtlOp -> Rule -> Either (Maybe CBORTermResult) ()
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
validateBool :: CDDL -> Bool -> Rule -> CDDLResult
validateBool cddl b rule =
  ($ rule) $ do
    case resolveIfRef cddl rule of
      -- a = any
      Postlude PTAny -> Valid . RuleMatch
      -- a = bool
      Postlude PTBool -> Valid . RuleMatch
      -- a = true
      Literal (Value (VBool b') _) -> check $ b == b'
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateBool cddl b) op tgt ctrl (controlBool cddl b)
      -- a = foo / bar
      Choice opts -> validateChoice (validateBool cddl b) opts
      _ -> Invalid . RuleFail

-- | Controls for `Bool`
controlBool :: HasCallStack => CDDL -> Bool -> CtlOp -> Rule -> Either (Maybe CBORTermResult) ()
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
validateSimple :: CDDL -> Word8 -> Rule -> CDDLResult
validateSimple cddl 23 rule =
  do
    case resolveIfRef cddl rule of
      -- a = any
      Postlude PTAny -> Valid $ RuleMatch rule
      -- a = undefined
      Postlude PTUndefined -> Valid $ RuleMatch rule
      -- a = foo / bar
      Choice opts -> validateChoice (validateSimple cddl 23) opts rule
      _ -> Invalid $ RuleFail rule
validateSimple _ n _ = error $ "Found simple different to 23! please report this somewhere! Found: " <> show n

--------------------------------------------------------------------------------
-- Null/nil

-- | Validating nil
validateNull :: CDDL -> Rule -> CDDLResult
validateNull cddl rule =
  case resolveIfRef cddl rule of
    -- a = any
    Postlude PTAny -> Valid $ RuleMatch rule
    -- a = nil
    Postlude PTNil -> Valid $ RuleMatch rule
    Choice opts -> validateChoice (validateNull cddl) opts rule
    _ -> Invalid $ RuleFail rule

--------------------------------------------------------------------------------
-- Bytes

-- | Validating a byte sequence
validateBytes :: CDDL -> BS.ByteString -> Rule -> CDDLResult
validateBytes cddl bs rule =
  case resolveIfRef cddl rule of
    -- a = any
    Postlude PTAny -> Valid $ RuleMatch rule
    -- a = bytes
    Postlude PTBytes -> Valid $ RuleMatch rule
    -- a = h'123456'
    Literal (Value (VBytes bs') _) -> check (bs == bs') rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateBytes cddl bs) op tgt ctrl (controlBytes cddl bs) rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateBytes cddl bs) opts rule
    _ -> Invalid $ RuleFail rule

-- | Controls for byte strings
controlBytes ::
  HasCallStack =>
  CDDL ->
  BS.ByteString ->
  CtlOp ->
  Rule ->
  Either (Maybe CBORTermResult) ()
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
        CBORTermResult _ err -> error $ show err
    _ -> error "Not yet implemented"
controlBytes _ _ _ _ = error "Not yet implmented"

--------------------------------------------------------------------------------
-- Text

-- | Validating text strings
validateText :: CDDL -> T.Text -> Rule -> CDDLResult
validateText cddl txt rule =
  case resolveIfRef cddl rule of
    -- a = any
    Postlude PTAny -> Valid $ RuleMatch rule
    -- a = text
    Postlude PTText -> Valid $ RuleMatch rule
    -- a = "foo"
    Literal (Value (VText txt') _) -> check (txt == txt') rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateText cddl txt) op tgt ctrl (controlText cddl txt) rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateText cddl txt) opts rule
    _ -> Invalid $ RuleFail rule

-- | Controls for text strings
controlText :: HasCallStack => CDDL -> T.Text -> CtlOp -> Rule -> Either (Maybe CBORTermResult) ()
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
validateTagged :: CDDL -> Word64 -> Term -> Rule -> CDDLResult
validateTagged cddl tag term rule =
  case resolveIfRef cddl rule of
    Postlude PTAny -> Valid $ RuleMatch rule
    Tag tag' rule' ->
      -- If the tag does not match, this is a direct fail
      if tag == tag'
        then case validateTerm cddl term rule' of
          CBORTermResult _ (Valid _) -> Valid $ RuleMatch rule
          CBORTermResult _ err -> err
        else Invalid $ RuleFail rule
    Choice opts -> validateChoice (validateTagged cddl tag term) opts rule
    _ -> Invalid $ RuleFail rule

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

validateList :: CDDL -> [Term] -> Rule -> CDDLResult
validateList cddl terms rule =
  case resolveIfRef cddl rule of
    Postlude PTAny -> Valid $ RuleMatch rule
    Array rules -> validate terms rules
    Choice opts -> validateChoice (validateList cddl terms) opts rule
    r -> Invalid $ RuleFail r
  where
    validate :: [Term] -> [CTree ValidatorStage] -> CDDLResult
    validate [] [] = Valid $ RuleMatch rule
    validate _ [] = Invalid $ RuleFail rule
    validate [] (r : rs)
      | isOptional r = validate [] rs
      | otherwise = Invalid $ RuleFail r
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
        OIBounded _ (Just ub) | ub < 0 -> Invalid $ RuleFail r
        OIBounded lb ub
          | (Valid {}, leftover) <- validateTermInList (t : ts) ct ->
              validate leftover (Occur ct (decrementBounds lb ub) : rs)
          | isWithinBoundsInclusive 0 lb ub ->
              validate (t : ts) rs
          | otherwise -> Invalid $ RuleFail r
      _ -> case validateTermInList (t : ts) (resolveIfRef cddl r) of
        (Valid {}, leftover) -> validate leftover rs
        (err, _) -> err

    validateTermInList ts (KV _ v _) = validateTermInList ts v
    validateTermInList ts (Group grp) = case grp of
      (resolveIfRef cddl -> g) : gs
        | (Valid {}, leftover) <- validateTermInList ts g -> validateTermInList leftover (Group gs)
        | otherwise -> (Invalid $ RuleFail g, ts)
      [] -> (Valid $ RuleMatch rule, ts)
    validateTermInList (t : ts) r =
      let CBORTermResult _ res = validateTerm cddl t r
       in (res, ts)
    validateTermInList [] g = (validate [] [g], [])

--------------------------------------------------------------------------------
-- Maps

validateMap :: HasCallStack => CDDL -> [(Term, Term)] -> Rule -> CDDLResult
validateMap cddl terms rule =
  case resolveIfRef cddl rule of
    Postlude PTAny -> Valid $ RuleMatch rule
    Map rules -> validate [] terms rules
    Choice opts -> validateChoice (validateMap cddl terms) opts rule
    r -> Invalid $ RuleFail r
  where
    validate :: [Rule] -> [(Term, Term)] -> [Rule] -> CDDLResult
    validate [] [] [] = Valid $ RuleMatch rule
    validate _ _ [] = Invalid $ RuleFail rule
    validate [] [] (r : rs)
      | isOptional r = validate [] [] rs
      | otherwise = Invalid $ RuleFail r
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
    validateKVInMap _ x = error $ "Unexpected value in map: " <> show x

--------------------------------------------------------------------------------
-- Choices

validateChoice :: (Rule -> CDDLResult) -> NE.NonEmpty Rule -> Rule -> CDDLResult
validateChoice v rules = go rules
  where
    go :: NE.NonEmpty Rule -> Rule -> CDDLResult
    go (choice NE.:| xs) rule = do
      case v choice of
        Valid _ -> Valid $ RuleMatch rule
        err -> case NE.nonEmpty xs of
          Nothing -> err
          Just choices ->
            case go choices rule of
              Valid _ -> Valid $ RuleMatch rule
              e -> e

--------------------------------------------------------------------------------
-- Control helpers

-- | Validate both rules
ctrlAnd :: (Rule -> CDDLResult) -> Rule -> Rule -> Rule -> CDDLResult
ctrlAnd v tgt ctrl rule =
  case v tgt of
    Valid {} | Valid {} <- v ctrl -> Valid $ RuleMatch rule
    _ -> Invalid $ RuleFail rule

-- | Dispatch to the appropriate control
ctrlDispatch ::
  (Rule -> CDDLResult) ->
  CtlOp ->
  Rule ->
  Rule ->
  (CtlOp -> Rule -> Either (Maybe CBORTermResult) ()) ->
  Rule ->
  CDDLResult
ctrlDispatch v And tgt ctrl _ rule = ctrlAnd v tgt ctrl rule
ctrlDispatch v Within tgt ctrl _ rule = ctrlAnd v tgt ctrl rule
ctrlDispatch v op tgt ctrl vctrl rule =
  case v tgt of
    Valid _ ->
      case vctrl op ctrl of
        Left _err -> Invalid $ RuleFail rule
        Right () -> Valid $ RuleMatch rule
    _ -> Invalid $ RuleFail rule

-- | A boolean control
boolCtrl :: Bool -> Either (Maybe CBORTermResult) ()
boolCtrl c = if c then Right () else Left Nothing

--------------------------------------------------------------------------------
-- Bits control

getIndicesOfChoice :: CDDL -> NE.NonEmpty Rule -> [Word64]
getIndicesOfChoice cddl =
  concatMap $ \case
    Literal (Value (VUInt v) _) -> [fromIntegral v]
    KV _ v _ ->
      case resolveIfRef cddl v of
        Literal (Value (VUInt v') _) -> [fromIntegral v']
        somethingElse ->
          error $
            "Malformed value in KV in choice in .bits: "
              <> show somethingElse
    Range ff tt incl -> getIndicesOfRange cddl ff tt incl
    Enum g -> getIndicesOfEnum cddl g
    somethingElse ->
      error $
        "Malformed alternative in choice in .bits: "
          <> show somethingElse

getIndicesOfRange :: CDDL -> Rule -> Rule -> RangeBound -> [Word64]
getIndicesOfRange cddl ff tt incl =
  case (resolveIfRef cddl ff, resolveIfRef cddl tt) of
    (Literal (Value (VUInt ff') _), Literal (Value (VUInt tt') _)) ->
      case incl of
        ClOpen -> init rng
        Closed -> rng
      where
        rng = [ff' .. tt']
    somethingElse -> error $ "Malformed range in .bits: " <> show somethingElse

getIndicesOfEnum :: CDDL -> Rule -> [Word64]
getIndicesOfEnum cddl g =
  case resolveIfRef cddl g of
    Group g' -> getIndicesOfChoice cddl (fromJust $ NE.nonEmpty g')
    somethingElse -> error $ "Malformed enum in .bits: " <> show somethingElse

--------------------------------------------------------------------------------
-- Resolving rules from the CDDL spec

resolveIfRef :: CDDL -> Rule -> Rule
resolveIfRef ct@(CTreeRoot cddl) (CTreeE (VRuleRef n)) = do
  case Map.lookup n cddl of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val -> resolveIfRef ct val
resolveIfRef _ r = r

--------------------------------------------------------------------------------
-- Utils

check :: Bool -> Rule -> CDDLResult
check c = if c then Valid . RuleMatch else Invalid . RuleFail

range :: Ord a => RangeBound -> a -> a -> Bool
range Closed = (<=)
range ClOpen = (<)
