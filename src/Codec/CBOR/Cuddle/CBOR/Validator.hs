{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.CBOR.Cuddle.CBOR.Validator (
  validateCBOR,
  CDDLResult (..),
  CBORTermResult (..),
  ValidatorStage,
) where

import Codec.CBOR.Cuddle.CDDL hiding (CDDL, Group, Rule)
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, XXCTree (..))
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Codec.CBOR.Read
import Codec.CBOR.Term
import Data.Bifunctor
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
  deriving (Show)

newtype instance XXCTree ValidatorStage = VRuleRef Name
  deriving (Show)

instance IndexMappable CTreeRoot MonoReferenced ValidatorStage where
  mapIndex (CTreeRoot m) = CTreeRoot $ mapIndex <$> m

instance IndexMappable CTree MonoReferenced ValidatorStage where
  mapIndex = foldCTree mapExt mapIndex
    where
      mapExt (MRuleRef n) = CTreeE $ VRuleRef n
      mapExt (MGenerator _ x) = mapIndex x

type CDDL = CTreeRoot ValidatorStage
type Rule = CTree ValidatorStage

data CBORTermResult = CBORTermResult Term CDDLResult
  deriving (Show)

data CDDLResult
  = -- | The rule was valid
    Valid Rule
  | -- | All alternatives failed
    ChoiceFail
      -- | Rule we are trying
      Rule
      -- | The alternatives that arise from said rule
      (NE.NonEmpty Rule)
      -- | For each alternative, the result
      (NE.NonEmpty (Rule, CDDLResult))
  | -- | All expansions failed
    --
    -- An expansion is: Given a CBOR @TList@ of @N@ elements, we will expand the
    -- rules in a list spec to match the number of items in the list.
    ListExpansionFail
      -- | Rule we are trying
      Rule
      -- | List of expansions of rules
      [[Rule]]
      -- | For each expansion, for each of the rules in the expansion, the result
      [[(Rule, CBORTermResult)]]
  | -- | All expansions failed
    --
    -- An expansion is: Given a CBOR @TMap@ of @N@ elements, we will expand the
    -- rules in a map spec to match the number of items in the map.
    MapExpansionFail
      -- | Rule we are trying
      Rule
      -- | List of expansions
      [[Rule]]
      -- | A list of matched items @(key, value, rule)@ and the unmatched item
      [([AMatchedItem], ANonMatchedItem)]
  | -- | The rule was valid but the control failed
    InvalidControl
      -- | Control we are trying
      Rule
      -- | If it is a .cbor, the result of the underlying validation
      (Maybe CBORTermResult)
  | InvalidRule Rule
  | -- | A tagged was invalid
    InvalidTagged
      -- | Rule we are trying
      Rule
      -- | Either the tag is wrong, or the contents are wrong
      (Either Word64 CBORTermResult)
  | -- | The rule we are trying is not applicable to the CBOR term
    UnapplicableRule
      -- | Rule we are trying
      Rule
  deriving (Show)

data ANonMatchedItem = ANonMatchedItem
  { anmiKey :: Term
  , anmiValue :: Term
  , anmiResults :: [Either (Rule, CDDLResult) (Rule, CDDLResult, CDDLResult)]
  -- ^ For all the tried rules, either the key failed or the key succeeded and
  -- the value failed
  }
  deriving (Show)

data AMatchedItem = AMatchedItem
  { amiKey :: Term
  , amiValue :: Term
  , amiRule :: Rule
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- Main entry point

validateCBOR :: BS.ByteString -> Name -> CDDL -> CBORTermResult
validateCBOR bs rule cddl@(CTreeRoot tree) =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict bs) of
    Left e -> error $ show e
    Right (rest, term) ->
      validateTerm cddl t (tree Map.! rule)
      where
        t
          | BSL.null rest = term
          | otherwise = TBytes bs

--------------------------------------------------------------------------------
-- Terms

-- | Core function that validates a CBOR term to a particular rule of the CDDL
-- spec
validateTerm :: CDDL -> Term -> Rule -> CBORTermResult
validateTerm cddl term =
  CBORTermResult term . case term of
    TInt i -> validateInteger (fromIntegral i)
    TInteger i -> validateInteger i
    TBytes b -> validateBytes cddl b
    TBytesI b -> validateBytes cddl (BSL.toStrict b)
    TString s -> validateText s
    TStringI s -> validateText (TL.toStrict s)
    TList ts -> validateList cddl ts
    TListI ts -> validateList cddl ts
    TMap ts -> validateMap cddl ts
    TMapI ts -> validateMap cddl ts
    TTagged w t -> validateTagged cddl w t
    TBool b -> validateBool b
    TNull -> validateNull
    TSimple s -> validateSimple s
    THalf h -> validateHalf h
    TFloat h -> validateFloat h
    TDouble d -> validateDouble d

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
validateInteger :: HasCallStack => Integer -> Rule -> CDDLResult
validateInteger i rule =
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
    Control op tgt ctrl -> ctrlDispatch (validateInteger i) op tgt ctrl (controlInteger i) rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateInteger i) opts rule
    -- a = x..y
    Range low high bound ->
      check
        ( case (low, high) of
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= i && range bound i m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> -n <= i && range bound i m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> -n <= i && range bound i (-m)
            (Literal (Value VUInt {} _), Literal (Value VNInt {} _)) -> False
            (Literal (Value (VBignum n) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= i && range bound i m
            (Literal (Value (VBignum n) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> n <= i && range bound i (-m)
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _)) -> n <= i && range bound i m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _)) -> (-n) <= i && range bound i m
            _ -> error "Not yet implemented"
        )
        rule
    -- a = &(x, y, z)
    Enum g ->
      case g of
        Group g' -> replaceRule (validateInteger i (Choice (NE.fromList g'))) rule
        _ -> error "Not yet implemented"
    -- a = x: y
    -- Note KV cannot appear on its own, but we will use this when validating
    -- lists.
    KV _ v _ -> replaceRule (validateInteger i v) rule
    Tag 2 (Postlude PTBytes) -> Valid rule
    Tag 3 (Postlude PTBytes) -> Valid rule
    _ -> UnapplicableRule rule

-- | Controls for an Integer
controlInteger :: HasCallStack => Integer -> CtlOp -> Rule -> Either (Maybe CBORTermResult) ()
controlInteger i Size ctrl =
  case ctrl of
    Literal (Value (VUInt sz) _) ->
      boolCtrl $ 0 <= i && i < 256 ^ sz
    _ -> error "Not yet implemented"
controlInteger i Bits ctrl = do
  let
    indices = case ctrl of
      Literal (Value (VUInt i') _) -> [i']
      Choice nodes -> getIndicesOfChoice nodes
      Range ff tt incl -> getIndicesOfRange ff tt incl
      Enum g -> getIndicesOfEnum g
      _ -> error "Not yet implemented"
  boolCtrl $ go (IS.fromList (map fromIntegral indices)) i 0
  where
    go _ 0 _ = True
    go indices n idx =
      let bitSet = testBit n 0
          allowed = not bitSet || IS.member idx indices
       in (allowed && go indices (shiftR n 1) (idx + 1))
controlInteger i Lt ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VUInt i') _) -> i < fromIntegral i'
    Literal (Value (VNInt i') _) -> i < -fromIntegral i'
    Literal (Value (VBignum i') _) -> i < i'
    _ -> error "Not yet implemented"
controlInteger i Gt ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VUInt i') _) -> i > fromIntegral i'
    Literal (Value (VNInt i') _) -> i > -fromIntegral i'
    Literal (Value (VBignum i') _) -> i > i'
    _ -> error "Not yet implemented"
controlInteger i Le ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VUInt i') _) -> i <= fromIntegral i'
    Literal (Value (VNInt i') _) -> i <= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i <= i'
    _ -> error "Not yet implemented"
controlInteger i Ge ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VUInt i') _) -> i >= fromIntegral i'
    Literal (Value (VNInt i') _) -> i >= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i >= i'
    _ -> error "Not yet implemented"
controlInteger i Eq ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VUInt i') _) -> i == fromIntegral i'
    Literal (Value (VNInt i') _) -> i == -fromIntegral i'
    Literal (Value (VBignum i') _) -> i == i'
    _ -> error "Not yet implemented"
controlInteger i Ne ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VUInt i') _) -> i /= fromIntegral i'
    Literal (Value (VNInt i') _) -> i /= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i /= i'
    _ -> error "Not yet implemented"
controlInteger _ _ _ = error "Not yet implemented"

--------------------------------------------------------------------------------
-- Floating point (Float16, Float32, Float64)
--
-- As opposed to Integral types, there seems to be no ambiguity when encoding
-- and decoding floating-point numbers.

-- | Validating a `Float16`
validateHalf :: HasCallStack => Float -> Rule -> CDDLResult
validateHalf f rule =
  ($ rule) $ do
    case rule of
      -- a = any
      Postlude PTAny -> Valid
      -- a = float16
      Postlude PTHalf -> Valid
      -- a = 0.5
      Literal (Value (VFloat16 f') _) -> check $ f == f'
      -- a = foo / bar
      Choice opts -> validateChoice (validateHalf f) opts
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateHalf f) op tgt ctrl (controlHalf f)
      -- a = x..y
      Range low high bound ->
        check $ case (low, high) of
          (Literal (Value (VFloat16 n) _), Literal (Value (VFloat16 m) _)) -> n <= f && range bound f m
          _ -> error "Not yet implemented"
      _ -> UnapplicableRule

-- | Controls for `Float16`
controlHalf :: HasCallStack => Float -> CtlOp -> Rule -> Either (Maybe CBORTermResult) ()
controlHalf f Eq ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f == f'
    _ -> error "Not yet implemented"
controlHalf f Ne ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f /= f'
    _ -> error "Not yet implemented"
controlHalf _ _ _ = error "Not yet implemented"

-- | Validating a `Float32`
validateFloat :: HasCallStack => Float -> Rule -> CDDLResult
validateFloat f rule =
  ($ rule) $ do
    case rule of
      -- a = any
      Postlude PTAny -> Valid
      -- a = float32
      Postlude PTFloat -> Valid
      -- a = 0.000000005
      -- TODO: it is unclear if smaller floats should also validate
      Literal (Value (VFloat32 f') _) -> check $ f == f'
      -- a = foo / bar
      Choice opts -> validateChoice (validateFloat f) opts
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateFloat f) op tgt ctrl (controlFloat f)
      -- a = x..y
      -- TODO it is unclear if this should mix floating point types too
      Range low high bound ->
        check $ case (low, high) of
          (Literal (Value (VFloat16 n) _), Literal (Value (VFloat16 m) _)) -> n <= f && range bound f m
          (Literal (Value (VFloat32 n) _), Literal (Value (VFloat32 m) _)) -> n <= f && range bound f m
          _ -> error "Not yet implemented"
      _ -> UnapplicableRule

-- | Controls for `Float32`
controlFloat :: HasCallStack => Float -> CtlOp -> Rule -> Either (Maybe CBORTermResult) ()
controlFloat f Eq ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f == f'
    Literal (Value (VFloat32 f') _) -> f == f'
    _ -> error "Not yet implemented"
controlFloat f Ne ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f /= f'
    Literal (Value (VFloat32 f') _) -> f /= f'
    _ -> error "Not yet implemented"
controlFloat _ _ _ = error "Not yet implemented"

-- | Validating a `Float64`
validateDouble :: HasCallStack => Double -> Rule -> CDDLResult
validateDouble f rule =
  ($ rule) $ do
    case rule of
      -- a = any
      Postlude PTAny -> Valid
      -- a = float64
      Postlude PTDouble -> Valid
      -- a = 0.0000000000000000000000000000000000000000000005
      -- TODO: it is unclear if smaller floats should also validate
      Literal (Value (VFloat64 f') _) -> check $ f == f'
      -- a = foo / bar
      Choice opts -> validateChoice (validateDouble f) opts
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateDouble f) op tgt ctrl (controlDouble f)
      -- a = x..y
      -- TODO it is unclear if this should mix floating point types too
      Range low high bound ->
        check $ case (low, high) of
          (Literal (Value (VFloat16 (float2Double -> n)) _), Literal (Value (VFloat16 (float2Double -> m)) _)) -> n <= f && range bound f m
          (Literal (Value (VFloat32 (float2Double -> n)) _), Literal (Value (VFloat32 (float2Double -> m)) _)) -> n <= f && range bound f m
          (Literal (Value (VFloat64 n) _), Literal (Value (VFloat64 m) _)) -> n <= f && range bound f m
          _ -> error "Not yet implemented"
      _ -> UnapplicableRule

-- | Controls for `Float64`
controlDouble :: HasCallStack => Double -> CtlOp -> Rule -> Either (Maybe CBORTermResult) ()
controlDouble f Eq ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f == float2Double f'
    Literal (Value (VFloat32 f') _) -> f == float2Double f'
    Literal (Value (VFloat64 f') _) -> f == f'
    _ -> error "Not yet implemented"
controlDouble f Ne ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f /= float2Double f'
    Literal (Value (VFloat32 f') _) -> f /= float2Double f'
    Literal (Value (VFloat64 f') _) -> f /= f'
    _ -> error "Not yet implemented"
controlDouble _ _ _ = error "Not yet implmented"

--------------------------------------------------------------------------------
-- Bool

-- | Validating a boolean
validateBool :: Bool -> Rule -> CDDLResult
validateBool b rule =
  ($ rule) $ do
    case rule of
      -- a = any
      Postlude PTAny -> Valid
      -- a = bool
      Postlude PTBool -> Valid
      -- a = true
      Literal (Value (VBool b') _) -> check $ b == b'
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateBool b) op tgt ctrl (controlBool b)
      -- a = foo / bar
      Choice opts -> validateChoice (validateBool b) opts
      _ -> UnapplicableRule

-- | Controls for `Bool`
controlBool :: HasCallStack => Bool -> CtlOp -> Rule -> Either (Maybe CBORTermResult) ()
controlBool b Eq ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VBool b') _) -> b == b'
    _ -> error "Not yet implemented"
controlBool b Ne ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VBool b') _) -> b /= b'
    _ -> error "Not yet implemented"
controlBool _ _ _ = error "Not yet implemented"

--------------------------------------------------------------------------------
-- Simple

-- | Validating a `TSimple`. It is unclear if this is used for anything else than undefined.
validateSimple :: Word8 -> Rule -> CDDLResult
validateSimple 23 rule =
  ($ rule) $ do
    case rule of
      -- a = any
      Postlude PTAny -> Valid
      -- a = undefined
      Postlude PTUndefined -> Valid
      -- a = foo / bar
      Choice opts -> validateChoice (validateSimple 23) opts
      _ -> UnapplicableRule
validateSimple n _ = error $ "Found simple different to 23! please report this somewhere! Found: " <> show n

--------------------------------------------------------------------------------
-- Null/nil

-- | Validating nil
validateNull :: Rule -> CDDLResult
validateNull rule =
  ($ rule) $ do
    case rule of
      -- a = any
      Postlude PTAny -> Valid
      -- a = nil
      Postlude PTNil -> Valid
      Choice opts -> validateChoice validateNull opts
      _ -> UnapplicableRule

--------------------------------------------------------------------------------
-- Bytes

-- | Validating a byte sequence
validateBytes :: CDDL -> BS.ByteString -> Rule -> CDDLResult
validateBytes cddl bs rule =
  case rule of
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
    _ -> UnapplicableRule rule

-- | Controls for byte strings
controlBytes ::
  HasCallStack =>
  CDDL ->
  BS.ByteString ->
  CtlOp ->
  Rule ->
  Either (Maybe CBORTermResult) ()
controlBytes _ bs Size ctrl =
  case ctrl of
    Literal (Value (VUInt (fromIntegral -> sz)) _) -> boolCtrl $ BS.length bs == sz
    Range low high bound ->
      let i = BS.length bs
       in boolCtrl $ case (low, high) of
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= i && range bound i m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> -n <= i && range bound i m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> -n <= i && range bound i (-m)
            (Literal (Value VUInt {} _), Literal (Value VNInt {} _)) -> False
            _ -> error "Not yet implemented"
    _ -> error "Not yet implemented"
controlBytes _ bs Bits ctrl = do
  let
    indices =
      case ctrl of
        Literal (Value (VUInt i') _) -> [i']
        Choice nodes -> getIndicesOfChoice nodes
        Range ff tt incl -> getIndicesOfRange ff tt incl
        Enum g -> getIndicesOfEnum g
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
validateText :: T.Text -> Rule -> CDDLResult
validateText txt rule =
  case rule of
    -- a = any
    Postlude PTAny -> Valid rule
    -- a = text
    Postlude PTText -> Valid rule
    -- a = "foo"
    Literal (Value (VText txt') _) -> check (txt == txt') rule
    -- a = foo .ctrl bar
    Control op tgt ctrl -> ctrlDispatch (validateText txt) op tgt ctrl (controlText txt) rule
    -- a = foo / bar
    Choice opts -> validateChoice (validateText txt) opts rule
    _ -> UnapplicableRule rule

-- | Controls for text strings
controlText :: HasCallStack => T.Text -> CtlOp -> Rule -> Either (Maybe CBORTermResult) ()
controlText bs Size ctrl =
  case ctrl of
    Literal (Value (VUInt (fromIntegral -> sz)) _) -> boolCtrl $ T.length bs == sz
    Range ff tt bound ->
      boolCtrl $ case (ff, tt) of
        (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= T.length bs && range bound (T.length bs) m
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> -n <= T.length bs && range bound (T.length bs) m
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> -n <= T.length bs && range bound (T.length bs) (-m)
        _ -> error "Not yet implemented"
    _ -> error "Not yet implemented"
controlText s Regexp ctrl =
  boolCtrl $ case ctrl of
    Literal (Value (VText rxp) _) -> case s =~ rxp :: (T.Text, T.Text, T.Text) of
      ("", s', "") -> s == s'
      _ -> error "Not yet implemented"
    _ -> error "Not yet implemented"
controlText _ _ _ = error "Not yet implemented"

--------------------------------------------------------------------------------
-- Tagged values

-- | Validating a `TTagged`
validateTagged :: CDDL -> Word64 -> Term -> Rule -> CDDLResult
validateTagged cddl tag term rule =
  case rule of
    Postlude PTAny -> Valid rule
    Tag tag' rule' ->
      -- If the tag does not match, this is a direct fail
      if tag == tag'
        then case validateTerm cddl term rule' of
          CBORTermResult _ (Valid _) -> Valid rule
          err -> InvalidTagged rule (Right err)
        else InvalidTagged rule (Left tag)
    Choice opts -> validateChoice (validateTagged cddl tag term) opts rule
    _ -> UnapplicableRule rule

--------------------------------------------------------------------------------
-- Collection helpers

-- | Groups might contain enums, or unwraps inside. This resolves all those to
-- the top level of the group.
flattenGroup :: HasCallStack => CDDL -> [Rule] -> [Rule]
flattenGroup cddl nodes =
  mconcat
    [ case rule of
        Literal {} -> [rule]
        Postlude {} -> [rule]
        Map {} -> [rule]
        Array {} -> [rule]
        Choice {} -> [rule]
        KV {} -> [rule]
        Occur {} -> [rule]
        Range {} -> [rule]
        Control {} -> [rule]
        Enum e -> case e of
          Group g -> flattenGroup cddl g
          _ -> error "Malformed cddl"
        Unwrap g -> case g of
          Map n -> flattenGroup cddl n
          Array n -> flattenGroup cddl n
          Tag _ n -> [n]
          _ -> error "Malformed cddl"
        Tag {} -> [rule]
        Group g -> flattenGroup cddl g
        _ -> error "Not yet implemented"
    | rule <- nodes
    ]

-- | Expand rules to reach exactly the wanted length, which must be the number
-- of items in the container. For example, if we want to validate 3 elements,
-- and we have the following CDDL:
--
-- > a = [* int, * bool]
--
-- this will be expanded to `[int, int, int], [int, int, bool], [int, bool,
-- bool], [bool, bool, bool]`.
--
-- Essentially the rules we will parse is the choice among the expansions of the
-- original rules.
expandRules :: CDDL -> Int -> [Rule] -> [[Rule]]
expandRules _ remainingLen []
  | remainingLen /= 0 = []
expandRules _ _ [] = [[]]
expandRules _ remainingLen _
  | remainingLen < 0 = []
  | remainingLen == 0 = [[]]
expandRules cddl remainingLen (x : xs) =
  concatMap
    ( \y' -> do
        suffixes <- expandRules cddl (remainingLen - length y') xs
        [y' ++ [ys'] | ys' <- suffixes]
    )
    (expandRule cddl remainingLen x)

expandRule :: CDDL -> Int -> Rule -> [[Rule]]
expandRule cddl maxLen rule
  | maxLen < 0 = []
  | otherwise =
      case rule of
        Occur o OIOptional -> [] : [[o] | maxLen > 0]
        Occur o OIZeroOrMore -> ([] :) $ expandRule cddl maxLen (Occur o OIOneOrMore)
        Occur o OIOneOrMore ->
          if maxLen > 0
            then ([o] :) . map (o :) $ expandRule cddl (maxLen - 1) (Occur o OIOneOrMore)
            else []
        Occur o (OIBounded low high) -> case (low, high) of
          (Nothing, Nothing) -> expandRule cddl maxLen (Occur o OIZeroOrMore)
          (Just (fromIntegral -> low'), Nothing) ->
            if maxLen >= low'
              then (replicate low' o ++) <$> expandRule cddl (maxLen - low') (Occur o OIZeroOrMore)
              else []
          (Nothing, Just (fromIntegral -> high')) ->
            [replicate n o | n <- [0 .. min maxLen high']]
          (Just (fromIntegral -> low'), Just (fromIntegral -> high')) ->
            if maxLen >= low'
              then [replicate n o | n <- [low' .. min maxLen high']]
              else []
        _ -> [[rule | maxLen > 0]]

-- | Which rules are optional?
isOptional :: Rule -> Bool
isOptional rule =
  case rule of
    Occur _ OIOptional -> True
    Occur _ OIZeroOrMore -> True
    Occur _ (OIBounded Nothing _) -> True
    Occur _ (OIBounded (Just 0) _) -> True
    _ -> False

-- --------------------------------------------------------------------------------
-- -- Lists

validateListWithExpandedRules :: CDDL -> [Term] -> [Rule] -> [(Rule, CBORTermResult)]
validateListWithExpandedRules cddl terms rules =
  go (zip terms rules)
  where
    go ::
      [(Term, Rule)] -> [(Rule, CBORTermResult)]
    go [] = []
    go ((t, r) : ts) =
      case r of
        -- Should the rule be a KV, then we validate the rule for the value
        KV _ v _ ->
          -- We need to do this juggling because validateTerm has a different
          -- error type
          case validateTerm cddl t v of
            ok@(CBORTermResult _ (Valid _)) -> ((r, ok) :) $ go ts
            err -> [(r, err)]
        _ ->
          case validateTerm cddl t r of
            ok@(CBORTermResult _ (Valid _)) -> ((r, ok) :) $ go ts
            err -> [(r, err)]

validateExpandedList :: CDDL -> [Term] -> [[Rule]] -> Rule -> CDDLResult
validateExpandedList cddl terms rules = go rules
  where
    go :: [[Rule]] -> Rule -> CDDLResult
    go [] rule = ListExpansionFail rule rules []
    go (choice : choices) rule = do
      let res = validateListWithExpandedRules cddl terms choice
      case res of
        [] -> Valid rule
        _ -> case last res of
          (_, CBORTermResult _ (Valid _)) -> Valid rule
          _ ->
            case go choices rule of
              Valid _ -> Valid rule
              ListExpansionFail _ _ errors -> ListExpansionFail rule rules (res : errors)
              _ -> error "Not yet implemented"

validateList :: CDDL -> [Term] -> Rule -> CDDLResult
validateList cddl terms rule =
  case rule of
    Postlude PTAny -> Valid rule
    Array rules ->
      case terms of
        [] -> if all isOptional rules then Valid rule else InvalidRule rule
        _ ->
          let sequencesOfRules =
                expandRules cddl (length terms) $ flattenGroup cddl rules
           in validateExpandedList cddl terms sequencesOfRules rule
    Choice opts -> validateChoice (validateList cddl terms) opts rule
    _ -> UnapplicableRule rule

--------------------------------------------------------------------------------
-- Maps

validateMapWithExpandedRules ::
  HasCallStack =>
  CDDL ->
  [(Term, Term)] ->
  [Rule] ->
  ([AMatchedItem], Maybe ANonMatchedItem)
validateMapWithExpandedRules cddl =
  go
  where
    go ::
      [(Term, Term)] ->
      [Rule] ->
      ([AMatchedItem], Maybe ANonMatchedItem)
    go [] [] = ([], Nothing)
    go ((tk, tv) : ts) rs = do
      case go' tk tv rs of
        Left tt -> ([], Just tt)
        Right (res, rs') -> first (res :) $ go ts rs'
    go _ _ = error "Not yet implemented"

    -- For each pair of terms, try to find some rule that can be applied here,
    -- and returns the others if there is a succesful match.
    go' :: Term -> Term -> [Rule] -> Either ANonMatchedItem (AMatchedItem, [Rule])
    go' tk tv [] = Left $ ANonMatchedItem tk tv []
    go' tk tv (r : rs) =
      case r of
        KV k v _ ->
          case validateTerm cddl tk k of
            CBORTermResult _ r1@(Valid _) -> case validateTerm cddl tv v of
              CBORTermResult _ (Valid _) -> Right (AMatchedItem tk tv r, rs)
              CBORTermResult _ r2 ->
                bimap (\anmi -> anmi {anmiResults = Right (r, r1, r2) : anmiResults anmi}) (second (r :)) $
                  go' tk tv rs
            CBORTermResult _ r1 ->
              bimap (\anmi -> anmi {anmiResults = Left (r, r1) : anmiResults anmi}) (second (r :)) $ go' tk tv rs
        _ -> error "Not yet implemented"

validateExpandedMap :: CDDL -> [(Term, Term)] -> [[Rule]] -> Rule -> CDDLResult
validateExpandedMap cddl terms rules = go rules
  where
    go :: [[Rule]] -> Rule -> CDDLResult
    go [] rule = MapExpansionFail rule rules []
    go (choice : choices) rule = do
      case validateMapWithExpandedRules cddl terms choice of
        (_, Nothing) -> Valid rule
        (matches, Just notMatched) ->
          case go choices rule of
            Valid _ -> Valid rule
            MapExpansionFail _ _ errors ->
              MapExpansionFail rule rules ((matches, notMatched) : errors)
            _ -> error "Not yet implemented"

validateMap :: CDDL -> [(Term, Term)] -> Rule -> CDDLResult
validateMap cddl terms rule =
  case rule of
    Postlude PTAny -> Valid rule
    Map rules ->
      case terms of
        [] -> if all isOptional rules then Valid rule else InvalidRule rule
        _ ->
          let sequencesOfRules =
                expandRules cddl (length terms) $ flattenGroup cddl rules
           in validateExpandedMap cddl terms sequencesOfRules rule
    Choice opts -> validateChoice (validateMap cddl terms) opts rule
    _ -> UnapplicableRule rule

--------------------------------------------------------------------------------
-- Choices

validateChoice :: (Rule -> CDDLResult) -> NE.NonEmpty Rule -> Rule -> CDDLResult
validateChoice v rules = go rules
  where
    go :: NE.NonEmpty Rule -> Rule -> CDDLResult
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
ctrlAnd :: (Rule -> CDDLResult) -> Rule -> Rule -> Rule -> CDDLResult
ctrlAnd v tgt ctrl rule =
  case v tgt of
    Valid _ ->
      case v ctrl of
        Valid _ -> Valid rule
        _ -> InvalidControl rule Nothing
    _ -> InvalidRule rule

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
        Left err -> InvalidControl rule err
        Right () -> Valid rule
    _ -> InvalidRule rule

-- | A boolean control
boolCtrl :: Bool -> Either (Maybe CBORTermResult) ()
boolCtrl c = if c then Right () else Left Nothing

--------------------------------------------------------------------------------
-- Bits control

getIndicesOfChoice :: NE.NonEmpty Rule -> [Word64]
getIndicesOfChoice nodes =
  concatMap
    ( \case
        Literal (Value (VUInt v) _) -> [fromIntegral v]
        KV _ v _ ->
          case v of
            Literal (Value (VUInt v') _) -> [fromIntegral v']
            somethingElse ->
              error $
                "Malformed value in KV in choice in .bits: "
                  <> show somethingElse
        Range ff tt incl -> getIndicesOfRange ff tt incl
        Enum g -> getIndicesOfEnum g
        somethingElse ->
          error $
            "Malformed alternative in choice in .bits: "
              <> show somethingElse
    )
    (NE.toList nodes)

getIndicesOfRange :: Rule -> Rule -> RangeBound -> [Word64]
getIndicesOfRange ff tt incl =
  case (ff, tt) of
    (Literal (Value (VUInt ff') _), Literal (Value (VUInt tt') _)) ->
      case incl of
        ClOpen -> init rng
        Closed -> rng
      where
        rng = [ff' .. tt']
    somethingElse -> error $ "Malformed range in .bits: " <> show somethingElse

getIndicesOfEnum :: Rule -> [Word64]
getIndicesOfEnum g =
  case g of
    Group g' -> getIndicesOfChoice (fromJust $ NE.nonEmpty g')
    somethingElse -> error $ "Malformed enum in .bits: " <> show somethingElse

--------------------------------------------------------------------------------
-- Utils

replaceRule :: CDDLResult -> Rule -> CDDLResult
replaceRule (ChoiceFail _ a b) r = ChoiceFail r a b
replaceRule (ListExpansionFail _ a b) r = ListExpansionFail r a b
replaceRule (MapExpansionFail _ a b) r = MapExpansionFail r a b
replaceRule (InvalidTagged _ a) r = InvalidTagged r a
replaceRule InvalidRule {} r = InvalidRule r
replaceRule (InvalidControl _ a) r = InvalidControl r a
replaceRule UnapplicableRule {} r = UnapplicableRule r
replaceRule Valid {} r = Valid r

check :: Bool -> Rule -> CDDLResult
check c = if c then Valid else InvalidRule

range :: Ord a => RangeBound -> a -> a -> Bool
range Closed = (<=)
range ClOpen = (<)
