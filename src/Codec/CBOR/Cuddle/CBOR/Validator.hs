{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.CBOR.Cuddle.CBOR.Validator (
  validateCBOR,
  validateCBOR',
  CDDLResult (..),
  CBORTermResult (..),
) where

import Codec.CBOR.Cuddle.CDDL hiding (CDDL, Group, Rule)
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoReferenced, XXCTree (..))
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Codec.CBOR.Read
import Codec.CBOR.Term
import Control.Exception
import Control.Monad.Reader
import Data.Bifunctor
import Data.Bits hiding (And)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IntSet qualified as IS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Word
import GHC.Float
import GHC.Stack (HasCallStack)
import System.Exit
import System.IO
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

validateCBOR :: BS.ByteString -> Name -> CDDL -> IO ()
validateCBOR bs rule cddl =
  ( case validateCBOR' bs rule cddl of
      ok@(CBORTermResult _ (Valid _)) -> do
        putStrLn $ "Valid " ++ show ok
        exitSuccess
      err -> do
        hPutStrLn stderr $ "Invalid " ++ show err
        exitFailure
  )
    `catch` ( \(e :: PatternMatchFail) ->
                putStrLn $
                  "You uncovered a path we thought was impossible! Please submit your CDDL and CBOR to `https://github.com/input-output-hk/cuddle/issues` for us to investigate\n"
                    <> displayException e
            )

validateCBOR' ::
  BS.ByteString -> Name -> CDDL -> CBORTermResult
validateCBOR' bs rule cddl@(CTreeRoot tree) =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict bs) of
    Left e -> error $ show e
    Right (rest, term) ->
      if BSL.null rest
        then runReader (validateTerm term (tree Map.! rule)) cddl
        else runReader (validateTerm (TBytes bs) (tree Map.! rule)) cddl

--------------------------------------------------------------------------------
-- Terms

-- | Core function that validates a CBOR term to a particular rule of the CDDL
-- spec
validateTerm ::
  MonadReader CDDL m =>
  Term ->
  Rule ->
  m CBORTermResult
validateTerm term rule =
  let f = case term of
        TInt i -> validateInteger (fromIntegral i)
        TInteger i -> validateInteger i
        TBytes b -> validateBytes b
        TBytesI b -> validateBytes (BSL.toStrict b)
        TString s -> validateText s
        TStringI s -> validateText (TL.toStrict s)
        TList ts -> validateList ts
        TListI ts -> validateList ts
        TMap ts -> validateMap ts
        TMapI ts -> validateMap ts
        TTagged w t -> validateTagged w t
        TBool b -> validateBool b
        TNull -> validateNull
        TSimple s -> validateSimple s
        THalf h -> validateHalf h
        TFloat h -> validateFloat h
        TDouble d -> validateDouble d
   in CBORTermResult term <$> f rule

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
  ( HasCallStack
  , MonadReader CDDL m
  ) =>
  Integer ->
  Rule ->
  m CDDLResult
validateInteger i rule =
  ($ rule) <$> do
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
      Postlude PTAny -> pure Valid
      -- a = int
      Postlude PTInt -> pure Valid
      -- a = uint
      Postlude PTUInt -> pure $ check (i >= 0)
      -- a = nint
      Postlude PTNInt -> pure $ check (i <= 0)
      -- a = x
      Literal (Value (VUInt i') _) -> pure $ check $ i == fromIntegral i'
      -- a = -x
      Literal (Value (VNInt i') _) -> pure $ check $ -i == fromIntegral i'
      -- a = <big number>
      Literal (Value (VBignum i') _) -> pure $ check $ i == i'
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateInteger i) op tgt ctrl (controlInteger i)
      -- a = foo / bar
      Choice opts -> validateChoice (validateInteger i) opts
      -- a = x..y
      Range low high bound ->
        pure . check $ case (low, high) of
          (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= i && range bound i m
          (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> -n <= i && range bound i m
          (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> -n <= i && range bound i (-m)
          (Literal (Value VUInt {} _), Literal (Value VNInt {} _)) -> False
          (Literal (Value (VBignum n) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= i && range bound i m
          (Literal (Value (VBignum n) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> n <= i && range bound i (-m)
          (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _)) -> n <= i && range bound i m
          (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _)) -> (-n) <= i && range bound i m
          _ -> error "Not yet implemented"
      -- a = &(x, y, z)
      Enum g ->
        case g of
          Group g' -> validateInteger i (Choice (NE.fromList g')) <&> replaceRule
          _ -> error "Not yet implemented"
      -- a = x: y
      -- Note KV cannot appear on its own, but we will use this when validating
      -- lists.
      KV _ v _ -> validateInteger i v <&> replaceRule
      Tag 2 (Postlude PTBytes) -> pure Valid
      Tag 3 (Postlude PTBytes) -> pure Valid
      _ -> pure UnapplicableRule

-- | Controls for an Integer
controlInteger ::
  forall m.
  (HasCallStack, MonadReader CDDL m) =>
  Integer ->
  CtlOp ->
  Rule ->
  m (Either (Maybe CBORTermResult) ())
controlInteger i Size ctrl =
  case ctrl of
    Literal (Value (VUInt sz) _) ->
      pure . boolCtrl $ 0 <= i && i < 256 ^ sz
    _ -> error "Not yet implemented"
controlInteger i Bits ctrl = do
  indices <-
    case ctrl of
      Literal (Value (VUInt i') _) -> pure [i']
      Choice nodes -> getIndicesOfChoice nodes
      Range ff tt incl -> getIndicesOfRange ff tt incl
      Enum g -> getIndicesOfEnum g
      _ -> error "Not yet implemented"
  pure $ boolCtrl $ go (IS.fromList (map fromIntegral indices)) i 0
  where
    go _ 0 _ = True
    go indices n idx =
      let bitSet = testBit n 0
          allowed = not bitSet || IS.member idx indices
       in (allowed && go indices (shiftR n 1) (idx + 1))
controlInteger i Lt ctrl =
  pure . boolCtrl $ case ctrl of
    Literal (Value (VUInt i') _) -> i < fromIntegral i'
    Literal (Value (VNInt i') _) -> i < -fromIntegral i'
    Literal (Value (VBignum i') _) -> i < i'
    _ -> error "Not yet implemented"
controlInteger i Gt ctrl =
  pure . boolCtrl $ case ctrl of
    Literal (Value (VUInt i') _) -> i > fromIntegral i'
    Literal (Value (VNInt i') _) -> i > -fromIntegral i'
    Literal (Value (VBignum i') _) -> i > i'
    _ -> error "Not yet implemented"
controlInteger i Le ctrl =
  pure . boolCtrl $ case ctrl of
    Literal (Value (VUInt i') _) -> i <= fromIntegral i'
    Literal (Value (VNInt i') _) -> i <= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i <= i'
    _ -> error "Not yet implemented"
controlInteger i Ge ctrl =
  pure . boolCtrl $ case ctrl of
    Literal (Value (VUInt i') _) -> i >= fromIntegral i'
    Literal (Value (VNInt i') _) -> i >= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i >= i'
    _ -> error "Not yet implemented"
controlInteger i Eq ctrl =
  pure . boolCtrl $ case ctrl of
    Literal (Value (VUInt i') _) -> i == fromIntegral i'
    Literal (Value (VNInt i') _) -> i == -fromIntegral i'
    Literal (Value (VBignum i') _) -> i == i'
    _ -> error "Not yet implemented"
controlInteger i Ne ctrl =
  pure . boolCtrl $ case ctrl of
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
validateHalf ::
  (HasCallStack, MonadReader CDDL m) =>
  Float ->
  Rule ->
  m CDDLResult
validateHalf f rule =
  ($ rule) <$> do
    case rule of
      -- a = any
      Postlude PTAny -> pure Valid
      -- a = float16
      Postlude PTHalf -> pure Valid
      -- a = 0.5
      Literal (Value (VFloat16 f') _) -> pure $ check $ f == f'
      -- a = foo / bar
      Choice opts -> validateChoice (validateHalf f) opts
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateHalf f) op tgt ctrl (controlHalf f)
      -- a = x..y
      Range low high bound ->
        pure . check $ case (low, high) of
          (Literal (Value (VFloat16 n) _), Literal (Value (VFloat16 m) _)) -> n <= f && range bound f m
          _ -> error "Not yet implemented"
      _ -> pure UnapplicableRule

-- | Controls for `Float16`
controlHalf ::
  ( HasCallStack
  , MonadReader CDDL m
  ) =>
  Float ->
  CtlOp ->
  Rule ->
  m (Either (Maybe CBORTermResult) ())
controlHalf f Eq ctrl =
  pure . boolCtrl $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f == f'
    _ -> error "Not yet implemented"
controlHalf f Ne ctrl =
  pure . boolCtrl $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f /= f'
    _ -> error "Not yet implemented"
controlHalf _ _ _ = error "Not yet implemented"

-- | Validating a `Float32`
validateFloat ::
  ( HasCallStack
  , MonadReader CDDL m
  ) =>
  Float ->
  Rule ->
  m CDDLResult
validateFloat f rule =
  ($ rule) <$> do
    case rule of
      -- a = any
      Postlude PTAny -> pure Valid
      -- a = float32
      Postlude PTFloat -> pure Valid
      -- a = 0.000000005
      -- TODO: it is unclear if smaller floats should also validate
      Literal (Value (VFloat32 f') _) -> pure $ check $ f == f'
      -- a = foo / bar
      Choice opts -> validateChoice (validateFloat f) opts
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateFloat f) op tgt ctrl (controlFloat f)
      -- a = x..y
      -- TODO it is unclear if this should mix floating point types too
      Range low high bound ->
        pure . check $ case (low, high) of
          (Literal (Value (VFloat16 n) _), Literal (Value (VFloat16 m) _)) -> n <= f && range bound f m
          (Literal (Value (VFloat32 n) _), Literal (Value (VFloat32 m) _)) -> n <= f && range bound f m
          _ -> error "Not yet implemented"
      _ -> pure UnapplicableRule

-- | Controls for `Float32`
controlFloat ::
  ( HasCallStack
  , MonadReader CDDL m
  ) =>
  Float ->
  CtlOp ->
  Rule ->
  m (Either (Maybe CBORTermResult) ())
controlFloat f Eq ctrl =
  pure . boolCtrl $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f == f'
    Literal (Value (VFloat32 f') _) -> f == f'
    _ -> error "Not yet implemented"
controlFloat f Ne ctrl =
  pure . boolCtrl $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f /= f'
    Literal (Value (VFloat32 f') _) -> f /= f'
    _ -> error "Not yet implemented"
controlFloat _ _ _ = error "Not yet implemented"

-- | Validating a `Float64`
validateDouble ::
  ( HasCallStack
  , MonadReader CDDL m
  ) =>
  Double ->
  Rule ->
  m CDDLResult
validateDouble f rule =
  ($ rule) <$> do
    case rule of
      -- a = any
      Postlude PTAny -> pure Valid
      -- a = float64
      Postlude PTDouble -> pure Valid
      -- a = 0.0000000000000000000000000000000000000000000005
      -- TODO: it is unclear if smaller floats should also validate
      Literal (Value (VFloat64 f') _) -> pure $ check $ f == f'
      -- a = foo / bar
      Choice opts -> validateChoice (validateDouble f) opts
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateDouble f) op tgt ctrl (controlDouble f)
      -- a = x..y
      -- TODO it is unclear if this should mix floating point types too
      Range low high bound ->
        pure . check $ case (low, high) of
          (Literal (Value (VFloat16 (float2Double -> n)) _), Literal (Value (VFloat16 (float2Double -> m)) _)) -> n <= f && range bound f m
          (Literal (Value (VFloat32 (float2Double -> n)) _), Literal (Value (VFloat32 (float2Double -> m)) _)) -> n <= f && range bound f m
          (Literal (Value (VFloat64 n) _), Literal (Value (VFloat64 m) _)) -> n <= f && range bound f m
          _ -> error "Not yet implemented"
      _ -> pure UnapplicableRule

-- | Controls for `Float64`
controlDouble ::
  ( HasCallStack
  , MonadReader CDDL m
  ) =>
  Double ->
  CtlOp ->
  Rule ->
  m (Either (Maybe CBORTermResult) ())
controlDouble f Eq ctrl =
  pure . boolCtrl $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f == float2Double f'
    Literal (Value (VFloat32 f') _) -> f == float2Double f'
    Literal (Value (VFloat64 f') _) -> f == f'
    _ -> error "Not yet implemented"
controlDouble f Ne ctrl =
  pure . boolCtrl $ case ctrl of
    Literal (Value (VFloat16 f') _) -> f /= float2Double f'
    Literal (Value (VFloat32 f') _) -> f /= float2Double f'
    Literal (Value (VFloat64 f') _) -> f /= f'
    _ -> error "Not yet implemented"
controlDouble _ _ _ = error "Not yet implmented"

--------------------------------------------------------------------------------
-- Bool

-- | Validating a boolean
validateBool ::
  MonadReader CDDL m =>
  Bool ->
  Rule ->
  m CDDLResult
validateBool b rule =
  ($ rule) <$> do
    case rule of
      -- a = any
      Postlude PTAny -> pure Valid
      -- a = bool
      Postlude PTBool -> pure Valid
      -- a = true
      Literal (Value (VBool b') _) -> pure $ check $ b == b'
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateBool b) op tgt ctrl (controlBool b)
      -- a = foo / bar
      Choice opts -> validateChoice (validateBool b) opts
      _ -> pure UnapplicableRule

-- | Controls for `Bool`
controlBool ::
  ( HasCallStack
  , MonadReader CDDL m
  ) =>
  Bool ->
  CtlOp ->
  Rule ->
  m (Either (Maybe CBORTermResult) ())
controlBool b Eq ctrl =
  pure . boolCtrl $ case ctrl of
    Literal (Value (VBool b') _) -> b == b'
    _ -> error "Not yet implemented"
controlBool b Ne ctrl =
  pure . boolCtrl $ case ctrl of
    Literal (Value (VBool b') _) -> b /= b'
    _ -> error "Not yet implemented"
controlBool _ _ _ = error "Not yet implemented"

--------------------------------------------------------------------------------
-- Simple

-- | Validating a `TSimple`. It is unclear if this is used for anything else than undefined.
validateSimple ::
  MonadReader CDDL m =>
  Word8 ->
  Rule ->
  m CDDLResult
validateSimple 23 rule =
  ($ rule) <$> do
    case rule of
      -- a = any
      Postlude PTAny -> pure Valid
      -- a = undefined
      Postlude PTUndefined -> pure Valid
      -- a = foo / bar
      Choice opts -> validateChoice (validateSimple 23) opts
      _ -> pure UnapplicableRule
validateSimple n _ = error $ "Found simple different to 23! please report this somewhere! Found: " <> show n

--------------------------------------------------------------------------------
-- Null/nil

-- | Validating nil
validateNull ::
  MonadReader CDDL m => Rule -> m CDDLResult
validateNull rule =
  ($ rule) <$> do
    case rule of
      -- a = any
      Postlude PTAny -> pure Valid
      -- a = nil
      Postlude PTNil -> pure Valid
      Choice opts -> validateChoice validateNull opts
      _ -> pure UnapplicableRule

--------------------------------------------------------------------------------
-- Bytes

-- | Validating a byte sequence
validateBytes ::
  MonadReader CDDL m =>
  BS.ByteString ->
  Rule ->
  m CDDLResult
validateBytes bs rule =
  ($ rule) <$> do
    case rule of
      -- a = any
      Postlude PTAny -> pure Valid
      -- a = bytes
      Postlude PTBytes -> pure Valid
      -- a = h'123456'
      Literal (Value (VBytes bs') _) -> pure $ check $ bs == bs'
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateBytes bs) op tgt ctrl (controlBytes bs)
      -- a = foo / bar
      Choice opts -> validateChoice (validateBytes bs) opts
      _ -> pure UnapplicableRule

-- | Controls for byte strings
controlBytes ::
  forall m.
  ( HasCallStack
  , MonadReader CDDL m
  ) =>
  BS.ByteString ->
  CtlOp ->
  Rule ->
  m (Either (Maybe CBORTermResult) ())
controlBytes bs Size ctrl =
  case ctrl of
    Literal (Value (VUInt (fromIntegral -> sz)) _) -> pure $ boolCtrl $ BS.length bs == sz
    Range low high bound ->
      let i = BS.length bs
       in pure . boolCtrl $ case (low, high) of
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= i && range bound i m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> -n <= i && range bound i m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> -n <= i && range bound i (-m)
            (Literal (Value VUInt {} _), Literal (Value VNInt {} _)) -> False
            _ -> error "Not yet implemented"
    _ -> error "Not yet implemented"
controlBytes bs Bits ctrl = do
  indices <-
    case ctrl of
      Literal (Value (VUInt i') _) -> pure [i']
      Choice nodes -> getIndicesOfChoice nodes
      Range ff tt incl -> getIndicesOfRange ff tt incl
      Enum g -> getIndicesOfEnum g
      _ -> error "Not yet implemented"
  pure $ boolCtrl $ bitsControlCheck (map fromIntegral indices)
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
    Right (BSL.null -> True, term) ->
      validateTerm term ctrl >>= \case
        CBORTermResult _ (Valid _) -> pure $ Right ()
        err -> pure $ Left $ Just err
    _ -> error "Not yet implemented"
controlBytes bs Cborseq ctrl =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict (BS.snoc (BS.cons 0x9f bs) 0xff)) of
    Right (BSL.null -> True, TListI terms) ->
      validateTerm (TList terms) (Array [Occur ctrl OIZeroOrMore]) >>= \case
        CBORTermResult _ (Valid _) -> pure $ Right ()
        CBORTermResult _ err -> error $ show err
    _ -> error "Not yet implemented"
controlBytes _ _ _ = error "Not yet implmented"

--------------------------------------------------------------------------------
-- Text

-- | Validating text strings
validateText ::
  MonadReader CDDL m =>
  T.Text ->
  Rule ->
  m CDDLResult
validateText txt rule =
  ($ rule) <$> do
    case rule of
      -- a = any
      Postlude PTAny -> pure Valid
      -- a = text
      Postlude PTText -> pure Valid
      -- a = "foo"
      Literal (Value (VText txt') _) -> pure $ check $ txt == txt'
      -- a = foo .ctrl bar
      Control op tgt ctrl -> ctrlDispatch (validateText txt) op tgt ctrl (controlText txt)
      -- a = foo / bar
      Choice opts -> validateChoice (validateText txt) opts
      _ -> pure UnapplicableRule

-- | Controls for text strings
controlText ::
  ( HasCallStack
  , MonadReader CDDL m
  ) =>
  T.Text ->
  CtlOp ->
  Rule ->
  m (Either (Maybe CBORTermResult) ())
controlText bs Size ctrl =
  case ctrl of
    Literal (Value (VUInt (fromIntegral -> sz)) _) -> pure $ boolCtrl $ T.length bs == sz
    Range ff tt bound ->
      pure . boolCtrl $ case (ff, tt) of
        (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= T.length bs && range bound (T.length bs) m
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> -n <= T.length bs && range bound (T.length bs) m
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> -n <= T.length bs && range bound (T.length bs) (-m)
        _ -> error "Not yet implemented"
    _ -> error "Not yet implemented"
controlText s Regexp ctrl =
  pure . boolCtrl $ case ctrl of
    Literal (Value (VText rxp) _) -> case s =~ rxp :: (T.Text, T.Text, T.Text) of
      ("", s', "") -> s == s'
      _ -> error "Not yet implemented"
    _ -> error "Not yet implemented"
controlText _ _ _ = error "Not yet implemented"

--------------------------------------------------------------------------------
-- Tagged values

-- | Validating a `TTagged`
validateTagged ::
  MonadReader CDDL m =>
  Word64 ->
  Term ->
  Rule ->
  m CDDLResult
validateTagged tag term rule =
  ($ rule) <$> do
    case rule of
      Postlude PTAny -> pure Valid
      Tag tag' rule' ->
        -- If the tag does not match, this is a direct fail
        if tag == tag'
          then
            ask >>= \cddl ->
              case runReader (validateTerm term rule') cddl of
                CBORTermResult _ (Valid _) -> pure Valid
                err -> pure $ \r -> InvalidTagged r (Right err)
          else pure $ \r -> InvalidTagged r (Left tag)
      Choice opts -> validateChoice (validateTagged tag term) opts
      _ -> pure UnapplicableRule

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
expandRules :: Int -> [Rule] -> Reader CDDL [[Rule]]
expandRules remainingLen []
  | remainingLen /= 0 = pure []
expandRules _ [] = pure [[]]
expandRules remainingLen _
  | remainingLen < 0 = pure []
  | remainingLen == 0 = pure [[]]
expandRules remainingLen (x : xs) = do
  y <- expandRule remainingLen x
  concat
    <$> mapM
      ( \y' -> do
          suffixes <- expandRules (remainingLen - length y') xs
          pure [y' ++ ys' | ys' <- suffixes]
      )
      y

expandRule :: Int -> Rule -> Reader CDDL [[Rule]]
expandRule maxLen _
  | maxLen < 0 = pure []
expandRule maxLen rule =
  case rule of
    Occur o OIOptional -> pure $ [] : [[o] | maxLen > 0]
    Occur o OIZeroOrMore -> ([] :) <$> expandRule maxLen (Occur o OIOneOrMore)
    Occur o OIOneOrMore ->
      if maxLen > 0
        then ([o] :) . map (o :) <$> expandRule (maxLen - 1) (Occur o OIOneOrMore)
        else pure []
    Occur o (OIBounded low high) -> case (low, high) of
      (Nothing, Nothing) -> expandRule maxLen (Occur o OIZeroOrMore)
      (Just (fromIntegral -> low'), Nothing) ->
        if maxLen >= low'
          then map (replicate low' o ++) <$> expandRule (maxLen - low') (Occur o OIZeroOrMore)
          else pure []
      (Nothing, Just (fromIntegral -> high')) ->
        pure [replicate n o | n <- [0 .. min maxLen high']]
      (Just (fromIntegral -> low'), Just (fromIntegral -> high')) ->
        if maxLen >= low'
          then pure [replicate n o | n <- [low' .. min maxLen high']]
          else pure []
    _ -> pure [[rule | maxLen > 0]]

-- | Which rules are optional?
isOptional :: MonadReader CDDL m => Rule -> m Bool
isOptional rule =
  pure $ case rule of
    Occur _ OIOptional -> True
    Occur _ OIZeroOrMore -> True
    Occur _ (OIBounded Nothing _) -> True
    Occur _ (OIBounded (Just 0) _) -> True
    _ -> False

-- --------------------------------------------------------------------------------
-- -- Lists

validateListWithExpandedRules ::
  forall m.
  MonadReader CDDL m =>
  [Term] ->
  [Rule] ->
  m [(Rule, CBORTermResult)]
validateListWithExpandedRules terms rules =
  go (zip terms rules)
  where
    go ::
      [(Term, Rule)] -> m [(Rule, CBORTermResult)]
    go [] = pure []
    go ((t, r) : ts) =
      case r of
        -- Should the rule be a KV, then we validate the rule for the value
        KV _ v _ ->
          -- We need to do this juggling because validateTerm has a different
          -- error type
          ask >>= \cddl ->
            case runReader (validateTerm t v) cddl of
              ok@(CBORTermResult _ (Valid _)) -> ((r, ok) :) <$> go ts
              err -> pure [(r, err)]
        _ ->
          ask >>= \cddl ->
            case runReader (validateTerm t r) cddl of
              ok@(CBORTermResult _ (Valid _)) -> ((r, ok) :) <$> go ts
              err -> pure [(r, err)]

validateExpandedList ::
  forall m.
  MonadReader CDDL m =>
  [Term] ->
  [[Rule]] ->
  m (Rule -> CDDLResult)
validateExpandedList terms rules = go rules
  where
    go :: [[Rule]] -> m (Rule -> CDDLResult)
    go [] = pure $ \r -> ListExpansionFail r rules []
    go (choice : choices) = do
      res <- validateListWithExpandedRules terms choice
      case res of
        [] -> pure Valid
        _ -> case last res of
          (_, CBORTermResult _ (Valid _)) -> pure Valid
          _ ->
            go choices
              >>= ( \case
                      Valid _ -> pure Valid
                      ListExpansionFail _ _ errors -> pure $ \r -> ListExpansionFail r rules (res : errors)
                      _ -> error "Not yet implemented"
                  )
                . ($ dummyRule)

validateList ::
  MonadReader CDDL m => [Term] -> Rule -> m CDDLResult
validateList terms rule =
  ($ rule) <$> do
    case rule of
      Postlude PTAny -> pure Valid
      Array rules ->
        case terms of
          [] -> ifM (and <$> mapM isOptional rules) (pure Valid) (pure InvalidRule)
          _ ->
            ask >>= \cddl ->
              let sequencesOfRules =
                    runReader (expandRules (length terms) $ flattenGroup cddl rules) cddl
               in validateExpandedList terms sequencesOfRules
      Choice opts -> validateChoice (validateList terms) opts
      _ -> pure UnapplicableRule

--------------------------------------------------------------------------------
-- Maps

validateMapWithExpandedRules ::
  forall m.
  ( HasCallStack
  , MonadReader CDDL m
  ) =>
  [(Term, Term)] ->
  [Rule] ->
  m ([AMatchedItem], Maybe ANonMatchedItem)
validateMapWithExpandedRules =
  go
  where
    go ::
      [(Term, Term)] ->
      [Rule] ->
      m ([AMatchedItem], Maybe ANonMatchedItem)
    go [] [] = pure ([], Nothing)
    go ((tk, tv) : ts) rs = do
      go' tk tv rs >>= \case
        Left tt -> pure ([], Just tt)
        Right (res, rs') ->
          first (res :) <$> go ts rs'
    go _ _ = error "Not yet implemented"

    -- For each pair of terms, try to find some rule that can be applied here,
    -- and returns the others if there is a succesful match.
    go' :: Term -> Term -> [Rule] -> m (Either ANonMatchedItem (AMatchedItem, [Rule]))
    go' tk tv [] = pure $ Left $ ANonMatchedItem tk tv []
    go' tk tv (r : rs) =
      case r of
        KV k v _ ->
          ask >>= \cddl ->
            case runReader (validateTerm tk k) cddl of
              CBORTermResult _ r1@(Valid _) -> case runReader (validateTerm tv v) cddl of
                CBORTermResult _ (Valid _) -> pure (Right (AMatchedItem tk tv r, rs))
                CBORTermResult _ r2 ->
                  bimap (\anmi -> anmi {anmiResults = Right (r, r1, r2) : anmiResults anmi}) (second (r :))
                    <$> go' tk tv rs
              CBORTermResult _ r1 ->
                bimap (\anmi -> anmi {anmiResults = Left (r, r1) : anmiResults anmi}) (second (r :))
                  <$> go' tk tv rs
        _ -> error "Not yet implemented"

validateExpandedMap ::
  forall m.
  MonadReader CDDL m =>
  [(Term, Term)] ->
  [[Rule]] ->
  m (Rule -> CDDLResult)
validateExpandedMap terms rules = go rules
  where
    go :: [[Rule]] -> m (Rule -> CDDLResult)
    go [] = pure $ \r -> MapExpansionFail r rules []
    go (choice : choices) = do
      res <- validateMapWithExpandedRules terms choice
      case res of
        (_, Nothing) -> pure Valid
        (matches, Just notMatched) ->
          go choices
            >>= ( \case
                    Valid _ -> pure Valid
                    MapExpansionFail _ _ errors ->
                      pure $ \r -> MapExpansionFail r rules ((matches, notMatched) : errors)
                    _ -> error "Not yet implemented"
                )
              . ($ dummyRule)

validateMap ::
  MonadReader CDDL m =>
  [(Term, Term)] ->
  Rule ->
  m CDDLResult
validateMap terms rule =
  ($ rule) <$> do
    case rule of
      Postlude PTAny -> pure Valid
      Map rules ->
        case terms of
          [] -> ifM (and <$> mapM isOptional rules) (pure Valid) (pure InvalidRule)
          _ ->
            ask >>= \cddl ->
              let sequencesOfRules =
                    runReader (expandRules (length terms) $ flattenGroup cddl rules) cddl
               in validateExpandedMap terms sequencesOfRules
      Choice opts -> validateChoice (validateMap terms) opts
      _ -> pure UnapplicableRule

--------------------------------------------------------------------------------
-- Choices

validateChoice ::
  forall m. Monad m => (Rule -> m CDDLResult) -> NE.NonEmpty Rule -> m (Rule -> CDDLResult)
validateChoice v rules = go rules
  where
    go :: NE.NonEmpty Rule -> m (Rule -> CDDLResult)
    go (choice NE.:| xs) = do
      v choice >>= \case
        Valid _ -> pure Valid
        err -> case NE.nonEmpty xs of
          Nothing -> pure $ \r -> ChoiceFail r rules ((choice, err) NE.:| [])
          Just choices ->
            go choices
              >>= ( \case
                      Valid _ -> pure Valid
                      ChoiceFail _ _ errors -> pure $ \r -> ChoiceFail r rules ((choice, err) NE.<| errors)
                      _ -> error "Not yet implemented"
                  )
                . ($ dummyRule)

dummyRule :: Rule
dummyRule = CTreeE $ VRuleRef "dummy"

--------------------------------------------------------------------------------
-- Control helpers

-- | Validate both rules
ctrlAnd ::
  Monad m =>
  (Rule -> m CDDLResult) ->
  Rule ->
  Rule ->
  m (Rule -> CDDLResult)
ctrlAnd v tgt ctrl =
  v tgt >>= \case
    Valid _ ->
      v ctrl <&> \case
        Valid _ -> Valid
        _ -> flip InvalidControl Nothing
    _ -> pure InvalidRule

-- | Dispatch to the appropriate control
ctrlDispatch ::
  Monad m =>
  (Rule -> m CDDLResult) ->
  CtlOp ->
  Rule ->
  Rule ->
  (CtlOp -> Rule -> m (Either (Maybe CBORTermResult) ())) ->
  m (Rule -> CDDLResult)
ctrlDispatch v And tgt ctrl _ = ctrlAnd v tgt ctrl
ctrlDispatch v Within tgt ctrl _ = ctrlAnd v tgt ctrl
ctrlDispatch v op tgt ctrl vctrl =
  v tgt >>= \case
    Valid _ ->
      vctrl op ctrl <&> \case
        Left err -> flip InvalidControl err
        Right () -> Valid
    _ -> pure InvalidRule

-- | A boolean control
boolCtrl :: Bool -> Either (Maybe CBORTermResult) ()
boolCtrl c = if c then Right () else Left Nothing

--------------------------------------------------------------------------------
-- Bits control

getIndicesOfChoice :: MonadReader CDDL m => NE.NonEmpty Rule -> m [Word64]
getIndicesOfChoice nodes =
  mconcat
    . NE.toList
    <$> mapM
      ( \case
          Literal (Value (VUInt v) _) -> pure [fromIntegral v]
          KV _ v _ ->
            case v of
              Literal (Value (VUInt v') _) -> pure [fromIntegral v']
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
      nodes

getIndicesOfRange :: MonadReader CDDL m => Rule -> Rule -> RangeBound -> m [Word64]
getIndicesOfRange ff tt incl =
  case (ff, tt) of
    (Literal (Value (VUInt ff') _), Literal (Value (VUInt tt') _)) ->
      pure $
        [ff' .. tt'] & case incl of
          ClOpen -> init
          Closed -> id
    somethingElse -> error $ "Malformed range in .bits: " <> show somethingElse

getIndicesOfEnum :: MonadReader CDDL m => Rule -> m [Word64]
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

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b' <- b; if b' then t else f

check :: Bool -> Rule -> CDDLResult
check c = if c then Valid else InvalidRule

range :: Ord a => RangeBound -> a -> a -> Bool
range Closed = (<=)
range ClOpen = (<)
