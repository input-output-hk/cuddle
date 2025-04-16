{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codec.CBOR.Cuddle.CBOR.Validate.Ops where

import Codec.CBOR.Cuddle.CBOR.Validate.Combinators
import Codec.CBOR.Cuddle.CBOR.Validate.Types
import Codec.CBOR.Cuddle.CDDL hiding (Group (..))
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Magic
import Codec.CBOR.Read
import Codec.CBOR.Term
import Control.Lens ((#))
import Control.Monad.Except
import Data.Bits (bit, complement, zeroBits, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.Functor.Alt
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text qualified as T
import Data.Validation
import Data.Word
import Debug.Trace
import Text.Regex.TDFA

type RunOp a = CDDL' -> Rule' -> a -> Validation [Reason] ()

class Ops a where
  sz :: RunOp a
  bts :: RunOp a
  rgx :: RunOp a
  cbr :: (CDDL' -> Term -> Rule' -> Validation [Reason] ()) -> RunOp a
  cbrsq :: (CDDL' -> Term -> Rule' -> Validation [Reason] ()) -> RunOp a
  lt :: RunOp a
  le :: RunOp a
  gt :: RunOp a
  ge :: RunOp a
  eq :: RunOp a
  ne :: RunOp a

  get :: CDDL' -> Rule' -> Maybe a

dispatchOp :: Ops a => CtlOp -> (CDDL' -> Term -> Rule' -> Validation [Reason] ()) -> RunOp a
dispatchOp Size _ = sz
dispatchOp Bits _ = bts
dispatchOp Regexp _ = rgx
dispatchOp Cbor val = cbr val
dispatchOp Cborseq val = cbrsq val
dispatchOp Lt _ = lt
dispatchOp Le _ = le
dispatchOp Gt _ = gt
dispatchOp Ge _ = ge
dispatchOp Eq _ = eq
dispatchOp Ne _ = ne
dispatchOp other _ = \_ _ _ -> _Failure # [DidNotValidate $ "Op " <> show other <> " should not be dispatched here"]

unapplicable :: CtlOp -> String -> p1 -> p2 -> p3 -> Validation [Reason] ()
unapplicable op s = \_ _ _ -> _Failure # [DidNotValidate $ "Op " <> show op <> " not applicable to " <> s]

rhsMalformed :: Validation [Reason] ()
rhsMalformed = _Failure # [DidNotValidate "RHS of control is malformed"]

instance Ops Int where
  sz = unapplicable Size "int"
  bts cddl rule = bitsControl cddl rule . toInteger
  rgx = unapplicable Regexp "int"
  cbr _ = unapplicable Cbor "int"
  cbrsq _ = unapplicable Cborseq "int"
  lt cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i < i') [Unexpected ("a int < " <> show i') (show i)])
        <$> get cddl rule
  le cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i <= i') [Unexpected ("a int <= " <> show i') (show i)])
        <$> get cddl rule
  gt cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i > i') [Unexpected ("a int > " <> show i') (show i)])
        <$> get cddl rule
  ge cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i >= i') [Unexpected ("a int >= " <> show i') (show i)])
        <$> get cddl rule
  eq cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i == i') [Unexpected ("a int == " <> show i') (show i)])
        <$> get cddl rule
  ne cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i /= i') [Unexpected ("a int /= " <> show i') (show i)])
        <$> get cddl rule

  get cddl ref = case resolveIfRef cddl ref of
    Literal (VUInt i) -> Just (fromIntegral i)
    Literal (VNInt i) -> Just (-fromIntegral @Word64 @Int i)
    _ -> Nothing

instance Ops Integer where
  sz = unapplicable Size "int"
  bts = bitsControl
  rgx = unapplicable Regexp "int"
  cbr _ = unapplicable Cbor "int"
  cbrsq _ = unapplicable Cborseq "int"
  lt cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i < i') [Unexpected ("a int < " <> show i') (show i)])
        <$> get cddl rule
  le cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i <= i') [Unexpected ("a int <= " <> show i') (show i)])
        <$> get cddl rule
  gt cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i > i') [Unexpected ("a int > " <> show i') (show i)])
        <$> get cddl rule
  ge cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i >= i') [Unexpected ("a int >= " <> show i') (show i)])
        <$> get cddl rule
  eq cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i == i') [Unexpected ("a int == " <> show i') (show i)])
        <$> get cddl rule
  ne cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i /= i') [Unexpected ("a int /= " <> show i') (show i)])
        <$> get cddl rule

  get cddl ref = case resolveIfRef cddl ref of
    Literal (VBignum i) -> Just i
    _ -> Nothing

newtype Half = Half {getHalf :: Float}
  deriving newtype (Eq, Show, Ord, Num)

instance Ops Half where
  sz = unapplicable Size "float"
  bts = unapplicable Bits "float"
  rgx = unapplicable Regexp "float"
  cbr _ = unapplicable Cbor "float"
  cbrsq _ = unapplicable Cborseq "float"
  lt cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i < i') [Unexpected ("a float < " <> show i') (show i)])
        <$> get cddl rule
  le cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i <= i') [Unexpected ("a float <= " <> show i') (show i)])
        <$> get cddl rule
  gt cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i > i') [Unexpected ("a float > " <> show i') (show i)])
        <$> get cddl rule
  ge cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i >= i') [Unexpected ("a float >= " <> show i') (show i)])
        <$> get cddl rule
  eq cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i == i') [Unexpected ("a float == " <> show i') (show i)])
        <$> get cddl rule
  ne cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i /= i') [Unexpected ("a float /= " <> show i') (show i)])
        <$> get cddl rule

  get cddl ref = case resolveIfRef cddl ref of
    Literal (VFloat16 i) -> Just (Half i)
    _ -> Nothing

instance Ops Float where
  sz = unapplicable Size "float"
  bts = unapplicable Bits "float"
  rgx = unapplicable Regexp "float"
  cbr _ = unapplicable Cbor "float"
  cbrsq _ = unapplicable Cborseq "float"
  lt cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i < i') [Unexpected ("a float < " <> show i') (show i)])
        <$> get cddl rule
  le cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i <= i') [Unexpected ("a float <= " <> show i') (show i)])
        <$> get cddl rule
  gt cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i > i') [Unexpected ("a float > " <> show i') (show i)])
        <$> get cddl rule
  ge cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i >= i') [Unexpected ("a float >= " <> show i') (show i)])
        <$> get cddl rule
  eq cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i == i') [Unexpected ("a float == " <> show i') (show i)])
        <$> get cddl rule
  ne cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i /= i') [Unexpected ("a float /= " <> show i') (show i)])
        <$> get cddl rule

  get cddl ref = case resolveIfRef cddl ref of
    Literal (VFloat32 i) -> Just i
    _ -> Nothing

instance Ops Double where
  sz = unapplicable Size "double"
  bts = unapplicable Bits "double"
  rgx = unapplicable Regexp "double"
  cbr _ = unapplicable Cbor "double"
  cbrsq _ = unapplicable Cborseq "double"
  lt cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i < i') [Unexpected ("a double < " <> show i') (show i)])
        <$> get cddl rule
  le cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i <= i') [Unexpected ("a double <= " <> show i') (show i)])
        <$> get cddl rule
  gt cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i > i') [Unexpected ("a double > " <> show i') (show i)])
        <$> get cddl rule
  ge cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i >= i') [Unexpected ("a double >= " <> show i') (show i)])
        <$> get cddl rule
  eq cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i == i') [Unexpected ("a double == " <> show i') (show i)])
        <$> get cddl rule
  ne cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i /= i') [Unexpected ("a double /= " <> show i') (show i)])
        <$> get cddl rule

  get cddl ref = case resolveIfRef cddl ref of
    Literal (VFloat64 i) -> Just i
    _ -> Nothing

instance Ops T.Text where
  sz cddl rule s =
    fromMaybe rhsMalformed $
      ( \expectedSize ->
          let theSize = T.length s
           in failUnless
                (theSize == expectedSize)
                [Unexpected ("a text of length " <> show expectedSize) ("a text of length " <> show theSize)]
      )
        <$> get @Int cddl rule
  bts = unapplicable Bits "text"
  rgx cddl rule s =
    case resolveIfRef cddl rule of
      Literal (VText s') -> failUnless (s =~ s') [DidNotValidate $ "Text " <> show s <> " did not match regexp " <> show s']
      somethingElse -> _Failure # [DidNotValidate $ "Malformed regexp rhs: " <> show somethingElse]
  cbr _ = unapplicable Cbor "text"
  cbrsq _ = unapplicable Cborseq "text"
  lt cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i < i') [Unexpected ("a text < " <> show i') (show i)])
        <$> get cddl rule
  le cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i <= i') [Unexpected ("a text <= " <> show i') (show i)])
        <$> get cddl rule
  gt cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i > i') [Unexpected ("a text > " <> show i') (show i)])
        <$> get cddl rule
  ge cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i >= i') [Unexpected ("a text >= " <> show i') (show i)])
        <$> get cddl rule
  eq cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i == i') [Unexpected ("a text == " <> show i') (show i)])
        <$> get cddl rule
  ne cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i /= i') [Unexpected ("a text /= " <> show i') (show i)])
        <$> get cddl rule

  get cddl rule = case resolveIfRef cddl rule of
    Literal (VText b) -> Just b
    _ -> Nothing

instance Ops BS.ByteString where
  sz cddl rule s =
    fromMaybe rhsMalformed $
      ( \expectedSize ->
          let theSize = BS.length s
           in failUnless
                (theSize == expectedSize)
                [ Unexpected
                    ("a bytestring of length " <> show expectedSize)
                    ("a bytestring of length " <> show theSize)
                ]
      )
        <$> get @Int cddl rule
  bts cddl rule bs = bitsControl cddl rule (uintegerFromBytes bs)
  rgx = unapplicable Regexp "bytestring"
  cbr doValidate cddl rule bs =
    case runExcept $
      liftEither $
        deserialiseFromBytes decodeTerm (BSL.fromStrict bs) of
      Left _err -> _Failure # [InvalidInnerCBOR]
      Right (rest, term) ->
        failUnless (BSL.null rest) [NotASingleInnerTerm]
          *> doValidate cddl term rule
  cbrsq doValidate cddl rule bs =
    case runExcept $
      liftEither $
        deserialiseFromBytes
          decodeTerm
          (traceShowWith (Base16.encode . BSL.toStrict) $ BSL.snoc (BSL.cons 0x9f $ BSL.fromStrict bs) 0xff) of
      Right (rest, TListI xs) ->
        failUnless (BSL.null rest) [NotASingleInnerTerm]
          *> L.foldl1' (<!>) [doValidate cddl t rule | t <- xs]
      _ -> _Failure # [InvalidInnerCBOR]
  lt cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i < i') [Unexpected ("a bytestring < " <> show i') (show i)])
        <$> get cddl rule
  le cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i <= i') [Unexpected ("a bytestring <= " <> show i') (show i)])
        <$> get cddl rule
  gt cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i > i') [Unexpected ("a bytestring > " <> show i') (show i)])
        <$> get cddl rule
  ge cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i >= i') [Unexpected ("a bytestring >= " <> show i') (show i)])
        <$> get cddl rule
  eq cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i == i') [Unexpected ("a bytestring == " <> show i') (show i)])
        <$> get cddl rule
  ne cddl rule i =
    fromMaybe rhsMalformed $
      (\i' -> failUnless (i /= i') [Unexpected ("a bytestring /= " <> show i') (show i)])
        <$> get cddl rule

  get cddl rule = case resolveIfRef cddl rule of
    Literal (VBytes b) -> Just b
    _ -> Nothing

getIndicesOfLiteral :: Word64 -> Integer
getIndicesOfLiteral = complement . bit . fromIntegral

getIndicesOfRange :: CDDL' -> Rule' -> Rule' -> RangeBound -> Integer
getIndicesOfRange cddl ff tt incl = case (resolveIfRef cddl ff, resolveIfRef cddl tt) of
  (Literal (VUInt v), Literal (VUInt w)) ->
    foldl' (.&.) (complement zeroBits) $
      map
        (complement . bit . fromIntegral)
        [ v
        .. case incl of
          ClOpen -> w - 1
          Closed -> w
        ]
  somethingElse -> error $ "Malformed range in .bits: " <> show somethingElse

getIndicesOfEnum :: CDDL' -> Rule' -> Integer
getIndicesOfEnum cddl group = case resolveIfRef cddl group of
  Group g -> getIndicesOfChoice cddl $ fromJust $ NE.nonEmpty g
  somethingElse -> error $ "Malformed enum in .bits: " <> show somethingElse

getIndicesOfChoice :: CDDL' -> NE.NonEmpty Rule' -> Integer
getIndicesOfChoice cddl nodes =
  foldl' (.&.) (complement zeroBits) $
    NE.map
      ( \x -> case resolveIfRef cddl x of
          Literal (VUInt v) -> getIndicesOfLiteral v
          KV _ v _ -> case resolveIfRef cddl v of
            Literal (VUInt v') -> getIndicesOfLiteral v'
            somethingElse -> error $ "Malformed value in KV in choice in .bits: " <> show somethingElse
          Range ff tt incl -> getIndicesOfRange cddl ff tt incl
          Enum group -> getIndicesOfEnum cddl group
          somethingElse -> error $ "Malformed alternative in choice in .bits: " <> show somethingElse
      )
      nodes

bitsControl :: CDDL' -> Rule' -> Integer -> Validation [Reason] ()
bitsControl cddl rule bs =
  failUnless
    ( bs
        .&. ( case resolveIfRef cddl rule of
                Literal (VUInt n) -> getIndicesOfLiteral n
                Choice nodes -> getIndicesOfChoice cddl nodes
                Range ff tt incl -> getIndicesOfRange cddl ff tt incl
                Enum g -> getIndicesOfEnum cddl g
                somethingElse -> error $ "Malformed rhs in .bits: " <> show somethingElse
            )
        == 0
    )
    [DidNotValidate "Bits control failed"]
