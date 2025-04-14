{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}

module Codec.CBOR.Cuddle.CBOR.Validate where

import Codec.CBOR.Cuddle.CDDL hiding (Group (..))
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.CDDL.Postlude
import Codec.CBOR.Cuddle.CDDL.Resolve
import Codec.CBOR.Read
import Codec.CBOR.Term
import Control.Lens ((#))
import Control.Monad (unless, when)
import Control.Monad.Except
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Either
import Data.Functor.Alt
import Data.Functor.Identity
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Validation
import Data.Word
import Debug.Trace

-- We make the conscious choice of parsing the provided term instead
-- of navigating the rule, as rules might be circular but the term
-- cannot.

-- TODOs
-- - cuts in maps
-- - .bits
-- - .regexp
-- - ranges

type CDDL' = CTreeRoot' Identity MonoRef
type Rule' = Node MonoRef

doValidate :: CDDL' -> Term -> Rule' -> Validation [Reason] ()
doValidate cddl t theRule = case resolveIfRef cddl theRule of
  Choice choices -> validateChoices cddl t (NE.toList choices)
  Control And r1 r2 ->
    doValidate cddl t r1 <* doValidate cddl t r2
  Control Within r1 r2 ->
    doValidate cddl t r1 <* doValidate cddl t r2
  _ -> case t of
    TInt i -> case resolveIfRef cddl theRule of
      Literal (VUInt i') ->
        if i == fromIntegral i'
          then _Success # ()
          else _Failure # [DidNotValidate "Expecting a different int"]
      Literal (VNInt i') ->
        if i == -(fromIntegral i')
          then _Success # ()
          else _Failure # [DidNotValidate "Expecting a different int"]
      Postlude PTInt -> _Success # ()
      Postlude PTUInt ->
        if i >= 0
          then _Success # ()
          else _Failure # [DidNotValidate "Expecting a positive int"]
      Postlude PTNInt ->
        if i < 0
          then _Success # ()
          else _Failure # [DidNotValidate "Expecting a negative int"]
      Control op n ctrller ->
        let intControl = case (op, resolveIfRef cddl ctrller) of
              (Lt, Literal (VUInt i')) ->
                if i < fromIntegral i'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Lt, Literal (VNInt i')) ->
                if i < -(fromIntegral i')
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Le, Literal (VUInt i')) ->
                if i <= fromIntegral i'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Le, Literal (VNInt i')) ->
                if i <= -(fromIntegral i')
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Gt, Literal (VUInt i')) ->
                if i > fromIntegral i'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Gt, Literal (VNInt i')) ->
                if i > -(fromIntegral i')
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ge, Literal (VUInt i')) ->
                if i >= fromIntegral i'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ge, Literal (VNInt i')) ->
                if i >= -(fromIntegral i')
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Eq, Literal (VUInt i')) ->
                if i == fromIntegral i'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Eq, Literal (VNInt i')) ->
                if i == -(fromIntegral i')
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ne, Literal (VUInt i')) ->
                if i /= fromIntegral i'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ne, Literal (VNInt i')) ->
                if i /= -(fromIntegral i')
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              _ -> error "nonsense"
         in case resolveIfRef cddl n of
              Postlude PTInt -> intControl
              Postlude PTUInt ->
                if i >= 0
                  then intControl
                  else _Failure # [DidNotValidate "Expecting a positive int"]
              Postlude PTNInt ->
                if i < 0
                  then intControl
                  else _Failure # [DidNotValidate "Expecting a positive int"]
              _ -> error "nonsense"
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TInteger i -> case resolveIfRef cddl theRule of
      Literal (VBignum i') ->
        if i == i'
          then _Success # ()
          else _Failure # [DidNotValidate "Expecting a different int"]
      -- TODO: can TInteger be a Literal VUInt or VNInt?
      Postlude PTInt -> _Success # ()
      Postlude PTUInt ->
        if i >= 0
          then _Success # ()
          else _Failure # [DidNotValidate "Expecting a positive int"]
      Postlude PTNInt ->
        if i < 0
          then _Success # ()
          else _Failure # [DidNotValidate "Expecting a negative int"]
      Control op n ctrller ->
        let intControl = case (op, resolveIfRef cddl ctrller) of
              (Lt, Literal (VBignum i')) ->
                if i < i'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Le, Literal (VBignum i')) ->
                if i <= i'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Gt, Literal (VBignum i')) ->
                if i > i'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ge, Literal (VBignum i')) ->
                if i >= i'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Eq, Literal (VBignum i')) ->
                if i == i'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ne, Literal (VBignum i')) ->
                if i /= i'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              _ -> error "nonsense"
         in case resolveIfRef cddl n of
              Postlude PTInt -> intControl
              Postlude PTUInt ->
                if i >= 0
                  then intControl
                  else _Failure # [DidNotValidate "Expecting a positive int"]
              Postlude PTNInt ->
                if i < 0
                  then intControl
                  else _Failure # [DidNotValidate "Expecting a positive int"]
              _ -> error "nonsense"
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TBytes b -> case resolveIfRef cddl theRule of
      Literal (VBytes b') ->
        if b == b'
          then _Success # ()
          else _Failure # [DidNotValidate "Expected a different bytestring"]
      Postlude PTBytes -> _Success # ()
      Control op n ctrller ->
        let bytesControl = case (op, resolveIfRef cddl ctrller) of
              (Size, Literal (VUInt i)) ->
                if BS.length b == fromIntegral i
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Lt, Literal (VBytes b')) ->
                if b < b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Le, Literal (VBytes b')) ->
                if b <= b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Gt, Literal (VBytes b')) ->
                if b > b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ge, Literal (VBytes b')) ->
                if b >= b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Eq, Literal (VBytes b')) ->
                if b == b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ne, Literal (VBytes b')) ->
                if b /= b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              _ -> error "nonsense"
         in case resolveIfRef cddl n of
              Postlude PTBytes -> bytesControl
              _ -> error "nonsense"
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TBytesI (BSL.toStrict -> b) -> case resolveIfRef cddl theRule of
      Literal (VBytes b') ->
        if b == b'
          then _Success # ()
          else _Failure # [DidNotValidate "Expected a different bytestring"]
      Postlude PTBytes -> _Success # ()
      Control op n ctrller ->
        let bytesControl = case (op, resolveIfRef cddl ctrller) of
              (Size, Literal (VUInt i)) ->
                if BS.length b == fromIntegral i
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Lt, Literal (VBytes b')) ->
                if b < b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Le, Literal (VBytes b')) ->
                if b <= b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Gt, Literal (VBytes b')) ->
                if b > b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ge, Literal (VBytes b')) ->
                if b >= b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Eq, Literal (VBytes b')) ->
                if b == b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ne, Literal (VBytes b')) ->
                if b /= b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              _ -> error "nonsense"
         in case resolveIfRef cddl n of
              Postlude PTBytes -> bytesControl
              _ -> error "nonsense"
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TString s -> case resolveIfRef cddl theRule of
      Literal (VText s') ->
        if s == s'
          then _Success # ()
          else _Failure # [DidNotValidate "Expected a different text"]
      Postlude PTText -> _Success # ()
      Control op n ctrller ->
        let textControl = case (op, resolveIfRef cddl ctrller) of
              (Size, Literal (VUInt i)) ->
                if T.length s == fromIntegral i
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Lt, Literal (VText b')) ->
                if s < b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Le, Literal (VText b')) ->
                if s <= b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Gt, Literal (VText b')) ->
                if s > b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ge, Literal (VText b')) ->
                if s >= b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Eq, Literal (VText b')) ->
                if s == b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ne, Literal (VText b')) ->
                if s /= b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              _ -> error "nonsense"
         in case resolveIfRef cddl n of
              Postlude PTText -> textControl
              _ -> error "nonsense"
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TStringI (TL.toStrict -> s) -> case resolveIfRef cddl theRule of
      Literal (VText s') ->
        if s == s'
          then _Success # ()
          else _Failure # [DidNotValidate "Expected a different text"]
      Postlude PTText -> _Success # ()
      Control op n ctrller ->
        let textControl = case (op, resolveIfRef cddl ctrller) of
              (Size, Literal (VUInt i)) ->
                if T.length s == fromIntegral i
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Lt, Literal (VText b')) ->
                if s < b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Le, Literal (VText b')) ->
                if s <= b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Gt, Literal (VText b')) ->
                if s > b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ge, Literal (VText b')) ->
                if s >= b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Eq, Literal (VText b')) ->
                if s == b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ne, Literal (VText b')) ->
                if s /= b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              _ -> error "nonsense"
         in case resolveIfRef cddl n of
              Postlude PTText -> textControl
              _ -> error "nonsense"
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TTagged tg t' -> case resolveIfRef cddl theRule of
      Tag tg' rule' ->
        if tg == tg'
          then doValidate cddl t' rule'
          else _Failure # [DidNotValidate "Expected a different tag"]
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TBool b -> case resolveIfRef cddl theRule of
      Literal (VBool b') ->
        if b == b'
          then _Success # ()
          else _Failure # [DidNotValidate "Expected a different bool"]
      Postlude PTBool -> _Success # ()
      Control op n ctrller ->
        let boolControl = case (op, resolveIfRef cddl ctrller) of
              (Eq, Literal (VBool b')) ->
                if b == b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              (Ne, Literal (VBool b')) ->
                if b /= b'
                  then _Success # ()
                  else _Failure # [DidNotValidate "Failed control"]
              _ -> error "nonsense"
         in case resolveIfRef cddl n of
              Postlude PTBool -> boolControl
              _ -> error "nonsense"
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TNull -> case resolveIfRef cddl theRule of
      Postlude PTNil -> _Success # ()
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    THalf h -> case resolveIfRef cddl theRule of
      Literal (VFloat16 h') ->
        if h == h'
          then _Success # ()
          else _Failure # [DidNotValidate "Expected a different float16"]
      Postlude PTHalf -> _Success # ()
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TFloat f -> case resolveIfRef cddl theRule of
      Literal (VFloat32 f') ->
        if f == f'
          then _Success # ()
          else _Failure # [DidNotValidate "Expected a different float32"]
      Postlude PTFloat -> _Success # ()
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TDouble d -> case resolveIfRef cddl theRule of
      Literal (VFloat64 d') ->
        if d == d'
          then _Success # ()
          else _Failure # [DidNotValidate "Expected a different float64"]
      Postlude PTDouble -> _Success # ()
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TList nodes -> case resolveIfRef cddl theRule of
      Array nodes' -> validateGroup cddl nodes nodes'
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TListI nodes -> case resolveIfRef cddl theRule of
      Array nodes' -> validateGroup cddl nodes nodes'
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TMap nodes -> case resolveIfRef cddl theRule of
      Map nodes' -> validateGroup2 cddl nodes nodes'
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TMapI nodes -> case resolveIfRef cddl theRule of
      Map nodes' -> validateGroup2 cddl nodes nodes'
      _ -> _Failure # [DidNotValidate "Types mismatch!"]
    TSimple _s -> undefined

validateChoices ::
  CDDL' -> Term -> [Node MonoRef] -> Validation [Reason] ()
validateChoices _ _ [] = _Failure # [DidNotValidate "None in the choice was valid"]
validateChoices cddl t (choice : choices) =
  doValidate cddl t choice <!> validateChoices cddl t choices

validateGroup ::
  CDDL' ->
  [Term] ->
  [Node MonoRef] ->
  Validation [Reason] ()
validateGroup _ [] [] = _Success # ()
validateGroup _ _ [] = _Failure # [DidNotValidate "Remaining items in array"]
validateGroup cddl [] ns =
  if all isOptional ns
    then _Success # ()
    else _Failure # [DidNotValidate "Non-optional rules remaining"]
  where
    isOptional n = case resolveIfRef cddl n of
      Occur _ OIOptional -> True
      Occur _ OIZeroOrMore -> True
      Occur _ (OIBounded Nothing _) -> True
      Occur _ (OIBounded (Just 0) _) -> True
      _ -> False
validateGroup cddl tss@(t : ts) (n : ns) = case resolveIfRef cddl n of
  Occur newRule OIOptional ->
    validateGroup cddl (t : ts) ns
      <!> (validateItem cddl (t NE.:| []) newRule *> validateGroup cddl ts ns)
  Occur newRule OIOneOrMore ->
    L.foldl1'
      (<!>)
      [ validateItem cddl pre' newRule *> validateGroup cddl post ns
      | (pre, post) <-
          [ splitAt l tss
          | l <- [1 .. length tss]
          ]
      , let pre' = fromJust $ NE.nonEmpty pre
      ]
  Occur newRule OIZeroOrMore ->
    validateGroup cddl (t : ts) ns
      <!> L.foldl1'
        (<!>)
        [ validateItem cddl pre' newRule *> validateGroup cddl post ns
        | (pre, post) <-
            [ splitAt l tss
            | l <- [1 .. length tss]
            ]
        , let pre' = fromJust $ NE.nonEmpty pre
        ]
  Occur newRule (OIBounded (fromIntegral . fromMaybe 0 -> lower) (fmap fromIntegral -> upper)) ->
    L.foldl1'
      (<!>)
      [ validateItem cddl pre' newRule *> validateGroup cddl post ns
      | (pre, post) <-
          [ splitAt l tss
          | l <- maybe [lower .. length tss] (\x -> [lower .. x]) upper
          ]
      , let pre' = fromJust $ NE.nonEmpty pre
      ]
  _ -> validateItem cddl (t NE.:| []) n *> validateGroup cddl ts ns

validateItem :: CDDL' -> NE.NonEmpty Term -> Node MonoRef -> Validation [Reason] ()
validateItem cddl tss rule = case (tss, resolveIfRef cddl rule) of
  (tv NE.:| ts, KV _ v _) ->
    doValidate cddl tv v
      *> (maybe (pure ()) (\ne -> validateItem cddl ne rule) $ NE.nonEmpty ts)
  (_, Group newRules) ->
    validateGroup cddl (NE.toList tss) newRules
  (t NE.:| ts, _) ->
    doValidate cddl t rule
      *> (maybe (pure ()) (\ne -> validateItem cddl ne rule) $ NE.nonEmpty ts)

validateGroup2 ::
  CDDL' ->
  [(Term, Term)] ->
  [Node MonoRef] ->
  Validation [Reason] ()
validateGroup2 _ [] [] = _Success # ()
validateGroup2 _ _ [] = _Failure # [DidNotValidate "Remaining items in array"]
validateGroup2 cddl [] ns =
  if all isOptional ns
    then _Success # ()
    else _Failure # [DidNotValidate "Non-optional rules remaining"]
  where
    isOptional n = case resolveIfRef cddl n of
      Occur _ OIOptional -> True
      Occur _ OIZeroOrMore -> True
      Occur _ (OIBounded Nothing _) -> True
      Occur _ (OIBounded (Just 0) _) -> True
      _ -> False
validateGroup2 cddl tss@(t : ts) (n : ns) = case traceShowId $ resolveIfRef cddl n of
  Occur newRule OIOptional ->
    validateGroup2 cddl (t : ts) ns
      <!> (validateItem2 cddl (t NE.:| []) newRule *> validateGroup2 cddl ts ns)
  Occur newRule OIOneOrMore ->
    L.foldl1'
      (<!>)
      [ validateItem2 cddl pre' newRule *> validateGroup2 cddl post ns
      | (pre, post) <-
          [ splitAt l tss
          | l <- [1 .. length tss]
          ]
      , let pre' = fromJust $ NE.nonEmpty pre
      ]
  Occur newRule OIZeroOrMore ->
    validateGroup2 cddl (t : ts) ns
      <!> L.foldl1'
        (<!>)
        [ validateItem2 cddl pre' newRule *> validateGroup2 cddl post ns
        | (pre, post) <-
            [ splitAt l tss
            | l <- [1 .. length tss]
            ]
        , let pre' = fromJust $ NE.nonEmpty pre
        ]
  Occur newRule (OIBounded (fromIntegral . fromMaybe 0 -> lower) (fmap fromIntegral -> upper)) ->
    L.foldl1'
      (<!>)
      [ validateItem2 cddl pre' newRule *> validateGroup2 cddl post ns
      | (pre, post) <-
          [ splitAt l tss
          | l <- maybe [lower .. length tss] (\x -> [lower .. x]) upper
          ]
      , let pre' = fromJust $ NE.nonEmpty pre
      ]
  _ -> validateItem2 cddl (t NE.:| []) n *> validateGroup2 cddl ts ns

validateItem2 :: CDDL' -> NE.NonEmpty (Term, Term) -> Node MonoRef -> Validation [Reason] ()
validateItem2 cddl tss rule = case (tss, resolveIfRef cddl rule) of
  ((tk, tv) NE.:| ts, KV k v _) ->
    doValidate cddl tk k
      *> doValidate cddl tv v
      *> (maybe (pure ()) (\ne -> validateItem2 cddl ne rule) $ NE.nonEmpty ts)
  (_, Group newRules) ->
    validateGroup2 cddl (NE.toList tss) newRules
  (_ NE.:| _, _) -> _Failure # [DidNotValidate "Not group in map?"]

data Reason
  = InvalidCBOR
  | NotASingleTerm
  | DidNotValidate String
  deriving (Show)

validateCBOR :: BSL.ByteString -> Name -> CTreeRoot' Identity MonoRef -> Except [Reason] ()
validateCBOR bs name ct@(CTreeRoot cddl) = do
  case Map.lookup name cddl of
    Nothing -> error $ "Unbound reference: " <> show name
    Just rule -> do
      (rest, term) <- modifyError (const [InvalidCBOR]) $ liftEither $ deserialiseFromBytes decodeTerm bs
      when (not (BSL.null rest)) $ throwError [NotASingleTerm]
      liftEither $ toEither $ traceShow term $ doValidate ct term (runIdentity rule)

resolveIfRef :: CTreeRoot' Identity MonoRef -> Node MonoRef -> CTree MonoRef
resolveIfRef _ (MIt aa) = aa
resolveIfRef ct@(CTreeRoot cddl) (MRuleRef n) = do
  case Map.lookup n cddl of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val -> resolveIfRef ct $ runIdentity val
