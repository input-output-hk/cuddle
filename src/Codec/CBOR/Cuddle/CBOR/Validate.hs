{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}

module Codec.CBOR.Cuddle.CBOR.Validate where

import Codec.CBOR.Cuddle.CBOR.Validate.Combinators
import Codec.CBOR.Cuddle.CBOR.Validate.Ops
import Codec.CBOR.Cuddle.CBOR.Validate.Types
import Codec.CBOR.Cuddle.CDDL hiding (Group (..))
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.CDDL.Postlude
import Codec.CBOR.Cuddle.CDDL.Resolve
import Codec.CBOR.Magic
import Codec.CBOR.Read
import Codec.CBOR.Term
import Control.Lens ((#))
import Control.Monad (unless, when)
import Control.Monad.Except
import Data.Bits (Bits, bit, complement, zeroBits, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
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

-- We make the conscious choice of parsing the provided term instead
-- of navigating the rule, as rules might be circular but the term
-- cannot.

-- TODOs
-- - cuts in maps
-- - .default
-- - Unwrap

{-------------------------------------------------------------------------------
  Choices
-------------------------------------------------------------------------------}

validateChoices ::
  CDDL' -> Term -> [Node MonoRef] -> Validation [Reason] ()
validateChoices _ _ [] = _Failure # [DidNotValidate "None in the choice was valid"]
validateChoices cddl t (choice : choices) =
  doValidate cddl t choice <!> validateChoices cddl t choices

{-------------------------------------------------------------------------------
  Int and integers
-------------------------------------------------------------------------------}

validateIntegral ::
  (Integral a, Show a, Ops a) => CDDL' -> a -> Rule' -> Validation [Reason] ()
validateIntegral cddl i theRule = case (resolveIfRef cddl theRule, get cddl theRule) of
  (_, Just i') ->
    failUnless (i == i') [Unexpected ("the fixed int " <> show i') (show i)]
  (Postlude PTInt, Nothing) -> _Success # ()
  (Postlude PTUInt, Nothing) ->
    failUnless (i >= 0) [Unexpected "a positive int" (show i)]
  (Postlude PTNInt, Nothing) ->
    failUnless (i < 0) [Unexpected "a negative int" (show i)]
  (Range from to incl, Nothing) -> case (get cddl from, get cddl to) of
    (Just low, Just high) ->
      failUnless
        ( low <= i && case incl of
            ClOpen -> i < high
            Closed -> i <= high
        )
        [Unexpected ("an int in " <> show (low, high, incl)) (show i)]
    _ -> _Failure # [DidNotValidate $ "Got malformed range " <> show (Range from to incl)]
  (Tag 2 theRule', Nothing) -> case resolveIfRef cddl theRule' of
    Postlude PTBytes -> _Success # ()
    _ -> _Failure # [DidNotValidate $ "Malformed rule! " <> show (Tag 3 theRule')]
  (Tag 3 theRule', Nothing) -> case resolveIfRef cddl theRule' of
    Postlude PTBytes -> _Success # ()
    _ -> _Failure # [DidNotValidate $ "Malformed rule! " <> show (Tag 3 theRule')]
  (Control op n ctrller, Nothing) ->
    let intControl = dispatchOp op doValidate cddl ctrller i
     in case (resolveIfRef cddl n, get cddl n) of
          (_, Just i') ->
            validateWithUnless
              (i == i')
              intControl
              [Unexpected ("the fixed int " <> show i') (show i)]
          (Postlude PTInt, Nothing) -> intControl
          (Postlude PTUInt, Nothing) ->
            validateWithUnless (i >= 0) intControl [DidNotValidate "Expecting a positive int"]
          (Range from to incl, Nothing) -> case (get cddl from, get cddl to) of
            (Just low, Just high) ->
              validateWithUnless
                ( low <= i && case incl of
                    ClOpen -> i < high
                    Closed -> i <= high
                )
                intControl
                [Unexpected ("an int in " <> show (low, high, incl)) (show i)]
            _ -> _Failure # [DidNotValidate $ "Got malformed range " <> show (Range from to incl)]
          (Postlude PTNInt, Nothing) ->
            validateWithUnless (i < 0) intControl [DidNotValidate "Expecting a positive int"]
          lhs -> _Failure # [DidNotValidate $ "Control lhs is not valid for ints: " <> show lhs]
  rr -> _Failure # [Unexpected "a rule for ints" (show rr)]

{-------------------------------------------------------------------------------
  ByteStrings
-------------------------------------------------------------------------------}

validateBytes :: CDDL' -> BS.ByteString -> Rule' -> Validation [Reason] ()
validateBytes cddl b theRule = case resolveIfRef cddl theRule of
  Literal (VBytes (Base16.decode -> Right b')) ->
    failUnless (b == b') [Unexpected ("the exact bytestring " <> show b') (show b)]
  Postlude PTBytes -> _Success # ()
  Control op n ctrller ->
    let bytesControl = dispatchOp op doValidate cddl ctrller b
     in case resolveIfRef cddl n of
          Postlude PTBytes -> bytesControl
          lhs -> _Failure # [DidNotValidate $ "Control lhs is not valid for bytestrings: " <> show lhs]
  rr -> _Failure # [Unexpected "a rule for bytestrings" (show rr)]

{-------------------------------------------------------------------------------
  Text
-------------------------------------------------------------------------------}

validateText :: CDDL' -> T.Text -> Rule' -> Validation [Reason] ()
validateText cddl b theRule = case resolveIfRef cddl theRule of
  Literal (VText b') ->
    failUnless (b == b') [Unexpected ("the exact textstring " <> show b') (show b)]
  Postlude PTText -> _Success # ()
  Control op n ctrller ->
    let textControl = dispatchOp op doValidate cddl ctrller b
     in case resolveIfRef cddl n of
          Postlude PTText -> textControl
          lhs -> _Failure # [DidNotValidate $ "Control lhs is not valid for textstrings: " <> show lhs]
  rr -> _Failure # [Unexpected "a rule for textstrings" (show rr)]

{-------------------------------------------------------------------------------
  Tags
-------------------------------------------------------------------------------}

validateTag :: CDDL' -> Word64 -> Term -> Rule' -> Validation [Reason] ()
validateTag cddl tg t' theRule = case resolveIfRef cddl theRule of
  Tag tg' rule' ->
    validateWithUnless
      (tg == tg')
      (doValidate cddl t' rule')
      [DidNotValidate "Expected a different tag"]
  rr -> _Failure # [Unexpected "a rule for tags" (show rr)]

{-------------------------------------------------------------------------------
  Bool
-------------------------------------------------------------------------------}

validateBool :: CDDL' -> Bool -> Rule' -> Validation [Reason] ()
validateBool cddl b theRule = case resolveIfRef cddl theRule of
  Literal (VBool b') ->
    failUnless (b == b') [DidNotValidate "Expected a different bool"]
  Postlude PTBool -> _Success # ()
  Control op n ctrller ->
    let boolControl = case (op, resolveIfRef cddl ctrller) of
          (Eq, Literal (VBool b')) ->
            failUnless (b == b') [Unexpected ("the exact boolean " <> show b') (show b)]
          (Ne, Literal (VBool b')) ->
            failUnless (b /= b') [Unexpected ("the exact boolean " <> show (not b')) (show b)]
          _ -> _Failure # [DidNotValidate $ "Control is not applicable to bool: " <> show op]
     in case resolveIfRef cddl n of
          Postlude PTBool -> boolControl
          lhs -> _Failure # [DidNotValidate $ "Control lhs is not valid for bool: " <> show lhs]
  rr -> _Failure # [Unexpected "a rule for bool" (show rr)]

{-------------------------------------------------------------------------------
  Floating points
-------------------------------------------------------------------------------}

precHalf :: PTerm -> Validation [Reason] () -> Validation [Reason] ()
precHalf PTHalf = id
precHalf pt = const $ _Failure # [DidNotValidate $ "Wanted type float16 but rule is " <> show pt]

precFloat :: PTerm -> Validation [Reason] () -> Validation [Reason] ()
precFloat PTFloat = id
precFloat pt = const $ _Failure # [DidNotValidate $ "Wanted type float32 but rule is " <> show pt]

precDouble :: PTerm -> Validation [Reason] () -> Validation [Reason] ()
precDouble PTDouble = id
precDouble pt = const $ _Failure # [DidNotValidate $ "Wanted type float64 but rule is " <> show pt]

validateFloat ::
  (Eq a, Show a, Ord a, Num a, Ops a) =>
  CDDL' ->
  a ->
  (PTerm -> Validation [Reason] () -> Validation [Reason] ()) ->
  Rule' ->
  Validation [Reason] ()
validateFloat cddl i checkPrecision theRule = case (resolveIfRef cddl theRule, get cddl theRule) of
  (_, Just i') ->
    failUnless (i == i') [Unexpected ("the fixed float " <> show i') (show i)]
  (Postlude pttype, Nothing) -> checkPrecision pttype (_Success # ())
  (Range from to incl, Nothing) -> case (get cddl from, get cddl to) of
    (Just low, Just high) ->
      failUnless
        ( low <= i && case incl of
            ClOpen -> i < high
            Closed -> i <= high
        )
        [Unexpected ("a float in " <> show (low, high, incl)) (show i)]
    _ -> _Failure # [DidNotValidate $ "Got malformed range " <> show (Range from to incl)]
  (Control op n ctrller, Nothing) ->
    let floatControl = dispatchOp op doValidate cddl ctrller i
     in case (resolveIfRef cddl n, get cddl n) of
          (_, Just i') ->
            validateWithUnless
              (i == i')
              floatControl
              [Unexpected ("the fixed float " <> show i') (show i)]
          (Range from to incl, Nothing) -> case (get cddl from, get cddl to) of
            (Just low, Just high) ->
              validateWithUnless
                ( low <= i && case incl of
                    ClOpen -> i < high
                    Closed -> i <= high
                )
                floatControl
                [Unexpected ("a float in " <> show (low, high, incl)) (show i)]
            _ -> _Failure # [DidNotValidate $ "Got malformed range " <> show (Range from to incl)]
          (Postlude pttype, Nothing) -> checkPrecision pttype floatControl
          lhs -> _Failure # [DidNotValidate $ "Control lhs is not valid for floats: " <> show lhs]
  rr -> _Failure # [Unexpected "a rule for floats" (show rr)]

{-------------------------------------------------------------------------------
  Groups
-------------------------------------------------------------------------------}

validateGroup ::
  CDDL' ->
  (CDDL' -> NE.NonEmpty a -> Rule' -> Validation [Reason] ()) ->
  [a] ->
  [Node MonoRef] ->
  Validation [Reason] ()
validateGroup _ _ [] [] = _Success # ()
validateGroup _ _ _ [] = _Failure # [DidNotValidate "Remaining items in array"]
validateGroup cddl _ [] ns =
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
validateGroup cddl doValidateItem tss@(t : ts) (n : ns) = case resolveIfRef cddl n of
  Occur newRule OIOptional ->
    validateGroup cddl doValidateItem (t : ts) ns
      <!> (doValidateItem cddl (t NE.:| []) newRule *> validateGroup cddl doValidateItem ts ns)
  Occur newRule OIOneOrMore ->
    L.foldl1'
      (<!>)
      [ doValidateItem cddl (t NE.:| pre) newRule *> validateGroup cddl doValidateItem post ns
      | (pre, post) <-
          [ splitAt l tss
          | l <- [0 .. length ts]
          ]
      ]
  Occur newRule OIZeroOrMore ->
    validateGroup cddl doValidateItem (t : ts) ns
      <!> L.foldl1'
        (<!>)
        [ doValidateItem cddl (t NE.:| pre) newRule *> validateGroup cddl doValidateItem post ns
        | (pre, post) <-
            [ splitAt l tss
            | l <- [0 .. length ts]
            ]
        ]
  Occur newRule (OIBounded (fromIntegral . fromMaybe 0 -> lower) (fmap fromIntegral -> upper)) ->
    L.foldl1'
      (<!>)
      [ ( case NE.nonEmpty pre of
            Nothing -> pure ()
            Just pre' -> doValidateItem cddl pre' newRule
        )
          *> validateGroup cddl doValidateItem post ns
      | (pre, post) <-
          [ splitAt l tss
          | l <- [lower .. fromMaybe (length tss) upper]
          ]
      ]
  _ -> doValidateItem cddl (t NE.:| []) n *> validateGroup cddl doValidateItem ts ns

validateItem :: CDDL' -> NE.NonEmpty Term -> Node MonoRef -> Validation [Reason] ()
validateItem cddl tss rule = case (tss, resolveIfRef cddl rule) of
  (tv NE.:| ts, KV _ v _) ->
    doValidate cddl tv v
      *> (maybe (pure ()) (\next -> validateItem cddl next rule) $ NE.nonEmpty ts)
  (_, Group newRules) ->
    validateGroup cddl validateItem (NE.toList tss) newRules
  (t NE.:| ts, _) ->
    doValidate cddl t rule
      *> (maybe (pure ()) (\next -> validateItem cddl next rule) $ NE.nonEmpty ts)

validateItem2 :: CDDL' -> NE.NonEmpty (Term, Term) -> Node MonoRef -> Validation [Reason] ()
validateItem2 cddl tss rule = case (tss, resolveIfRef cddl rule) of
  ((tk, tv) NE.:| ts, KV k v _) ->
    doValidate cddl tk k
      *> doValidate cddl tv v
      *> (maybe (pure ()) (\next -> validateItem2 cddl next rule) $ NE.nonEmpty ts)
  (_, Group newRules) ->
    validateGroup cddl validateItem2 (NE.toList tss) newRules
  (_ NE.:| _, _) -> _Failure # [DidNotValidate $ "No group and no key-value rule in map: " <> show rule]

{-------------------------------------------------------------------------------
  Do validate
-------------------------------------------------------------------------------}

doValidate :: CDDL' -> Term -> Rule' -> Validation [Reason] ()
doValidate cddl t theRule = case resolveIfRef cddl theRule of
  Choice choices -> validateChoices cddl t (NE.toList choices)
  Control And r1 r2 ->
    doValidate cddl t r1 <* doValidate cddl t r2
  Control Within r1 r2 ->
    doValidate cddl t r1 <* doValidate cddl t r2
  Postlude PTAny -> _Success # ()
  _ -> case t of
    TInt i -> validateIntegral cddl i theRule
    TInteger i -> validateIntegral cddl i theRule
    TBytes b -> validateBytes cddl b theRule
    TBytesI (BSL.toStrict -> b) -> validateBytes cddl b theRule
    TString s -> validateText cddl s theRule
    TStringI (TL.toStrict -> s) -> validateText cddl s theRule
    TTagged tg t' -> validateTag cddl tg t' theRule
    TBool b -> validateBool cddl b theRule
    THalf h -> validateFloat cddl (Half h) precHalf theRule
    TFloat f -> validateFloat cddl f precFloat theRule
    TDouble d -> validateFloat cddl d precDouble theRule
    TList nodes -> case resolveIfRef cddl theRule of
      Array nodes' -> validateGroup cddl validateItem nodes nodes'
      rr -> _Failure # [Unexpected "a rule for lists" (show rr)]
    TListI nodes -> case resolveIfRef cddl theRule of
      Array nodes' -> validateGroup cddl validateItem nodes nodes'
      rr -> _Failure # [Unexpected "a rule for lists" (show rr)]
    TMap nodes -> case resolveIfRef cddl theRule of
      Map nodes' -> validateGroup cddl validateItem2 nodes nodes'
      rr -> _Failure # [Unexpected "a rule for maps" (show rr)]
    TMapI nodes -> case resolveIfRef cddl theRule of
      Map nodes' -> validateGroup cddl validateItem2 nodes nodes'
      rr -> _Failure # [Unexpected "a rule for maps" (show rr)]
    TNull -> case resolveIfRef cddl theRule of
      Postlude PTNil -> _Success # ()
      rr -> _Failure # [Unexpected "a rule for nil" (show rr)]
    TSimple _s -> error "Validating TSimple unimplemented"

validateCBOR :: BS.ByteString -> Name -> CDDL' -> Except [Reason] ()
validateCBOR bs name ct@(CTreeRoot cddl) = do
  case Map.lookup name cddl of
    Nothing -> throwError [UnboundRef name]
    Just rule -> do
      (rest, term) <-
        modifyError (const [InvalidTopCBOR]) $
          liftEither $
            deserialiseFromBytes decodeTerm (BSL.fromStrict bs)
      unless (BSL.null rest) $ throwError [NotASingleTopTerm]
      liftEither $ toEither $ doValidate ct term (runIdentity rule)
