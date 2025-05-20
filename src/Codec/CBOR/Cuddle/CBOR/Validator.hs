{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Codec.CBOR.Cuddle.CBOR.Validator where

import Codec.CBOR.Cuddle.CDDL hiding (CDDL, Group, Rule)
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.CDDL.Postlude
import Codec.CBOR.Cuddle.CDDL.Resolve
import Codec.CBOR.Read
import Codec.CBOR.Term
import Control.Monad (void)
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor
import Data.Bits hiding (And)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Either
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity
import Data.IntSet qualified as IS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Word
import GHC.Float
import Text.Regex.TDFA
import System.IO
import System.Exit
import Control.Exception

type CDDL = CTreeRoot' Identity MonoRef
type Rule = Node MonoRef
type ResolvedRule = CTree MonoRef

-- | The result of validation. `leaf` will be instantiated to either `Term` on
-- success or something that contains `CDDLFail` on failure.
data ValidationTree leaf
  = -- | The beginning of the validation
    ValTop Rule (ValidationTree leaf)
  | -- | A leaf of the validation
    ValLeaf Rule leaf
  | -- | Validation of subitems in a container
    ValContainer Rule [ValidationTree Term] [ValidationTree leaf]
  deriving (Show, Functor)

data CDDLFail
  = -- | The rule directly does not validate
    DirectFail
  | -- | The rule validated on the target but not on the control
    ControlFail Rule
  | -- | Each of the rules in a choice failed
    ChoiceFail [(Rule, CDDLFail)]
  | -- | Each of the rules in a choice failed, but for each rule, either it
    -- directly fails or it is able to validate some items and then fail.
    ChoiceFail' [(Rule, Either ([ValidationTree Term], [ValidationTree CDDLFail]) CDDLFail)]
  | -- | The rule cannot be applied to the type of the item
    UnapplicableRule
  | -- | There are remaining rules to be applied and no values to validate
    RemainingRules
  | -- | There are remaining items to be validated and no rules left
    RemainingItems
  | -- | A KV in a map didn't find a rule to validate
    NoApplicableRuleFound (Term, Term)
                          [( Rule
                           , Either
                               (ValidationTree (Term, CDDLFail))
                               (ValidationTree Term, ValidationTree (Term, CDDLFail))
                           )
                          ]
  deriving (Show)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b' <- b; if b' then t else f

check ::
  MonadError (ValidationTree CDDLFail) m => (CDDLFail -> ValidationTree CDDLFail) -> Bool -> m ()
check f c = if c then pure () else throwError $ f DirectFail

range :: Ord a => RangeBound -> a -> a -> Bool
range Closed = (<=)
range ClOpen = (<)

--------------------------------------------------------------------------------
-- Resolving rules from the CDDL spec

resolveIfRef :: CDDL -> Rule -> ResolvedRule
resolveIfRef _ (MIt aa) = aa
resolveIfRef ct@(CTreeRoot cddl) (MRuleRef n) = do
  case Map.lookup n cddl of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val -> resolveIfRef ct $ runIdentity val

getRule :: MonadReader CDDL m => Rule -> m ResolvedRule
getRule rule = flip resolveIfRef rule <$> ask

--------------------------------------------------------------------------------
-- Main entry point

validateCBOR :: BS.ByteString -> Name -> CDDL -> IO ()
validateCBOR bs rule cddl =
  (case validateCBOR' bs rule cddl of
    Right {} -> putStrLn "Valid"
    Left err -> do
      hPutStrLn stderr $ "Invalid " ++ show err
      exitFailure
  ) `catch` (\(e :: PatternMatchFail) -> putStrLn $ "You uncovered a path we thought was impossible! Please submit your CDDL and CBOR to `https://github.com/input-output-hk/cuddle/issues` for us to investigate\n" <> displayException e)

validateCBOR' ::
  BS.ByteString -> Name -> CDDL -> Either (ValidationTree (Term, CDDLFail)) (ValidationTree Term)
validateCBOR' bs rule cddl@(CTreeRoot tree) =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict bs) of
    Left e -> error $ show e
    Right (_, term) -> runReader (runExceptT (validateTerm term (runIdentity $ tree Map.! rule))) cddl

--------------------------------------------------------------------------------
-- Terms

-- | Core function that validates a CBOR term to a particular rule of the CDDL
-- spec
validateTerm ::
  (MonadError (ValidationTree (Term, CDDLFail)) m, MonadReader CDDL m) =>
  Term -> Rule -> m (ValidationTree Term)
validateTerm term =
  let f = modifyError (fmap (term,)) . fmap (fmap (const term))
      g = modifyError (fmap (term,))
   in case term of
        TInt i -> f . validateInt i
        TInteger i -> f . validateInteger i
        TBytes b -> f . validateBytes b
        TBytesI b -> f . validateBytes (BSL.toStrict b)
        TString s -> f . validateText s
        TStringI s -> f . validateText (TL.toStrict s)
        TList ts -> g . fmap ($ term) . validateList ts
        TListI ts -> g . fmap ($ term) . validateList ts
        TMap ts -> g . fmap ($ term) . validateMap ts
        TMapI ts -> g . fmap ($ term) . validateMap ts
        TTagged w t -> g . fmap ($ term) . validateTagged w t
        TBool b -> f . validateBool b
        TNull -> f . validateNull
        TSimple s -> f . validateSimple s
        THalf h -> f . validateHalf h
        TFloat h -> f . validateFloat h
        TDouble d -> f . validateDouble d

--------------------------------------------------------------------------------
-- Ints and integers

-- | Validation of an Int. CBOR categorizes every integral in `TInt` or `TInteger` but it can be the case that we are decoding something that is expected to be a `Word64` even if we get a `TInt`.
--
-- > ghci> encodeWord64 15
-- > [TkInt 15]
-- > ghci> encodeWord64 maxBound
-- > [TkInteger 18446744073709551615]
--
-- For this reason, we cannot assume that bounds or literals are going to be
-- Ints, so we convert everything to Integer.
validateInt ::
  (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) => Int -> Rule -> m (ValidationTree ())
validateInt ii@(fromIntegral @Int @Integer -> i) rule = do
  getRule rule >>= \case
    -- a = any
    Postlude PTAny -> pure ()
    -- a = int
    Postlude PTInt -> pure ()
    -- a = uint
    Postlude PTUInt -> check (ValLeaf rule) $ i >= 0
    -- a = nint
    Postlude PTNInt -> check (ValLeaf rule) $ i <= 0
    -- a = x
    Literal (Value (VUInt i') _) -> check (ValLeaf rule) $ i == fromIntegral i'
    -- a = -x
    Literal (Value (VNInt i') _) -> check (ValLeaf rule) $ -i == fromIntegral i'
    -- a = foo .and bar
    Control And tgt ctrl ->
      validateInt ii tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateInt ii ctrl)
    -- a = foo .within bar
    Control Within tgt ctrl ->
      validateInt ii tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateInt ii ctrl)
    -- a = foo .ctrl bar
    Control op tgt ctrl ->
      validateInt ii tgt
        >> ifM (controlInt ii op ctrl) (pure ()) (throwError (ValLeaf rule (ControlFail ctrl)))
    -- a = foo / bar
    Choice opts ->
      void $ validateChoice (validateInt ii) rule opts
    -- a = x..y
    Range low high bound -> do
      ((,) <$> getRule low <*> getRule high)
        >>= check (ValLeaf rule) . \case
          (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= i && range bound i m
          (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> -n <= i && range bound i m
          (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> -n <= i && range bound i (-m)
    -- a = &(x, y, z)
    Enum g ->
      getRule g >>= \case
        Group g' -> void $ validateInt ii (MIt (Choice (NE.fromList g')))
    -- a = x: y
    -- Note KV cannot appear on its own, but we will use this when validating
    -- lists.
    KV _ v _ -> void $ validateInt ii v
    -- No other rule can be applied to ints
    _ -> throwError (ValLeaf rule UnapplicableRule)
  pure (ValLeaf rule ())

-- | Validating a `Choice` either succeeds or throws an error with a
-- `ValidationTree` that contains `ChoiceFail` or `ChoiceFail'`.
validateChoice ::
  forall m t a.
  (MonadError (ValidationTree CDDLFail) m, Foldable t) => (Rule -> m a) -> Rule -> t Rule -> m a
validateChoice f rule = go . toList
  where
    go :: [Rule] -> m a
    go [] = throwError (ValLeaf rule (ChoiceFail []))
    go (x : xs) = do
      tryError (f x) >>= \case
        -- if x succeeds then we are done
        Right a -> pure a
        -- if x fails, we try the rest. Should they succeed we are done, but if
        -- they throw an error we append our error to the one they return.
        Left (ValLeaf r e) ->
          tryError (go xs) >>= \case
            Right a -> pure a
            Left (ValLeaf _ (ChoiceFail es)) -> throwError $ ValLeaf rule (ChoiceFail ((x, e) : es))
            Left (ValLeaf _ (ChoiceFail' es)) -> throwError $ ValLeaf rule (ChoiceFail' ((x, Left ([], [ValLeaf r e])) : es))
        -- if x fails, we try the rest. Should they succeed we are done, but if
        -- they throw an error we append our error to the one they return.
        Left (ValContainer _ v e) ->
          tryError (go xs) >>= \case
            Right a -> pure a
            Left (ValLeaf _ (ChoiceFail' es)) -> throwError $ ValLeaf rule (ChoiceFail' ((x, Left (v, e)) : es))
            Left (ValLeaf _ (ChoiceFail es)) -> throwError $ ValLeaf rule (ChoiceFail' ((x, Left (v, e)) : map (second Right) es))

-- | The control operators for ints
controlInt :: forall m. MonadReader CDDL m => Int -> CtlOp -> Rule -> m Bool
controlInt (fromIntegral @Int @Integer -> i) Size ctrl =
  getRule ctrl <&> \case
    Literal (Value (VUInt sz) _) -> 0 <= i && i < 256 ^ sz
controlInt i Bits ctrl = do
  indices <-
    getRule ctrl >>= \case
      Literal (Value (VUInt i') _) -> pure [i']
      Choice nodes -> getIndicesOfChoice nodes
      Range ff tt incl -> getIndicesOfRange ff tt incl
      Enum g -> getIndicesOfEnum g
  go (IS.fromList (map fromIntegral indices)) i 0
  where
    go :: IS.IntSet -> Int -> IS.Key -> m Bool
    go _ 0 _ = pure True
    go indices n idx =
      let bitSet = testBit n 0
          allowed = not bitSet || IS.member idx indices
       in if allowed
            then go indices (shiftR n 1) (idx + 1)
            else pure False
controlInt (fromIntegral @Int @Integer -> i) Lt ctrl =
  getRule ctrl <&> \case
    Literal (Value (VUInt i') _) -> i < fromIntegral i'
    Literal (Value (VNInt i') _) -> i < -fromIntegral i'
controlInt (fromIntegral @Int @Integer -> i) Gt ctrl =
  getRule ctrl <&> \case
    Literal (Value (VUInt i') _) -> i > fromIntegral i'
    Literal (Value (VNInt i') _) -> i > -fromIntegral i'
controlInt (fromIntegral @Int @Integer -> i) Le ctrl =
  getRule ctrl <&> \case
    Literal (Value (VUInt i') _) -> i <= fromIntegral i'
    Literal (Value (VNInt i') _) -> i <= -fromIntegral i'
controlInt (fromIntegral @Int @Integer -> i) Ge ctrl =
  getRule ctrl <&> \case
    Literal (Value (VUInt i') _) -> i >= fromIntegral i'
    Literal (Value (VNInt i') _) -> i >= -fromIntegral i'
controlInt (fromIntegral @Int @Integer -> i) Eq ctrl =
  getRule ctrl <&> \case
    Literal (Value (VUInt i') _) -> i == fromIntegral i'
    Literal (Value (VNInt i') _) -> i == -fromIntegral i'
controlInt (fromIntegral @Int @Integer -> i) Ne ctrl =
  getRule ctrl <&> \case
    Literal (Value (VUInt i') _) -> i /= fromIntegral i'
    Literal (Value (VNInt i') _) -> i /= -fromIntegral i'

-- | Validating an Integer
validateInteger ::
  (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) =>
  Integer -> Rule -> m (ValidationTree ())
validateInteger i rule = do
  getRule rule >>= \case
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
    Postlude PTAny -> pure ()
    -- a = int
    Postlude PTInt -> pure ()
    -- a = uint
    Postlude PTUInt -> check (ValLeaf rule) (i >= 0)
    -- a = nint
    Postlude PTNInt -> check (ValLeaf rule) (i <= 0)
    -- a = x
    Literal (Value (VUInt i') _) -> check (ValLeaf rule) $ i == fromIntegral i'
    -- a = -x
    Literal (Value (VNInt i') _) -> check (ValLeaf rule) $ -i == fromIntegral i'
    -- a = <big number>
    Literal (Value (VBignum i') _) -> check (ValLeaf rule) $ i == i'
    -- a = foo .and bar
    Control And tgt ctrl ->
      validateInteger i tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateInteger i ctrl)
    -- a = foo .within bar
    Control Within tgt ctrl ->
      validateInteger i tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateInteger i ctrl)
    -- a = foo .ctrl bar
    Control op tgt ctrl ->
      validateInteger i tgt
        >> ifM (controlInteger i op ctrl) (pure ()) (throwError (ValLeaf rule (ControlFail ctrl)))
    -- a = foo / bar
    Choice opts ->
      void $ validateChoice (validateInteger i) rule opts
    -- a = x..y
    Range low high bound ->
      ((,) <$> getRule low <*> getRule high)
        >>= check (ValLeaf rule) . \case
          (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= i && range bound i m
          (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> -n <= i && range bound i m
          (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> -n <= i && range bound i (-m)
          (Literal (Value (VBignum n) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= i && range bound i m
          (Literal (Value (VBignum n) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> n <= i && range bound i (-m)
          (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _)) -> n <= i && range bound i m
          (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VBignum m) _)) -> (-n) <= i && range bound i m
    _ -> throwError (ValLeaf rule UnapplicableRule)
  pure (ValLeaf rule ())

-- | Controls for an Integer
controlInteger :: MonadReader CDDL m => Integer -> CtlOp -> Rule -> m Bool
controlInteger i Size ctrl =
  getRule ctrl <&> \case
    Literal (Value (VUInt sz) _) -> 0 <= i && i < 256 ^ sz
controlInteger i Bits ctrl = do
  indices <-
    getRule ctrl >>= \case
      Literal (Value (VUInt i') _) -> pure [i']
      Choice nodes -> getIndicesOfChoice nodes
      Range ff tt incl -> getIndicesOfRange ff tt incl
      Enum g -> getIndicesOfEnum g
  go (IS.fromList (map fromIntegral indices)) i 0
  where
    go _ 0 _ = pure True
    go indices n idx =
      let bitSet = testBit n 0
          allowed = not bitSet || IS.member idx indices
       in if allowed
            then go indices (shiftR n 1) (idx + 1)
            else pure False
controlInteger i Lt ctrl =
  getRule ctrl <&> \case
    Literal (Value (VUInt i') _) -> i < fromIntegral i'
    Literal (Value (VNInt i') _) -> i < -fromIntegral i'
    Literal (Value (VBignum i') _) -> i < i'
controlInteger i Gt ctrl =
  getRule ctrl <&> \case
    Literal (Value (VUInt i') _) -> i > fromIntegral i'
    Literal (Value (VNInt i') _) -> i > -fromIntegral i'
    Literal (Value (VBignum i') _) -> i > i'
controlInteger i Le ctrl =
  getRule ctrl <&> \case
    Literal (Value (VUInt i') _) -> i <= fromIntegral i'
    Literal (Value (VNInt i') _) -> i <= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i <= i'
controlInteger i Ge ctrl =
  getRule ctrl <&> \case
    Literal (Value (VUInt i') _) -> i >= fromIntegral i'
    Literal (Value (VNInt i') _) -> i >= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i >= i'
controlInteger i Eq ctrl =
  getRule ctrl <&> \case
    Literal (Value (VUInt i') _) -> i == fromIntegral i'
    Literal (Value (VNInt i') _) -> i == -fromIntegral i'
    Literal (Value (VBignum i') _) -> i == i'
controlInteger i Ne ctrl =
  getRule ctrl <&> \case
    Literal (Value (VUInt i') _) -> i /= fromIntegral i'
    Literal (Value (VNInt i') _) -> i /= -fromIntegral i'
    Literal (Value (VBignum i') _) -> i /= i'

--------------------------------------------------------------------------------
-- Floating point (Float16, Float32, Float64)
--
-- As opposed to Integral types, there seems to be no ambiguity when encoding
-- and decoding floating-point numbers.

-- | Validating a `Float16`
validateHalf ::
  (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) =>
  Float -> Rule -> m (ValidationTree ())
validateHalf f rule = do
  getRule rule >>= \case
    -- a = any
    Postlude PTAny -> pure ()
    -- a = float16
    Postlude PTHalf -> pure ()
    -- a = 0.5
    Literal (Value (VFloat16 f') _) -> check (ValLeaf rule) $ f == f'
    -- a = foo / bar
    Choice opts ->
      void $ validateChoice (validateHalf f) rule opts
    -- a = foo .and bar
    Control And tgt ctrl -> do
      validateHalf f tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateHalf f ctrl)
    -- a = foo .within bar
    Control Within tgt ctrl ->
      validateHalf f tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateHalf f ctrl)
    -- a = foo .ctrl bar
    Control op tgt ctrl ->
      validateHalf f tgt
        >> ifM (controlHalf f op ctrl) (pure ()) (throwError (ValLeaf rule (ControlFail ctrl)))
    -- a = x..y
    Range low high bound ->
      ((,) <$> getRule low <*> getRule high)
        >>= check (ValLeaf rule) . \case
          (Literal (Value (VFloat16 n) _), Literal (Value (VFloat16 m) _)) -> n <= f && range bound f m
    _ -> throwError (ValLeaf rule UnapplicableRule)
  pure (ValLeaf rule ())

-- | Controls for `Float16`
controlHalf :: MonadReader CDDL m => Float -> CtlOp -> Rule -> m Bool
controlHalf f Eq ctrl =
  getRule ctrl <&> \case
    Literal (Value (VFloat16 f') _) -> f == f'
controlHalf f Ne ctrl =
  getRule ctrl <&> \case
    Literal (Value (VFloat16 f') _) -> f /= f'

-- | Validating a `Float32`
validateFloat ::
  (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) =>
  Float -> Rule -> m (ValidationTree ())
validateFloat f rule = do
  getRule rule >>= \case
    -- a = any
    Postlude PTAny -> pure ()
    -- a = float32
    Postlude PTFloat -> pure ()
    -- a = 0.000000005
    -- TODO: it is unclear if smaller floats should also validate
    Literal (Value (VFloat32 f') _) -> check (ValLeaf rule) $ f == f'
    -- a = foo / bar
    Choice opts ->
      void $ validateChoice (validateFloat f) rule opts
    -- a = foo .and bar
    Control And tgt ctrl ->
      validateFloat f tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateFloat f ctrl)
    -- a = foo .within bar
    Control Within tgt ctrl ->
      validateFloat f tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateFloat f ctrl)
    -- a = foo .ctrl bar
    Control op tgt ctrl ->
      validateFloat f tgt
        >> ifM (controlFloat f op ctrl) (pure ()) (throwError (ValLeaf rule (ControlFail ctrl)))
    -- a = x..y
    -- TODO it is unclear if this should mix floating point types too
    Range low high bound ->
      ((,) <$> getRule low <*> getRule high)
        >>= check (ValLeaf rule) . \case
          (Literal (Value (VFloat16 n) _), Literal (Value (VFloat16 m) _)) -> n <= f && range bound f m
          (Literal (Value (VFloat32 n) _), Literal (Value (VFloat32 m) _)) -> n <= f && range bound f m
    _ -> throwError (ValLeaf rule UnapplicableRule)
  pure (ValLeaf rule ())

-- | Controls for `Float32`
controlFloat :: MonadReader CDDL m => Float -> CtlOp -> Rule -> m Bool
controlFloat f Eq ctrl =
  getRule ctrl <&> \case
    Literal (Value (VFloat16 f') _) -> f == f'
    Literal (Value (VFloat32 f') _) -> f == f'
controlFloat f Ne ctrl =
  getRule ctrl <&> \case
    Literal (Value (VFloat16 f') _) -> f /= f'
    Literal (Value (VFloat32 f') _) -> f /= f'

-- | Validating a `Float64`
validateDouble ::
  (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) =>
  Double -> Rule -> m (ValidationTree ())
validateDouble f rule = do
  getRule rule >>= \case
    -- a = any
    Postlude PTAny -> pure ()
    -- a = float64
    Postlude PTDouble -> pure ()
    -- a = 0.0000000000000000000000000000000000000000000005
    -- TODO: it is unclear if smaller floats should also validate
    Literal (Value (VFloat64 f') _) -> check (ValLeaf rule) $ f == f'
    -- a = foo / bar
    Choice opts -> void $ validateChoice (validateDouble f) rule opts
    -- a = foo .and bar
    Control And tgt ctrl ->
      validateDouble f tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateDouble f ctrl)
    -- a = foo .within bar
    Control Within tgt ctrl ->
      validateDouble f tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateDouble f ctrl)
    -- a = foo .ctrl bar
    Control op tgt ctrl -> do
      validateDouble f tgt
        >> ifM (controlDouble f op ctrl) (pure ()) (throwError (ValLeaf rule (ControlFail ctrl)))
    -- a = x..y
    -- TODO it is unclear if this should mix floating point types too
    Range low high bound ->
      ((,) <$> getRule low <*> getRule high)
        >>= check (ValLeaf rule) . \case
          (Literal (Value (VFloat16 (float2Double -> n)) _), Literal (Value (VFloat16 (float2Double -> m)) _)) -> n <= f && range bound f m
          (Literal (Value (VFloat32 (float2Double -> n)) _), Literal (Value (VFloat32 (float2Double -> m)) _)) -> n <= f && range bound f m
          (Literal (Value (VFloat64 n) _), Literal (Value (VFloat64 m) _)) -> n <= f && range bound f m
    _ -> throwError (ValLeaf rule UnapplicableRule)
  pure (ValLeaf rule ())

-- | Controls for `Float64`
controlDouble :: MonadReader CDDL m => Double -> CtlOp -> Rule -> m Bool
controlDouble f Eq ctrl =
  getRule ctrl <&> \case
    Literal (Value (VFloat16 f') _) -> f == float2Double f'
    Literal (Value (VFloat32 f') _) -> f == float2Double f'
    Literal (Value (VFloat64 f') _) -> f == f'
controlDouble f Ne ctrl =
  getRule ctrl <&> \case
    Literal (Value (VFloat16 f') _) -> f /= float2Double f'
    Literal (Value (VFloat32 f') _) -> f /= float2Double f'
    Literal (Value (VFloat64 f') _) -> f /= f'

--------------------------------------------------------------------------------
-- Bool

-- | Validating a boolean
validateBool ::
  (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) =>
  Bool -> Rule -> m (ValidationTree ())
validateBool b rule = do
  getRule rule >>= \case
    -- a = any
    Postlude PTAny -> pure ()
    -- a = bool
    Postlude PTBool -> pure ()
    -- a = true
    Literal (Value (VBool b') _) -> check (ValLeaf rule) $ b == b'
    -- a = foo .and bar
    Control And tgt ctrl ->
      validateBool b tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateBool b ctrl)
    -- a = foo .within bar
    Control Within tgt ctrl ->
      validateBool b tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateBool b ctrl)
    -- a = foo .ctrl bar
    Control op tgt ctrl ->
      validateBool b tgt
        >> ifM (controlBool b op ctrl) (pure ()) (throwError (ValLeaf rule (ControlFail ctrl)))
    -- a = foo / bar
    Choice opts -> void $ validateChoice (validateBool b) rule opts
    _ -> throwError (ValLeaf rule UnapplicableRule)
  pure (ValLeaf rule ())

-- | Controls for `Bool`
controlBool :: MonadReader CDDL m => Bool -> CtlOp -> Rule -> m Bool
controlBool b Eq ctrl =
  getRule ctrl <&> \case
    Literal (Value (VBool b') _) -> b == b'
controlBool b Ne ctrl =
  getRule ctrl <&> \case
    Literal (Value (VBool b') _) -> b /= b'

--------------------------------------------------------------------------------
-- Simple

-- | Validating a `TSimple`. It is unclear if this is used for anything else than undefined.
validateSimple ::
  (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) =>
  Word8 -> Rule -> m (ValidationTree ())
validateSimple 23 rule = do
  getRule rule >>= \case
    -- a = any
    Postlude PTAny -> pure ()
    -- a = undefined
    Postlude PTUndefined -> pure ()
    -- a = foo / bar
    Choice opts -> void $ validateChoice (validateSimple 23) rule opts
    _ -> throwError (ValLeaf rule UnapplicableRule)
  pure (ValLeaf rule ())
validateSimple n _ = error $ "Found simple different to 23! please report this somewhere! Found: " <> show n

--------------------------------------------------------------------------------
-- Null/nil

-- | Validating nil
validateNull ::
  (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) => Rule -> m (ValidationTree ())
validateNull rule = do
  getRule rule >>= \case
    -- a = any
    Postlude PTAny -> pure ()
    -- a = nil
    Postlude PTNil -> pure ()
    _ -> throwError (ValLeaf rule UnapplicableRule)
  pure (ValLeaf rule ())

--------------------------------------------------------------------------------
-- Bytes

-- | Validating a byte sequence
validateBytes ::
  (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) =>
  BS.ByteString -> Rule -> m (ValidationTree ())
validateBytes bs rule = do
  getRule rule >>= \case
    -- a = any
    Postlude PTAny -> pure ()
    -- a = bytes
    Postlude PTBytes -> pure ()
    -- a = h'123456'
    Literal (Value (VBytes bs') _) -> check (ValLeaf rule) $ bs == bs'
    -- a = foo .and bar
    Control And tgt ctrl ->
      validateBytes bs tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateBytes bs ctrl)
    -- a = foo .within bar
    Control Within tgt ctrl ->
      validateBytes bs tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateBytes bs ctrl)
    -- a = foo .ctrl bar
    Control op tgt ctrl ->
      validateBytes bs tgt
        >> ifM (controlBytes bs op ctrl) (pure ()) (throwError (ValLeaf rule (ControlFail ctrl)))
    -- a = foo / bar
    Choice opts -> void $ validateChoice (validateBytes bs) rule opts
    _ -> throwError (ValLeaf rule UnapplicableRule)
  pure (ValLeaf rule ())

-- | Controls for byte strings
controlBytes :: forall m. MonadReader CDDL m => BS.ByteString -> CtlOp -> Rule -> m Bool
controlBytes bs Size ctrl =
  getRule ctrl >>= \case
    Literal (Value (VUInt (fromIntegral -> sz)) _) -> pure $ BS.length bs == sz
    Range low high bound ->
      let i = BS.length bs
       in ((,) <$> getRule low <*> getRule high) <&> \case
            (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= i && range bound i m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> -n <= i && range bound i m
            (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> -n <= i && range bound i (-m)
controlBytes bs Bits ctrl = do
  indices <-
    getRule ctrl >>= \case
      Literal (Value (VUInt i') _) -> pure [i']
      Choice nodes -> getIndicesOfChoice nodes
      Range ff tt incl -> getIndicesOfRange ff tt incl
      Enum g -> getIndicesOfEnum g
  bitsControlCheck (map fromIntegral indices)
  where
    bitsControlCheck :: [Int] -> m Bool
    bitsControlCheck allowedBits =
      let allowedSet = IS.fromList allowedBits
          totalBits = BS.length bs * 8
          isAllowedBit n =
            let byteIndex = n `shiftR` 3
                bitIndex = n .&. 7
             in case BS.indexMaybe bs byteIndex of
                  Just byte -> if not (testBit byte bitIndex) || IS.member n allowedSet then pure True else pure False
                  Nothing -> pure True
       in and <$> mapM isAllowedBit [0 .. totalBits - 1]
controlBytes bs Cbor ctrl =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict bs) of
    Right (BSL.null -> True, term) ->
      isRight <$> runExceptT (validateTerm term ctrl)
controlBytes bs Cborseq ctrl =
  case deserialiseFromBytes decodeTerm (BSL.fromStrict (BS.snoc (BS.cons 0x9f bs) 0xff)) of
    Right (BSL.null -> True, TList terms) -> isRight <$> runExceptT (validateList terms ctrl)

--------------------------------------------------------------------------------
-- Text

-- | Validating text strings
validateText ::
  (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) =>
  T.Text -> Rule -> m (ValidationTree ())
validateText txt rule = do
  getRule rule >>= \case
    -- a = any
    Postlude PTAny -> pure ()
    -- a = text
    Postlude PTText -> pure ()
    -- a = "foo"
    Literal (Value (VText txt') _) -> check (ValLeaf rule) $ txt == txt'
    -- a = foo .and bar
    Control And tgt ctrl ->
      validateText txt tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateText txt ctrl)
    -- a = foo .within bar
    Control Within tgt ctrl ->
      validateText txt tgt >> void (withError (fmap (const $ ControlFail ctrl)) $ validateText txt ctrl)
    -- a = foo .ctrl bar
    Control op tgt ctrl ->
      validateText txt tgt
        >> ifM (controlText txt op ctrl) (pure ()) (throwError (ValLeaf rule (ControlFail ctrl)))
    -- a = foo / bar
    Choice opts -> void $ validateChoice (validateText txt) rule opts
    _ -> throwError (ValLeaf rule UnapplicableRule)
  pure (ValLeaf rule ())

-- | Controls for text strings
controlText :: MonadReader CDDL m => T.Text -> CtlOp -> Rule -> m Bool
controlText bs Size ctrl =
  getRule ctrl >>= \case
    Literal (Value (VUInt (fromIntegral -> sz)) _) -> pure $ T.length bs == sz
    Range ff tt bound ->
      ((,) <$> getRule ff <*> getRule tt) <&> \case
        (Literal (Value (VUInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> n <= T.length bs && range bound (T.length bs) m
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VUInt (fromIntegral -> m)) _)) -> -n <= T.length bs && range bound (T.length bs) m
        (Literal (Value (VNInt (fromIntegral -> n)) _), Literal (Value (VNInt (fromIntegral -> m)) _)) -> -n <= T.length bs && range bound (T.length bs) (-m)
controlText s Regexp ctrl =
  getRule ctrl <&> \case
    Literal (Value (VText rxp) _) -> case s =~ rxp :: (T.Text, T.Text, T.Text) of
      ("", s', "") -> s == s'

--------------------------------------------------------------------------------
-- Tagged values

-- | Validating a `TTagged`
validateTagged ::
  (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) =>
  Word64 -> Term -> Rule -> m (Term -> ValidationTree Term)
validateTagged tag term rule = do
  getRule rule >>= \case
    Postlude PTAny -> pure (ValLeaf rule)
    Tag tag' rule' ->
      -- If the tag does not match, this is a direct fail
      if tag == tag'
        then
          ask >>= \cddl ->
            case runReader (runExceptT (validateTerm term rule')) cddl of
              Left vt -> throwError (ValContainer rule [] [fmap snd vt])
              Right vt -> pure (const $ ValContainer rule [] [vt])
        else throwError (ValLeaf rule DirectFail)
    Choice opts -> validateChoice (validateTagged tag term) rule opts
    _ -> throwError (ValLeaf rule UnapplicableRule)

--------------------------------------------------------------------------------
-- Collection helpers

-- | Groups might contain enums, or unwraps inside. This resolves all those to
-- the top level of the group.
flattenGroup :: CDDL -> [Rule] -> [Rule]
flattenGroup cddl nodes =
  mconcat
    [ case resolveIfRef cddl rule of
        Literal{} -> [rule]
        Postlude {} -> [rule]
        Map {} -> [rule]
        Array {} -> [rule]
        Choice {} -> [rule]
        KV {} -> [rule]
        Occur {} -> [rule]
        Range {} -> [rule]
        Control {} -> [rule]
        Enum e -> case resolveIfRef cddl e of
          Group g -> flattenGroup cddl g
          _ -> error "Malformed cddl"
        Unwrap g -> case resolveIfRef cddl g of
          Map n -> flattenGroup cddl n
          Array n -> flattenGroup cddl n
          Tag _ n -> [n]
          _ -> error "Malformed cddl"
        Tag {} -> [rule]
        Group g -> flattenGroup cddl g
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
  | remainingLen <= 0 = pure []
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
  | maxLen <= 0 = pure []
expandRule maxLen rule =
  getRule rule >>= \case
    Occur o OIOptional -> pure $ ([] :) $ if maxLen > 0 then [[o]] else []
    Occur o OIZeroOrMore -> ([] :) <$> expandRule maxLen (MIt (Occur o OIOneOrMore))
    Occur o OIOneOrMore ->
      if maxLen > 0
        then ([o] :) . (map (o :)) <$> expandRule (maxLen - 1) (MIt (Occur o OIOneOrMore))
        else pure []
    Occur o (OIBounded low high) -> case (low, high) of
      (Nothing, Nothing) -> expandRule maxLen (MIt (Occur o OIZeroOrMore))
      (Just (fromIntegral -> low'), Nothing) ->
        if maxLen >= low'
          then map (replicate low' o ++) <$> expandRule (maxLen - low') (MIt (Occur o OIZeroOrMore))
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
  getRule rule
    >>= pure . \case
      Occur _ OIOptional -> True
      Occur _ OIZeroOrMore -> True
      Occur _ (OIBounded Nothing _) -> True
      Occur _ (OIBounded (Just 0) _) -> True
      _ -> False

--------------------------------------------------------------------------------
-- Lists

validateListWithExpandedRules ::
  forall m. (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) =>
  [Term] -> Rule -> m (Term -> ValidationTree Term)
validateListWithExpandedRules terms rule =
  getRule rule >>= \case
    Array rules -> go terms rules
  where
    go ::
      [Term] -> [Rule] -> m (Term -> ValidationTree Term)
    go [] rs =
      -- This should never be called because we had as many rules as terms
      ifM
        (and <$> mapM isOptional rs)
        (pure (ValLeaf rule))
        (throwError (ValLeaf rule RemainingRules))
    go _ [] = throwError (ValLeaf rule RemainingItems)
    go (t : ts) (r : rs) =
      getRule r >>= \case
        -- Should the rule be a KV, then we validate the rule for the value
        KV _ v _ ->
          -- We need to do this juggling because validateTerm has a different
          -- error type
          ask >>= \cddl ->
            case runReader (runExceptT (validateTerm t v)) cddl of
              Left vt -> throwError (ValContainer rule [] [fmap snd vt])
              Right vt -> withError (f vt) $ go ts rs
        _ ->
          ask >>= \cddl ->
            case runReader (runExceptT (validateTerm t r)) cddl of
              Left vt -> throwError (ValContainer rule [] [fmap snd vt])
              Right vt -> withError (f vt) $ go ts rs
    f vt (ValLeaf r g) = ValContainer rule [vt] [ValLeaf r g]
    f vt (ValContainer r vv ii) = ValContainer r (vv ++ [vt]) ii

validateList ::
  (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) =>
  [Term] -> Rule -> m (Term -> ValidationTree Term)
validateList terms rule =
  getRule rule >>= \case
    Postlude PTAny -> pure (ValLeaf rule)
    Array rules ->
      ask >>= \cddl ->
        let sequencesOfRules = map (MIt . Array)
              $ runReader (expandRules (length terms) $ flattenGroup cddl rules) cddl
         in validateChoice (validateListWithExpandedRules terms) rule sequencesOfRules
    Choice opts -> validateChoice (validateList terms) rule opts
    _ -> throwError (ValLeaf rule UnapplicableRule)

--------------------------------------------------------------------------------
-- Maps

validateMapWithExpandedRules ::
  forall m. (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) =>
  [(Term, Term)] -> Rule -> m (Term -> ValidationTree Term)
validateMapWithExpandedRules terms rule =
  getRule rule >>= \case
    Map rules -> go terms rules
  where
    go ::
      [(Term, Term)] ->
      [Rule] ->
      m (Term -> ValidationTree Term)
    go [] rs =
      -- This should never be called because we have as many rules as pairs
      ifM
        (and <$> mapM isOptional rs)
        (pure (ValLeaf rule))
        (throwError (ValLeaf rule DirectFail))
    go _ [] = throwError (ValLeaf rule DirectFail)
    go ((tk, tv) : ts) rs = do
      rs' <- go' tk tv [] rs
      go ts rs'

    -- For each pair of terms, try to find some rule that can be applied here,
    -- and returns the others if there is a succesful match.
    go' :: Term -> Term -> [(Rule, Either (ValidationTree (Term, CDDLFail)) (ValidationTree Term, ValidationTree (Term, CDDLFail)))] -> [Rule] -> m [Rule]
    go' tk tv prevFail [] = throwError (ValLeaf rule $ NoApplicableRuleFound (tk, tv) prevFail)
    go' tk tv prevFail (r:rs) =
      getRule r >>= \case
        KV k v _ -> ask >>= \cddl ->
          case runReader (runExceptT (validateTerm tk k)) cddl of
            Left vt -> go' tk tv (prevFail ++ [(r, Left vt)]) rs
            Right kvt -> case runReader (runExceptT (validateTerm tv v)) cddl of
              Left vt -> go' tk tv (prevFail ++ [(r, Right (kvt, vt))]) rs
              Right _ -> pure (map fst prevFail ++ rs)

validateMap ::
  (MonadError (ValidationTree CDDLFail) m, MonadReader CDDL m) =>
  [(Term, Term)] -> Rule -> m (Term -> ValidationTree Term)
validateMap terms rule =
  getRule rule >>= \case
    Postlude PTAny -> pure (ValLeaf rule)
    Map rules ->
      ask >>= \cddl ->
        let sequencesOfRules = map (MIt . Map)
              $ runReader (expandRules (length terms) $ flattenGroup cddl rules) cddl
         in validateChoice (validateMapWithExpandedRules terms) rule sequencesOfRules
    Choice opts -> validateChoice (validateMap terms) rule opts
    _ -> throwError (ValLeaf rule UnapplicableRule)


--------------------------------------------------------------------------------
-- Bits control

getIndicesOfChoice :: MonadReader CDDL m => NE.NonEmpty Rule -> m [Word64]
getIndicesOfChoice nodes =
  mconcat
    . NE.toList
    <$> mapM
      ( \x ->
          getRule x >>= \case
            Literal (Value (VUInt v) _) -> pure [fromIntegral v]
            KV _ v _ ->
              getRule v >>= \case
                Literal (Value (VUInt v') _) -> pure [fromIntegral v']
                somethingElse -> error $ "Malformed value in KV in choice in .bits: " <> show somethingElse
            Range ff tt incl -> getIndicesOfRange ff tt incl
            Enum g -> getIndicesOfEnum g
            somethingElse -> error $ "Malformed alternative in choice in .bits: " <> show somethingElse
      )
      nodes

getIndicesOfRange :: MonadReader CDDL m => Rule -> Rule -> RangeBound -> m [Word64]
getIndicesOfRange ff tt incl =
  ((,) <$> getRule ff <*> getRule tt) >>= \case
    (Literal (Value (VUInt ff') _), Literal (Value (VUInt tt') _)) ->
      pure $
        [ff' .. tt'] & case incl of
          ClOpen -> init
          Closed -> id
    somethingElse -> error $ "Malformed range in .bits: " <> show somethingElse

getIndicesOfEnum :: MonadReader CDDL m => Rule -> m [Word64]
getIndicesOfEnum g =
  getRule g >>= \case
    Group g' -> getIndicesOfChoice (fromJust $ NE.nonEmpty g')
    somethingElse -> error $ "Malformed enum in .bits: " <> show somethingElse
