{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.CBOR.Cuddle.CBOR.Validate (toAnnTerm, validateAnnTerm) where

import Capability.Reader
import Capability.Source
import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.CTree (CTree, CTreeRoot')
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.Postlude (PTerm (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoRef)
import Codec.CBOR.Term (Term (..))
import Control.Monad.Reader (Reader)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Functor.Identity (Identity (Identity))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Slightly modified term tree

-- We add a new term tree for the following reasons:
-- - Making the recursive structure clearer
-- - Allowing annotation with the corresponding CDDL
--------------------------------------------------------------------------------

-- | Primitive term, that is, one which does not have any subterms
data PrimTerm
  = PInt !Int
  | PInteger !Integer
  | PBytes !ByteString
  | PString !Text
  | PBool !Bool
  | PSimple !Word8
  | PHalf !Float
  | PFloat !Float
  | PDouble !Double
  | PNull

newtype ListTerm f = ListTerm (f [AnnTerm f])

newtype MapTerm f = MapTerm (f [(AnnTerm f, AnnTerm f)])

data AnnTerm f where
  Prim :: PrimTerm -> AnnTerm f
  List :: ListTerm f -> AnnTerm f
  Map :: MapTerm f -> AnnTerm f
  Tagged :: Word64 -> f (AnnTerm f) -> AnnTerm f

toAnnTerm :: Term -> AnnTerm Identity
toAnnTerm (TInt i) = Prim $ PInt i
toAnnTerm (TInteger i) = Prim $ PInteger i
toAnnTerm (TBytes b) = Prim $ PBytes b
toAnnTerm (TBytesI b) = Prim $ PBytes (BL.toStrict b)
toAnnTerm (TString b) = Prim $ PString b
toAnnTerm (TStringI b) = Prim $ PString (TL.toStrict b)
toAnnTerm (TList l) = List . ListTerm . Identity $ fmap toAnnTerm l
toAnnTerm (TListI l) = List . ListTerm . Identity $ fmap toAnnTerm l
toAnnTerm (TMap m) = Map . MapTerm . Identity $ fmap (bimap toAnnTerm toAnnTerm) m
toAnnTerm (TMapI m) = Map . MapTerm . Identity $ fmap (bimap toAnnTerm toAnnTerm) m
toAnnTerm (TTagged w t) = Tagged w . Identity $ toAnnTerm t
toAnnTerm (TBool b) = Prim $ PBool b
toAnnTerm TNull = Prim PNull
toAnnTerm (TSimple w) = Prim $ PSimple w
toAnnTerm (THalf f) = Prim $ PHalf f
toAnnTerm (TFloat f) = Prim $ PFloat f
toAnnTerm (TDouble f) = Prim $ PDouble f

--------------------------------------------------------------------------------
-- Validating the tree

-- Need to validate along the Term tree
--------------------------------------------------------------------------------

validatePrimTerm :: PrimTerm -> CTree f -> Bool
validatePrimTerm pt c = case c of
  CTree.Literal v -> case (pt, v) of
    (termInteger -> n, valueInteger -> m) | n == m -> True
    (PHalf m, VFloat16 n) | n == m -> True
    (PFloat m, VFloat32 n) | n == m -> True
    (PDouble m, VFloat64 n) | n == m -> True
    (PString s, VText r) | r == s -> True
    (PBytes s, VBytes r) | r == s -> True
    _ -> False
  CTree.Postlude v -> case (v, pt) of
    (PTBool, PBool _) -> True
    (PTUInt, PInt _) -> True
    (PTUInt, PInteger _) -> True
    (PTNInt, PInt n) | n < 0 -> True
    (PTNInt, PInteger n) | n < 0 -> True
    (PTInt, PInt _) -> True
    (PTInt, PInteger _) -> True
    (PTHalf, PHalf _) -> True
    (PTFloat, PFloat _) -> True
    (PTDouble, PDouble _) -> True
    (PTBytes, PBytes _) -> True
    (PTText, PString _) -> True
    (PTAny, _) -> True
    (PTNil, _) -> False
    _ -> False
  _ -> False
  where
    -- Interpret a term as an integer type for comparison
    termInteger (PInt n) = Just $ fromIntegral n
    termInteger (PInteger n) = Just n
    termInteger _ = Nothing
    -- Interpret a value as an integer for comparison
    valueInteger (VUInt n) = Just $ fromIntegral n
    valueInteger (VNInt n) = Just $ fromIntegral n
    valueInteger (VBignum n) = Just n
    valueInteger _ = Nothing

newtype ValidateEnv = ValidateEnv
  { cddl :: CTreeRoot' Identity MonoRef
  }
  deriving (Generic)

newtype ValidateM a = ValidateM {runValidateM :: Reader ValidateEnv a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasSource "cddl" (CTreeRoot' Identity MonoRef),
      HasReader "cddl" (CTreeRoot' Identity MonoRef)
    )
    via Field "cddl" () (MonadReader (Reader ValidateEnv))

data ValidationFailure
  = PrimTypeValidationFailed
  | MissingRequiredEntry (CTree MonoRef)
  | MultipleFailures [ValidationFailure]

instance Semigroup ValidationFailure where
  (MultipleFailures xs) <> (MultipleFailures ys) = MultipleFailures (xs <> ys)
  (MultipleFailures xs) <> y = MultipleFailures (y : xs)
  x <> (MultipleFailures ys) = MultipleFailures (x : ys)
  x <> y = MultipleFailures [x, y]

data ValidationResult
  = -- | The term validates against the given CDDL item, and need not be validated
    -- further.
    ValidatesConsumes
  | -- | The term validates against the given CDDL item, but only because the
    --   given CDDL production is optional. The term still needs to be validated
    --   against a matching term.
    ValidatesSkips
  | -- | The term fails validation against the given CDDL item, which must be matched.
    ValidatesFails

-- | Annotate an 'AnnTree' with the result of validation.
data ValidatedWith ct a
  = Valid a ct
  | -- | The node fails to validate against the given CDDL item(s), which must be
    --   matched.
    Invalid a [ct] ValidationFailure
  | -- | Indicates that the subtree was not validated, probably because a
    --   higher-level node did not validate.
    Unvalidated a

-- | Given a functor-parametrised algebra 'fp' and a thing to validate it
-- against, construct the 'ValidatedWith' that closes that algebra over the
-- 'ValidatedWith' functor.
type VWith fp ct = ValidatedWith ct (fp (ValidatedWith ct))

-- | Validated term
type VTerm = VWith AnnTerm (CTree MonoRef)

validateList ::
  ListTerm Identity ->
  [CTree MonoRef] ->
  -- We return a ValidatedWith, since there can be failures related to the
  -- entire list, rather than individual terms.
  ValidateM (VWith ListTerm [CTree MonoRef])
validateList lt@(ListTerm terms) againstTerms = case go terms againstTerms of
  Right acc -> pure $ Valid againstTerms acc
  Left (acc, cts) ->
    validateEmptyList cts >>= \case
      Right _ -> pure $ Valid lt acc
      Left vf -> pure $ Invalid againstTerms acc vf
  where
    go (t : xs) cts acc = case goTerm t cts of
      Right (vt, cts') -> go xs cts' (vt : acc)
      Left ivt -> go xs cts (ivt : acc)
    go [] [] acc = Right acc
    go [] cts acc = Left (acc, cts)
    -- Attempt to validate a list entry against a configuration tree
    validateListEntry :: AnnTerm Identity -> CTree MonoRef -> ValidateM ValidationResult
    validateListEntry at ct = undefined

-- | Validate that an empty list is allowable given the remaining entries which
-- should form part of the list.
validateEmptyList :: [CTree MonoRef] -> ValidateM (Either ValidationFailure ())
validateEmptyList [] = pure $ Right ()
validateEmptyList xs = do
  mfs <- traverse validateMissingEntry xs
  case catMaybes mfs of
    [] -> pure $ Right ()
    errs -> pure . Left $ MultipleFailures errs
  where
    -- Check that it is allowed to miss an entry. If not, construct the
    -- validation failure
    validateMissingEntry :: CTree MonoRef -> ValidateM (Maybe ValidationFailure)
    validateMissingEntry ct@(CTree.Occur _ oi) =
      if oiLowerBound oi == 0
        then pure Nothing
        else pure $ Just (MissingRequiredEntry ct)
    validateMissingEntry ct = error $ "Invalid entry in CTree list: " <> show ct

validateAnnTerm :: AnnTerm Identity -> CTree MonoRef -> ValidateM VTerm
validateAnnTerm at@(Prim pt) ct =
  if validatePrimTerm pt ct
    then pure $ Valid ct at
    else pure $ Invalid ct at PrimTypeValidationFailed
