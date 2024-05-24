{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.CBOR.Cuddle.CBOR.Validate
  ( toAnnTerm,
    validateAnnTerm,
    validateRule,
    AnnTerm (..),
    PrimTerm (..),
    ValidatedWith (..),
    AnnotationContext (..),
  )
where

import Capability.Reader
import Capability.Sink (HasSink)
import Capability.Source
import Capability.State (HasState, get, gets, put)
import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL qualified as CDDL
import Codec.CBOR.Cuddle.CDDL.CTree (CTree, CTreeRoot')
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.Postlude (PTerm (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (MonoRef (..))
import Codec.CBOR.Term (Term (..))
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State (StateT, evalStateT)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Default.Class (Default (def))
import Data.Functor.Identity (Identity (Identity))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
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
  deriving (Eq, Show)

newtype ListTerm f = ListTerm (f [AnnTerm f])

deriving instance Eq (ListTerm Identity)

deriving instance Show (ListTerm Identity)

deriving instance Eq (ListTerm ValidatedWith)

deriving instance Show (ListTerm ValidatedWith)

newtype MapTerm f = MapTerm (f [(AnnTerm f, AnnTerm f)])

deriving instance Eq (MapTerm Identity)

deriving instance Show (MapTerm Identity)

deriving instance Eq (MapTerm ValidatedWith)

deriving instance Show (MapTerm ValidatedWith)

data AnnTerm f where
  Prim :: PrimTerm -> AnnTerm f
  List :: ListTerm f -> AnnTerm f
  Map :: MapTerm f -> AnnTerm f
  Tagged :: Word64 -> AnnTerm f -> AnnTerm f

deriving instance Eq (AnnTerm Identity)

deriving instance Show (AnnTerm Identity)

deriving instance Eq (AnnTerm ValidatedWith)

deriving instance Show (AnnTerm ValidatedWith)

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
toAnnTerm (TTagged w t) = Tagged w $ toAnnTerm t
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

--------------------------------------------------------------------------------

-- * Validating composite terms

-- We always validate a single CBOR term against a single CDDL term. There can
-- be a few potential outcomes:
-- - The CBOR validates against the term, and
--   - Consumes the term (e.g. CDDL requires an int and we find one)
--   - Modifies the term (e.g. we have matched the first item in a list)
-- - The CBOR fails to validate against the term
--
-- In order to maintain the idea of a single CBOR term against a single CDDL
-- term, we maintain a lens from the top-level term. We will also need to do
-- thing like lazily inlining references, since we need to manipulate the
-- reference tree. Sometimes it is not clear where we need to make
-- modifications. For example, if we are parsing in a group, we need to remove
-- that entry from the group. If we then complete the group, we need to
-- decrement the counter on the occurrence for the group.
--
-- As such, the valdidator maintains the following:
-- - In the state monad, the full validation tree
-- - A stack representing the validation state machine. This is inverted from
--   the regular tree representation. We validate against the top of the stack
--   and mutate the new top whenever we pop an item.
--
-- When pushing elements onto the stack, we remove them from the higher stack
-- elements - that is, when validating against a list with two elements, our
-- stack looks like:
--
-- ```
-- Array [a,b]     Array [b]
--                 a
-- ```
--
-- Depending on the result of the validation, we may end up pushing the element
-- back into the higher elements, for example if validation fails for a map.
--
-- As we walk through, we need to keep track of where to proceed to in the case
-- of a failed validation.
--------------------------------------------------------------------------------

newtype ValidateEnv = ValidateEnv
  { cddl :: CTreeRoot' Identity MonoRef
  }
  deriving (Generic)

-- | This will be used to annotate the CBOR tree on a successful validation
data AnnotationContext = AnnotationContext
  { -- | Name of the CDDL rule we are currently validating.
    ruleName :: T.Text,
    -- | Name of the surrounding CDDL group, should we be processing such a
    --   thing. We record this separately from the "rule" name since a group
    -- never corresponds to a specific CBOR term, so we should display it
    -- differently.
    groupName :: T.Text,
    -- | Type of the current thing being processed.
    ruleType :: T.Text
  }
  deriving (Eq, Generic, Show)

instance Default AnnotationContext where
  def = AnnotationContext T.empty T.empty T.empty

data ValidateState = ValidateState
  { validationStack :: NE.NonEmpty (CTree MonoRef),
    -- -- | Container context. Note that this is mostly a cached version of
    -- -- information present in the stack, reflecting the topmost array or map in
    -- -- the stack.
    -- cntrCtx :: ContainerContext,

    -- | Annotation context. Again, this is summarised data from the validation
    -- stack.
    annCtx :: AnnotationContext
  }
  deriving (Generic)

newtype ValidateM a = ValidateM
  { runValidateM :: StateT ValidateState (Reader ValidateEnv) a
  }
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasSource "cddl" (CTreeRoot' Identity MonoRef),
      HasReader "cddl" (CTreeRoot' Identity MonoRef)
    )
    via Field
          "cddl"
          ()
          ( Lift
              ( StateT
                  ValidateState
                  (MonadReader (Reader ValidateEnv))
              )
          )
  deriving
    ( HasSource "validationStack" (NE.NonEmpty (CTree MonoRef)),
      HasSink "validationStack" (NE.NonEmpty (CTree MonoRef)),
      HasState "validationStack" (NE.NonEmpty (CTree MonoRef))
    )
    via Field
          "validationStack"
          ()
          ( MonadState
              (StateT ValidateState (Reader ValidateEnv))
          )
  deriving
    ( HasSource "annCtx" AnnotationContext,
      HasSink "annCtx" AnnotationContext,
      HasState "annCtx" AnnotationContext
    )
    via Field
          "annCtx"
          ()
          ( MonadState
              (StateT ValidateState (Reader ValidateEnv))
          )

data ValidationFailure
  = PrimTypeValidationFailed
  | MissingRequiredEntry
  | MultipleFailures [ValidationFailure]
  deriving (Eq, Show)

instance Semigroup ValidationFailure where
  (MultipleFailures xs) <> (MultipleFailures ys) = MultipleFailures (xs <> ys)
  (MultipleFailures xs) <> y = MultipleFailures (y : xs)
  x <> (MultipleFailures ys) = MultipleFailures (x : ys)
  x <> y = MultipleFailures [x, y]

-- | Annotate an 'AnnTree' with the result of validation.
data ValidatedWith a
  = Valid a AnnotationContext
  | -- | The node fails to validate against the given CDDL item(s), which must be
    --   matched.
    Invalid a ValidationFailure
  | -- | Indicates that the subtree was not validated, probably because a
    --   higher-level node did not validate.
    Unvalidated a
  deriving (Eq, Show)

type VTerm = ValidatedWith (AnnTerm ValidatedWith)

-- | Mark a subtree as being unvalidated, due to a higher-level match failing
unvalidated :: AnnTerm Identity -> AnnTerm ValidatedWith
unvalidated (Prim p) = Prim p
unvalidated (List (ListTerm (Identity xs))) =
  List . ListTerm . Unvalidated $ unvalidated <$> xs
unvalidated (Map (MapTerm (Identity xs))) =
  Map . MapTerm . Unvalidated $ bimap unvalidated unvalidated <$> xs
unvalidated (Tagged x p) = Tagged x $ unvalidated p

validateRule ::
  AnnTerm Identity ->
  CTreeRoot' Identity MonoRef ->
  CDDL.Name ->
  VTerm
validateRule at ctr@(CTree.CTreeRoot m) n@(Name name) = case M.lookup n m of
  Just (Identity (MIt r)) ->
    let vs =
          ValidateState
            { validationStack = NE.singleton r,
              annCtx = def {ruleName = name}
            }
        a1 = runValidateM $ validateAnnTerm at
        a2 = evalStateT a1 vs
        a3 = runReader a2 (ValidateEnv ctr)
     in a3
  Just _ -> error "Top-level rule defined by indirection!"
  Nothing -> error $ "Rule not defined: " <> show n

validateAnnTerm :: AnnTerm Identity -> ValidateM VTerm
validateAnnTerm at = do
  ct <- gets @"validationStack" NE.head
  case at of
    Prim pt ->
      if validatePrimTerm pt ct
        then succeedMatch $ Prim pt
        else failMatch at PrimTypeValidationFailed

succeedMatch ::
  AnnTerm ValidatedWith ->
  ValidateM VTerm
succeedMatch at = do
  annCtx <- get @"annCtx"
  popStack True
  pure $ Valid at annCtx

-- | Mark that validation failed with the given reason
failMatch ::
  AnnTerm Identity ->
  ValidationFailure ->
  ValidateM VTerm
failMatch at failReason = do
  popStack False
  pure $ Invalid uat failReason
  where
    uat = unvalidated at

-- | Pop an item off of the top of the parsing stack.
--
--   We call this in two situations:
--   - When a primitive parse (one that either succeeds or fails) completes
--   - When a composite parser has exhausted all possibility (e.g. all choices
--     exhausted, or occurrence bounds exceeded)
--
--   It takes as an argument the resulting status of validating the subparser.
--   This is needed to make choices about what to do with the next parser.
--
--   This function is responsible for making any adjustments to the next element
--   in the stack consumnate with the element being popped. This can involve
--   recursive calls to this function.
--
--   This function terminates when the head of the stack contains the next
--   parser to be considered in validation.
popStack :: Bool -> ValidateM ()
popStack vres = do
  (oldHead, mNewHead) <- gets @"validationStack" NE.uncons
  case mNewHead of
    Nothing ->
      -- This is the final rule in the validation stack (e.g. the top-level
      -- thing we've been asked to validate against). If the parsing was
      -- successful, we are done provided there is no more data to validate, and
      -- the resulting status is the final status of the validation.
      return ()
    Just ns@(newHead NE.:| restStack) -> do
      -- Initially, drop the element of the stack. We might modify this later,
      -- but this is the basic case if we don't do anything else
      put @"validationStack" ns
      -- We have a higher level thing in the stack to validate
      case newHead of
        -- Array which has completed validation
        CTree.Array [] ->
          popStack vres
        -- Remaining elements in the array to parse
        CTree.Array xs ->
          put @"validationStack" $
            CTree.Array xs NE.:| restStack
        CTree.Choice (x NE.:| []) -> do
          x' <- resolveRef x
          -- Final option. We replace the choice constructor with the option
          -- directly
          put @"validationStack" $ x' NE.:| restStack
        CTree.Choice (_ NE.:| (x : xs)) ->
          put @"validationStack" $ CTree.Choice (x NE.:| xs) NE.:| restStack
        CTree.Group [] -> popStack vres
        CTree.Group xs ->
          put @"validationStack" $
            CTree.Group xs NE.:| restStack
        CTree.Occur x bounds ->
          -- If the subitem validates correctly, then we decrement the
          -- occurrence bounds of the surrounding term but leave the subterm
          -- on the validation stack. The fact that we're still here implies
          -- that this validation was within bounds.
          if vres
            then case decBounds bounds of
              -- We still have bounds on this item and should continue parsing it
              Just bounds' ->
                put @"validationStack" $
                  oldHead NE.:| CTree.Occur x bounds' : restStack
              Nothing ->
                -- This item has occurred as much as it can - drop it and the
                -- bounded item and proceed from the super item
                popStack vres
            else -- If the parser failed, that's OK provided the bounds allow for
            -- the element to be optional, so we drop the occur term and invert
            -- the result (returning a successful status). Otherwise, we reflect
            -- the failed state.

              if oiLowerBound bounds == 0
                then popStack (not vres)
                else popStack vres
        _ -> return ()

-- | Decrement the bounds on an occurrence indicator, since we have already
-- parsed one occurrence. If the  bounds are such that no more occurences are
-- allowed, we return 'Nothing'.
decBounds :: OccurrenceIndicator -> Maybe OccurrenceIndicator
decBounds OIOptional = Nothing
decBounds OIZeroOrMore = Just OIZeroOrMore
decBounds OIOneOrMore = Just OIZeroOrMore
decBounds (OIBounded mlb mub) = case mub of
  Just 1 -> Nothing
  _ ->
    let mlb' = fmap (\x -> min 0 (x - 1)) mlb
        mub' = fmap (\x -> min 1 (x - 1)) mub
     in Just $ OIBounded mlb' mub'

resolveRef :: CTree.Node MonoRef -> ValidateM (CTree MonoRef)
resolveRef (MIt it) = pure it
resolveRef (MRuleRef n) = do
  CTree.CTreeRoot cddl <- ask @"cddl"
  case M.lookup n cddl of
    Just (Identity node) -> resolveRef node
    Nothing -> error $ "Missing reference " <> show n
