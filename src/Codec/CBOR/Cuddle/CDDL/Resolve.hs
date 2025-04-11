{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Module containing tools for 'resolving' CDDL
--
-- Resolving the CDDL is a process of simplifying the representation to make
-- further operations, such as CBOR generation or validation, easier. We operate
-- with a number of passes:
--
-- 1. First, we deal with any rule extensions and create a single map from
--    identifiers to (potentially parametrised) entities.
-- 2. Second, we flatten the structure to a 'CTree', which discards a lot of the
--    extrenuous information.
-- 3. Then we resolve identifiers. Specifically, we do three things:
--    - Resolve identifiers that map to the postlude.
--    - Differentiate between generic args and references to top-level rules.
--    - Validate that all references exist. Note that we cannot resolve all
--    references since they may be circular.
-- 4. Finally, we monomorphise, synthesizing instances of rules with their
--    generic arguments bound.
module Codec.CBOR.Cuddle.CDDL.Resolve (
  buildResolvedCTree,
  monoCTree,
  buildRefCTree,
  asMap,
  buildMonoCTree,
  fullResolveCDDL,
  MonoRef (..),
  NameResolutionFailure (..),
)
where

import Capability.Accessors (Field (..), Lift (..))
import Capability.Error (HasThrow, MonadError (..), throw)
import Capability.Reader (HasReader, MonadReader (..), ask)
import Capability.Reader qualified as Reader (local)
import Capability.Sink (HasSink)
import Capability.Source (HasSource)
import Capability.State (HasState, MonadState (..), modify)
import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.CTree (
  CTree,
  CTreeRoot,
  CTreeRoot' (CTreeRoot),
  ParametrisedWith (..),
 )
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.Postlude (PTerm (..))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Reader (Reader, ReaderT (..), runReader)
import Control.Monad.State.Strict (StateT (..))
import Data.Foldable (foldl')
import Data.Functor.Identity (Identity (..))
import Data.Generics.Product
import Data.Generics.Sum
import Data.Hashable
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics.Core

--------------------------------------------------------------------------------
-- 1. Rule extensions
--------------------------------------------------------------------------------

type CDDLMap = Map.Map Name (Parametrised TypeOrGroup)

type Parametrised a = ParametrisedWith [Name] a

toParametrised :: a -> Maybe GenericParam -> Parametrised a
toParametrised a Nothing = Unparametrised a
toParametrised a (Just (GenericParam gps)) = Parametrised a (NE.toList gps)

parameters :: Parametrised a -> [Name]
parameters (Unparametrised _) = mempty
parameters (Parametrised _ ps) = ps

asMap :: CDDL -> CDDLMap
asMap (CDDL rules) = foldl' go Map.empty rules
  where
    go x (TopLevelComment _) = x
    go x (TopLevelRule _ r _) = assignOrExtend x r

    assignOrExtend :: CDDLMap -> Rule -> CDDLMap
    assignOrExtend m (Rule n gps assign tog) = case assign of
      -- Equals assignment
      AssignEq -> Map.insert n (toParametrised tog gps) m
      AssignExt -> Map.alter (extend tog gps) n m

    extend ::
      TypeOrGroup ->
      Maybe GenericParam ->
      Maybe (Parametrised TypeOrGroup) ->
      Maybe (Parametrised TypeOrGroup)
    extend tog _gps (Just existing) = case (underlying existing, tog) of
      (TOGType _, TOGType (Type0 new)) ->
        Just $
          existing
            & field @"underlying"
            % _Ctor @"TOGType"
            % _Ctor @"Type0"
            %~ (<> new)
      -- From the CDDL spec, I don't see how one is meant to extend a group.
      -- According to the description, it's meant to add a group choice, but the
      -- assignment to a group takes a 'GrpEntry', not a Group, and there is no
      -- ability to add a choice. For now, we simply ignore attempt at
      -- extension.
      (TOGGroup _, TOGGroup _new) -> Just existing
      (TOGType _, _) -> error "Attempting to extend a type with a group"
      (TOGGroup _, _) -> error "Attempting to extend a group with a type"
    extend tog gps Nothing = Just $ toParametrised tog gps

--------------------------------------------------------------------------------
-- 2. Conversion to CTree
--------------------------------------------------------------------------------

-- | Indicates that an item may be referenced rather than defined.
data OrRef a
  = -- | The item is inlined directly
    It a
  | -- | Reference to another node with possible generic arguments supplied
    Ref Name [CTree.Node OrRef]
  deriving (Show, Functor)

type RefCTree = CTreeRoot OrRef

deriving instance Show (CTree OrRef)

deriving instance Show (CTreeRoot OrRef)

-- | Build a CTree incorporating references.
--
-- This translation cannot fail.
buildRefCTree :: CDDLMap -> RefCTree
buildRefCTree rules = CTreeRoot $ fmap toCTreeRule rules
  where
    toCTreeRule ::
      Parametrised TypeOrGroup ->
      ParametrisedWith [Name] (CTree.Node OrRef)
    toCTreeRule = fmap toCTreeTOG

    toCTreeTOG :: TypeOrGroup -> CTree.Node OrRef
    toCTreeTOG (TOGType t0) = toCTreeT0 t0
    toCTreeTOG (TOGGroup ge) = toCTreeGroupEntry ge

    toCTreeT0 :: Type0 -> CTree.Node OrRef
    toCTreeT0 (Type0 (t1 NE.:| [])) = toCTreeT1 t1
    toCTreeT0 (Type0 xs) = It . CTree.Choice $ toCTreeT1 <$> xs

    toCTreeT1 :: Type1 -> CTree.Node OrRef
    toCTreeT1 (Type1 t2 Nothing) = toCTreeT2 t2
    toCTreeT1 (Type1 t2 (Just (op, t2'))) = case op of
      RangeOp bound ->
        It $
          CTree.Range
            { CTree.from = toCTreeT2 t2
            , CTree.to = toCTreeT2 t2'
            , CTree.inclusive = bound
            }
      CtrlOp ctlop ->
        It $
          CTree.Control
            { CTree.op = ctlop
            , CTree.target = toCTreeT2 t2
            , CTree.controller = toCTreeT2 t2'
            }

    toCTreeT2 :: Type2 -> CTree.Node OrRef
    toCTreeT2 (T2Value v) = It $ CTree.Literal v
    toCTreeT2 (T2Name n garg) =
      Ref n (fromGenArgs garg)
    toCTreeT2 (T2Group t0) =
      -- This behaviour seems questionable, but I don't really see how better to
      -- interpret the spec here.
      toCTreeT0 t0
    toCTreeT2 (T2Map g) = toCTreeMap g
    toCTreeT2 (T2Array g) = toCTreeArray g
    toCTreeT2 (T2Unwrapped n margs) =
      It . CTree.Unwrap $
        Ref n (fromGenArgs margs)
    toCTreeT2 (T2Enum g) = toCTreeEnum g
    toCTreeT2 (T2EnumRef n margs) = Ref n $ fromGenArgs margs
    toCTreeT2 (T2Tag Nothing t0) =
      -- Currently not validating tags
      toCTreeT0 t0
    toCTreeT2 (T2Tag (Just tag) t0) =
      It . CTree.Tag tag $ toCTreeT0 t0
    toCTreeTe (T2DataItem 7 (Just mmin) =
      toCTreeDataItem mmin
    toCTreeT2 (T2DataItem _maj _mmin) =
      -- We don't validate numerical items yet
      It $ CTree.Postlude PTAny
    toCTreeT2 T2Any = It $ CTree.Postlude PTAny

    toCTreeDataItem 20 =
      It . CTree.Literal $ VBool False
    toCTreeDataItem 21 =
      It . CTree.Literal $ VBool True
    toCTreeDataItem 25 =
      It $ CTree.Postlude PTHalf
    toCTreeDataItem 26 =
      It $ CTree.Postlude PTFloat
    toCTreeDataItem 27 =
      It $ CTree.Postlude PTDouble
    toCTreeDataItem 23 =
      It $ CTree.Postlude PTUndefined
    toCTreeDataItem _ =
      It $ CTree.Postlude PTAny

    toCTreeGroupEntryNC :: WithComments GroupEntry -> CTree.Node OrRef
    toCTreeGroupEntryNC = toCTreeGroupEntry . stripComment

    toCTreeGroupEntry :: GroupEntry -> CTree.Node OrRef
    toCTreeGroupEntry (GEType (Just occi) mmkey t0) =
      It $
        CTree.Occur
          { CTree.item = toKVPair mmkey t0
          , CTree.occurs = occi
          }
    toCTreeGroupEntry (GEType Nothing mmkey t0) = toKVPair mmkey t0
    toCTreeGroupEntry (GERef (Just occi) n margs) =
      It $
        CTree.Occur
          { CTree.item = Ref n (fromGenArgs margs)
          , CTree.occurs = occi
          }
    toCTreeGroupEntry (GERef Nothing n margs) = Ref n (fromGenArgs margs)
    toCTreeGroupEntry (GEGroup (Just occi) g) =
      It $
        CTree.Occur
          { CTree.item = groupToGroup g
          , CTree.occurs = occi
          }
    toCTreeGroupEntry (GEGroup Nothing g) = groupToGroup g

    fromGenArgs :: Maybe GenericArg -> [CTree.Node OrRef]
    fromGenArgs = maybe [] (\(GenericArg xs) -> NE.toList $ fmap toCTreeT1 xs)

    -- Interpret a group as an enumeration. Note that we float out the
    -- choice options
    toCTreeEnum :: Group -> CTree.Node OrRef
    toCTreeEnum (Group (a NE.:| [])) =
      It . CTree.Enum . It . CTree.Group $ fmap toCTreeGroupEntryNC a
    toCTreeEnum (Group xs) =
      It . CTree.Choice $
        fmap (It . CTree.Enum . It . CTree.Group . fmap toCTreeGroupEntryNC) xs

    -- Embed a group in another group, again floating out the choice options
    groupToGroup :: Group -> CTree.Node OrRef
    groupToGroup (Group (a NE.:| [])) =
      It . CTree.Group $ fmap toCTreeGroupEntryNC a
    groupToGroup (Group xs) =
      It . CTree.Choice $
        fmap (It . CTree.Group . fmap toCTreeGroupEntryNC) xs

    toKVPair :: Maybe MemberKey -> Type0 -> CTree.Node OrRef
    toKVPair Nothing t0 = toCTreeT0 t0
    toKVPair (Just mkey) t0 =
      It $
        CTree.KV
          { CTree.key = toCTreeMemberKey mkey
          , CTree.value = toCTreeT0 t0
          , -- TODO Handle cut semantics
            CTree.cut = False
          }

    -- Interpret a group as a map. Note that we float out the choice options
    toCTreeMap :: Group -> CTree.Node OrRef
    toCTreeMap (Group (a NE.:| [])) = It . CTree.Map $ fmap toCTreeGroupEntryNC a
    toCTreeMap (Group xs) =
      It
        . CTree.Choice
        $ fmap (It . CTree.Map . fmap toCTreeGroupEntryNC) xs

    -- Interpret a group as an array. Note that we float out the choice
    -- options
    toCTreeArray :: Group -> CTree.Node OrRef
    toCTreeArray (Group (a NE.:| [])) =
      It . CTree.Array $ fmap toCTreeGroupEntryNC a
    toCTreeArray (Group xs) =
      It . CTree.Choice $
        fmap (It . CTree.Array . fmap toCTreeGroupEntryNC) xs

    toCTreeMemberKey :: MemberKey -> CTree.Node OrRef
    toCTreeMemberKey (MKValue v) = It $ CTree.Literal v
    toCTreeMemberKey (MKBareword (Name n)) = It $ CTree.Literal (VText n)
    toCTreeMemberKey (MKType t1) = toCTreeT1 t1

--------------------------------------------------------------------------------
-- 3. Name resolution
--------------------------------------------------------------------------------

data NameResolutionFailure
  = UnboundReference Name
  | MismatchingArgs Name [Name]
  | ArgsToPostlude PTerm [CTree.Node OrRef]
  deriving (Show)

postludeBinding :: Map.Map Name PTerm
postludeBinding =
  Map.fromList
    [ (Name "bool", PTBool)
    , (Name "uint", PTUInt)
    , (Name "nint", PTNInt)
    , (Name "int", PTInt)
    , (Name "half", PTHalf)
    , (Name "float", PTFloat)
    , (Name "double", PTDouble)
    , (Name "bytes", PTBytes)
    , (Name "bstr", PTBytes)
    , (Name "text", PTText)
    , (Name "tstr", PTText)
    , (Name "any", PTAny)
    , (Name "nil", PTNil)
    , (Name "null", PTNil)
    ]

data BindingEnv poly f = BindingEnv
  { global :: Map.Map Name (poly (CTree.Node f))
  -- ^ Global name bindings via 'RuleDef'
  , local :: Map.Map Name (CTree.Node f)
  -- ^ Local bindings for generic parameters
  }
  deriving (Generic)

data DistRef a
  = DIt a
  | -- | Reference to a generic parameter
    GenericRef Name
  | -- | Reference to a rule definition, possibly with generic arguments
    RuleRef Name [CTree.Node DistRef]
  deriving (Eq, Generic, Functor, Show)

instance Hashable a => Hashable (DistRef a)

deriving instance Show (CTree DistRef)

deriving instance Eq (CTree DistRef)

instance Hashable (CTree DistRef)

deriving instance Show (CTreeRoot DistRef)

deriving instance Eq (CTreeRoot DistRef)

instance Hashable (CTreeRoot DistRef)

resolveRef ::
  BindingEnv (ParametrisedWith [Name]) OrRef ->
  CTree.Node OrRef ->
  Either NameResolutionFailure (DistRef (CTree DistRef))
resolveRef env (It a) = DIt <$> resolveCTree env a
resolveRef env (Ref n args) = case Map.lookup n postludeBinding of
  Just pterm -> case args of
    [] -> Right . DIt $ CTree.Postlude pterm
    xs -> Left $ ArgsToPostlude pterm xs
  Nothing -> case Map.lookup n (global env) of
    Just (parameters -> params') ->
      if length params' == length args
        then
          let localBinds = Map.fromList $ zip params' args
              newEnv = env & field @"local" %~ Map.union localBinds
           in RuleRef n <$> traverse (resolveRef newEnv) args
        else Left $ MismatchingArgs n params'
    Nothing -> case Map.lookup n (local env) of
      Just _ -> Right $ GenericRef n
      Nothing -> Left $ UnboundReference n

resolveCTree ::
  BindingEnv (ParametrisedWith [Name]) OrRef ->
  CTree OrRef ->
  Either NameResolutionFailure (CTree DistRef)
resolveCTree e = CTree.traverseCTree (resolveRef e)

buildResolvedCTree ::
  CTreeRoot OrRef ->
  Either NameResolutionFailure (CTreeRoot DistRef)
buildResolvedCTree (CTreeRoot ct) = CTreeRoot <$> traverse go ct
  where
    initBindingEnv = BindingEnv ct mempty
    go pn =
      let args = parameters pn
          localBinds = Map.fromList $ zip args (flip Ref [] <$> args)
          env = initBindingEnv & field @"local" %~ Map.union localBinds
       in traverse (resolveRef env) pn

--------------------------------------------------------------------------------
-- 4. Monomorphisation
--------------------------------------------------------------------------------

data MonoRef a
  = MIt a
  | MRuleRef Name
  deriving (Functor, Show)

deriving instance Show (CTree MonoRef)

deriving instance
  Show (poly (CTree.Node MonoRef)) =>
  Show (CTreeRoot' poly MonoRef)

type MonoEnv = BindingEnv (ParametrisedWith [Name]) DistRef

-- | We introduce additional bindings in the state
type MonoState = Map.Map Name (CTree.Node MonoRef)

-- | Monad to run the monomorphisation process. We need some additional
-- capabilities for this, so 'Either' doesn't fully cut it anymore.
newtype MonoM a = MonoM
  { runMonoM ::
      ExceptT
        NameResolutionFailure
        (StateT MonoState (Reader MonoEnv))
        a
  }
  deriving (Functor, Applicative, Monad)
  deriving
    (HasThrow "nameResolution" NameResolutionFailure)
    via MonadError
          ( ExceptT
              NameResolutionFailure
              (StateT MonoState (Reader MonoEnv))
          )
  deriving
    ( HasSource
        "local"
        (Map.Map Name (CTree.Node DistRef))
    , HasReader
        "local"
        (Map.Map Name (CTree.Node DistRef))
    )
    via Field
          "local"
          ()
          ( Lift
              ( ExceptT
                  NameResolutionFailure
                  (Lift (StateT MonoState (MonadReader (Reader MonoEnv))))
              )
          )
  deriving
    ( HasSource
        "global"
        (Map.Map Name (ParametrisedWith [Name] (CTree.Node DistRef)))
    , HasReader
        "global"
        (Map.Map Name (ParametrisedWith [Name] (CTree.Node DistRef)))
    )
    via Field
          "global"
          ()
          ( Lift
              ( ExceptT
                  NameResolutionFailure
                  (Lift (StateT MonoState (MonadReader (Reader MonoEnv))))
              )
          )
  deriving
    ( HasSource "synth" MonoState
    , HasSink "synth" MonoState
    , HasState "synth" MonoState
    )
    via Lift
          ( ExceptT
              NameResolutionFailure
              (MonadState (StateT MonoState (Reader MonoEnv)))
          )

throwNR :: NameResolutionFailure -> MonoM a
throwNR = throw @"nameResolution"

-- | Synthesize a monomorphic rule definition, returning the name
synthMono :: Name -> [CTree.Node DistRef] -> MonoM Name
synthMono n@(Name origName) args =
  let fresh =
        -- % is not a valid CBOR name, so this should avoid conflict
        Name (origName <> "%" <> T.pack (show $ hash args))
   in do
        -- Lookup the original name in the global bindings
        globalBinds <- ask @"global"
        case Map.lookup n globalBinds of
          Just (Unparametrised _) -> throwNR $ MismatchingArgs n []
          Just (Parametrised r params') ->
            if length params' == length args
              then
                let localBinds = Map.fromList $ zip params' args
                 in Reader.local @"local" (Map.union localBinds) $ do
                      foo <- resolveGenericRef r
                      modify @"synth" $ Map.insert fresh foo
              else throwNR $ MismatchingArgs n params'
          Nothing -> throwNR $ UnboundReference n
        pure fresh

resolveGenericRef ::
  CTree.Node DistRef ->
  MonoM (MonoRef (CTree MonoRef))
resolveGenericRef (DIt a) = MIt <$> resolveGenericCTree a
resolveGenericRef (RuleRef n margs) =
  case margs of
    [] -> pure $ MRuleRef n
    args -> do
      fresh <- synthMono n args
      pure $ MRuleRef fresh
resolveGenericRef (GenericRef n) = do
  localBinds <- ask @"local"
  case Map.lookup n localBinds of
    Just node -> resolveGenericRef node
    Nothing -> throwNR $ UnboundReference n

resolveGenericCTree ::
  CTree DistRef ->
  MonoM (CTree MonoRef)
resolveGenericCTree = CTree.traverseCTree resolveGenericRef

-- | Monomorphise the CTree
--
-- Concretely, for each reference in the tree to a generic rule, we synthesize a
-- new monomorphic instance of that rule at top-level with the correct
-- parameters applied.
monoCTree ::
  CTreeRoot' Identity DistRef ->
  MonoM (CTreeRoot' Identity MonoRef)
monoCTree (CTreeRoot ct) = CTreeRoot <$> traverse go ct
  where
    go = traverse resolveGenericRef

buildMonoCTree ::
  CTreeRoot DistRef ->
  Either NameResolutionFailure (CTreeRoot' Identity MonoRef)
buildMonoCTree (CTreeRoot ct) = do
  let a1 = runExceptT $ runMonoM (monoCTree monoC)
      a2 = runStateT a1 mempty
      (er, newBindings) = runReader a2 initBindingEnv
  CTreeRoot r <- er
  pure . CTreeRoot $ Map.union r $ fmap Identity newBindings
  where
    initBindingEnv = BindingEnv ct mempty
    monoC =
      CTreeRoot $
        Map.mapMaybe
          ( \case
              Unparametrised f -> Just $ Identity f
              Parametrised _ _ -> Nothing
          )
          ct

--------------------------------------------------------------------------------
-- Combined resolution
--------------------------------------------------------------------------------

fullResolveCDDL :: CDDL -> Either NameResolutionFailure (CTreeRoot' Identity MonoRef)
fullResolveCDDL cddl = do
  let refCTree = buildRefCTree (asMap cddl)
  rCTree <- buildResolvedCTree refCTree
  buildMonoCTree rCTree
