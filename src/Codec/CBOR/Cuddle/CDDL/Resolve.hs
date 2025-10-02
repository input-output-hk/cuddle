{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
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
  buildRefCTree,
  asMap,
  buildMonoCTree,
  fullResolveCDDL,
  NameResolutionFailure (..),
  MonoReferenced,
  MonoRef (..),
)
where

import Capability.Accessors (Field (..), Lift (..))
import Capability.Error (HasThrow, MonadError (..), throw)
import Capability.Reader (HasReader, MonadReader (..), ask)
import Capability.Reader qualified as Reader (local)
import Capability.Sink (HasSink)
import Capability.Source (HasSource)
import Capability.State (HasState, MonadState (..), modify)
import Codec.CBOR.Cuddle.CDDL as CDDL
import Codec.CBOR.Cuddle.CDDL.CTree (
  CTree (..),
  CTreeExt,
  ProvidedParameters (..), CTreeRoot (..),
 )
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.Postlude (PTerm (..))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Reader (Reader, ReaderT (..), runReader)
import Control.Monad.State.Strict (StateT (..))
import Data.Generics.Product
import Data.Generics.Sum
import Data.Hashable
#if __GLASGOW_HASKELL__ < 910
import Data.List (foldl')
#endif
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics.Core

--------------------------------------------------------------------------------
-- 1. Rule extensions
--------------------------------------------------------------------------------

newtype PartialCTreeRoot i = PartialCTreeRoot (Map.Map Name (ProvidedParameters (CTree i)))
  deriving (Generic)

type CDDLMap = Map.Map Name (ProvidedParameters TypeOrGroup)

toParametrised :: a -> Maybe GenericParam -> ProvidedParameters a
toParametrised a Nothing = ProvidedParameters [] a
toParametrised a (Just (GenericParam gps)) = CTree.ProvidedParameters (NE.toList gps) a

asMap :: CDDL -> CDDLMap
asMap cddl = foldl' go Map.empty rules
  where
    rules = cddlTopLevel cddl
    go x (TopLevelComment _) = x
    go x (TopLevelRule r) = assignOrExtend x r

    assignOrExtend :: CDDLMap -> Rule -> CDDLMap
    assignOrExtend m (Rule n gps assign tog _) = case assign of
      -- Equals assignment
      AssignEq -> Map.insert n (toParametrised tog gps) m
      AssignExt -> Map.alter (extend tog gps) n m

    extend ::
      TypeOrGroup ->
      Maybe GenericParam ->
      Maybe (ProvidedParameters TypeOrGroup) ->
      Maybe (ProvidedParameters TypeOrGroup)
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

data OrReferenced

type instance CTreeExt OrReferenced = OrRef (CTree OrReferenced)

-- | Indicates that an item may be referenced rather than defined.
data OrRef a
  = -- | Reference to another node with possible generic arguments supplied
    Ref Name [CTree OrReferenced]
  deriving (Eq, Show, Functor)

type RefCTree = PartialCTreeRoot OrReferenced

deriving instance Show (CTree OrReferenced)

deriving instance Show (PartialCTreeRoot OrReferenced)

-- | Build a CTree incorporating references.
--
-- This translation cannot fail.
buildRefCTree :: CDDLMap -> RefCTree
buildRefCTree rules = PartialCTreeRoot $ toCTreeRule <$> rules
  where
    toCTreeRule ::
      ProvidedParameters TypeOrGroup ->
      ProvidedParameters (CTree OrReferenced)
    toCTreeRule = fmap toCTreeTOG

    toCTreeTOG :: TypeOrGroup -> CTree OrReferenced
    toCTreeTOG (TOGType t0) = toCTreeT0 t0
    toCTreeTOG (TOGGroup ge) = toCTreeGroupEntry ge

    toCTreeT0 :: Type0 -> CTree OrReferenced
    toCTreeT0 (Type0 (t1 NE.:| [])) = toCTreeT1 t1
    toCTreeT0 (Type0 xs) = CTree.Choice $ toCTreeT1 <$> xs

    toCTreeT1 :: Type1 -> CTree OrReferenced
    toCTreeT1 (Type1 t2 Nothing _) = toCTreeT2 t2
    toCTreeT1 (Type1 t2 (Just (op, t2')) _) = case op of
      RangeOp bound ->
        CTree.Range
          { CTree.from = toCTreeT2 t2
          , CTree.to = toCTreeT2 t2'
          , CTree.inclusive = bound
          }
      CtrlOp ctlop ->
        CTree.Control
          { CTree.op = ctlop
          , CTree.target = toCTreeT2 t2
          , CTree.controller = toCTreeT2 t2'
          }

    toCTreeT2 :: Type2 -> CTree OrReferenced
    toCTreeT2 (T2Value v) = CTree.Literal v
    toCTreeT2 (T2Name n garg) = CTreeE $ Ref n (fromGenArgs garg)
    toCTreeT2 (T2Group t0) =
      -- This behaviour seems questionable, but I don't really see how better to
      -- interpret the spec here.
      toCTreeT0 t0
    toCTreeT2 (T2Map g) = toCTreeMap g
    toCTreeT2 (T2Array g) = toCTreeArray g
    toCTreeT2 (T2Unwrapped n margs) =
      CTree.Unwrap . CTreeE $
        Ref n (fromGenArgs margs)
    toCTreeT2 (T2Enum g) = toCTreeEnum g
    toCTreeT2 (T2EnumRef n margs) = CTreeE . Ref n $ fromGenArgs margs
    toCTreeT2 (T2Tag Nothing t0) =
      -- Currently not validating tags
      toCTreeT0 t0
    toCTreeT2 (T2Tag (Just tag) t0) =
      CTree.Tag tag $ toCTreeT0 t0
    toCTreeT2 (T2DataItem 7 (Just mmin)) =
      toCTreeDataItem mmin
    toCTreeT2 (T2DataItem _maj _mmin) =
      -- We don't validate numerical items yet
      CTree.Postlude PTAny
    toCTreeT2 T2Any = CTree.Postlude PTAny

    toCTreeDataItem 20 =
      CTree.Literal $ Value (VBool False) mempty
    toCTreeDataItem 21 =
      CTree.Literal $ Value (VBool True) mempty
    toCTreeDataItem 25 =
      CTree.Postlude PTHalf
    toCTreeDataItem 26 =
      CTree.Postlude PTFloat
    toCTreeDataItem 27 =
      CTree.Postlude PTDouble
    toCTreeDataItem 23 =
      CTree.Postlude PTUndefined
    toCTreeDataItem _ =
      CTree.Postlude PTAny

    toCTreeGroupEntry :: GroupEntry -> CTree OrReferenced
    toCTreeGroupEntry (GroupEntry (Just occi) _ (GEType mmkey t0)) =
      CTree.Occur
        { CTree.item = toKVPair mmkey t0
        , CTree.occurs = occi
        }
    toCTreeGroupEntry (GroupEntry Nothing _ (GEType mmkey t0)) = toKVPair mmkey t0
    toCTreeGroupEntry (GroupEntry (Just occi) _ (GERef n margs)) =
      CTree.Occur
        { CTree.item = CTreeE $ Ref n (fromGenArgs margs)
        , CTree.occurs = occi
        }
    toCTreeGroupEntry (GroupEntry Nothing _ (GERef n margs)) = CTreeE $ Ref n (fromGenArgs margs)
    toCTreeGroupEntry (GroupEntry (Just occi) _ (GEGroup g)) =
      CTree.Occur
        { CTree.item = groupToGroup g
        , CTree.occurs = occi
        }
    toCTreeGroupEntry (GroupEntry Nothing _ (GEGroup g)) = groupToGroup g

    fromGenArgs :: Maybe GenericArg -> [CTree OrReferenced]
    fromGenArgs = maybe [] (\(GenericArg xs) -> NE.toList $ fmap toCTreeT1 xs)

    -- Interpret a group as an enumeration. Note that we float out the
    -- choice options
    toCTreeEnum :: Group -> CTree OrReferenced
    toCTreeEnum (CDDL.Group (a NE.:| [])) =
      CTree.Enum . CTree.Group $ toCTreeGroupEntry <$> gcGroupEntries a
    toCTreeEnum (CDDL.Group xs) =
      CTree.Choice $ CTree.Enum . CTree.Group . fmap toCTreeGroupEntry <$> groupEntries
      where
        groupEntries = fmap gcGroupEntries xs

    -- Embed a group in another group, again floating out the choice options
    groupToGroup :: Group -> CTree OrReferenced
    groupToGroup (CDDL.Group (a NE.:| [])) =
      CTree.Group $ fmap toCTreeGroupEntry (gcGroupEntries a)
    groupToGroup (CDDL.Group xs) =
      CTree.Choice $ fmap (CTree.Group . fmap toCTreeGroupEntry) (gcGroupEntries <$> xs)

    toKVPair :: Maybe MemberKey -> Type0 -> CTree OrReferenced
    toKVPair Nothing t0 = toCTreeT0 t0
    toKVPair (Just mkey) t0 =
      CTree.KV
        { CTree.key = toCTreeMemberKey mkey
        , CTree.value = toCTreeT0 t0
        , -- TODO Handle cut semantics
          CTree.cut = False
        }

    -- Interpret a group as a map. Note that we float out the choice options
    toCTreeMap :: Group -> CTree OrReferenced
    toCTreeMap (CDDL.Group (a NE.:| [])) = CTree.Map $ fmap toCTreeGroupEntry (gcGroupEntries a)
    toCTreeMap (CDDL.Group xs) =
      CTree.Choice $
        fmap (CTree.Map . fmap toCTreeGroupEntry) (gcGroupEntries <$> xs)

    -- Interpret a group as an array. Note that we float out the choice
    -- options
    toCTreeArray :: Group -> CTree OrReferenced
    toCTreeArray (CDDL.Group (a NE.:| [])) =
      CTree.Array $ fmap toCTreeGroupEntry (gcGroupEntries a)
    toCTreeArray (CDDL.Group xs) =
      CTree.Choice $
        fmap (CTree.Array . fmap toCTreeGroupEntry) (gcGroupEntries <$> xs)

    toCTreeMemberKey :: MemberKey -> CTree OrReferenced
    toCTreeMemberKey (MKValue v) = CTree.Literal v
    toCTreeMemberKey (MKBareword (Name n _)) = CTree.Literal (Value (VText n) mempty)
    toCTreeMemberKey (MKType t1) = toCTreeT1 t1

--------------------------------------------------------------------------------
-- 3. Name resolution
--------------------------------------------------------------------------------

data NameResolutionFailure
  = UnboundReference Name
  | MismatchingArgs Name [Name]
  | ArgsToPostlude PTerm [CTree OrReferenced]
  deriving (Show)

deriving instance Eq (CTree.Node OrReferenced) => Eq NameResolutionFailure

postludeBinding :: Map.Map Name PTerm
postludeBinding =
  Map.fromList
    [ (Name "bool" mempty, PTBool)
    , (Name "uint" mempty, PTUInt)
    , (Name "nint" mempty, PTNInt)
    , (Name "int" mempty, PTInt)
    , (Name "half" mempty, PTHalf)
    , (Name "float" mempty, PTFloat)
    , (Name "double" mempty, PTDouble)
    , (Name "bytes" mempty, PTBytes)
    , (Name "bstr" mempty, PTBytes)
    , (Name "text" mempty, PTText)
    , (Name "tstr" mempty, PTText)
    , (Name "any" mempty, PTAny)
    , (Name "nil" mempty, PTNil)
    , (Name "null" mempty, PTNil)
    ]

data BindingEnv i j = BindingEnv
  { global :: Map.Map Name (ProvidedParameters (CTree i))
  -- ^ Global name bindings via 'RuleDef'
  , local :: Map.Map Name (CTree j)
  -- ^ Local bindings for generic parameters
  }
  deriving (Generic)

data DistReferenced

type instance CTreeExt DistReferenced = DistRef

data DistRef
  = -- | Reference to a generic parameter
    GenericRef Name
  | -- | Reference to a rule definition, possibly with generic arguments
    RuleRef Name [CTree DistReferenced]
  deriving (Eq, Generic, Show)

instance Hashable DistRef

deriving instance Show (CTree DistReferenced)

instance Hashable (CTree DistReferenced)

deriving instance Show (PartialCTreeRoot DistReferenced)

deriving instance Eq (PartialCTreeRoot DistReferenced)

instance Hashable (PartialCTreeRoot DistReferenced)

resolveRef ::
  BindingEnv OrReferenced OrReferenced ->
  CTree.Node OrReferenced ->
  Either NameResolutionFailure (CTree DistReferenced)
resolveRef env (Ref n args) = case Map.lookup n postludeBinding of
  Just pterm -> case args of
    [] -> Right $ CTree.Postlude pterm
    xs -> Left $ ArgsToPostlude pterm xs
  Nothing -> case Map.lookup n (global env) of
    Just (parameters -> params') ->
      if length params' == length args
        then
          let localBinds = Map.fromList $ zip params' args
              newEnv = env & #local %~ Map.union localBinds
           in CTreeE . RuleRef n <$> traverse (resolveCTree newEnv) args
        else Left $ MismatchingArgs n params'
    Nothing -> case Map.lookup n (local env) of
      Just _ -> Right . CTreeE $ GenericRef n
      Nothing -> Left $ UnboundReference n

resolveCTree ::
  BindingEnv OrReferenced OrReferenced ->
  CTree OrReferenced ->
  Either NameResolutionFailure (CTree DistReferenced)
resolveCTree e = CTree.traverseCTree (resolveRef e) (resolveCTree e)

buildResolvedCTree ::
  PartialCTreeRoot OrReferenced ->
  Either NameResolutionFailure (PartialCTreeRoot DistReferenced)
buildResolvedCTree (PartialCTreeRoot ct) = PartialCTreeRoot <$> traverse go ct
  where
    go pn =
      let args = parameters pn
          localBinds = Map.fromList $ zip args (CTreeE . flip Ref [] <$> args)
          env = BindingEnv @OrReferenced @OrReferenced ct localBinds
       in traverse (resolveCTree env) pn

--------------------------------------------------------------------------------
-- 4. Monomorphisation
--------------------------------------------------------------------------------

data MonoReferenced

type instance CTreeExt MonoReferenced = MonoRef (CTree MonoReferenced)

newtype MonoRef a
  = MRuleRef Name
  deriving (Functor, Show)

deriving instance Show (CTree MonoReferenced)

deriving instance Show (PartialCTreeRoot MonoReferenced)

type MonoEnv = BindingEnv DistReferenced MonoReferenced

-- | We introduce additional bindings in the state
type MonoState = Map.Map Name (CTree MonoReferenced)

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
        (Map.Map Name (CTree MonoReferenced))
    , HasReader
        "local"
        (Map.Map Name (CTree MonoReferenced))
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
        (Map.Map Name (ProvidedParameters (CTree DistReferenced)))
    , HasReader
        "global"
        (Map.Map Name (ProvidedParameters (CTree DistReferenced)))
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
synthMono :: Name -> [CTree DistReferenced] -> MonoM Name
synthMono n@(Name origName _) args =
  let fresh =
        -- % is not a valid CBOR name, so this should avoid conflict
        Name (origName <> "%" <> T.pack (show $ hash args)) mempty
   in do
        -- Lookup the original name in the global bindings
        globalBinds <- ask @"global"
        case Map.lookup n globalBinds of
          Just (CTree.ProvidedParameters [] _) -> throwNR $ MismatchingArgs n []
          Just (CTree.ProvidedParameters params' r) ->
            if length params' == length args
              then do
                rargs <- traverse resolveGenericCTree args
                let localBinds = Map.fromList $ zip params' rargs
                Reader.local @"local" (Map.union localBinds) $ do
                  foo <- resolveGenericCTree r
                  modify @"synth" $ Map.insert fresh foo
              else throwNR $ MismatchingArgs n params'
          Nothing -> throwNR $ UnboundReference n
        pure fresh

resolveGenericRef ::
  CTree.Node DistReferenced ->
  MonoM (CTree MonoReferenced)
resolveGenericRef (RuleRef n []) = pure . CTreeE $ MRuleRef n
resolveGenericRef (RuleRef n args) = do
  fresh <- synthMono n args
  pure . CTreeE $ MRuleRef fresh
resolveGenericRef (GenericRef n) = do
  localBinds <- ask @"local"
  case Map.lookup n localBinds of
    Just node -> pure node
    Nothing -> throwNR $ UnboundReference n

resolveGenericCTree ::
  CTree DistReferenced ->
  MonoM (CTree MonoReferenced)
resolveGenericCTree = CTree.traverseCTree resolveGenericRef resolveGenericCTree

-- | Monomorphise the CTree
--
-- Concretely, for each reference in the tree to a generic rule, we synthesize a
-- new monomorphic instance of that rule at top-level with the correct
-- parameters applied.
buildMonoCTree ::
  PartialCTreeRoot DistReferenced ->
  Either NameResolutionFailure (CTreeRoot MonoReferenced)
buildMonoCTree (PartialCTreeRoot ct) = do
  let a1 = runExceptT $ runMonoM (traverse resolveGenericCTree monoC)
      a2 = runStateT a1 mempty
      (r, newBindings) = runReader a2 initBindingEnv
  CTreeRoot . (`Map.union` newBindings) <$> r
  where
    initBindingEnv = BindingEnv ct mempty
    monoC =
      Map.mapMaybe
        ( \case
            ProvidedParameters [] f -> Just f
            _ -> Nothing
        )
        ct

--------------------------------------------------------------------------------
-- Combined resolution
--------------------------------------------------------------------------------

fullResolveCDDL :: CDDL -> Either NameResolutionFailure (CTreeRoot MonoReferenced)
fullResolveCDDL cddl = do
  let refCTree = buildRefCTree (asMap cddl)
  rCTree <- buildResolvedCTree refCTree
  buildMonoCTree rCTree
