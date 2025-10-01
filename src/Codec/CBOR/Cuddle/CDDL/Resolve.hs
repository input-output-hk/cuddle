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
  MonoRef (..),
  OrRef (..),
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
import Codec.CBOR.Cuddle.CDDL as CDDL
import Codec.CBOR.Cuddle.CDDL.CTree (
  CTree (..),
  CTreeExt,
  CTreeRoot (..),
  Parametrisation (..),
 )
import Codec.CBOR.Cuddle.CDDL.CTree qualified as CTree
import Codec.CBOR.Cuddle.CDDL.Postlude (PTerm (..))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Reader (Reader, ReaderT (..), runReader)
import Control.Monad.State.Strict (StateT (..))
import Data.Functor.Identity (Identity (..))
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

type CDDLMap = Map.Map Name (Parametrisation TypeOrGroup)

toParametrised :: a -> Maybe GenericParam -> Parametrisation a
toParametrised a Nothing = Parametrisation [] a
toParametrised a (Just (GenericParam gps)) = CTree.Parametrisation (NE.toList gps) a

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
      Maybe (Parametrisation TypeOrGroup) ->
      Maybe (Parametrisation TypeOrGroup)
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
  = -- | The item is inlined directly
    It a
  | -- | Reference to another node with possible generic arguments supplied
    Ref Name [CTree.Node OrReferenced]
  deriving (Eq, Show, Functor)

type RefCTree = CTreeRoot OrReferenced

deriving instance Show (CTree OrReferenced)

deriving instance Show (CTreeRoot OrReferenced)

-- | Build a CTree incorporating references.
--
-- This translation cannot fail.
buildRefCTree :: CDDLMap -> RefCTree
buildRefCTree rules = CTreeRoot $ toCTreeRule <$> rules
  where
    toCTreeRule ::
      Parametrisation TypeOrGroup ->
      Parametrisation (CTree.Node OrReferenced)
    toCTreeRule = fmap toCTreeTOG

    toCTreeTOG :: TypeOrGroup -> CTree.Node OrReferenced
    toCTreeTOG (TOGType t0) = toCTreeT0 t0
    toCTreeTOG (TOGGroup ge) = toCTreeGroupEntry ge

    toCTreeT0 :: Type0 -> CTree.Node OrReferenced
    toCTreeT0 (Type0 (t1 NE.:| [])) = toCTreeT1 t1
    toCTreeT0 (Type0 xs) = It . CTree.Choice $ CTreeE . toCTreeT1 <$> xs

    toCTreeT1 :: Type1 -> CTree.Node OrReferenced
    toCTreeT1 (Type1 t2 Nothing _) = toCTreeT2 t2
    toCTreeT1 (Type1 t2 (Just (op, t2')) _) = case op of
      RangeOp bound ->
        It $
          CTree.Range
            { CTree.from = CTreeE $ toCTreeT2 t2
            , CTree.to = CTreeE $ toCTreeT2 t2'
            , CTree.inclusive = bound
            }
      CtrlOp ctlop ->
        It $
          CTree.Control
            { CTree.op = ctlop
            , CTree.target = CTreeE $ toCTreeT2 t2
            , CTree.controller = CTreeE $ toCTreeT2 t2'
            }

    toCTreeT2 :: Type2 -> CTree.Node OrReferenced
    toCTreeT2 (T2Value v) = It $ CTree.Literal v
    toCTreeT2 (T2Name n garg) = Ref n (fromGenArgs garg)
    toCTreeT2 (T2Group t0) =
      -- This behaviour seems questionable, but I don't really see how better to
      -- interpret the spec here.
      toCTreeT0 t0
    toCTreeT2 (T2Map g) = toCTreeMap g
    toCTreeT2 (T2Array g) = toCTreeArray g
    toCTreeT2 (T2Unwrapped n margs) =
      It . CTree.Unwrap . CTreeE $
        Ref n (fromGenArgs margs)
    toCTreeT2 (T2Enum g) = toCTreeEnum g
    toCTreeT2 (T2EnumRef n margs) = Ref n $ fromGenArgs margs
    toCTreeT2 (T2Tag Nothing t0) =
      -- Currently not validating tags
      toCTreeT0 t0
    toCTreeT2 (T2Tag (Just tag) t0) =
      It . CTree.Tag tag . CTreeE $ toCTreeT0 t0
    toCTreeT2 (T2DataItem 7 (Just mmin)) =
      toCTreeDataItem mmin
    toCTreeT2 (T2DataItem _maj _mmin) =
      -- We don't validate numerical items yet
      It $ CTree.Postlude PTAny
    toCTreeT2 T2Any = It $ CTree.Postlude PTAny

    toCTreeDataItem 20 =
      It . CTree.Literal $ Value (VBool False) mempty
    toCTreeDataItem 21 =
      It . CTree.Literal $ Value (VBool True) mempty
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

    toCTreeGroupEntry :: GroupEntry -> CTree.Node OrReferenced
    toCTreeGroupEntry (GroupEntry (Just occi) _ (GEType mmkey t0)) =
      It $
        CTree.Occur
          { CTree.item = CTreeE $ toKVPair mmkey t0
          , CTree.occurs = occi
          }
    toCTreeGroupEntry (GroupEntry Nothing _ (GEType mmkey t0)) = toKVPair mmkey t0
    toCTreeGroupEntry (GroupEntry (Just occi) _ (GERef n margs)) =
      It $
        CTree.Occur
          { CTree.item = CTreeE $ Ref n (fromGenArgs margs)
          , CTree.occurs = occi
          }
    toCTreeGroupEntry (GroupEntry Nothing _ (GERef n margs)) = Ref n (fromGenArgs margs)
    toCTreeGroupEntry (GroupEntry (Just occi) _ (GEGroup g)) =
      It $
        CTree.Occur
          { CTree.item = CTreeE $ groupToGroup g
          , CTree.occurs = occi
          }
    toCTreeGroupEntry (GroupEntry Nothing _ (GEGroup g)) = groupToGroup g

    fromGenArgs :: Maybe GenericArg -> [CTree.Node OrReferenced]
    fromGenArgs = maybe [] (\(GenericArg xs) -> NE.toList $ fmap toCTreeT1 xs)

    -- Interpret a group as an enumeration. Note that we float out the
    -- choice options
    toCTreeEnum :: Group -> CTree.Node OrReferenced
    toCTreeEnum (CDDL.Group (a NE.:| [])) =
      It . CTree.Enum . CTreeE . It . CTree.Group $ CTreeE . toCTreeGroupEntry <$> gcGroupEntries a
    toCTreeEnum (CDDL.Group xs) =
      It . CTree.Choice $
        CTreeE . It . CTree.Enum . CTreeE . It . CTree.Group . fmap (CTreeE . toCTreeGroupEntry)
          <$> groupEntries
      where
        groupEntries = fmap gcGroupEntries xs

    -- Embed a group in another group, again floating out the choice options
    groupToGroup :: Group -> CTree.Node OrReferenced
    groupToGroup (CDDL.Group (a NE.:| [])) =
      It . CTree.Group $ fmap (CTreeE . toCTreeGroupEntry) (gcGroupEntries a)
    groupToGroup (CDDL.Group xs) =
      It . CTree.Choice $
        fmap (CTreeE . It . CTree.Group . fmap (CTreeE . toCTreeGroupEntry)) (gcGroupEntries <$> xs)

    toKVPair :: Maybe MemberKey -> Type0 -> CTree.Node OrReferenced
    toKVPair Nothing t0 = toCTreeT0 t0
    toKVPair (Just mkey) t0 =
      It $
        CTree.KV
          { CTree.key = CTreeE $ toCTreeMemberKey mkey
          , CTree.value = CTreeE $ toCTreeT0 t0
          , -- TODO Handle cut semantics
            CTree.cut = False
          }

    -- Interpret a group as a map. Note that we float out the choice options
    toCTreeMap :: Group -> CTree.Node OrReferenced
    toCTreeMap (CDDL.Group (a NE.:| [])) = It . CTree.Map $ fmap (CTreeE . toCTreeGroupEntry) (gcGroupEntries a)
    toCTreeMap (CDDL.Group xs) =
      It
        . CTree.Choice
        $ fmap (CTreeE . It . CTree.Map . fmap (CTreeE . toCTreeGroupEntry)) (gcGroupEntries <$> xs)

    -- Interpret a group as an array. Note that we float out the choice
    -- options
    toCTreeArray :: Group -> CTree.Node OrReferenced
    toCTreeArray (CDDL.Group (a NE.:| [])) =
      It . CTree.Array $ fmap (CTreeE . toCTreeGroupEntry) (gcGroupEntries a)
    toCTreeArray (CDDL.Group xs) =
      It . CTree.Choice $
        fmap (CTreeE . It . CTree.Array . fmap (CTreeE . toCTreeGroupEntry)) (gcGroupEntries <$> xs)

    toCTreeMemberKey :: MemberKey -> CTree.Node OrReferenced
    toCTreeMemberKey (MKValue v) = It $ CTree.Literal v
    toCTreeMemberKey (MKBareword (Name n _)) = It $ CTree.Literal (Value (VText n) mempty)
    toCTreeMemberKey (MKType t1) = toCTreeT1 t1

--------------------------------------------------------------------------------
-- 3. Name resolution
--------------------------------------------------------------------------------

data NameResolutionFailure
  = UnboundReference Name
  | MismatchingArgs Name [Name]
  | ArgsToPostlude PTerm [CTree.Node OrReferenced]
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
  { global :: Map.Map Name (Parametrisation (CTree.Node i))
  -- ^ Global name bindings via 'RuleDef'
  , local :: Map.Map Name (CTree.Node j)
  -- ^ Local bindings for generic parameters
  }
  deriving (Generic)

data DistReferenced

type instance CTreeExt DistReferenced = DistRef (CTree DistReferenced)

data DistRef a
  = DIt a
  | -- | Reference to a generic parameter
    GenericRef Name
  | -- | Reference to a rule definition, possibly with generic arguments
    RuleRef Name [CTree.Node DistReferenced]
  deriving (Eq, Generic, Functor, Show)

instance Hashable (DistRef a)

deriving instance Show (CTree DistReferenced)

instance Hashable (CTree DistReferenced)

deriving instance Show (CTreeRoot DistReferenced)

deriving instance Eq (CTreeRoot DistReferenced)

instance Hashable (CTreeRoot DistReferenced)

resolveRef ::
  BindingEnv OrReferenced OrReferenced ->
  CTree.Node OrReferenced ->
  Either NameResolutionFailure (CTree.Node DistReferenced)
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
              newEnv = env & #local %~ Map.union localBinds
           in RuleRef n <$> traverse (resolveRef newEnv) args
        else Left $ MismatchingArgs n params'
    Nothing -> case Map.lookup n (local env) of
      Just _ -> Right $ GenericRef n
      Nothing -> Left $ UnboundReference n

resolveCTree ::
  BindingEnv OrReferenced OrReferenced ->
  CTree OrReferenced ->
  Either NameResolutionFailure (CTree DistReferenced)
resolveCTree e = CTree.traverseCTree (resolveRef e) (resolveCTree e)

buildResolvedCTree ::
  CTreeRoot OrReferenced ->
  Either NameResolutionFailure (CTreeRoot DistReferenced)
buildResolvedCTree (CTreeRoot ct) = CTreeRoot <$> traverse go ct
  where
    initBindingEnv = BindingEnv @OrReferenced @OrReferenced ct mempty
    go pn =
      let args = parameters pn
          localBinds = Map.fromList $ zip args (flip Ref [] <$> args)
          env = initBindingEnv & #local %~ Map.union localBinds
       in traverse (resolveRef env) pn

--------------------------------------------------------------------------------
-- 4. Monomorphisation
--------------------------------------------------------------------------------

data MonoReferenced

type instance CTreeExt MonoReferenced = MonoRef (CTree MonoReferenced)

data MonoRef a
  = MIt a
  | MRuleRef Name
  deriving (Functor, Show)

deriving instance Show (CTree MonoReferenced)

deriving instance Show (CTreeRoot MonoReferenced)

type MonoEnv = BindingEnv DistReferenced MonoReferenced

-- | We introduce additional bindings in the state
type MonoState = Map.Map Name (MonoRef (CTree MonoReferenced))

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
        (Map.Map Name (MonoRef (CTree MonoReferenced)))
    , HasReader
        "local"
        (Map.Map Name (MonoRef (CTree MonoReferenced)))
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
        (Map.Map Name (Parametrisation (DistRef (CTree DistReferenced))))
    , HasReader
        "global"
        (Map.Map Name (Parametrisation (DistRef (CTree DistReferenced))))
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
synthMono :: Name -> [CTree.Node DistReferenced] -> MonoM Name
synthMono n@(Name origName _) args =
  let fresh =
        -- % is not a valid CBOR name, so this should avoid conflict
        Name (origName <> "%" <> T.pack (show $ hash args)) mempty
   in do
        -- Lookup the original name in the global bindings
        globalBinds <- ask @"global"
        case Map.lookup n globalBinds of
          Just (CTree.Parametrisation [] _) -> throwNR $ MismatchingArgs n []
          Just (CTree.Parametrisation params' r) ->
            if length params' == length args
              then do
                rargs <- traverse resolveGenericRef args
                let localBinds = Map.fromList $ zip params' rargs
                Reader.local @"local" (Map.union localBinds) $ do
                  foo <- resolveGenericRef r
                  modify @"synth" $ Map.insert fresh foo
              else throwNR $ MismatchingArgs n params'
          Nothing -> throwNR $ UnboundReference n
        pure fresh

resolveGenericRef ::
  CTree.Node DistReferenced ->
  MonoM (CTree.Node MonoReferenced)
resolveGenericRef (DIt a) = MIt <$> resolveGenericCTree a
resolveGenericRef (RuleRef n []) = pure $ MRuleRef n
resolveGenericRef (RuleRef n args) = do
  fresh <- synthMono n args
  pure $ MRuleRef fresh
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
monoCTree ::
  CTreeRoot DistReferenced ->
  MonoM (CTreeRoot MonoReferenced)
monoCTree (CTreeRoot ct) = CTreeRoot <$> traverse go ct
  where
    go = traverse resolveGenericRef

buildMonoCTree ::
  CTreeRoot DistReferenced ->
  Either NameResolutionFailure (Map.Map Name (CTree.Node MonoReferenced))
buildMonoCTree (CTreeRoot ct) = do
  let a1 = runExceptT $ runMonoM (monoCTree monoC)
      a2 = runStateT a1 mempty
      (er, newBindings) = runReader a2 initBindingEnv
  CTreeRoot r <- er
  pure $ Map.union r $ newBindings
  where
    initBindingEnv = BindingEnv ct mempty
    monoC =
      CTreeRoot $
        Map.mapMaybe
          ( \case
              CTree.Parametrisation [] f -> Just $ Identity f
              CTree.Parametrisation _ _ -> Nothing
          )
          ct

--------------------------------------------------------------------------------
-- Combined resolution
--------------------------------------------------------------------------------

fullResolveCDDL :: CDDL -> Either NameResolutionFailure (Map.Map Name (CTree.Node MonoReferenced))
fullResolveCDDL cddl = do
  let refCTree = buildRefCTree (asMap cddl)
  rCTree <- buildResolvedCTree refCTree
  buildMonoCTree rCTree
