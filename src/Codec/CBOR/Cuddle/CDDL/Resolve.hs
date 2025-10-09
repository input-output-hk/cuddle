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
)
where

import Capability.Accessors (Field (..), Lift (..))
import Capability.Error (HasThrow, MonadError (..), throw)
import Capability.Reader (HasReader, MonadReader (..), ask)
import Capability.Reader qualified as Reader (local)
import Capability.Sink (HasSink)
import Capability.Source (HasSource)
import Capability.State (HasState, MonadState (..), modify)
import Codec.CBOR.Cuddle.CDDL (
  Assign (..),
  CDDL,
  GenericArg (..),
  GenericParam (..),
  Group (..),
  GroupEntry (..),
  GrpChoice (..),
  MemberKey (..),
  Name (..),
  Rule (..),
  TopLevel (..),
  Type0 (..),
  Type1 (..),
  Type2 (..),
  TypeOrGroup (..),
  Value (..),
  ValueVariant (..),
  XCddl,
  XTerm,
  XXTopLevel,
  XXType2,
  cddlTopLevel,
 )
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
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Optics.Core

data ProvidedParameters i = ProvidedParameters
  { parameters :: [Name i]
  , underlying :: TypeOrGroup i
  }
  deriving (Generic)

instance Hashable (ProvidedParameters i)

data Parametrised

newtype instance XXType2 Parametrised
  = ParametrisedXXType2 (ProvidedParameters Parametrised)

--------------------------------------------------------------------------------
-- 1. Rule extensions
--------------------------------------------------------------------------------

newtype PartialCTreeRoot i
  = PartialCTreeRoot (Map.Map (Name i) (ProvidedParameters i))
  deriving (Generic)

type CDDLMap i = Map.Map (Name i) (ProvidedParameters i)

toParametrised :: TypeOrGroup i -> Maybe (GenericParam i) -> ProvidedParameters i
toParametrised a Nothing = ProvidedParameters [] a
toParametrised a (Just (GenericParam gps)) = ProvidedParameters (NE.toList gps) a

asMap :: CDDL i -> CDDLMap i
asMap cddl = foldl' go Map.empty rules
  where
    rules = cddlTopLevel cddl
    go x (XXTopLevel _) = x
    go x (TopLevelRule r) = assignOrExtend x r

    assignOrExtend :: CDDLMap i -> Rule i -> CDDLMap i
    assignOrExtend m (Rule n gps assign tog _) = case assign of
      -- Equals assignment
      AssignEq -> Map.insert n (toParametrised tog gps) m
      AssignExt -> Map.alter (extend tog gps) n m

    extend ::
      TypeOrGroup i ->
      Maybe (GenericParam i) ->
      Maybe (ProvidedParameters i) ->
      Maybe (ProvidedParameters i)
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

data instance XTerm OrReferenced = OrReferencedXTerm
  deriving (Eq, Show)

data instance XCddl OrReferenced = OrReferencedXCddl
  deriving (Eq, Show)

newtype instance XXTopLevel OrReferenced = OrReferencedXXTopLevel Void
  deriving (Eq, Show)

-- | Indicates that an item may be referenced rather than defined.
data instance XXType2 OrReferenced
  = -- | Reference to another node with possible generic arguments supplied
    Ref (Name OrReferenced) [TypeOrGroup OrReferenced]
  deriving (Eq, Show)

type RefCTree = PartialCTreeRoot OrReferenced

deriving instance Show (PartialCTreeRoot OrReferenced)

-- | Build a CTree incorporating references.
--
-- This translation cannot fail.
buildRefCTree :: CDDLMap i -> RefCTree
buildRefCTree rules = PartialCTreeRoot $ bimap mapIndex toCTreeRule rules
  where
    toCTreeRule ::
      ProvidedParameters i ->
      ProvidedParameters OrReferenced
    toCTreeRule (ProvidedParameters ns t) = ProvidedParameters (undefined <$> ns) (toCTreeTOG t)

    toCTreeTOG :: TypeOrGroup i -> TypeOrGroup OrReferenced
    toCTreeTOG (TOGType t0) = TOGType $ toCTreeT0 t0
    toCTreeTOG (TOGGroup ge) = TOGGroup $ toCTreeGroupEntry ge

    toCTreeT0 :: Type0 i -> Type0 OrReferenced
    toCTreeT0 (Type0 ts) = Type0 $ toCTreeT1 <$> ts

    toCTreeT1 :: Type1 i -> Type1 OrReferenced
    toCTreeT1 (Type1 t mr e) = Type1 (toCTreeT2 t) (second toCTreeT2 <$> mr) (mapIndex e)

    toCTreeT2 :: Type2 i -> Type2 OrReferenced
    toCTreeT2 (T2Value v) = T2Value v
    toCTreeT2 (T2Name n garg) = XXType2 $ Ref (mapIndex n) (fromGenArgs garg)
    toCTreeT2 (T2Group t0) =
      -- This behaviour seems questionable, but I don't really see how better to
      -- interpret the spec here.
      T2Group $ toCTreeT0 t0
    toCTreeT2 (T2Map g) = T2Map $ toCTreeMap g
    toCTreeT2 (T2Array g) = T2Array $ toCTreeArray g
    toCTreeT2 (T2Unwrapped n margs) =
      undefined
    -- CTree.Unwrap . CTreeE $
    --  Ref n (fromGenArgs margs)
    toCTreeT2 (T2Enum g) = T2Enum $ toCTreeEnum g
    toCTreeT2 (T2EnumRef n margs) = XXType2 . Ref (mapIndex n) $ fromGenArgs margs
    toCTreeT2 (T2Tag mtag t0) =
      -- Currently not validating tags
      T2Tag mtag $ toCTreeT0 t0
    toCTreeT2 (T2DataItem 7 (Just mmin)) =
      toCTreeDataItem mmin
    toCTreeT2 (T2DataItem _maj _mmin) =
      -- We don't validate numerical items yet
      T2Any
    toCTreeT2 T2Any = T2Any
    toCTreeT2 (XXType2 x) = undefined

    toCTreeDataItem :: Word64 -> Type2 OrReferenced
    toCTreeDataItem 20 =
      T2Value $ Value (VBool False) mempty
    toCTreeDataItem 21 =
      T2Value $ Value (VBool True) mempty
    toCTreeDataItem 25 =
      CTree.Postlude PTHalf
    toCTreeDataItem 26 =
      CTree.Postlude PTFloat
    toCTreeDataItem 27 =
      CTree.Postlude PTDouble
    toCTreeDataItem 23 =
      CTree.Postlude PTUndefined
    toCTreeDataItem _ =
      T2Any

    toCTreeGroupEntry :: GroupEntry i -> GroupEntry OrReferenced
    toCTreeGroupEntry = undefined
    -- toCTreeGroupEntry (GroupEntry (Just occi) (GEType mmkey t0) _) =
    --  CTree.Occur
    --    { CTree.item = toKVPair mmkey t0
    --    , CTree.occurs = occi
    --    }
    -- toCTreeGroupEntry (GroupEntry Nothing (GEType mmkey t0) _) = toKVPair mmkey t0
    -- toCTreeGroupEntry (GroupEntry (Just occi) (GERef n margs) _) =
    --  CTree.Occur
    --    { CTree.item = CTreeE $ Ref n (fromGenArgs margs)
    --    , CTree.occurs = occi
    --    }
    -- toCTreeGroupEntry (GroupEntry Nothing (GERef n margs) _) = CTreeE $ Ref n (fromGenArgs margs)
    -- toCTreeGroupEntry (GroupEntry (Just occi) (GEGroup g) _) =
    --  CTree.Occur
    --    { CTree.item = groupToGroup g
    --    , CTree.occurs = occi
    --    }
    -- toCTreeGroupEntry (GroupEntry Nothing (GEGroup g) _) = groupToGroup g

    fromGenArgs :: Maybe (GenericArg i) -> [TypeOrGroup OrReferenced]
    fromGenArgs = maybe [] (\(GenericArg xs) -> NE.toList $ fmap (undefined . toCTreeT1) xs)

    -- Interpret a group as an enumeration. Note that we float out the
    -- choice options
    toCTreeEnum :: Group i -> Group OrReferenced
    toCTreeEnum (Group (a NE.:| [])) =
      undefined -- CTree.Enum . CTree.Group $ toCTreeGroupEntry <$> gcGroupEntries a
    toCTreeEnum (Group xs) =
      undefined -- CTree.Choice $ CTree.Enum . CTree.Group . fmap toCTreeGroupEntry <$> groupEntries
      where
        groupEntries = fmap gcGroupEntries xs

    -- Embed a group in another group, again floating out the choice options
    groupToGroup :: Group i -> Group OrReferenced
    groupToGroup (Group (a NE.:| [])) =
      undefined -- Group $ fmap toCTreeGroupEntry (gcGroupEntries a)
    groupToGroup (Group xs) =
      undefined -- CTree.Choice $ fmap (Group . fmap toCTreeGroupEntry) (gcGroupEntries <$> xs)
    toKVPair :: Maybe (MemberKey i) -> Type0 i -> TypeOrGroup OrReferenced
    toKVPair = undefined
    -- toKVPair Nothing t0 = toCTreeT0 t0
    -- toKVPair (Just mkey) t0 =
    --  CTree.KV
    --    { CTree.key = toCTreeMemberKey mkey
    --    , CTree.value = toCTreeT0 t0
    --    , -- TODO Handle cut semantics
    --      CTree.cut = False
    --    }

    -- Interpret a group as a map. Note that we float out the choice options
    toCTreeMap :: Group i -> Type0 OrReferenced
    -- toCTreeMap (Group (a NE.:| [])) = CTree.Map $ fmap toCTreeGroupEntry (gcGroupEntries a)
    toCTreeMap (Group xs) =
      Type0 $
        xs <&> \(GrpChoice ges c) ->
          Type1
            (T2Map . Group . NE.singleton $ GrpChoice (toCTreeGroupEntry <$> ges) (mapIndex c))
            Nothing
            mempty
    -- fmap (CTree.Map . fmap toCTreeGroupEntry . gcGroupEntries) xs

    -- Interpret a group as an array. Note that we float out the choice
    -- options
    toCTreeArray :: Group i -> Type0 OrReferenced
    toCTreeArray (Group xs) =
      Type0 $
        xs <&> \(GrpChoice ges c) ->
          Type1
            (T2Array . Group . NE.singleton $ GrpChoice (toCTreeGroupEntry <$> ges) (mapIndex c))
            Nothing
            mempty
    -- toCTreeArray (Group (a NE.:| [])) =
    --  CTree.Array $ fmap toCTreeGroupEntry (gcGroupEntries a)
    -- toCTreeArray (Group xs) =
    --  CTree.Choice $
    --    fmap (CTree.Array . fmap toCTreeGroupEntry) (gcGroupEntries <$> xs)

    toCTreeMemberKey :: MemberKey i -> Type2 OrReferenced
    toCTreeMemberKey (MKValue v) = T2Value v
    toCTreeMemberKey (MKBareword (Name n _)) = T2Value (Value (VText n) mempty)
    toCTreeMemberKey (MKType t1) = undefined . MKType $ toCTreeT1 t1

--------------------------------------------------------------------------------
-- 3. Name resolution
--------------------------------------------------------------------------------

data NameResolutionFailure
  = UnboundReference (Name OrReferenced)
  | MismatchingArgs (Name OrReferenced) [Name OrReferenced]
  | ArgsToPostlude PTerm [TypeOrGroup OrReferenced]
  deriving (Show)

deriving instance Eq NameResolutionFailure

postludeBinding :: Map.Map (Name phase) PTerm
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
  { global :: Map.Map (Name i) (ProvidedParameters i)
  -- ^ Global name bindings via 'RuleDef'
  , local :: Map.Map (Name j) (TypeOrGroup j)
  -- ^ Local bindings for generic parameters
  }
  deriving (Generic)

data DistReferenced

data instance XTerm DistReferenced = DistReferencedXTerm
  deriving (Eq, Show)

data instance XCddl DistReferenced = DistReferencedXCddl
  deriving (Eq, Show)

data instance XXTopLevel DistReferenced = DistReferencedXXTopLevel
  deriving (Eq, Show)

data instance XXType2 DistReferenced
  = -- | Reference to a generic parameter
    GenericRef (Name DistReferenced)
  | -- | Reference to a rule definition, possibly with generic arguments
    RuleRef (Name DistReferenced) [TypeOrGroup DistReferenced]
  deriving (Eq, Generic, Show)

instance Hashable (TypeOrGroup DistReferenced)

deriving instance Show (PartialCTreeRoot DistReferenced)

deriving instance Eq (PartialCTreeRoot DistReferenced)

instance Hashable (PartialCTreeRoot DistReferenced)

resolveRef ::
  BindingEnv OrReferenced OrReferenced ->
  XXType2 OrReferenced ->
  Either NameResolutionFailure (TypeOrGroup DistReferenced)
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
           in Right . TOGType . Type0 . NE.singleton $
                Type1
                  (XXType2 . RuleRef (mapIndex n) <$> traverse (resolveCTree newEnv) args)
                  undefined
                  undefined
        else Left $ MismatchingArgs n params'
    Nothing -> case Map.lookup n (local env) of
      Just _ -> Right . CTreeE $ GenericRef n
      Nothing -> Left $ UnboundReference n

resolveCTree ::
  BindingEnv OrReferenced OrReferenced ->
  TypeOrGroup OrReferenced ->
  Either NameResolutionFailure (TypeOrGroup DistReferenced)
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

data instance XTerm MonoReferenced = MonoReferencedXTerm
  deriving (Show)

newtype instance XXType2 MonoReferenced = MRuleRef (Name MonoReferenced)
  deriving (Show)

deriving instance Show (TypeOrGroup MonoReferenced)

deriving instance Show (PartialCTreeRoot MonoReferenced)

type MonoEnv = BindingEnv DistReferenced MonoReferenced

-- | We introduce additional bindings in the state
type MonoState = Map.Map (Name MonoReferenced) (TypeOrGroup MonoReferenced)

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
        (Map.Map (Name MonoReferenced) (TypeOrGroup MonoReferenced))
    , HasReader
        "local"
        (Map.Map (Name MonoReferenced) (TypeOrGroup MonoReferenced))
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
        (Map.Map (Name DistReferenced) (ProvidedParameters DistReferenced))
    , HasReader
        "global"
        (Map.Map (Name DistReferenced) (ProvidedParameters DistReferenced))
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
synthMono :: Name DistReferenced -> [TypeOrGroup DistReferenced] -> MonoM (Name phase)
synthMono n@(Name origName _) args =
  let fresh =
        -- % is not a valid CBOR name, so this should avoid conflict
        Name (origName <> "%" <> T.pack (show $ hash args)) mempty
   in do
        -- Lookup the original name in the global bindings
        globalBinds <- ask @"global"
        case Map.lookup n globalBinds of
          Just (ProvidedParameters [] _) -> throwNR $ MismatchingArgs (mapIndex n) []
          Just (ProvidedParameters params' r) ->
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
  XXType2 DistReferenced ->
  MonoM (TypeOrGroup MonoReferenced)
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
  TypeOrGroup DistReferenced ->
  MonoM (TypeOrGroup MonoReferenced)
resolveGenericCTree = CTree.traverseCTree resolveGenericRef resolveGenericCTree

data CTreeRoot i = CTreeRoot

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

fullResolveCDDL ::
  CDDL phase -> Either NameResolutionFailure (CTreeRoot MonoReferenced)
fullResolveCDDL cddl = do
  let refCTree = buildRefCTree (asMap cddl)
  rCTree <- buildResolvedCTree refCTree
  buildMonoCTree rCTree
