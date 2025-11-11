{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
  ForAllExtensions,
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
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Reader (Reader, ReaderT (..), runReader)
import Control.Monad.State.Strict (StateT (..))
import Data.Foldable (Foldable (..))
import Data.Foldable1 (Foldable1 (..))
import Data.Generics.Product
import Data.Generics.Sum
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Optics.Core

data ProvidedParameters i = ProvidedParameters
  { parameters :: [Name]
  , underlying :: TypeOrGroup i
  }
  deriving (Generic)

deriving instance ForAllExtensions i Eq => Eq (ProvidedParameters i)

deriving instance ForAllExtensions i Show => Show (ProvidedParameters i)

instance ForAllExtensions i Hashable => Hashable (ProvidedParameters i)

data Parametrised

newtype instance XXType2 Parametrised
  = ParametrisedXXType2 (ProvidedParameters Parametrised)

--------------------------------------------------------------------------------
-- 1. Rule extensions
--------------------------------------------------------------------------------

newtype PartialCTreeRoot i
  = PartialCTreeRoot (Map.Map Name (ProvidedParameters i))
  deriving (Generic)

type CDDLMap i = Map.Map Name (ProvidedParameters i)

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
    --   The boolean field determines whether the reference should be unwrapped
    Ref Bool Name [Type1 OrReferenced]
  | OrPostlude PTerm
  deriving (Eq, Show)

type RefCTree = PartialCTreeRoot OrReferenced

deriving instance Show (PartialCTreeRoot OrReferenced)

-- | Build a CTree incorporating references.
--
-- This translation cannot fail.
buildRefCTree ::
  forall i.
  ( IndexMappable XTerm i OrReferenced
  , IndexMappable XXType2 i OrReferenced
  ) =>
  CDDLMap i -> RefCTree
buildRefCTree rules = PartialCTreeRoot $ toCTreeRule <$> rules
  where
    toCTreeRule ::
      ProvidedParameters i ->
      ProvidedParameters OrReferenced
    toCTreeRule (ProvidedParameters ns t) = ProvidedParameters ns (toCTreeTOG t)

    toCTreeTOG :: TypeOrGroup i -> TypeOrGroup OrReferenced
    toCTreeTOG (TOGType t0) = TOGType $ toCTreeT0 t0
    toCTreeTOG (TOGGroup ge) = TOGGroup $ toCTreeGroupEntry ge

    toCTreeT0 :: Type0 i -> Type0 OrReferenced
    toCTreeT0 (Type0 xs) = Type0 $ foldMap1 toCTreeT1 xs
    toCTreeT1 :: Type1 i -> NonEmpty (Type1 OrReferenced)
    toCTreeT1 (Type1 t mr ex) = (\x y -> Type1 x y (mapIndex @_ @i ex)) <$> t' <*> r'
      where
        t' = toCTreeT2 t
        r' = case mr of
          Just (op, x) -> Just . (op,) <$> toCTreeT2 x
          Nothing -> NE.singleton Nothing

    toCTreeT2 :: Type2 i -> NonEmpty (Type2 OrReferenced)
    toCTreeT2 (T2Value v) = NE.singleton $ T2Value v
    toCTreeT2 (T2Name n garg) = NE.singleton . XXType2 $ Ref False n (fromGenArgs garg)
    toCTreeT2 (T2Group t0) =
      -- This behaviour seems questionable, but I don't really see how better to
      -- interpret the spec here.
      NE.singleton . T2Group $ toCTreeT0 t0
    toCTreeT2 (T2Map g) = liftChoice T2Map g
    toCTreeT2 (T2Array g) = liftChoice T2Map g
    toCTreeT2 (T2Unwrapped n margs) =
      NE.singleton . XXType2 $ Ref True n (fromGenArgs margs)
    toCTreeT2 (T2Enum g) = NE.singleton . T2Enum $ toCTreeEnum g
    toCTreeT2 (T2EnumRef n margs) = NE.singleton . XXType2 . Ref False n $ fromGenArgs margs
    toCTreeT2 (T2Tag mtag t0) =
      -- Currently not validating tags
      NE.singleton . T2Tag mtag $ toCTreeT0 t0
    toCTreeT2 (T2DataItem 7 (Just mmin)) =
      NE.singleton $ toCTreeDataItem mmin
    toCTreeT2 (T2DataItem _maj _mmin) =
      -- We don't validate numerical items yet
      NE.singleton T2Any
    toCTreeT2 T2Any = NE.singleton T2Any
    toCTreeT2 x@(XXType2 _) = NE.singleton $ mapIndex x

    toCTreeDataItem :: Word64 -> Type2 OrReferenced
    toCTreeDataItem 20 =
      T2Value $ Value (VBool False) mempty
    toCTreeDataItem 21 =
      T2Value $ Value (VBool True) mempty
    toCTreeDataItem 25 =
      XXType2 $ OrPostlude PTHalf
    toCTreeDataItem 26 =
      XXType2 $ OrPostlude PTFloat
    toCTreeDataItem 27 =
      XXType2 $ OrPostlude PTDouble
    toCTreeDataItem 23 =
      XXType2 $ OrPostlude PTUndefined
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

    fromGenArgs :: Maybe (GenericArg i) -> [Type1 OrReferenced]
    fromGenArgs = maybe [] (\(GenericArg xs) -> NE.toList $ foldMap1 toCTreeT1 xs)

    -- Interpret a group as an enumeration. Note that we float out the
    -- choice options
    toCTreeEnum :: Group i -> Group OrReferenced
    toCTreeEnum g = undefined $ liftChoice T2Enum g
    -- CTree.Enum . CTree.Group $ toCTreeGroupEntry <$> gcGroupEntries a
    -- CTree.Choice $ CTree.Enum . CTree.Group . fmap toCTreeGroupEntry <$> groupEntries
    -- where
    --   groupEntries = fmap gcGroupEntries xs

    -- Embed a group in another group, again floating out the choice options
    groupToGroup :: Group i -> Group OrReferenced
    groupToGroup g =
      Group . fmap (\x -> GrpChoice [GroupEntry Nothing undefined undefined] undefined) $
        liftChoice undefined g

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
    liftChoice :: (Group OrReferenced -> Type2 OrReferenced) -> Group i -> NonEmpty (Type2 OrReferenced)
    liftChoice f (Group xs) =
      xs <&> \(GrpChoice ges c) ->
        f . Group . NE.singleton $ GrpChoice (toCTreeGroupEntry <$> ges) (mapIndex c)

    toCTreeMemberKey :: MemberKey i -> MemberKey OrReferenced
    toCTreeMemberKey (MKValue v) = MKValue v
    toCTreeMemberKey (MKBareword n) = MKBareword n
    toCTreeMemberKey (MKType t1) = undefined $ MKType <$> toCTreeT1 t1

--------------------------------------------------------------------------------
-- 3. Name resolution
--------------------------------------------------------------------------------

data NameResolutionFailure
  = UnboundReference Name
  | MismatchingArgs Name [Name]
  | ArgsToPostlude PTerm [TypeOrGroup OrReferenced]
  deriving (Show)

deriving instance Eq NameResolutionFailure

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

data BindingEnv i j = BindingEnv
  { global :: Map.Map Name (ProvidedParameters i)
  -- ^ Global name bindings via 'RuleDef'
  , local :: Map.Map Name (Type1 j)
  -- ^ Local bindings for generic parameters
  }
  deriving (Generic)

data DistReferenced

data instance XTerm DistReferenced = DistReferencedXTerm
  deriving (Eq, Show, Generic, Hashable)

data instance XCddl DistReferenced = DistReferencedXCddl
  deriving (Eq, Show, Generic, Hashable)

data instance XXTopLevel DistReferenced = DistReferencedXXTopLevel
  deriving (Eq, Show, Generic, Hashable)

data instance XXType2 DistReferenced
  = -- | Reference to a generic parameter
    GenericRef Name
  | -- | Reference to a rule definition, possibly with generic arguments
    RuleRef Name [TypeOrGroup DistReferenced]
  | DistPostlude PTerm
  deriving (Eq, Generic, Show, Hashable)

deriving instance Show (PartialCTreeRoot DistReferenced)

deriving instance Eq (PartialCTreeRoot DistReferenced)

instance Hashable (PartialCTreeRoot DistReferenced)

resolveRef ::
  BindingEnv OrReferenced OrReferenced ->
  XXType2 OrReferenced ->
  Either NameResolutionFailure (TypeOrGroup DistReferenced)
resolveRef env = \case
  Ref unwrap n args -> resolveRef_ unwrap n args
  OrPostlude t -> undefined t
  where
    resolveRef_ ::
      Bool ->
      Name ->
      [Type1 OrReferenced] ->
      Either NameResolutionFailure (TypeOrGroup DistReferenced)
    resolveRef_ unwrap n args = case Map.lookup n postludeBinding of
      Just pterm -> case args of
        [] -> Right . undefined . XXType2 $ DistPostlude pterm
        xs -> Left $ ArgsToPostlude pterm $ undefined xs
      Nothing -> case Map.lookup n (global env) of
        Just (parameters -> params') ->
          if length params' == length args
            then do
              let localBinds = Map.fromList $ zip params' args
                  newEnv = env & #local %~ Map.union localBinds
              ref <- XXType2 . RuleRef n <$> traverse (resolveCTree newEnv) args
              Right . TOGType . Type0 . NE.singleton $
                Type1 ref Nothing undefined
            else Left $ MismatchingArgs n params'
        Nothing -> case Map.lookup n (local env) of
          Just _ -> Right . undefined . XXType2 $ GenericRef n
          Nothing -> Left $ UnboundReference n

resolveCTree ::
  BindingEnv OrReferenced OrReferenced ->
  Type1 OrReferenced ->
  Either NameResolutionFailure (TypeOrGroup DistReferenced)
resolveCTree e = undefined -- CTree.traverseCTree (resolveRef e) (resolveCTree e)

buildResolvedCTree ::
  PartialCTreeRoot OrReferenced ->
  Either NameResolutionFailure (PartialCTreeRoot DistReferenced)
buildResolvedCTree (PartialCTreeRoot ct) = undefined -- PartialCTreeRoot <$> traverse go ct
  where
    go pn =
      let argNames = parameters pn
          argTerms = (\x -> Type1 x Nothing undefined) . XXType2 . (\n -> Ref False n []) <$> argNames
          localBinds =
            Map.fromList $ zip argNames argTerms
          env = BindingEnv @OrReferenced @OrReferenced ct localBinds
       in undefined pn -- traverse (resolveCTree env) pn

--------------------------------------------------------------------------------
-- 4. Monomorphisation
--------------------------------------------------------------------------------

data MonoReferenced

data instance XTerm MonoReferenced = MonoReferencedXTerm
  deriving (Show)

data instance XCddl MonoReferenced = MonoReferencedXCddl
  deriving (Show)

newtype instance XXType2 MonoReferenced = MRuleRef Name
  deriving (Show)

newtype instance XXTopLevel MonoReferenced = MonoReferencedXXTopLevel Void
  deriving (Show)

type MonoEnv = BindingEnv DistReferenced MonoReferenced

-- | We introduce additional bindings in the state
type MonoState = Map.Map Name (TypeOrGroup MonoReferenced)

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
        (Map.Map Name (Type1 MonoReferenced))
    , HasReader
        "local"
        (Map.Map Name (Type1 MonoReferenced))
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
        (Map.Map Name (ProvidedParameters DistReferenced))
    , HasReader
        "global"
        (Map.Map Name (ProvidedParameters DistReferenced))
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
synthMono :: Name -> [TypeOrGroup DistReferenced] -> MonoM Name
synthMono n@(Name origName) args =
  let fresh =
        -- % is not a valid CBOR name, so this should avoid conflict
        Name $ origName <> "%" <> T.pack (show $ hash args)
   in do
        -- Lookup the original name in the global bindings
        globalBinds <- ask @"global"
        case Map.lookup n globalBinds of
          Just (ProvidedParameters [] _) -> throwNR $ MismatchingArgs n []
          Just (ProvidedParameters params' r) ->
            if length params' == length args
              then do
                rargs <- undefined -- traverse resolveGenericCTree args
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
resolveGenericRef (RuleRef n []) = pure . undefined $ MRuleRef n
resolveGenericRef (RuleRef n args) = do
  fresh <- synthMono n args
  pure . TOGType . Type0 . NE.singleton $ Type1 (XXType2 $ MRuleRef fresh) Nothing undefined
resolveGenericRef (GenericRef n) = do
  localBinds <- undefined -- ask @"local"
  case Map.lookup n localBinds of
    Just node -> pure node
    Nothing -> throwNR $ UnboundReference n
resolveGenericRef (DistPostlude _) = undefined

resolveGenericCTree ::
  TypeOrGroup DistReferenced ->
  MonoM (TypeOrGroup MonoReferenced)
resolveGenericCTree = undefined -- CTree.traverseCTree resolveGenericRef resolveGenericCTree

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
  undefined -- CTreeRoot . (`Map.union` newBindings) <$> r
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
  ( IndexMappable XTerm phase OrReferenced
  , IndexMappable XXType2 phase OrReferenced
  ) =>
  CDDL phase -> Either NameResolutionFailure (CTreeRoot MonoReferenced)
fullResolveCDDL cddl = do
  let refCTree = buildRefCTree (asMap cddl)
  rCTree <- buildResolvedCTree refCTree
  buildMonoCTree rCTree

-- |
--
--  CDDL predefines a number of names.  This subsection summarizes these
--  names, but please see Appendix D for the exact definitions.
--
--  The following keywords for primitive datatypes are defined:
--
--  "bool"  Boolean value (major type 7, additional information 20
--    or 21).
--
--  "uint"  An unsigned integer (major type 0).
--
--  "nint"  A negative integer (major type 1).
--
--  "int"  An unsigned integer or a negative integer.
--
--  "float16"  A number representable as a half-precision float [IEEE754]
--    (major type 7, additional information 25).
--
--  "float32"  A number representable as a single-precision float
--    [IEEE754] (major type 7, additional information 26).
--
--
--  "float64"  A number representable as a double-precision float
--    [IEEE754] (major type 7, additional information 27).
--
--  "float"  One of float16, float32, or float64.
--
--  "bstr" or "bytes"  A byte string (major type 2).
--
--  "tstr" or "text"  Text string (major type 3).
--
--  (Note that there are no predefined names for arrays or maps; these
--  are defined with the syntax given below.)
data PTerm
  = PTBool
  | PTUInt
  | PTNInt
  | PTInt
  | PTHalf
  | PTFloat
  | PTDouble
  | PTBytes
  | PTText
  | PTAny
  | PTNil
  | PTUndefined
  deriving (Eq, Generic, Ord, Show)

instance Hashable PTerm
