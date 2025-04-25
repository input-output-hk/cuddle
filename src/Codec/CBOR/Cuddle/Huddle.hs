{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Module for building CDDL in Haskell
--
-- Compared to the builders, this is less about creating a DSL for CDDL in
-- Haskell as about using Haskell's higher-level capabilities to express CDDL
-- constraints. So we ditch a bunch of CDDL concepts where we can instead use
-- Haskell's capabilities there.
module Codec.CBOR.Cuddle.Huddle (
  -- * Core Types
  Huddle,
  HuddleItem (..),
  huddleJoin,
  Rule,
  Named,
  IsType0 (..),
  Value (..),

  -- * Rules and assignment
  (=:=),
  (=:~),
  comment,

  -- * Maps
  (==>),
  mp,
  asKey,
  idx,

  -- * Arrays
  a,
  arr,

  -- * Groups
  Group,
  grp,

  -- * Quantification
  CanQuantify (..),
  opt,

  -- * Choices
  (/),
  seal,
  sarr,
  smp,

  -- * Literals
  Literal,
  bstr,
  int,
  text,

  -- * Ctl operators
  IsConstrainable,
  IsSizeable,
  sized,
  cbor,
  le,

  -- * Ranged
  (...),

  -- * Tagging
  tag,

  -- * Generics
  GRef,
  GRuleDef,
  GRuleCall,
  binding,
  binding2,
  callToDef,

  -- * Conversion to CDDL
  collectFrom,
  collectFromInit,
  toCDDL,
  toCDDLNoRoot,
)
where

import Codec.CBOR.Cuddle.CDDL (CDDL)
import Codec.CBOR.Cuddle.CDDL qualified as C
import Codec.CBOR.Cuddle.CDDL.CtlOp qualified as CtlOp
import Codec.CBOR.Cuddle.Comments qualified as C
import Control.Monad (when)
import Control.Monad.State (MonadState (get), execState, modify)
import Data.ByteString (ByteString)
import Data.Default.Class (Default (..))
import Data.Function (on)
import Data.Generics.Product (HasField' (field'), field, getField)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Ordered.Strict (OMap, (|<>))
import Data.Map.Ordered.Strict qualified as OMap
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Tuple.Optics (Field2 (..))
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Exts (IsList (Item, fromList, toList))
import GHC.Generics (Generic)
import Optics.Core (lens, view, (%~), (&), (.~), (^.))
import Prelude hiding ((/))

data Named a = Named
  { name :: T.Text
  , value :: a
  , description :: Maybe T.Text
  }
  deriving (Functor, Generic)

-- | Add a description to a rule or group entry, to be included as a comment.
comment :: HasField' "description" a (Maybe T.Text) => T.Text -> a -> a
comment desc n = n & field' @"description" .~ Just desc

instance Show (Named a) where
  show (Named n _ _) = T.unpack n

type Rule = Named Type0

data HuddleItem
  = HIRule Rule
  | HIGRule GRuleDef
  | HIGroup (Named Group)
  deriving (Generic, Show)

-- | Top-level Huddle type is a list of rules.
data Huddle = Huddle
  { roots :: [Rule]
  -- ^ Root elements
  , items :: OMap T.Text HuddleItem
  }
  deriving (Generic, Show)

huddleJoin :: Huddle -> Huddle -> Huddle
huddleJoin (Huddle rootsL itemsL) (Huddle rootsR itemsR) =
  Huddle (L.nubBy ((==) `on` name) $ rootsL <> rootsR) (itemsL |<> itemsR)

-- | This semigroup instance:
--   - Takes takes the roots from the RHS unless they are empty, in which case
--     it takes the roots from the LHS
--   - Uses the RHS to override items on the LHS where they share a name.
--     The value from the RHS is taken, but the index from the LHS is used.
--
--   Note that this allows replacing items in the middle of a tree without
--   updating higher-level items which make use of them - that is, we do not
--   need to "close over" higher-level terms, since by the time they have been
--   built into a huddle structure, the references have been converted to keys.
instance Semigroup Huddle where
  h1 <> h2 =
    Huddle
      { roots = case roots h2 of
          [] -> roots h1
          xs -> xs
      , items = OMap.unionWithL (\_ _ v2 -> v2) (items h1) (items h2)
      }

-- | This instance is mostly used for testing
instance IsList Huddle where
  type Item Huddle = Rule
  fromList [] = Huddle mempty OMap.empty
  fromList (x : xs) =
    (field @"items" %~ (OMap.|> (x ^. field @"name", HIRule x))) $ fromList xs

  toList = const []

instance Default Huddle where
  def = Huddle [] OMap.empty

data Choice a
  = NoChoice a
  | ChoiceOf a (Choice a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

choiceToList :: Choice a -> [a]
choiceToList (NoChoice x) = [x]
choiceToList (ChoiceOf x xs) = x : choiceToList xs

choiceToNE :: Choice a -> NE.NonEmpty a
choiceToNE (NoChoice c) = c NE.:| []
choiceToNE (ChoiceOf c cs) = c NE.:| choiceToList cs

data Key
  = LiteralKey Literal
  | TypeKey Type2
  deriving (Show)

-- | Instance for the very general case where we use text keys
instance IsString Key where
  fromString x = LiteralKey $ Literal (LText $ T.pack x) mempty

-- | Use a number as a key
idx :: Word64 -> Key
idx x = LiteralKey $ Literal (LInt x) mempty

asKey :: IsType0 r => r -> Key
asKey r = case toType0 r of
  NoChoice x -> TypeKey x
  ChoiceOf _ _ -> error "Cannot use a choice of types as a map key"

data MapEntry = MapEntry
  { key :: Key
  , value :: Type0
  , quantifier :: Occurs
  , meDescription :: C.Comment
  }
  deriving (Generic, Show)

instance C.HasComment MapEntry where
  commentL = lens meDescription (\x y -> x {meDescription = y})

newtype MapChoice = MapChoice {unMapChoice :: [MapEntry]}
  deriving (Show)

instance IsList MapChoice where
  type Item MapChoice = MapEntry

  fromList = MapChoice
  toList (MapChoice m) = m

type Map = Choice MapChoice

data ArrayEntry = ArrayEntry
  { key :: Maybe Key
  -- ^ Arrays can have keys, but they have no semantic meaning. We add them
  -- here because they can be illustrative in the generated CDDL.
  , value :: Type0
  , quantifier :: Occurs
  , aeDescription :: C.Comment
  }
  deriving (Generic, Show)

instance C.HasComment ArrayEntry where
  commentL = lens aeDescription (\x y -> x {aeDescription = y})

instance Num ArrayEntry where
  fromInteger i =
    ArrayEntry
      Nothing
      (NoChoice . T2Range . Unranged $ Literal (LInt (fromIntegral i)) mempty)
      def
      mempty
  (+) = error "Cannot treat ArrayEntry as a number"
  (*) = error "Cannot treat ArrayEntry as a number"
  abs = error "Cannot treat ArrayEntry as a number"
  signum = error "Cannot treat ArrayEntry as a number"
  negate = error "Cannot treat ArrayEntry as a number"

data ArrayChoice = ArrayChoice
  { unArrayChoice :: [ArrayEntry]
  , acComment :: C.Comment
  }
  deriving (Show)

instance Semigroup ArrayChoice where
  ArrayChoice x xc <> ArrayChoice y yc = ArrayChoice (x <> y) (xc <> yc)

instance Monoid ArrayChoice where
  mempty = ArrayChoice mempty mempty

instance C.HasComment ArrayChoice where
  commentL = lens acComment (\x y -> x {acComment = y})

instance IsList ArrayChoice where
  type Item ArrayChoice = ArrayEntry

  fromList = (`ArrayChoice` mempty)
  toList (ArrayChoice l _) = l

type Array = Choice ArrayChoice

newtype Group = Group {unGroup :: [ArrayEntry]}
  deriving (Show, Monoid, Semigroup)

instance IsList Group where
  type Item Group = ArrayEntry

  fromList = Group
  toList (Group l) = l

data Type2
  = T2Constrained Constrained
  | T2Range Ranged
  | T2Map Map
  | T2Array Array
  | T2Tagged (Tagged Type0)
  | T2Ref (Named Type0)
  | T2Group (Named Group)
  | -- | Call to a generic rule, binding arguments
    T2Generic GRuleCall
  | -- | Reference to a generic parameter within the body of the definition
    T2GenericRef GRef
  deriving (Show)

type Type0 = Choice Type2

instance Num Type0 where
  fromInteger i = NoChoice . T2Range . Unranged $ Literal (LInt (fromIntegral i)) mempty
  (+) = error "Cannot treat Type0 as a number"
  (*) = error "Cannot treat Type0 as a number"
  abs = error "Cannot treat Type0 as a number"
  signum = error "Cannot treat Type0 as a number"
  negate = error "Cannot treat Type0 as a number"

-- | Occurrence bounds.
data Occurs = Occurs
  { lb :: Maybe Word64
  , ub :: Maybe Word64
  }
  deriving (Eq, Generic, Show)

instance Default Occurs where
  def = Occurs Nothing Nothing

-- | Type-parametrised value type handling CBOR primitives. This is used to
-- constrain the set of constraints which can apply to a given postlude type.
data Value a where
  VBool :: Value Bool
  VUInt :: Value Int
  VNInt :: Value Int
  VInt :: Value Int
  VHalf :: Value Float
  VFloat :: Value Float
  VDouble :: Value Double
  VBytes :: Value ByteString
  VText :: Value T.Text
  VAny :: Value Void
  VNil :: Value Void

deriving instance Show (Value a)

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

data Literal = Literal
  { litVariant :: LiteralVariant
  , litComment :: C.Comment
  }
  deriving (Show)

instance C.HasComment Literal where
  commentL = lens litComment (\x y -> x {litComment = y})

data LiteralVariant where
  -- | We store both int and nint as a Word64, since the sign is indicated in
  -- the type.
  LInt :: Word64 -> LiteralVariant
  LNInt :: Word64 -> LiteralVariant
  LBignum :: Integer -> LiteralVariant
  LText :: T.Text -> LiteralVariant
  LFloat :: Float -> LiteralVariant
  LDouble :: Double -> LiteralVariant
  LBytes :: ByteString -> LiteralVariant
  deriving (Show)

int :: Integer -> Literal
int = inferInteger

bstr :: ByteString -> Literal
bstr x = Literal (LBytes x) mempty

text :: T.Text -> Literal
text x = Literal (LText x) mempty

inferInteger :: Integer -> Literal
inferInteger i
  | i >= 0 && i < fromIntegral (maxBound @Word64) = Literal (LInt (fromInteger i)) mempty
  | i < 0 && (-i) < fromIntegral (maxBound @Word64) = Literal (LNInt (fromInteger (-i))) mempty
  | otherwise = Literal (LBignum i) mempty

--------------------------------------------------------------------------------
-- Constraints and Ranges
--------------------------------------------------------------------------------

-- | A reference can be to any type, so we allow it to inhabit all
type AnyRef a = Named Type0

data Constrainable a
  = CValue (Value a)
  | CRef (AnyRef a)
  | CGRef GRef
  deriving (Show)

-- | Uninhabited type used as marker for the type of thing a CRef sizes
data CRefType

-- | Uninhabited type used as marker for the type of thing a CGRef sizes
data CGRefType

-- | We only allow constraining basic values, or references. Of course, we
--   can't check what the references refer to.
data Constrained where
  Constrained ::
    forall a.
    { value :: Constrainable a
    , constraint :: ValueConstraint a
    , refs :: [Rule]
    -- ^ Sometimes constraints reference rules. In this case we need to
    -- collect the references in order to traverse them when collecting all
    -- relevant rules.
    } ->
    Constrained

deriving instance Show Constrained

class IsConstrainable a x | a -> x where
  toConstrainable :: a -> Constrainable x

instance IsConstrainable (AnyRef a) CRefType where
  toConstrainable = CRef

instance IsConstrainable (Value a) a where
  toConstrainable = CValue

instance IsConstrainable GRef CGRefType where
  toConstrainable = CGRef

unconstrained :: Value a -> Constrained
unconstrained v = Constrained (CValue v) def []

-- | A constraint on a 'Value' is something applied via CtlOp or RangeOp on a
-- Type2, forming a Type1.
data ValueConstraint a = ValueConstraint
  { applyConstraint :: C.Type2 -> C.Type1
  , showConstraint :: String
  }

instance Show (ValueConstraint a) where
  show = showConstraint

instance Default (ValueConstraint a) where
  def =
    ValueConstraint
      { applyConstraint = \x -> C.Type1 x Nothing mempty
      , showConstraint = ""
      }

-- | Marker that we can apply the size CtlOp to something. Not intended for
-- export.
class IsSizeable a

instance IsSizeable Int

instance IsSizeable ByteString

instance IsSizeable T.Text

instance IsSizeable CRefType

instance IsSizeable CGRefType

-- | Things which can be used on the RHS of the '.size' operator.
class IsSize a where
  sizeAsCDDL :: a -> C.Type2
  sizeAsString :: a -> String

instance IsSize Word where
  sizeAsCDDL x = C.T2Value $ C.Value (C.VUInt $ fromIntegral x) mempty
  sizeAsString = show

instance IsSize Word64 where
  sizeAsCDDL x = C.T2Value $ C.Value (C.VUInt x) mempty
  sizeAsString = show

instance IsSize (Word64, Word64) where
  sizeAsCDDL (x, y) =
    C.T2Group
      ( C.Type0
          ( C.Type1
              (C.T2Value (C.Value (C.VUInt x) mempty))
              (Just (C.RangeOp C.Closed, C.T2Value (C.Value (C.VUInt y) mempty)))
              mempty
              NE.:| []
          )
      )
  sizeAsString (x, y) = show x <> ".." <> show y

-- | Declare a size constraint on an int-style type or reference.
--   Since 0.3.4 this has worked for reference types as well as values.
sized ::
  forall c a s.
  ( IsSizeable a
  , IsSize s
  , IsConstrainable c a
  ) =>
  c ->
  s ->
  Constrained
sized v sz =
  Constrained
    (toConstrainable @c @a v)
    ValueConstraint
      { applyConstraint = \t2 ->
          C.Type1
            t2
            (Just (C.CtrlOp CtlOp.Size, sizeAsCDDL sz))
            mempty
      , showConstraint = ".size " <> sizeAsString sz
      }
    []

class IsCborable a
instance IsCborable ByteString
instance IsCborable (AnyRef a)
instance IsCborable GRef

cbor :: (IsCborable b, IsConstrainable c b) => c -> Rule -> Constrained
cbor v r@(Named n _ _) =
  Constrained
    (toConstrainable v)
    ValueConstraint
      { applyConstraint = \t2 ->
          C.Type1
            t2
            (Just (C.CtrlOp CtlOp.Cbor, C.T2Name (C.Name n mempty) Nothing))
            mempty
      , showConstraint = ".cbor " <> T.unpack n
      }
    [r]

class IsComparable a
instance IsComparable Int
instance IsComparable (AnyRef a)
instance IsComparable GRef

le :: (IsComparable a, IsConstrainable c a) => c -> Word64 -> Constrained
le v bound =
  Constrained
    (toConstrainable v)
    ValueConstraint
      { applyConstraint = \t2 ->
          C.Type1
            t2
            (Just (C.CtrlOp CtlOp.Le, C.T2Value (C.Value (C.VUInt $ fromIntegral bound) mempty)))
            mempty
      , showConstraint = ".le " <> show bound
      }
    []

-- Ranges

data RangeBound
  = RangeBoundLiteral Literal
  | RangeBoundRef (Named Type0)
  deriving (Show)

class IsRangeBound a where
  toRangeBound :: a -> RangeBound

instance IsRangeBound Literal where
  toRangeBound = RangeBoundLiteral

instance IsRangeBound Integer where
  toRangeBound = RangeBoundLiteral . inferInteger

instance IsRangeBound (Named Type0) where
  toRangeBound = RangeBoundRef

data Ranged where
  Ranged ::
    { lb :: RangeBound
    , ub :: RangeBound
    , bounds :: C.RangeBound
    } ->
    Ranged
  Unranged :: Literal -> Ranged
  deriving (Show)

-- | Establish a closed range bound.
(...) :: (IsRangeBound a, IsRangeBound b) => a -> b -> Ranged
l ... u = Ranged (toRangeBound l) (toRangeBound u) C.Closed

infixl 9 ...

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

class IsType0 a where
  toType0 :: a -> Type0

instance IsType0 Rule where
  toType0 = NoChoice . T2Ref

instance IsType0 (Choice Type2) where
  toType0 = id

instance IsType0 Constrained where
  toType0 = NoChoice . T2Constrained

instance IsType0 Map where
  toType0 = NoChoice . T2Map

instance IsType0 MapChoice where
  toType0 = NoChoice . T2Map . NoChoice

instance IsType0 Array where
  toType0 = NoChoice . T2Array

instance IsType0 ArrayChoice where
  toType0 = NoChoice . T2Array . NoChoice

instance IsType0 Ranged where
  toType0 = NoChoice . T2Range

instance IsType0 Literal where
  toType0 = NoChoice . T2Range . Unranged

-- We also allow going directly from primitive types to Type2
instance IsType0 Integer where
  toType0 = NoChoice . T2Range . Unranged . inferInteger

instance IsType0 T.Text where
  toType0 :: T.Text -> Type0
  toType0 x = NoChoice . T2Range . Unranged $ Literal (LText x) mempty

instance IsType0 ByteString where
  toType0 x = NoChoice . T2Range . Unranged $ Literal (LBytes x) mempty

instance IsType0 Float where
  toType0 x = NoChoice . T2Range . Unranged $ Literal (LFloat x) mempty

instance IsType0 Double where
  toType0 x = NoChoice . T2Range . Unranged $ Literal (LDouble x) mempty

instance IsType0 (Value a) where
  toType0 = NoChoice . T2Constrained . unconstrained

instance IsType0 (Named Group) where
  toType0 = NoChoice . T2Group

instance IsType0 GRuleCall where
  toType0 = NoChoice . T2Generic

instance IsType0 GRef where
  toType0 = NoChoice . T2GenericRef

instance IsType0 a => IsType0 (Tagged a) where
  toType0 = NoChoice . T2Tagged . fmap toType0

instance IsType0 HuddleItem where
  toType0 (HIRule r) = toType0 r
  toType0 (HIGroup g) = toType0 g
  toType0 (HIGRule g) =
    error $
      "Attempt to reference generic rule from HuddleItem not supported: " <> show g

class CanQuantify a where
  -- | Apply a lower bound
  (<+) :: Word64 -> a -> a

  -- | Apply an upper bound
  (+>) :: a -> Word64 -> a

infixl 7 <+

infixr 6 +>

opt :: CanQuantify a => a -> a
opt r = 0 <+ r +> 1

instance CanQuantify Occurs where
  lb <+ (Occurs _ ub) = Occurs (Just lb) ub
  (Occurs lb _) +> ub = Occurs lb (Just ub)

instance CanQuantify ArrayEntry where
  lb <+ ae = ae & field @"quantifier" %~ (lb <+)
  ae +> ub = ae & field @"quantifier" %~ (+> ub)

instance CanQuantify MapEntry where
  lb <+ ae = ae & field @"quantifier" %~ (lb <+)
  ae +> ub = ae & field @"quantifier" %~ (+> ub)

-- | A quantifier on a choice can be rewritten as a choice of quantifiers
instance CanQuantify a => CanQuantify (Choice a) where
  lb <+ c = fmap (lb <+) c
  c +> ub = fmap (+> ub) c

class IsEntryLike a where
  fromMapEntry :: MapEntry -> a

instance IsEntryLike MapEntry where
  fromMapEntry = id

instance IsEntryLike ArrayEntry where
  fromMapEntry me =
    ArrayEntry
      { key = Just $ getField @"key" me
      , value =
          getField @"value" me
      , quantifier = getField @"quantifier" me
      , aeDescription = mempty
      }

instance IsEntryLike Type0 where
  fromMapEntry = getField @"value"

(==>) :: (IsType0 a, IsEntryLike me) => Key -> a -> me
k ==> gc =
  fromMapEntry
    MapEntry
      { key = k
      , value = toType0 gc
      , quantifier = def
      , meDescription = mempty
      }

infixl 8 ==>

-- | Assign a rule
(=:=) :: IsType0 a => T.Text -> a -> Rule
n =:= b = Named n (toType0 b) Nothing

infixl 1 =:=

(=:~) :: T.Text -> Group -> Named Group
n =:~ b = Named n b Nothing

infixl 1 =:~

class IsGroupOrArrayEntry a where
  toGroupOrArrayEntry :: IsType0 x => x -> a

instance IsGroupOrArrayEntry ArrayEntry where
  toGroupOrArrayEntry x =
    ArrayEntry
      { key = Nothing
      , value = toType0 x
      , quantifier = def
      , aeDescription = mempty
      }

instance IsGroupOrArrayEntry Type0 where
  toGroupOrArrayEntry = toType0

-- | Explicitly cast an item in an Array as an ArrayEntry.
a :: (IsType0 a, IsGroupOrArrayEntry e) => a -> e
a = toGroupOrArrayEntry

--------------------------------------------------------------------------------
-- Choices
--------------------------------------------------------------------------------
class IsChoosable a b | a -> b where
  toChoice :: a -> Choice b

instance IsChoosable (Choice a) a where
  toChoice = id

instance IsChoosable ArrayChoice ArrayChoice where
  toChoice = NoChoice

instance IsChoosable MapChoice MapChoice where
  toChoice = NoChoice

instance IsChoosable Type2 Type2 where
  toChoice = NoChoice

instance IsChoosable Rule Type2 where
  toChoice = toChoice . T2Ref

instance IsChoosable GRuleCall Type2 where
  toChoice = toChoice . T2Generic

instance IsChoosable GRef Type2 where
  toChoice = toChoice . T2GenericRef

instance IsChoosable ByteString Type2 where
  toChoice x = toChoice . T2Range . Unranged $ Literal (LBytes x) mempty

instance IsChoosable Constrained Type2 where
  toChoice = toChoice . T2Constrained

instance IsType0 a => IsChoosable (Tagged a) Type2 where
  toChoice = toChoice . T2Tagged . fmap toType0

instance IsChoosable Literal Type2 where
  toChoice = toChoice . T2Range . Unranged

instance IsChoosable (Value a) Type2 where
  toChoice = toChoice . T2Constrained . unconstrained

instance IsChoosable (Named Group) Type2 where
  toChoice = toChoice . T2Group

instance IsChoosable (Seal Array) Type2 where
  toChoice (Seal x) = NoChoice $ T2Array x

instance IsChoosable (Seal Map) Type2 where
  toChoice (Seal m) = NoChoice $ T2Map m

instance IsChoosable (Seal ArrayChoice) Type2 where
  toChoice (Seal m) = NoChoice . T2Array $ NoChoice m

instance IsChoosable (Seal MapChoice) Type2 where
  toChoice (Seal m) = NoChoice . T2Map $ NoChoice m

-- | Allow choices between constructions
--
-- in CDDL, '/'  a choice between types (concretely, between Type1 values, to
-- make a Type0). '//' allows choice between groups. We can illustrate the
-- difference with the following snippet:
--
-- @ foo = [ 0 / 1, uint // 2 /3, tstr ] @
--
-- This construction would match either of the following:
--
-- @ [0, 3] [2, "Hello World"] @
--
-- In other words, the '//' binds less strongly than comma (',') in CDDL.
--
-- In Haskell, of course, we cannot have syntax inside an array which binds
-- stronger than the comma. so we have to do things a little differently. The
-- way this is handled at the moment is that '/' has special treatment for
-- arrays/groups, where it will, instead of creating a type-level choice, merge
-- the two arrays/groups/maps into a single one containing a group choice.
--
-- If one instead wants the behaviour corresponding to the CDDL '/' for arrays,
-- maps or groups, one can "seal" the array or group using the 'seal', 'sarr' or
-- 'smp' functions. For example:
--
-- @ "foo" =:= sarr [0, a VUInt] / sarr [1, a VText] @
--
-- Generates a choice (at the 'Type0') level between two arrays, whereas
--
-- @ "foo" =:= arr [0, a VUInt] / arr [1, a VUInt] @
--
-- will generate a single array containing a group choice between two groups.
--
-- As such, there is no `//` operator in Huddle.
(/) :: (IsChoosable a c, IsChoosable b c) => a -> b -> Choice c
x / b = go (toChoice x) (toChoice b)
  where
    go (NoChoice x') b' = ChoiceOf x' b'
    go (ChoiceOf x' b') c = ChoiceOf x' (go b' c)

infixl 9 /

-- Choices within maps or arrays
--
-- Maps and arrays allow an "internal" choice - as per [1, 'a' // 2, 'b']. This
-- means that the array can be either [1, 'a'] or [2, 'b']. Since this would not
-- work within Haskell's array syntax, we instead pull the option outside of the
-- array, as with [1, 'a'] // [2, 'b'].
--
-- This, however, leaves us with a problem. When we write [1, 'a'] // [2, 'b']
-- we have two possible interpretations - as a top-level choice (in CDDL terms,
-- a choice in the 'Type0'. In Huddle terms, as a Choice Array) or as a choice
-- inside the array (in CDDL terms, a choice inside the Group. In Huddle terms,
-- as a Choice ArrayChoice (itself an Array!)).
--
-- To resolve this, we allow "sealing" an array or map. A sealed array or map
-- will no longer absorb (//).

newtype Seal a = Seal a

-- | Seal an array or map, indicating that it will no longer absorb (//). This
-- is needed if you wish to include an array or map inside a top-level choice.
seal :: a -> Seal a
seal = Seal

-- | This function is used solely to resolve type inference by explicitly
-- identifying something as an array.
arr :: ArrayChoice -> ArrayChoice
arr = id

-- | Create and seal an array, marking it as accepting no additional choices
sarr :: ArrayChoice -> Seal Array
sarr = seal . NoChoice

mp :: MapChoice -> MapChoice
mp = id

-- | Create and seal a map, marking it as accepting no additional choices.
smp :: MapChoice -> Seal Map
smp = seal . NoChoice

grp :: Group -> Group
grp = id

--------------------------------------------------------------------------------
-- Tagged types
--------------------------------------------------------------------------------

-- | A tagged type carries an optional tag
data Tagged a = Tagged (Maybe Word64) a
  deriving (Show, Functor)

-- | Tag a CBOR item with a CDDL minor type. Thus, `tag n x` is equivalent to
-- `#6.n(x)` in CDDL.
tag :: Word64 -> a -> Tagged a
tag mi = Tagged (Just mi)

--------------------------------------------------------------------------------
-- Generics
--------------------------------------------------------------------------------

newtype GRef = GRef T.Text
  deriving (Show)

freshName :: Int -> GRef
freshName ix =
  GRef $
    T.singleton (['a' .. 'z'] !! (ix `rem` 26))
      <> T.pack (show $ ix `quot` 26)

data GRule a = GRule
  { args :: NE.NonEmpty a
  , body :: Type0
  }
  deriving (Show)

type GRuleCall = Named (GRule Type2)

type GRuleDef = Named (GRule GRef)

callToDef :: GRule Type2 -> GRule GRef
callToDef gr = gr {args = refs}
  where
    refs =
      NE.unfoldr
        ( \ix ->
            ( freshName ix
            , if ix < NE.length (args gr) - 1 then Just (ix + 1) else Nothing
            )
        )
        0

-- | Bind a single variable into a generic call
binding :: IsType0 t0 => (GRef -> Rule) -> t0 -> GRuleCall
binding fRule t0 =
  Named
    (name rule)
    GRule
      { args = t2 NE.:| []
      , body = getField @"value" rule
      }
    Nothing
  where
    rule = fRule (freshName 0)
    t2 = case toType0 t0 of
      NoChoice x -> x
      _ -> error "Cannot use a choice of types as a generic argument"

-- | Bind two variables as a generic call
binding2 :: (IsType0 t0, IsType0 t1) => (GRef -> GRef -> Rule) -> t0 -> t1 -> GRuleCall
binding2 fRule t0 t1 =
  Named
    (name rule)
    GRule
      { args = t02 NE.:| [t12]
      , body = getField @"value" rule
      }
    Nothing
  where
    rule = fRule (freshName 0) (freshName 1)
    t02 = case toType0 t0 of
      NoChoice x -> x
      _ -> error "Cannot use a choice of types as a generic argument"
    t12 = case toType0 t1 of
      NoChoice x -> x
      _ -> error "Cannot use a choice of types as a generic argument"

--------------------------------------------------------------------------------
-- Collecting all top-level rules
--------------------------------------------------------------------------------

hiRule :: HuddleItem -> [Rule]
hiRule (HIRule r) = [r]
hiRule _ = []

hiName :: HuddleItem -> T.Text
hiName (HIRule (Named n _ _)) = n
hiName (HIGroup (Named n _ _)) = n
hiName (HIGRule (Named n _ _)) = n

-- | Collect all rules starting from a given point. This will also insert a
--   single pseudo-rule as the first element which references the specified
--   top-level rules.
collectFrom :: [HuddleItem] -> Huddle
collectFrom topRs =
  toHuddle $
    execState
      (traverse goHuddleItem topRs)
      OMap.empty
  where
    toHuddle items =
      Huddle
        { roots = concatMap hiRule topRs
        , items = items
        }
    goHuddleItem (HIRule r) = goRule r
    goHuddleItem (HIGroup g) = goNamedGroup g
    goHuddleItem (HIGRule (Named _ (GRule _ t0) _)) = goT0 t0
    goRule r@(Named n t0 _) = do
      items <- get
      when (OMap.notMember n items) $ do
        modify (OMap.|> (n, HIRule r))
        goT0 t0
    goChoice f (NoChoice x) = f x
    goChoice f (ChoiceOf x xs) = f x >> goChoice f xs
    goT0 = goChoice goT2
    goNamedGroup r@(Named n g _) = do
      items <- get
      when (OMap.notMember n items) $ do
        modify (OMap.|> (n, HIGroup r))
        goGroup g
    goGRule r@(Named n g _) = do
      items <- get
      when (OMap.notMember n items) $ do
        modify (OMap.|> (n, HIGRule $ fmap callToDef r))
        goT0 (body g)
      -- Note that the parameters here may be different, so this doesn't live
      -- under the guard
      mapM_ goT2 $ args g
    goT2 (T2Range r) = goRanged r
    goT2 (T2Map m) = goChoice (mapM_ goMapEntry . unMapChoice) m
    goT2 (T2Array m) = goChoice (mapM_ goArrayEntry . unArrayChoice) m
    goT2 (T2Tagged (Tagged _ t0)) = goT0 t0
    goT2 (T2Ref n) = goRule n
    goT2 (T2Group r) = goNamedGroup r
    goT2 (T2Generic x) = goGRule x
    goT2 (T2Constrained (Constrained c _ refs)) =
      ( case c of
          CValue _ -> pure ()
          CRef r -> goRule r
          CGRef _ -> pure ()
      )
        >> mapM_ goRule refs
    goT2 _ = pure ()
    goArrayEntry (ArrayEntry (Just k) t0 _ _) = goKey k >> goT0 t0
    goArrayEntry (ArrayEntry Nothing t0 _ _) = goT0 t0
    goMapEntry (MapEntry k t0 _ _) = goKey k >> goT0 t0
    goKey (TypeKey k) = goT2 k
    goKey _ = pure ()
    goGroup (Group g) = mapM_ goArrayEntry g
    goRanged (Unranged _) = pure ()
    goRanged (Ranged lb ub _) = goRangeBound lb >> goRangeBound ub
    goRangeBound (RangeBoundLiteral _) = pure ()
    goRangeBound (RangeBoundRef r) = goRule r

-- | Same as `collectFrom`, but the rules passed into this function will be put
--   at the top of the Huddle, and all of their dependencies will be added at
--   the end in depth-first order.
collectFromInit :: [HuddleItem] -> Huddle
collectFromInit rules =
  Huddle (concatMap hiRule rules) (OMap.fromList $ (\x -> (hiName x, x)) <$> rules)
    `huddleJoin` collectFrom rules

--------------------------------------------------------------------------------
-- Conversion to CDDL
--------------------------------------------------------------------------------

-- | Convert from Huddle to CDDL, generating a top level root element.
toCDDL :: Huddle -> CDDL
toCDDL = toCDDL' True

-- | Convert from Huddle to CDDL, skipping a root element.
toCDDLNoRoot :: Huddle -> CDDL
toCDDLNoRoot = toCDDL' False

-- | Convert from Huddle to CDDL for the purpose of pretty-printing.
toCDDL' :: Bool -> Huddle -> CDDL
toCDDL' mkPseudoRoot hdl =
  C.fromRules
    $ ( if mkPseudoRoot
          then (toTopLevelPseudoRoot (roots hdl) NE.<|)
          else id
      )
    $ fmap toCDDLItem (NE.fromList $ fmap (view _2) $ toList $ items hdl)
  where
    toCDDLItem (HIRule r) = toCDDLRule r
    toCDDLItem (HIGroup g) = toCDDLGroup g
    toCDDLItem (HIGRule g) = toGenRuleDef g
    toTopLevelPseudoRoot :: [Rule] -> C.Rule
    toTopLevelPseudoRoot topRs =
      toCDDLRule $
        comment "Pseudo-rule introduced by Cuddle to collect root elements" $
          "huddle_root_defs" =:= arr (fromList (fmap a topRs))
    toCDDLRule :: Rule -> C.Rule
    toCDDLRule (Named n t0 c) =
      (\x -> C.Rule (C.Name n mempty) Nothing C.AssignEq x (foldMap C.Comment c))
        . C.TOGType
        . C.Type0
        $ toCDDLType1 <$> choiceToNE t0
    toCDDLValue :: Literal -> C.Value
    toCDDLValue (Literal x cmt) = C.Value (toCDDLValue' x) cmt
    toCDDLValue' (LInt i) = C.VUInt i
    toCDDLValue' (LNInt i) = C.VNInt i
    toCDDLValue' (LBignum i) = C.VBignum i
    toCDDLValue' (LFloat i) = C.VFloat32 i
    toCDDLValue' (LDouble d) = C.VFloat64 d
    toCDDLValue' (LText t) = C.VText t
    toCDDLValue' (LBytes b) = C.VBytes b

    mapToCDDLGroup :: Map -> C.Group
    mapToCDDLGroup xs = C.Group $ mapChoiceToCDDL <$> choiceToNE xs

    mapChoiceToCDDL :: MapChoice -> C.GrpChoice
    mapChoiceToCDDL (MapChoice entries) = C.GrpChoice (fmap mapEntryToCDDL entries) mempty

    mapEntryToCDDL :: MapEntry -> C.GroupEntry
    mapEntryToCDDL (MapEntry k v occ cmnt) =
      C.GroupEntry
        (toOccurrenceIndicator occ)
        cmnt
        (C.GEType (Just $ toMemberKey k) (toCDDLType0 v))

    toOccurrenceIndicator :: Occurs -> Maybe C.OccurrenceIndicator
    toOccurrenceIndicator (Occurs Nothing Nothing) = Nothing
    toOccurrenceIndicator (Occurs (Just 0) (Just 1)) = Just C.OIOptional
    toOccurrenceIndicator (Occurs (Just 0) Nothing) = Just C.OIZeroOrMore
    toOccurrenceIndicator (Occurs (Just 1) Nothing) = Just C.OIOneOrMore
    toOccurrenceIndicator (Occurs lb ub) = Just $ C.OIBounded lb ub

    toCDDLType1 :: Type2 -> C.Type1
    toCDDLType1 = \case
      T2Constrained (Constrained x constr _) ->
        -- TODO Need to handle choices at the top level
        applyConstraint constr (C.T2Name (toCDDLConstrainable x) Nothing)
      T2Range l -> toCDDLRanged l
      T2Map m ->
        C.Type1
          (C.T2Map $ mapToCDDLGroup m)
          Nothing
          mempty
      T2Array x -> C.Type1 (C.T2Array $ arrayToCDDLGroup x) Nothing mempty
      T2Tagged (Tagged mmin x) ->
        C.Type1 (C.T2Tag mmin $ toCDDLType0 x) Nothing mempty
      T2Ref (Named n _ _) -> C.Type1 (C.T2Name (C.Name n mempty) Nothing) Nothing mempty
      T2Group (Named n _ _) -> C.Type1 (C.T2Name (C.Name n mempty) Nothing) Nothing mempty
      T2Generic g -> C.Type1 (toGenericCall g) Nothing mempty
      T2GenericRef (GRef n) -> C.Type1 (C.T2Name (C.Name n mempty) Nothing) Nothing mempty

    toMemberKey :: Key -> C.MemberKey
    toMemberKey (LiteralKey (Literal (LText t) _)) = C.MKBareword (C.Name t mempty)
    toMemberKey (LiteralKey v) = C.MKValue $ toCDDLValue v
    toMemberKey (TypeKey t) = C.MKType (toCDDLType1 t)

    toCDDLType0 :: Type0 -> C.Type0
    toCDDLType0 = C.Type0 . fmap toCDDLType1 . choiceToNE

    arrayToCDDLGroup :: Array -> C.Group
    arrayToCDDLGroup xs = C.Group $ arrayChoiceToCDDL <$> choiceToNE xs

    arrayChoiceToCDDL :: ArrayChoice -> C.GrpChoice
    arrayChoiceToCDDL (ArrayChoice entries cmt) = C.GrpChoice (fmap arrayEntryToCDDL entries) cmt

    arrayEntryToCDDL :: ArrayEntry -> C.GroupEntry
    arrayEntryToCDDL (ArrayEntry k v occ cmnt) =
      C.GroupEntry
        (toOccurrenceIndicator occ)
        cmnt
        (C.GEType (fmap toMemberKey k) (toCDDLType0 v))

    toCDDLPostlude :: Value a -> C.Name
    toCDDLPostlude VBool = C.Name "bool" mempty
    toCDDLPostlude VUInt = C.Name "uint" mempty
    toCDDLPostlude VNInt = C.Name "nint" mempty
    toCDDLPostlude VInt = C.Name "int" mempty
    toCDDLPostlude VHalf = C.Name "half" mempty
    toCDDLPostlude VFloat = C.Name "float" mempty
    toCDDLPostlude VDouble = C.Name "double" mempty
    toCDDLPostlude VBytes = C.Name "bytes" mempty
    toCDDLPostlude VText = C.Name "text" mempty
    toCDDLPostlude VAny = C.Name "any" mempty
    toCDDLPostlude VNil = C.Name "nil" mempty

    toCDDLConstrainable c = case c of
      CValue v -> toCDDLPostlude v
      CRef r -> C.Name (name r) mempty
      CGRef (GRef n) -> C.Name n mempty

    toCDDLRanged :: Ranged -> C.Type1
    toCDDLRanged (Unranged x) =
      C.Type1 (C.T2Value $ toCDDLValue x) Nothing mempty
    toCDDLRanged (Ranged lb ub rop) =
      C.Type1
        (toCDDLRangeBound lb)
        (Just (C.RangeOp rop, toCDDLRangeBound ub))
        mempty

    toCDDLRangeBound :: RangeBound -> C.Type2
    toCDDLRangeBound (RangeBoundLiteral l) = C.T2Value $ toCDDLValue l
    toCDDLRangeBound (RangeBoundRef (Named n _ _)) = C.T2Name (C.Name n mempty) Nothing

    toCDDLGroup :: Named Group -> C.Rule
    toCDDLGroup (Named n (Group t0s) c) =
      C.Rule
        (C.Name n mempty)
        Nothing
        C.AssignEq
        ( C.TOGGroup
            . C.GroupEntry Nothing mempty
            . C.GEGroup
            . C.Group
            . (NE.:| [])
            . (`C.GrpChoice` mempty)
            $ fmap
              arrayEntryToCDDL
              t0s
        )
        (foldMap C.Comment c)

    toGenericCall :: GRuleCall -> C.Type2
    toGenericCall (Named n gr _) =
      C.T2Name
        (C.Name n mempty)
        (Just . C.GenericArg $ fmap toCDDLType1 (args gr))

    toGenRuleDef :: GRuleDef -> C.Rule
    toGenRuleDef (Named n gr c) =
      C.Rule
        (C.Name n mempty)
        (Just gps)
        C.AssignEq
        ( C.TOGType
            . C.Type0
            $ toCDDLType1 <$> choiceToNE (body gr)
        )
        (foldMap C.Comment c)
      where
        gps =
          C.GenericParam $ fmap (\(GRef t) -> C.Name t mempty) (args gr)
