{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Module for building CDDL in Haskell
--
-- Compared to the builders, this is less about creating a DSL for CDDL in
-- Haskell as about using Haskell's higher-level capabilities to express CDDL
-- constraints. So we ditch a bunch of CDDL concepts where we can instead use
-- Haskell's capabilities there.
module Codec.CBOR.Cuddle.Huddle
  ( Array,
    ArrayChoice,
    (==>),
    (##),
    OrRef (Ref),
    toCDDL,
    (=:=),
    Value (..),
    CanQuantify (..),
    a,
    (//),
    arr,
    mp,
  )
where

import Codec.CBOR.Cuddle.CDDL (CDDL)
import Codec.CBOR.Cuddle.CDDL qualified as C
import Data.ByteString (ByteString)
import Data.Default.Class (Default (..))
import Data.Generics.Product (field)
import Data.List.NonEmpty qualified as NE
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Generics (Generic)
import GHC.IsList (IsList (Item, fromList, toList))
import Optics.Core ((%~), (&))

data Named a = Named T.Text a
  deriving (Functor)

instance Show (Named a) where
  show (Named n _) = show n

type Rule = Named (Choice Type0)

-- | Top-level Huddle type is a list of rules.
newtype Huddle = Huddle {unHuddle :: [Rule]}
  deriving (Show)

instance IsList Huddle where
  type Item Huddle = Rule
  fromList = Huddle
  toList (Huddle h) = h

data OrRef a
  = Val a
  | Ref (Named a)
  deriving (Show, Functor)

data Choice a
  = NoChoice a
  | ChoiceOf a (Choice a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

choiceToList :: Choice a -> [a]
choiceToList (NoChoice x) = [x]
choiceToList (ChoiceOf x xs) = x : choiceToList xs

choiceToNE :: Choice a -> NE.NonEmpty a
choiceToNE (NoChoice c) = NE.singleton c
choiceToNE (ChoiceOf c cs) = c NE.:| choiceToList cs

data Key
  = LiteralKey Literal
  | TypeKey (OrRef Type0)
  deriving (Show)

-- | Instance for the very general case where we use text keys
instance IsString Key where
  fromString = LiteralKey . LText . T.pack

data MapEntry = MapEntry
  { key :: Key,
    value :: OrRef (Choice Type0),
    quantifier :: Occurs
  }
  deriving (Generic, Show)

newtype MapChoice = MapChoice [MapEntry]
  deriving (Show)

instance IsList MapChoice where
  type Item MapChoice = MapEntry

  fromList = MapChoice
  toList (MapChoice m) = m

type Map = Choice MapChoice

data ArrayEntry = ArrayEntry
  { -- | Arrays can have keys, but they have no semantic meaning. We add them here because they can be illustrative in the generated CDDL.
    key :: Maybe Key,
    value :: OrRef (Choice Type0),
    quantifier :: Occurs
  }
  deriving (Generic, Show)

newtype ArrayChoice = ArrayChoice {unArrayChoice :: [ArrayEntry]}
  deriving (Show, Monoid, Semigroup)

instance IsList ArrayChoice where
  type Item ArrayChoice = ArrayEntry

  fromList = ArrayChoice
  toList (ArrayChoice l) = l

type Array = Choice ArrayChoice

data Type0
  = T0Basic Constrained
  | T0Literal Literal
  | T0Map Map
  | T0Array Array
  deriving (Show)

-- | Occurrence bounds.
data Occurs = Occurs
  { lb :: Maybe Int,
    ub :: Maybe Int
  }
  deriving (Eq, Generic, Show)

instance Default Occurs where
  def = Occurs Nothing Nothing

-- | We only allow constraining basic values
data Constrained where
  Constrained ::
    forall a.
    { value :: Value a,
      constraint :: ValueConstraint a
    } ->
    Constrained

deriving instance Show Constrained

unconstrained :: Value a -> Constrained
unconstrained v = Constrained v def

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

-- | A constraint on a 'Value' is something applied via CtlOp or RangeOp on a
-- Type2, forming a Type1.
data ValueConstraint a = ValueConstraint
  { applyConstraint :: C.Type2 -> C.Type1,
    showConstraint :: String
  }

instance Show (ValueConstraint a) where
  show x = x.showConstraint

instance Default (ValueConstraint a) where
  def =
    ValueConstraint
      { applyConstraint = (`C.Type1` Nothing),
        showConstraint = ""
      }

data Literal where
  LInt :: Int -> Literal
  LText :: T.Text -> Literal
  LFloat :: Float -> Literal
  LDouble :: Double -> Literal
  LBytes :: ByteString -> Literal
  deriving (Show)

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

class IsType0 a where
  toType0 :: a -> Type0

instance IsType0 Constrained where
  toType0 = T0Basic

instance IsType0 Map where
  toType0 = T0Map

instance IsType0 MapChoice where
  toType0 = T0Map . NoChoice

instance IsType0 Array where
  toType0 = T0Array

instance IsType0 ArrayChoice where
  toType0 = T0Array . NoChoice

-- We also allow going directly from primitive types to Type0
instance IsType0 Int where
  toType0 = T0Literal . LInt

instance IsType0 T.Text where
  toType0 :: T.Text -> Type0
  toType0 = T0Literal . LText

instance IsType0 ByteString where
  toType0 = T0Literal . LBytes

instance IsType0 Float where
  toType0 = T0Literal . LFloat

instance IsType0 Double where
  toType0 = T0Literal . LDouble

instance IsType0 (Value a) where
  toType0 = T0Basic . unconstrained

class CanQuantify a where
  -- | Apply a lower bound
  (<+) :: Int -> a -> a

  -- | Apply an upper bound
  (+>) :: a -> Int -> a

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
instance (CanQuantify a) => CanQuantify (Choice a) where
  lb <+ c = fmap (lb <+) c
  c +> ub = fmap (+> ub) c

-- | Indicates that something can be seen as a reference to a choice of 'Type0'
-- entities.
class IsRefType a where
  toRefType :: a -> OrRef (Choice Type0)

instance IsRefType Type0 where
  toRefType = Val . NoChoice

instance IsRefType (Choice Type0) where
  toRefType = Val

instance IsRefType (OrRef (Choice Type0)) where
  toRefType = id

instance IsRefType (OrRef Type0) where
  toRefType = fmap NoChoice

instance IsRefType Constrained where
  toRefType = Val . NoChoice . T0Basic

instance IsRefType Map where
  toRefType = Val . NoChoice . T0Map

instance IsRefType Array where
  toRefType = Val . NoChoice . T0Array

-- We also allow going directly from primitive types to RefType
instance IsRefType Int where
  toRefType = toRefType . T0Literal . LInt

instance IsRefType T.Text where
  toRefType = toRefType . T0Literal . LText

instance IsRefType ByteString where
  toRefType = toRefType . T0Literal . LBytes

instance IsRefType Float where
  toRefType = toRefType . T0Literal . LFloat

instance IsRefType Double where
  toRefType = toRefType . T0Literal . LDouble

instance IsRefType (Value a) where
  toRefType = toRefType . toType0

-- | Name something to turn it into a reference
(##) :: T.Text -> a -> OrRef a
name ## b = Ref $ Named name b

class IsEntryLike a where
  fromMapEntry :: MapEntry -> a

instance IsEntryLike MapEntry where
  fromMapEntry = id

instance IsEntryLike ArrayEntry where
  fromMapEntry me =
    ArrayEntry
      { key = Just me.key,
        value =
          me.value,
        quantifier = me.quantifier
      }

(==>) :: (IsRefType a, IsEntryLike me) => Key -> a -> me
k ==> gc =
  fromMapEntry
    MapEntry
      { key = k,
        value = toRefType gc,
        quantifier = def
      }

-- | Assign a rule
(=:=) :: (IsType0 a) => T.Text -> a -> Rule
n =:= b = Named n (NoChoice $ toType0 b)

infixl 0 =:=

a :: (IsRefType a) => a -> ArrayEntry
a x = ArrayEntry {key = Nothing, value = toRefType x, quantifier = def}

class IsChoosable a b | a -> b where
  toChoice :: a -> Choice b

instance IsChoosable (Choice a) a where
  toChoice = id

instance IsChoosable ArrayChoice ArrayChoice where
  toChoice = NoChoice

instance IsChoosable MapChoice MapChoice where
  toChoice = NoChoice

instance IsChoosable Type0 Type0 where
  toChoice = NoChoice

(//) :: (IsChoosable a c, IsChoosable b c) => a -> b -> Choice c
x // b = go (toChoice x) (toChoice b)
  where
    go (NoChoice x') b' = ChoiceOf x' b'
    go (ChoiceOf x' b') c = ChoiceOf x' (go b' c)

-- | This function is used solely to resolve type inference by explicitly
-- identifying something as an array.
arr :: ArrayChoice -> ArrayChoice
arr = id

mp :: MapChoice -> MapChoice
mp = id

--------------------------------------------------------------------------------
-- Conversion to CDDL
--------------------------------------------------------------------------------

-- | Convert from Huddle to CDDL for the purpose of pretty-printing.
toCDDL :: Huddle -> CDDL
toCDDL (Huddle []) = error "Empty list of rules to generate"
toCDDL (Huddle (r : rs)) = C.CDDL . fmap toCDDLRule $ (r NE.:| rs)
  where
    toCDDLRule :: Rule -> C.Rule
    toCDDLRule (Named n t0) =
      C.Rule (C.Name n) Nothing C.AssignEq
        . C.TOGType
        . C.Type0
        $ toCDDLType1 . Val <$> choiceToNE t0
    toCDDLValue :: Literal -> C.Value
    toCDDLValue (LInt i) = C.VNum i
    toCDDLValue (LText t) = C.VText t
    toCDDLValue (LBytes b) = C.VBytes b
    toCDDLValue _ = error "I haven't done this bit yet"

    mapToCDDLGroup :: Map -> C.Group
    mapToCDDLGroup xs = C.Group $ mapChoiceToCDDL <$> choiceToNE xs

    mapChoiceToCDDL :: MapChoice -> C.GrpChoice
    mapChoiceToCDDL (MapChoice entries) = fmap mapEntryToCDDL entries

    mapEntryToCDDL :: MapEntry -> C.GroupEntry
    mapEntryToCDDL (MapEntry k v occ) =
      C.GEType
        (toOccurrenceIndicator occ)
        (Just $ toMemberKey k)
        (toCDDLType0 v)

    toOccurrenceIndicator :: Occurs -> Maybe C.OccurrenceIndicator
    toOccurrenceIndicator (Occurs Nothing Nothing) = Nothing
    toOccurrenceIndicator (Occurs (Just 0) (Just 1)) = Just C.OIOptional
    toOccurrenceIndicator (Occurs (Just 0) Nothing) = Just C.OIZeroOrMore
    toOccurrenceIndicator (Occurs (Just 1) Nothing) = Just C.OIOneOrMore
    toOccurrenceIndicator (Occurs lb ub) = Just $ C.OIBounded lb ub

    toCDDLType1 :: OrRef Type0 -> C.Type1
    toCDDLType1 (Ref (Named n _)) =
      C.Type1 (C.T2Name (C.Name n) Nothing) Nothing
    toCDDLType1 (Val t0) = case t0 of
      T0Basic (Constrained x constr) ->
        -- TODO Need to handle choices at the top level
        constr.applyConstraint (C.T2Name (toCDDLPostlude x) Nothing)
      T0Literal l -> C.Type1 (C.T2Value $ toCDDLValue l) Nothing
      T0Map m ->
        C.Type1
          (C.T2Map $ mapToCDDLGroup m)
          Nothing
      T0Array x -> C.Type1 (C.T2Array $ arrayToCDDLGroup x) Nothing

    toMemberKey :: Key -> C.MemberKey
    toMemberKey (LiteralKey (LText t)) = C.MKBareword (C.Name t)
    toMemberKey (LiteralKey v) = C.MKValue $ toCDDLValue v
    toMemberKey (TypeKey t) = C.MKType (toCDDLType1 t)

    toCDDLType0 :: OrRef (Choice Type0) -> C.Type0
    toCDDLType0 (Ref (Named n _)) =
      C.Type0 . NE.singleton $
        C.Type1 (C.T2Name (C.Name n) Nothing) Nothing
    toCDDLType0 (Val ct0) = C.Type0 $ fmap (toCDDLType1 . Val) (choiceToNE ct0)

    arrayToCDDLGroup :: Array -> C.Group
    arrayToCDDLGroup xs = C.Group $ arrayChoiceToCDDL <$> choiceToNE xs

    arrayChoiceToCDDL :: ArrayChoice -> C.GrpChoice
    arrayChoiceToCDDL (ArrayChoice entries) = fmap arrayEntryToCDDL entries

    arrayEntryToCDDL :: ArrayEntry -> C.GroupEntry
    arrayEntryToCDDL (ArrayEntry k v occ) =
      C.GEType
        (toOccurrenceIndicator occ)
        (fmap toMemberKey k)
        (toCDDLType0 v)

    toCDDLPostlude :: Value a -> C.Name
    toCDDLPostlude VBool = C.Name "bool"
    toCDDLPostlude VUInt = C.Name "uint"
    toCDDLPostlude VNInt = C.Name "nint"
    toCDDLPostlude VInt = C.Name "int"
    toCDDLPostlude VHalf = C.Name "half"
    toCDDLPostlude VFloat = C.Name "float"
    toCDDLPostlude VDouble = C.Name "double"
    toCDDLPostlude VBytes = C.Name "bytes"
    toCDDLPostlude VText = C.Name "text"
    toCDDLPostlude VAny = C.Name "any"
    toCDDLPostlude VNil = C.Name "nil"
