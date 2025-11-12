{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defined the data structure of CDDL as specified in
--   https://datatracker.ietf.org/doc/rfc8610/
module Codec.CBOR.Cuddle.CDDL (
  CDDL (..),
  sortCDDL,
  cddlTopLevel,
  fromRule,
  fromRules,
  appendRules,
  TopLevel (..),
  Name (..),
  Rule (..),
  TypeOrGroup (..),
  Assign (..),
  GenericArg (..),
  GenericParam (..),
  Type0 (..),
  Type1 (..),
  Type2 (..),
  TyOp (..),
  RangeBound (..),
  OccurrenceIndicator (..),
  Group (..),
  GroupEntry (..),
  GroupEntryVariant (..),
  MemberKey (..),
  Value (..),
  value,
  ValueVariant (..),
  GrpChoice (..),
  unwrap,
  compareRuleName,
  -- Extension
  ForAllExtensions,
  XCddl,
  XTerm,
  XRule,
  XXTopLevel,
  XXType2,
) where

import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Codec.CBOR.Cuddle.Comments (CollectComments (..), Comment, HasComment (..))
import Data.ByteString qualified as B
import Data.Default.Class (Default (..))
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty (..), prependList)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.TreeDiff (ToExpr)
import Data.Word (Word64, Word8)
import GHC.Base (Constraint, Type)
import GHC.Generics (Generic)
import Optics.Core ((%), (%~), (&))

data family XXTopLevel i

data family XCddl i

data family XTerm i

data family XRule i

data family XXType2 i

type ForAllExtensions i (c :: Type -> Constraint) =
  ( c (XCddl i)
  , c (XTerm i)
  , c (XRule i)
  , c (XXTopLevel i)
  , c (XXType2 i)
  )

-- | The CDDL constructor takes three arguments:
--     1. Top level comments that precede the first definition
--     2. The root definition
--     3. All the other top level comments and definitions
--   This ensures that `CDDL` is correct by construction.
data CDDL i = CDDL
  { rootDefinition :: Rule i
  , topLevelDefinitions :: [TopLevel i]
  , cddlExt :: [XXTopLevel i]
  -- ^ This extension is used for comments that appear before the root definition
  }
  deriving (Generic)

deriving instance ForAllExtensions i Eq => Eq (CDDL i)

deriving instance ForAllExtensions i Show => Show (CDDL i)

deriving instance ForAllExtensions i ToExpr => ToExpr (CDDL i)

ruleTopLevel :: TopLevel i -> Maybe (Rule i)
ruleTopLevel (TopLevelRule r) = Just r
ruleTopLevel _ = Nothing

-- | Sort the CDDL Rules on the basis of their names
sortCDDL :: CDDL i -> NonEmpty (Rule i)
sortCDDL (CDDL r rs _) = NE.sortBy (compare `on` name . ruleName) $ r :| mapMaybe ruleTopLevel rs

fromRules :: Monoid (XCddl i) => NonEmpty (Rule i) -> CDDL i
fromRules (x :| xs) = CDDL x (TopLevelRule <$> xs) mempty

fromRule :: Monoid (XCddl i) => Rule i -> CDDL i
fromRule x = CDDL x [] mempty

appendRules :: CDDL i -> [Rule i] -> CDDL i
appendRules cddl rs = cddl & #topLevelDefinitions %~ (<> fmap TopLevelRule rs)

cddlTopLevel :: CDDL i -> NonEmpty (TopLevel i)
cddlTopLevel (CDDL r tls e) = prependList (XXTopLevel <$> e) $ TopLevelRule r :| tls

data TopLevel i
  = TopLevelRule (Rule i)
  | XXTopLevel (XXTopLevel i)
  deriving (Generic)

deriving instance ForAllExtensions i Eq => Eq (TopLevel i)

deriving instance ForAllExtensions i Show => Show (TopLevel i)

deriving instance ForAllExtensions i ToExpr => ToExpr (TopLevel i)

-- |
--  A name can consist of any of the characters from the set {"A" to
--  "Z", "a" to "z", "0" to "9", "_", "-", "@", ".", "$"}, starting
--  with an alphabetic character (including "@", "_", "$") and ending
--  in such a character or a digit.
--
--  *  Names are case sensitive.
--
--  *  It is preferred style to start a name with a lowercase letter.
--
--  *  The hyphen is preferred over the underscore (except in a
--      "bareword" (Section 3.5.1), where the semantics may actually
--      require an underscore).
--
--  *  The period may be useful for larger specifications, to express
--      some module structure (as in "tcp.throughput" vs.
--      "udp.throughput").
--
--  *  A number of names are predefined in the CDDL prelude, as listed
--      in Appendix D.
--
--  *  Rule names (types or groups) do not appear in the actual CBOR
--      encoding, but names used as "barewords" in member keys do.
data Name i = Name
  { name :: T.Text
  , nameExt :: XTerm i
  }
  deriving (Generic)

deriving instance Eq (XTerm i) => Eq (Name i)

deriving instance Ord (XTerm i) => Ord (Name i)

deriving instance Show (XTerm i) => Show (Name i)

deriving instance ToExpr (XTerm i) => ToExpr (Name i)

instance Monoid (XTerm i) => IsString (Name i) where
  fromString x = Name (T.pack x) mempty

instance HasComment (XTerm i) => HasComment (Name i) where
  commentL = #nameExt % commentL

instance CollectComments (XTerm i) => CollectComments (Name i) where
  collectComments (Name _ c) = collectComments c

instance Hashable (XTerm i) => Hashable (Name i)

-- |
--   assignt = "=" / "/="
--   assigng = "=" / "//="
--
--   A plain equals sign defines the rule name as the equivalent of the
--   expression to the right; it is an error if the name was already
--   defined with a different expression.  A "/=" or "//=" extends a named
--   type or a group by additional choices; a number of these could be
--   replaced by collecting all the right-hand sides and creating a single
--   rule with a type choice or a group choice built from the right-hand
--   sides in the order of the rules given.  (It is not an error to extend
--   a rule name that has not yet been defined; this makes the right-hand
--   side the first entry in the choice being created.)
data Assign = AssignEq | AssignExt
  deriving (Eq, Generic, Show)
  deriving anyclass (ToExpr)

-- |
--  Generics
--
--   Using angle brackets, the left-hand side of a rule can add formal
--   parameters after the name being defined, as in:
--
--      messages = message<"reboot", "now"> / message<"sleep", 1..100>
--      message<t, v> = {type: t, value: v}
--
--   When using a generic rule, the formal parameters are bound to the
--   actual arguments supplied (also using angle brackets), within the
--   scope of the generic rule (as if there were a rule of the form
--   parameter = argument).
--
--   Generic rules can be used for establishing names for both types and
--   groups.
newtype GenericParam i = GenericParam (NE.NonEmpty (Name i))
  deriving (Generic)
  deriving newtype (Semigroup)

deriving instance Eq (XTerm i) => Eq (GenericParam i)

deriving instance Show (XTerm i) => Show (GenericParam i)

deriving anyclass instance ToExpr (XTerm i) => ToExpr (GenericParam i)

newtype GenericArg i = GenericArg (NE.NonEmpty (Type1 i))
  deriving (Generic)
  deriving newtype (Semigroup)

deriving instance ForAllExtensions i Eq => Eq (GenericArg i)

deriving instance ForAllExtensions i Show => Show (GenericArg i)

deriving anyclass instance ForAllExtensions i ToExpr => ToExpr (GenericArg i)

instance ForAllExtensions i CollectComments => CollectComments (GenericArg i)

-- |
--  rule = typename [genericparm] S assignt S type
--        / groupname [genericparm] S assigng S grpent
--
--   typename = id
--   groupname = id
--
--   A rule defines a name for a type expression (production "type") or
--   for a group expression (production "grpent"), with the intention that
--   the semantics does not change when the name is replaced by its
--   (parenthesized if needed) definition.  Note that whether the name
--   defined by a rule stands for a type or a group isn't always
--   determined by syntax alone: e.g., "a = b" can make "a" a type if "b"
--   is a type, or a group if "b" is a group.  More subtly, in "a = (b)",
--   "a" may be used as a type if "b" is a type, or as a group both when
--   "b" is a group and when "b" is a type (a good convention to make the
--   latter case stand out to the human reader is to write "a = (b,)").
--   (Note that the same dual meaning of parentheses applies within an
--   expression but often can be resolved by the context of the
--   parenthesized expression.  On the more general point, it may not be
--   clear immediately either whether "b" stands for a group or a type --
--   this semantic processing may need to span several levels of rule
--   definitions before a determination can be made.)
data Rule i = Rule
  { ruleName :: Name i
  , ruleGenParam :: Maybe (GenericParam i)
  , ruleAssign :: Assign
  , ruleTerm :: TypeOrGroup i
  , ruleExt :: XRule i
  }
  deriving (Generic)

deriving instance ForAllExtensions i Eq => Eq (Rule i)

deriving instance ForAllExtensions i Show => Show (Rule i)

deriving instance ForAllExtensions i ToExpr => ToExpr (Rule i)

instance HasComment (XRule i) => HasComment (Rule i) where
  commentL = #ruleExt % commentL

compareRuleName :: Ord (XTerm i) => Rule i -> Rule i -> Ordering
compareRuleName = compare `on` ruleName

-- |
--   A range operator can be used to join two type expressions that stand
--   for either two integer values or two floating-point values; it
--   matches any value that is between the two values, where the first
--   value is always included in the matching set and the second value is
--   included for ".." and excluded for "...".
data RangeBound = ClOpen | Closed
  deriving (Eq, Generic, Show)
  deriving anyclass (ToExpr)

instance Hashable RangeBound

data TyOp = RangeOp RangeBound | CtrlOp CtlOp
  deriving (Eq, Generic, Show)
  deriving anyclass (ToExpr)

data TypeOrGroup i = TOGType (Type0 i) | TOGGroup (GroupEntry i)
  deriving (Generic)

deriving instance ForAllExtensions i Eq => Eq (TypeOrGroup i)

deriving instance ForAllExtensions i Show => Show (TypeOrGroup i)

deriving instance ForAllExtensions i ToExpr => ToExpr (TypeOrGroup i)

instance ForAllExtensions i CollectComments => CollectComments (TypeOrGroup i)

{-- |
   The group that is used to define a map or an array can often be reused in the
   definition of another map or array.  Similarly, a type defined as a tag
   carries an internal data item that one would like to refer to.  In these
   cases, it is expedient to simply use the name of the map, array, or tag type
   as a handle for the group or type defined inside it.

   The "unwrap" operator (written by preceding a name by a tilde character "~")
   can be used to strip the type defined for a name by one layer, exposing the
   underlying group (for maps and arrays) or type (for tags).

   For example, an application might want to define a basic header and an
   advanced header.  Without unwrapping, this might be done as follows:

             basic-header-group = (
               field1: int,
               field2: text,
             )

             basic-header = [ basic-header-group ]

             advanced-header = [
               basic-header-group,
               field3: bytes,
               field4: number, ; as in the tagged type "time"
             ]

   Unwrapping simplifies this to:

                            basic-header = [
                              field1: int,
                              field2: text,
                            ]

                            advanced-header = [
                              ~basic-header,
                              field3: bytes,
                              field4: ~time,
                            ]

   (Note that leaving out the first unwrap operator in the latter example would
   lead to nesting the basic-header in its own array inside the advanced-header,
   while, with the unwrapped basic-header, the definition of the group inside
   basic-header is essentially repeated inside advanced-header, leading to a
   single array.  This can be used for various applications often solved by
   inheritance in programming languages.  The effect of unwrapping can also be
   described as "threading in" the group or type inside the referenced type,
   which suggested the thread-like "~" character.)
-}
unwrap :: TypeOrGroup i -> Maybe (Group i)
unwrap (TOGType (Type0 (Type1 t2 Nothing _ NE.:| []))) = case t2 of
  T2Map g -> Just g
  T2Array g -> Just g
  _ -> Nothing
unwrap _ = Nothing

-- |
-- A type can be given as a choice between one or more types.  The
--   choice matches a data item if the data item matches any one of the
--   types given in the choice.
newtype Type0 i = Type0 {t0Type1 :: NE.NonEmpty (Type1 i)}
  deriving (Generic)
  deriving newtype (Semigroup)

deriving instance ForAllExtensions i Eq => Eq (Type0 i)

deriving instance ForAllExtensions i Show => Show (Type0 i)

deriving anyclass instance ForAllExtensions i ToExpr => ToExpr (Type0 i)

instance ForAllExtensions i CollectComments => CollectComments (Type0 i)

-- |
-- Two types can be combined with a range operator (see below)
data Type1 i = Type1
  { t1Main :: Type2 i
  , t1TyOp :: Maybe (TyOp, Type2 i)
  , t1Comment :: XTerm i
  }
  deriving (Generic)

deriving instance ForAllExtensions i Eq => Eq (Type1 i)

deriving instance ForAllExtensions i Show => Show (Type1 i)

deriving instance ForAllExtensions i ToExpr => ToExpr (Type1 i)

instance HasComment (XTerm i) => HasComment (Type1 i) where
  commentL = #t1Comment % commentL

instance ForAllExtensions i CollectComments => CollectComments (Type1 i) where
  collectComments (Type1 m tyOp c) = collectComments c <> collectComments m <> collectComments (fmap snd tyOp)

data Type2 i
  = -- | A type can be just a single value (such as 1 or "icecream" or
    --   h'0815'), which matches only a data item with that specific value
    --   (no conversions defined),
    T2Value Value
  | -- | or be defined by a rule giving a meaning to a name (possibly after
    --   supplying generic arguments as required by the generic parameters)
    T2Name (Name i) (Maybe (GenericArg i))
  | -- | or be defined in a parenthesized type expression (parentheses may be
    --   necessary to override some operator precedence),
    T2Group (Type0 i)
  | -- | a map expression, which matches a valid CBOR map the key/value pairs
    --  of which can be ordered in such a way that the resulting sequence
    --  matches the group expression, or
    T2Map (Group i)
  | -- | an array expression, which matches a CBOR array the elements of which
    -- when taken as values and complemented by a wildcard (matches
    -- anything) key each -- match the group, or
    T2Array (Group i)
  | -- | an "unwrapped" group (see Section 3.7), which matches the group
    --  inside a type defined as a map or an array by wrapping the group, or
    T2Unwrapped (Name i) (Maybe (GenericArg i))
  | -- | an enumeration expression, which matches any value that is within the
    --  set of values that the values of the group given can take, or
    T2Enum (Group i)
  | T2EnumRef (Name i) (Maybe (GenericArg i))
  | -- | a tagged data item, tagged with the "uint" given and containing the
    --  type given as the tagged value, or
    T2Tag (Maybe Word64) (Type0 i)
  | -- | a data item of a major type (given by the DIGIT), optionally
    --  constrained to the additional information given by the uint, or
    T2DataItem Word8 (Maybe Word64)
  | -- | Any data item
    T2Any
  | XXType2 (XXType2 i)
  deriving (Generic)

deriving instance ForAllExtensions i Eq => Eq (Type2 i)

deriving instance ForAllExtensions i Show => Show (Type2 i)

deriving instance ForAllExtensions i ToExpr => ToExpr (Type2 i)

instance ForAllExtensions i CollectComments => CollectComments (Type2 i)

-- |
--  An optional _occurrence_ indicator can be given in front of a group
--  entry.  It is either (1) one of the characters "?" (optional), "*"
--  (zero or more), or "+" (one or more) or (2) of the form n*m, where n
--  and m are optional unsigned integers and n is the lower limit
--  (default 0) and m is the upper limit (default no limit) of
--  occurrences.
--
--  If no occurrence indicator is specified, the group entry is to occur
--  exactly once (as if 1*1 were specified).  A group entry with an
--  occurrence indicator matches sequences of name/value pairs that are
--  composed by concatenating a number of sequences that the basic group
--  entry matches, where the number needs to be allowed by the occurrence
--  indicator.
data OccurrenceIndicator
  = OIOptional
  | OIZeroOrMore
  | OIOneOrMore
  | OIBounded (Maybe Word64) (Maybe Word64)
  deriving (Eq, Generic, Show)
  deriving anyclass (ToExpr)

instance Hashable OccurrenceIndicator

-- |
--   A group matches any sequence of key/value pairs that matches any of
--   the choices given (again using PEG semantics).
newtype Group i = Group {unGroup :: NE.NonEmpty (GrpChoice i)}
  deriving (Generic)
  deriving newtype (Semigroup)

deriving instance ForAllExtensions i Eq => Eq (Group i)

deriving instance ForAllExtensions i Show => Show (Group i)

deriving anyclass instance ForAllExtensions i ToExpr => ToExpr (Group i)

instance HasComment (XTerm i) => HasComment (Group i) where
  commentL = #unGroup % commentL

instance ForAllExtensions i CollectComments => CollectComments (Group i) where
  collectComments (Group xs) = concatMap collectComments xs

data GrpChoice i = GrpChoice
  { gcGroupEntries :: [GroupEntry i]
  , gcComment :: XTerm i
  }
  deriving (Generic)

deriving instance ForAllExtensions i Eq => Eq (GrpChoice i)

deriving instance ForAllExtensions i Show => Show (GrpChoice i)

deriving instance ForAllExtensions i ToExpr => ToExpr (GrpChoice i)

instance HasComment (XTerm i) => HasComment (GrpChoice i) where
  commentL = #gcComment % commentL

instance ForAllExtensions i CollectComments => CollectComments (GrpChoice i) where
  collectComments (GrpChoice ges c) = collectComments c <> concatMap collectComments ges

-- |
--  A group entry can be given by a value type, which needs to be matched
--  by the value part of a single element; and, optionally, a memberkey
--  type, which needs to be matched by the key part of the element, if
--  the memberkey is given.  If the memberkey is not given, the entry can
--  only be used for matching arrays, not for maps.  (See below for how
--  that is modified by the occurrence indicator.)
data GroupEntry i = GroupEntry
  { geOccurrenceIndicator :: Maybe OccurrenceIndicator
  , geVariant :: GroupEntryVariant i
  , geExt :: XTerm i
  }
  deriving (Generic)

deriving instance ForAllExtensions i Eq => Eq (GroupEntry i)

deriving instance ForAllExtensions i Show => Show (GroupEntry i)

deriving instance ForAllExtensions i ToExpr => ToExpr (GroupEntry i)

instance ForAllExtensions i CollectComments => CollectComments (GroupEntry i) where
  collectComments (GroupEntry _ c x) = collectComments c <> collectComments x

data GroupEntryVariant i
  = GEType (Maybe (MemberKey i)) (Type0 i)
  | GERef (Name i) (Maybe (GenericArg i))
  | GEGroup (Group i)
  deriving (Generic)

deriving instance ForAllExtensions i Eq => Eq (GroupEntryVariant i)

deriving instance ForAllExtensions i Show => Show (GroupEntryVariant i)

deriving instance ForAllExtensions i ToExpr => ToExpr (GroupEntryVariant i)

instance HasComment (XTerm i) => HasComment (GroupEntry i) where
  commentL = #geExt % commentL

instance ForAllExtensions i CollectComments => CollectComments (GroupEntryVariant i) where
  collectComments (GEType _ t0) = collectComments t0
  collectComments (GERef n mga) = collectComments n <> collectComments mga
  collectComments (GEGroup g) = collectComments g

-- |
--  Key types can be given by a type expression, a bareword (which stands
--  for a type that just contains a string value created from this
--  bareword), or a value (which stands for a type that just contains
--  this value).  A key value matches its key type if the key value is a
--  member of the key type, unless a cut preceding it in the group
--  applies (see Section 3.5.4 for how map matching is influenced by the
--  presence of the cuts denoted by "^" or ":" in previous entries).
data MemberKey i
  = MKType (Type1 i)
  | MKBareword (Name i)
  | MKValue Value
  deriving (Generic)

deriving instance ForAllExtensions i Eq => Eq (MemberKey i)

deriving instance ForAllExtensions i Show => Show (MemberKey i)

deriving instance ForAllExtensions i ToExpr => ToExpr (MemberKey i)

data Value = Value ValueVariant Comment
  deriving (Eq, Generic, Show, Default)
  deriving anyclass (ToExpr, Hashable, CollectComments)

value :: ValueVariant -> Value
value x = Value x mempty

data ValueVariant
  = VUInt Word64
  | VNInt Word64
  | VBignum Integer
  | VFloat16 Float
  | VFloat32 Float
  | VFloat64 Double
  | VText T.Text
  | VBytes B.ByteString
  | VBool Bool
  deriving (Eq, Generic, Show, Default)
  deriving anyclass (ToExpr, Hashable, CollectComments)
