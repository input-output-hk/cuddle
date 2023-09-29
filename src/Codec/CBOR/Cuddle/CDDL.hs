-- | This module defined the data structure of CDDL as specified in
--   https://datatracker.ietf.org/doc/rfc8610/
module Codec.CBOR.Cuddle.CDDL where

import Data.Text qualified as T
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NE

type CDDL = [Rule]

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
newtype Name = Name T.Text
  deriving Show

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
data Rule = Rule Name Assign TypeOrGroup

data RangeBound = ClOpen | Closed

data TyOp = RangeOp RangeBound | CtrlOp Name

data TypeOrGroup = TOGType Type0 | TOGGroup Group

-- |
-- A type can be given as a choice between one or more types.  The
--   choice matches a data item if the data item matches any one of the
--   types given in the choice.
newtype Type0 = Type0 (NE.NonEmpty Type1)

instance Semigroup Type0 where
  (Type0 a) <> (Type0 b) = Type0 $ a <> b

-- |
-- Two types can be combined with a range operator (see below)
data Type1 = Type1 Type2 (Maybe (TyOp, Type2))

data Type2
  = -- | A type can be just a single value (such as 1 or "icecream" or
    --   h'0815'), which matches only a data item with that specific value
    --   (no conversions defined),
    T2Value Value
  | -- | or be defined by a rule giving a meaning to a name (possibly after
    --   supplying generic arguments as required by the generic parameters)
    T2Name Name
  | -- | or be defined in a parenthesized type expression (parentheses may be
    --   necessary to override some operator precedence),
    T2Group Type0
  | -- | a map expression, which matches a valid CBOR map the key/value pairs
    --  of which can be ordered in such a way that the resulting sequence
    --  matches the group expression, or
    T2Map Group
  | -- | an array expression, which matches a CBOR array the elements of which
    -- when taken as values and complemented by a wildcard (matches
    -- anything) key each -- match the group, or
    T2Array Group
  | -- | an "unwrapped" group (see Section 3.7), which matches the group
    --  inside a type defined as a map or an array by wrapping the group, or
    T2Unwrapped Name
  | -- | an enumeration expression, which matches any value that is within the
    --  set of values that the values of the group given can take, or
    T2Enum Group
  | -- | a tagged data item, tagged with the "uint" given and containing the
    --  type given as the tagged value, or
    T2Tag Int Type0
  | -- | a data item of a major type (given by the DIGIT), optionally
    --  constrained to the additional information given by the uint, or
    T2DataItem Int (Maybe Int)
  | -- | Any data item
    T2Any

mkType :: Type2 -> Type0
mkType t = Type0 $ NE.singleton $ Type1 t Nothing

mkTypeRange :: Type2 -> Type2 -> RangeBound -> Type0
mkTypeRange t t' rb = Type0 $ NE.singleton $ Type1 t (Just (RangeOp rb, t'))

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
  | OIBounded (Maybe Int) (Maybe Int)

-- |
--   A group matches any sequence of key/value pairs that matches any of
--   the choices given (again using PEG semantics).
newtype Group = Group (NE.NonEmpty GrpChoice)

type GrpChoice = [GroupEntry]

-- |
--  A group entry can be given by a value type, which needs to be matched
--  by the value part of a single element; and, optionally, a memberkey
--  type, which needs to be matched by the key part of the element, if
--  the memberkey is given.  If the memberkey is not given, the entry can
--  only be used for matching arrays, not for maps.  (See below for how
--  that is modified by the occurrence indicator.)
data GroupEntry = GroupEntry (Maybe OccurrenceIndicator) (Maybe MemberKey) Type0

-- |
--  Key types can be given by a type expression, a bareword (which stands
--  for a type that just contains a string value created from this
--  bareword), or a value (which stands for a type that just contains
--  this value).  A key value matches its key type if the key value is a
--  member of the key type, unless a cut preceding it in the group
--  applies (see Section 3.5.4 for how map matching is influenced by the
--  presence of the cuts denoted by "^" or ":" in previous entries).
data MemberKey
  = MKType Type1
  | MKBareword Name
  | MKValue Value

data Value =
    -- Should be bigger than just Int
    VNum Int
  | VText T.Text
  | VBytes B.ByteString

newtype Comment = Comment T.Text
