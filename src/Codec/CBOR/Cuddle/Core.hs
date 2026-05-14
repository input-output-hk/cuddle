{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.CBOR.Cuddle.Core (
  RuleTerm (..),
  Name (..),
  GRef (..),
  uintMax,
  nintMin,
) where

import Codec.CBOR.Cuddle.Comments (CollectComments (..))
import Codec.CBOR.Term (Term)
import Data.Hashable (Hashable)
import Data.String (IsString (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..))

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
newtype Name = Name {unName :: T.Text}
  deriving (Generic)
  deriving (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

instance Pretty Name where
  pretty (Name name) = pretty name

instance CollectComments Name where
  collectComments _ = []

instance Hashable Name

-- | A reference to a generic parameter inside the body of a generic rule.
newtype GRef = GRef T.Text
  deriving (Show)

instance IsString Name where
  fromString = Name . T.pack

data RuleTerm
  = SingleTerm Term
  | PairTerm Term Term
  | GroupTerm [RuleTerm]
  deriving (Eq, Ord, Show)

-- Bounds

uintMax :: Integer
uintMax = 2 ^ (64 :: Int) - 1

nintMin :: Integer
nintMin = -(2 ^ (64 :: Int))
