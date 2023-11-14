-- \* CDDL Builder
--
-- Helps to build CDDL values in Haskell
module Codec.CBOR.Cuddle.Builder where

import Codec.CBOR.Cuddle.CDDL
import Data.Text qualified as T

class IsName a where
  toName :: a -> Name

instance IsName T.Text where
  toName = Name

instance IsName Name where
  toName = id

class IsTypeOrGroup a where
  toTypeOrGroup :: a -> TypeOrGroup

(=:=) :: (IsName a, IsTypeOrGroup b) => a -> b -> Rule
a =:= b = Rule (toName a) Nothing AssignEq (toTypeOrGroup b)

-- | Extend an assignment
(//=) :: (IsName a, IsTypeOrGroup b) => a -> b -> Rule
a //= b = Rule (toName a) Nothing AssignExt (toTypeOrGroup b)
