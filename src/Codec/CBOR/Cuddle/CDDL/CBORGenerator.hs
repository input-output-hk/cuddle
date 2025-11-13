module Codec.CBOR.Cuddle.CDDL.CBORGenerator (
  CBORGenerator (..),
  HasGenerator (..),
  WrappedTerm (..),
) where

import Codec.CBOR.Term (Term)
import Optics.Core (Lens')
import System.Random.Stateful (StatefulGen)

data WrappedTerm
  = -- | Single term
    S Term
  | -- | Pair term
    P Term Term
  | -- | Group term
    G [WrappedTerm]
  deriving (Eq, Show)

newtype CBORGenerator = CBORGenerator (forall g m. StatefulGen g m => g -> m WrappedTerm)

class HasGenerator a where
  generatorL :: Lens' a (Maybe CBORGenerator)
