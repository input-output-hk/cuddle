{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module Codec.CBOR.Cuddle.CDDL.CTreePhase (
  CTreePhase,
  XTerm (..),
  XXTopLevel (..),
  XCddl (..),
  XRule (..),
  XXType2 (..),
) where

import Codec.CBOR.Cuddle.CDDL (XCddl, XRule, XTerm, XXTopLevel, XXType2)
import Codec.CBOR.Cuddle.CDDL.CBORGenerator (CBORGenerator, CBORValidator)
import Data.Default.Class (Default)
import Data.Hashable (Hashable)
import Data.Void (Void)
import GHC.Generics (Generic)

type data CTreePhase

data instance XTerm CTreePhase = CTreeXTerm
  deriving (Generic, Show, Eq, Ord)
  deriving anyclass (Hashable, Default)

newtype instance XXTopLevel CTreePhase = CTreeXXTopLevel Void
  deriving (Generic, Show, Eq, Ord)

data instance XCddl CTreePhase = CTreeXCddl
  deriving (Generic, Show, Eq, Ord)

data instance XRule CTreePhase = CTreeXRule (Maybe CBORGenerator) (Maybe CBORValidator)
  deriving (Generic)

newtype instance XXType2 CTreePhase = CTreeXXType2 Void
  deriving (Generic, Show, Eq, Ord)
  deriving anyclass (Hashable)
