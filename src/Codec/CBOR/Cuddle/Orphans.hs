{-# OPTIONS_GHC -Wno-orphans #-}

module Codec.CBOR.Cuddle.Orphans () where

import Codec.CBOR.Cuddle.Comments (CollectComments (..))
import Data.Hashable (Hashable (..))
import Numeric.Half (Half (..))
import Prettyprinter (Pretty (..))
import Test.QuickCheck (Arbitrary (..))

instance CollectComments Half where
  collectComments _ = []

instance Hashable Half where
  hashWithSalt i (Half x) = hashWithSalt i $ toInteger x

instance Pretty Half where
  pretty (Half x) = pretty $ toInteger x

instance Arbitrary Half where
  arbitrary = do
    half <- Half <$> arbitrary
    if isDenormalized half
      then arbitrary
      else pure half
