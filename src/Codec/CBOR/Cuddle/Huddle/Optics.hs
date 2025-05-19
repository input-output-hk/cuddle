{-# LANGUAGE DataKinds #-}

-- | Optics for mutating Huddle rules
module Codec.CBOR.Cuddle.Huddle.Optics (commentL, nameL) where

import Codec.CBOR.Cuddle.Huddle
import Data.Generics.Product (HasField' (field'))
import Data.Text qualified as T
import Optics.Core

mcommentL ::
  HasField' "description" a (Maybe T.Text) =>
  Lens a a (Maybe T.Text) (Maybe T.Text)
mcommentL = field' @"description"

-- | Traversal to the comment field of a description. Using this we can for
--   example set the comment with 'a & commentL .~ "This is a comment"'
commentL ::
  HasField' "description" a (Maybe T.Text) =>
  AffineTraversal a a T.Text T.Text
commentL = mcommentL % _Just

-- | Lens to the name of a rule (or other named entity). Using this we can
--   for example append to the name with 'a & nameL %~ (<> "_1")'
nameL :: Lens (Named a) (Named a) T.Text T.Text
nameL = field' @"name"
