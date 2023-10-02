{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Codec.CBOR.Cuddle.Schema.Core
  ( Schema (..)
  , Schematic (..)
  , Inlining (..)
  , (<!>)
  , (<//>)
  , namedSchema
  ) where

import Codec.CBOR.Cuddle.CDDL
import qualified Data.Text as T
import qualified Data.ByteString as B

class Schematic a where
  schema :: Schema 'Inline a
  cddlName :: Name

instance Schematic Int where
  schema = PrimNum
  cddlName = "int"

instance Schematic T.Text where
  schema = PrimText
  cddlName = "text"

instance Schematic B.ByteString where
  schema = PrimBytes
  cddlName = "bytes"

instance (Schematic a, Schematic b) => Schematic (Either a b) where
  schema = PureP Left <!> namedSchema <//> PureP Right <!> namedSchema
  cddlName = "either_" <> cddlName @a <> "_" <> cddlName @b

instance (Schematic a, Schematic b) => Schematic (a, b) where
  schema = PureP (,) <!> namedSchema <!> namedSchema
  cddlName = "prod_" <> cddlName @a <> "_" <> cddlName @b

instance Schematic () where
  schema = PureP ()
  cddlName = "unit"

data Inlining
  = TopLevel
  | Inline

data Schema (i :: Inlining) a where
  PureP :: a -> Schema 'Inline a
  ArrP :: Schema i (x -> a) -> Schema j x -> Schema 'Inline a
  ChoiceP :: Schema i a -> Schema j a -> Schema 'Inline a
  NamedP :: Name -> Schema i a -> Schema 'TopLevel a
  PrimNum :: Schema 'Inline Int
  PrimText :: Schema 'Inline T.Text
  PrimBytes :: Schema 'Inline B.ByteString

namedSchema :: forall a. Schematic a => Schema 'TopLevel a
namedSchema = NamedP (cddlName @a) schema

(<!>) :: Schema i (x -> a) -> Schema j x -> Schema 'Inline a
(<!>) = ArrP

infixl 4 <!>

(<//>) :: Schema i a -> Schema j a -> Schema 'Inline a
(<//>) = ChoiceP

infixl 3 <//>
