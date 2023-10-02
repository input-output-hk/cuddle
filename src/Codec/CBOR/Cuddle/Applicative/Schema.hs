{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module Codec.CBOR.Cuddle.Applicative.Schema
  ( Schema (NamedP, RecP)
  , HasCDDL (..)
  , toCDDL
  , (<!>)
  , (<//>)
  , namedSchema
  ) where

import Codec.CBOR.Cuddle.CDDL
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Void (Void)
import Data.Map.Strict (Map)
import qualified Data.List.NonEmpty as NE
import Control.Monad.State.Strict (StateT, gets, modify, execStateT)
import qualified Data.Map.Strict as Map
import Control.Monad.Except (MonadError (..))
import Data.Text (Text)

class HasCDDL a where
  cddlSchema :: Schema 'Inline a
  cddlName :: Name

instance HasCDDL Int where
  cddlSchema = PrimNum
  cddlName = "int"

instance HasCDDL T.Text where
  cddlSchema = PrimText
  cddlName = "text"

instance HasCDDL B.ByteString where
  cddlSchema = PrimBytes
  cddlName = "bytes"

instance (HasCDDL a, HasCDDL b) => HasCDDL (Either a b) where
  cddlSchema = RecP Left <!> namedSchema <//> RecP Right <!> namedSchema
  cddlName = "either_" <> cddlName @a <> "_" <> cddlName @b

instance (HasCDDL a, HasCDDL b) => HasCDDL (a, b) where
  cddlSchema = RecP (,) <!> namedSchema <!> namedSchema
  cddlName = "prod_" <> cddlName @a <> "_" <> cddlName @b

instance HasCDDL () where
  cddlSchema = RecP ()
  cddlName = "unit"

instance HasCDDL Void where
  cddlSchema = FailP "void"
  cddlName = "void"

data Inlining
  = TopLevel
  | Inline

data Schema (i :: Inlining) a where
  RecP :: a -> Schema 'Inline a
  FailP :: Text -> Schema i a
  ArrP :: Schema i (x -> a) -> Schema j x -> Schema 'Inline a
  ChoiceP :: Schema i a -> Schema j a -> Schema 'Inline a
  NamedP :: Name -> Schema i a -> Schema 'TopLevel a
  PrimNum :: Schema 'Inline Int
  PrimText :: Schema 'Inline T.Text
  PrimBytes :: Schema 'Inline B.ByteString

namedSchema :: forall a. HasCDDL a => Schema 'TopLevel a
namedSchema = NamedP (cddlName @a) cddlSchema

(<!>) :: Schema i (x -> a) -> Schema j x -> Schema 'Inline a
(<!>) = ArrP

infixl 4 <!>

(<//>) :: Schema i a -> Schema j a -> Schema 'Inline a
(<//>) = ChoiceP

infixl 3 <//>

toCDDL :: Schema 'TopLevel a -> Either Text CDDL
toCDDL p = mkRules . Map.toList <$> execStateT (ppType0 p) mempty
  where
    mkRules vs = [Rule n AssignEq v | (n, Just v) <- vs]

type SchemaM a = StateT (Map Name (Maybe TypeOrGroup)) (Either Text) a

nameTOG :: Name -> TypeOrGroup
nameTOG n = TOGType . Type0 . NE.singleton $ Type1 (T2Name n) Nothing

ppType0 :: Schema i a -> SchemaM TypeOrGroup
ppType0 (FailP e) = throwError e
ppType0 p@(ArrP _ _) = TOGGroup . Group . NE.singleton <$> ppArr p
ppType0 p@(ChoiceP _ _) = TOGType <$> ppChoice p
ppType0 (NamedP n x) = do
  mbyRule <- gets $ Map.lookup n
  case mbyRule of
    Just _ -> -- TODO Check that the rule in the map is the same as `x`
      pure ()
    Nothing -> do
      modify $ Map.insert n Nothing
      res <- ppType0 x
      modify . Map.insert n $ Just res
  pure $ nameTOG n
ppType0 (RecP _) = pure $ nameTOG "!!pure!!"
ppType0 PrimNum = pure $ nameTOG "int"
ppType0 PrimText = pure $ nameTOG "text"
ppType0 PrimBytes = pure $ nameTOG "bytes"

ppChoice :: Schema i a -> SchemaM Type0
ppChoice (ChoiceP x y) = (<>) <$> ppChoice x <*> ppChoice y
ppChoice x = do
  v <- ppType0 x
  case v of
    TOGType t -> pure t
    TOGGroup _ -> error "Expected a type"

ppArr :: Schema i a -> SchemaM GrpChoice
ppArr (ArrP f x) = (<>) <$> ppArr f <*> ppArr x
ppArr (RecP _) = pure []
ppArr p = do
  v <- ppType0 p
  case v of
    TOGType t -> pure [GroupEntry Nothing Nothing t]
    TOGGroup _ -> error "Expected a type"
