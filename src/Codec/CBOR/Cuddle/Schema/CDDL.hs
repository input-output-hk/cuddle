{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.CBOR.Cuddle.Schema.CDDL 
  ( toCDDL
  ) where

import Data.Map.Strict (Map)
import qualified Data.List.NonEmpty as NE
import Control.Monad.State.Strict (gets, modify, State, execState)
import qualified Data.Map.Strict as Map
import Codec.CBOR.Cuddle.Schema.Core (Schema (..), Inlining (..))
import Codec.CBOR.Cuddle.CDDL 

toCDDL :: Schema 'TopLevel a -> CDDL
toCDDL p = mkRules . Map.toList $ execState (ppType0 p) mempty
  where
    mkRules vs = [Rule n Nothing AssignEq v | (n, Just v) <- vs]

type SchemaM a = State (Map Name (Maybe TypeOrGroup)) a

nameTOG :: Name -> TypeOrGroup
nameTOG n = TOGType . Type0 . NE.singleton $ Type1 (T2Name n Nothing) Nothing

ppType0 :: Schema i a -> SchemaM (Maybe TypeOrGroup)
ppType0 p@(ArrP _ _) = do
  gs <- ppArr p
  case gs of
    GrpChoice [] -> pure Nothing
    GrpChoice [GroupEntry _ _ x] -> pure . Just . TOGType $ x
    xs -> pure . Just . TOGGroup . Group $ NE.singleton xs
ppType0 p@(ChoiceP _ _) = do
  ch <- ppChoice 0 p
  case fst ch of
    x:xs -> pure . Just . TOGGroup . Group $ x NE.:| xs
    [] -> pure Nothing
ppType0 (NamedP n x) = do
  mbyRule <- gets $ Map.lookup n
  case mbyRule of
    Just _ -> -- TODO Check that the rule in the map is the same as `x`
      pure . Just $ nameTOG n
    Nothing -> do
      modify $ Map.insert n Nothing
      res <- ppType0 x
      case res of
        Just v -> do
          modify . Map.insert n $ Just v
          pure . Just $ nameTOG n
        Nothing -> pure Nothing
ppType0 (PureP _) = pure Nothing
ppType0 PrimNum = pure . Just $ nameTOG "int"
ppType0 PrimText = pure . Just $ nameTOG "text"
ppType0 PrimBytes = pure . Just $ nameTOG "bytes"

toType0 :: Type2 -> Type0
toType0 = Type0 . NE.singleton . (`Type1` Nothing)

ppChoice :: Int -> Schema i a -> SchemaM ([GrpChoice], Int)
ppChoice n (ChoiceP x y) = do
  (t, m) <- ppChoice n x
  (u, k) <- ppChoice m y
  pure (t <> u, k)
ppChoice n p@(ArrP _ _) = do
  r <- ppArr p
  pure ([r], n + 1)
ppChoice n x = do
  v <- ppType0 x
  case v of
    Just (TOGType t) -> pure 
      ( [GrpChoice pair]
      , n + 1
      )
        where 
          pair =
            [ GroupEntry Nothing Nothing . toType0 . T2Value $ VNum n
            , GroupEntry Nothing Nothing t
            ]
    Just (TOGGroup (Group g)) -> pure (NE.toList g, n + length g)
    Nothing -> pure ([], n)

ppArr :: Schema i a -> SchemaM GrpChoice
ppArr (ArrP f x) = (<>) <$> ppArr f <*> ppArr x
ppArr p = do
  v <- ppType0 p
  case v of
    Just (TOGType t) -> pure $ GrpChoice [GroupEntry Nothing Nothing t]
    Just (TOGGroup g) -> pure $ GrpChoice 
      [GroupEntry Nothing Nothing . toType0 $ T2Array g]
    Nothing -> pure $ GrpChoice []
