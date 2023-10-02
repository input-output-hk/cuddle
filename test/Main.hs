{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import qualified Data.ByteString as B
import Codec.CBOR.Cuddle.Schema
  ( Schematic (..)
  , Schema (..)
  , (<//>)
  , namedSchema
  , (<!>), Inlining (..), toCDDL
  )
import Codec.CBOR.Cuddle.Pretty ()
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Test.QuickCheck (Arbitrary (..), oneof, sized, scale, ioProperty, counterexample)
import Test.Hspec (describe, hspec, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Codec.CBOR.Cuddle.Schema.Encode (toEncoder)
import Codec.CBOR.Cuddle.Schema.Decode (toDecoder)
import Codec.CBOR.Read (deserialiseFromBytes)
import Data.ByteString.Builder (toLazyByteString)
import Codec.CBOR.Write (toBuilder)
import Data.Either (isRight)
import Prettyprinter (Pretty(..), indent)

data Credential 
  = KeyHash B.ByteString
  | Script B.ByteString B.ByteString

instance Schematic Credential where
  schema = 
    NamedP "key_hash" (PureP KeyHash <!> schema)
    <//> NamedP "script" (PureP Script <!> schema <!> schema)
  cddlName = "credential"

data Test1 = Test1
  Credential
  Credential
  Credential

instance Schematic Test1 where
  cddlName = "test1"
  schema = 
    PureP Test1
      <!> namedSchema
      <!> schema
      <!> PureP (KeyHash mempty)

data Expr 
  = EProd Expr Expr
  | ESum Expr Expr
  | EInt Int
  | EText T.Text
  | EBytes BS.ByteString
  deriving (Eq, Show)

instance Arbitrary Expr where
  arbitrary = sized $ \size -> oneof $
    [ EInt <$> arbitrary
    , EText . T.pack <$> arbitrary
    , EBytes . BS.pack <$> arbitrary
    ] <> 
    ( if size > 0
        then
          [ EProd <$> subtree <*> subtree
          , ESum <$> subtree <*> subtree
          ]
        else []
    )
      where
        subtree = scale (`div` 2) arbitrary
  shrink (EInt _) = []
  shrink (EText _) = []
  shrink (EBytes _) = []
  shrink (EProd x y) = [x, y, EInt 0, EText "", EBytes ""]
  shrink (ESum x y) = [x, y, EInt 0, EText "", EBytes ""]

toSchema :: Expr -> Schema 'Inline Expr
toSchema (EProd x y) = PureP EProd <!> toSchema x <!> toSchema y
toSchema (ESum x y) = toSchema x <//> toSchema y
toSchema (EInt _) = PureP EInt <!> PrimNum
toSchema (EText _) = PureP EText <!> PrimText
toSchema (EBytes _) = PureP EBytes <!> PrimBytes

main :: IO ()
main = hspec $ do
  describe "" $ do
    prop "Expr roundtrip succeeds" $ \expr -> do
      let 
        sch = toSchema expr
      enc <- toEncoder sch
      let 
        dec = toDecoder sch
        res = deserialiseFromBytes dec $ toLazyByteString $ toBuilder enc
        p = ioProperty $ res `shouldSatisfy` isRight
      pure $ 
        counterexample ("Encoder: " <> show enc) $
        counterexample ("Schema:\n" <> unlines (show . indent 2 . pretty <$> toCDDL (NamedP "expr" sch))) 
        p
