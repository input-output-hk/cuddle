{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString as B
import Codec.CBOR.Cuddle.Applicative.Schema (HasCDDL (..), Schema (..), (<//>), toCDDL, namedSchema, (<!>))
import Codec.CBOR.Cuddle.Pretty ()
import Prettyprinter (Pretty(..))
import Data.Either (fromRight)

data Credential 
  = KeyHash B.ByteString
  | Script B.ByteString B.ByteString

instance HasCDDL Credential where
  cddlSchema = 
    NamedP "key_hash" (RecP KeyHash <!> cddlSchema)
    <//> NamedP "script" (RecP Script <!> cddlSchema <!> cddlSchema)
  cddlName = "credential"

data Test1 = Test1
  Credential
  Credential

instance HasCDDL Test1 where
  cddlName = "test1"
  cddlSchema = 
    RecP Test1
      <!> namedSchema
      <!> cddlSchema

main :: IO ()
main = do
  putStrLn . unlines . fmap (show . pretty) . fromRight (error "Failed to get CDDL") . toCDDL $ 
    NamedP (cddlName @Test1) (cddlSchema @Test1)
