{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Codec.CBOR.Cuddle (fullResolveCDDL, mapCDDLDropExt, mapIndex, showSimple)
import Codec.CBOR.Cuddle.Generator (GenConfig (..), generateFromName, runCBORGen)
import Codec.CBOR.Cuddle.Huddle (toCDDL)
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Conway (conway)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Prettyprinter (Pretty (..))
import Prettyprinter.Util (putDocW)
import System.Environment (getArgs)
import Test.AntiGen (runAntiGen)
import Test.QuickCheck (generate)
import Text.Megaparsec (ParseErrorBundle, Parsec, errorBundlePretty, runParser)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      parseFromFile pCDDL fn >>= \case
        Left err -> putStrLn $ errorBundlePretty err
        Right res -> do
          putDocW 80 $ pretty res
          putStrLn "\n"
          putStrLn "--------------------------------------------------------------------------------"
          putStrLn " Resolving"
          putStrLn "--------------------------------------------------------------------------------"
          case fullResolveCDDL (mapCDDLDropExt res) of
            Left nre -> putStrLn $ "Resolution error: " <> show nre
            Right resolved -> putStrLn $ showSimple resolved
    [fn, name] -> do
      putStrLn "--------------------------------------------------------------------------------"
      putStrLn " Generating a term"
      putStrLn "--------------------------------------------------------------------------------"
      parseFromFile pCDDL fn >>= \case
        Left err -> putStrLn $ errorBundlePretty err
        Right res -> do
          case fullResolveCDDL (mapCDDLDropExt res) of
            Left nre -> error $ show nre
            Right resolved -> do
              let cfg = GenConfig {gcRoot = mapIndex resolved, gcTwiddle = True}
              term <- generate . runAntiGen . runCBORGen cfg $ generateFromName (fromString name)
              print term
    [] -> do
      let cw = toCDDL conway
      putDocW 80 $ pretty cw
    _ -> putStrLn "Expected filename"

parseFromFile ::
  Parsec e T.Text a ->
  String ->
  IO (Either (ParseErrorBundle T.Text e) a)
parseFromFile p file = runParser p file <$> T.readFile file
