{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (asMap, buildMonoCTree, buildRefCTree, buildResolvedCTree)
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Codec.CBOR.Cuddle.Pretty ()
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Prettyprinter (Pretty (pretty))
import Prettyprinter.Util (putDocW)
import System.Environment (getArgs)
import Text.Megaparsec (ParseErrorBundle, Parsec, errorBundlePretty, runParser)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      parseFromFile pCDDL fn >>= \case
        Left err -> putStrLn $ errorBundlePretty err
        Right res -> do
          print res
          putDocW 80 $ pretty res
          putStrLn "\n"
          putStrLn "--------------------------------------------------------------------------------"
          putStrLn " As a CTree"
          putStrLn "--------------------------------------------------------------------------------"
          let refCTree = buildRefCTree (asMap res)
          print refCTree
          putStrLn "--------------------------------------------------------------------------------"
          putStrLn " After name resolution"
          putStrLn "--------------------------------------------------------------------------------"
          let resolvedCTree = buildResolvedCTree refCTree
          print resolvedCTree
          putStrLn "--------------------------------------------------------------------------------"
          putStrLn " After monomorphisation"
          putStrLn "--------------------------------------------------------------------------------"
          let monoCTree = buildMonoCTree <$> resolvedCTree
          print monoCTree
    _ -> putStrLn "Expected filename"

parseFromFile ::
  Parsec e T.Text a ->
  String ->
  IO (Either (ParseErrorBundle T.Text e) a)
parseFromFile p file = runParser p file <$> T.readFile file
