{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.Resolve (
  asMap,
  buildMonoCTree,
  buildRefCTree,
  buildResolvedCTree,
 )
import Codec.CBOR.Cuddle.Huddle (toCDDL)
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Codec.CBOR.Cuddle.Pretty ()
import Conway (conway)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Monad qualified
import Prettyprinter (Pretty (pretty))
import Prettyprinter.Util (putDocW)
import System.Environment (getArgs)
import System.Random (getStdGen)
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
    [fn, name] -> do
      putStrLn "--------------------------------------------------------------------------------"
      putStrLn " Generating a term"
      putStrLn "--------------------------------------------------------------------------------"
      parseFromFile pCDDL fn >>= \case
        Left err -> putStrLn $ errorBundlePretty err
        Right res -> do
          stdGen <- getStdGen
          case buildMonoCTree =<< buildResolvedCTree (buildRefCTree (asMap res)) of
            Left nre -> error $ show nre
            Right mt ->
              let term = generateCBORTerm mt (Name (T.pack name) mempty) stdGen
               in print term
    [] -> do
      let cw = toCDDL conway
      putDocW 80 $ pretty cw
      putStrLn "--------------------------------------"
      putDocW 80 $ pretty (toCDDL Monad.spec)
      putStrLn "--------------------------------------"
      putDocW 80 $ pretty (toCDDL Monad.spec2)
    _ -> putStrLn "Expected filename"

parseFromFile ::
  Parsec e T.Text a ->
  String ->
  IO (Either (ParseErrorBundle T.Text e) a)
parseFromFile p file = runParser p file <$> T.readFile file
