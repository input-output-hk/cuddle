{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.Resolve
  ( fullResolveCDDL,
  )
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Codec.CBOR.Cuddle.Pretty ()
import Codec.CBOR.FlatTerm (toFlatTerm)
import Codec.CBOR.Pretty (prettyHexEnc)
import Codec.CBOR.Term (encodeTerm)
import Codec.CBOR.Write (toStrictByteString)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative
import Prettyprinter (Pretty (pretty))
import Prettyprinter.Util (putDocW)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Random (getStdGen)
import Text.Megaparsec (ParseErrorBundle, Parsec, errorBundlePretty, runParser)

data Opts = Opts Command String

data Command
  = Format
  | Validate
  | GenerateCBOR GenOpts

-- | Various formats for outputtting CBOR
data CBOROutputFormat
  = AsCBOR
  | AsPrettyCBOR
  | AsTerm
  | AsFlatTerm

pCBOROutputFormat :: ReadM CBOROutputFormat
pCBOROutputFormat = eitherReader $ \case
  "cbor" -> Right AsCBOR
  "pretty" -> Right AsPrettyCBOR
  "term" -> Right AsTerm
  "flat" -> Right AsFlatTerm
  s -> Left s

data GenOpts = GenOpts
  { itemName :: T.Text,
    outputFormat :: CBOROutputFormat
  }

pGenOpts :: Parser GenOpts
pGenOpts =
  GenOpts
    <$> strOption
      ( long "rule"
          <> short 'r'
          <> metavar "RULE"
          <> help "Name of the CDDL rule to generate a CBOR term for"
      )
    <*> option
      pCBOROutputFormat
      ( long "format"
          <> short 'f'
          <> help "Output format"
          <> value AsCBOR
      )

opts :: Parser Opts
opts =
  Opts
    <$> subparser
      ( command
          "format"
          ( info
              (pure Format)
              ( progDesc "Format the provided CDDL file"
              )
          )
          <> command
            "validate"
            ( info
                (pure Validate)
                ( progDesc "Validate the provided CDDL file"
                )
            )
          <> command
            "gen"
            ( info
                (GenerateCBOR <$> pGenOpts)
                ( progDesc "Generate a CBOR term matching the schema"
                )
            )
      )
    <*> argument str (metavar "CDDL_FILE")

main :: IO ()
main = do
  options <-
    execParser $
      info
        (opts <**> helper)
        ( fullDesc
            <> progDesc "Manipulate CDDL files"
            <> header "cuddle"
        )
  run options

run :: Opts -> IO ()
run (Opts cmd cddlFile) = do
  parseFromFile pCDDL cddlFile >>= \case
    Left err -> do
      putStrLnErr $ errorBundlePretty err
      exitFailure
    Right res -> case cmd of
      Format -> putDocW 80 $ pretty res
      Validate -> case fullResolveCDDL res of
        Left err -> putStrLnErr (show err) >> exitFailure
        Right _ -> exitSuccess
      (GenerateCBOR x) -> case fullResolveCDDL res of
        Left err -> putStrLnErr (show err) >> exitFailure
        Right mt -> do
          stdGen <- getStdGen
          let term = generateCBORTerm mt (Name $ itemName x) stdGen
           in case outputFormat x of
                AsTerm -> print term
                AsFlatTerm -> print $ toFlatTerm (encodeTerm term)
                AsCBOR -> print . toStrictByteString $ encodeTerm term
                AsPrettyCBOR -> putStrLn . prettyHexEnc $ encodeTerm term

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

parseFromFile ::
  Parsec e T.Text a ->
  String ->
  IO (Either (ParseErrorBundle T.Text e) a)
parseFromFile p file = runParser p file <$> T.readFile file
