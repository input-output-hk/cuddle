{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm)
import Codec.CBOR.Cuddle.CDDL (Name (..), sortCDDL)
import Codec.CBOR.Cuddle.CDDL.Prelude (prependPrelude)
import Codec.CBOR.Cuddle.CDDL.Resolve (
  fullResolveCDDL,
 )
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Codec.CBOR.Cuddle.Pretty ()
import Codec.CBOR.FlatTerm (toFlatTerm)
import Codec.CBOR.Pretty (prettyHexEnc)
import Codec.CBOR.Term (encodeTerm)
import Codec.CBOR.Write (toStrictByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
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

newtype ValidateOpts = ValidateOpts {vNoPrelude :: Bool}

data Command
  = Format FormatOpts
  | Validate ValidateOpts
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
  { itemName :: T.Text
  , outputFormat :: CBOROutputFormat
  , gNoPrelude :: Bool
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
    <*> switch
      ( long "no-prelude"
          <> help "Do not include the CDDL prelude."
      )

newtype FormatOpts = FormatOpts
  {sort :: Bool}

pFormatOpts :: Parser FormatOpts
pFormatOpts =
  FormatOpts
    <$> switch
      ( long "sort-rules"
          <> help "Sort the CDDL rule definitions before printing."
      )

pValidateOpts :: Parser ValidateOpts
pValidateOpts =
  ValidateOpts
    <$> switch
      ( long "no-prelude"
          <> help "Do not include the CDDL prelude."
      )

opts :: Parser Opts
opts =
  Opts
    <$> subparser
      ( command
          "format"
          ( info
              (Format <$> pFormatOpts <**> helper)
              (progDesc "Format the provided CDDL file")
          )
          <> command
            "validate"
            ( info
                (Validate <$> pValidateOpts <**> helper)
                (progDesc "Validate the provided CDDL file")
            )
          <> command
            "gen"
            ( info
                (GenerateCBOR <$> pGenOpts <**> helper)
                (progDesc "Generate a CBOR term matching the schema")
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
    Right res ->
      case cmd of
        Format fOpts ->
          let
            defs
              | sort fOpts = sortCDDL res
              | otherwise = res
           in
            putDocW 80 $ pretty defs
        Validate vOpts ->
          let
            res'
              | vNoPrelude vOpts = res
              | otherwise = prependPrelude res
           in
            case fullResolveCDDL res' of
              Left err -> putStrLnErr (show err) >> exitFailure
              Right _ -> exitSuccess
        (GenerateCBOR gOpts) ->
          let
            res'
              | gNoPrelude gOpts = res
              | otherwise = prependPrelude res
           in
            case fullResolveCDDL res' of
              Left err -> putStrLnErr (show err) >> exitFailure
              Right mt -> do
                stdGen <- getStdGen
                let term = generateCBORTerm mt (Name $ itemName gOpts) stdGen
                 in case outputFormat gOpts of
                      AsTerm -> print term
                      AsFlatTerm -> print $ toFlatTerm (encodeTerm term)
                      AsCBOR -> BSC.putStrLn . Base16.encode . toStrictByteString $ encodeTerm term
                      AsPrettyCBOR -> putStrLn . prettyHexEnc $ encodeTerm term

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

parseFromFile ::
  Parsec e T.Text a ->
  String ->
  IO (Either (ParseErrorBundle T.Text e) a)
parseFromFile p file = runParser p file <$> T.readFile file
