{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Codec.CBOR.Cuddle.CBOR.Gen (generateCBORTerm)
import Codec.CBOR.Cuddle.CBOR.Validator
import Codec.CBOR.Cuddle.CDDL (
  CDDL (..),
  Name (..),
  TopLevel (..),
  XRule,
  XTerm,
  XXType2,
  fromRules,
  sortCDDL,
 )
import Codec.CBOR.Cuddle.CDDL.Postlude (appendPostlude)
import Codec.CBOR.Cuddle.CDDL.Resolve (
  fullResolveCDDL,
 )
import Codec.CBOR.Cuddle.IndexMappable (IndexMappable (..))
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Codec.CBOR.Cuddle.Pretty (PrettyStage)
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

data Command
  = Format FormatOpts
  | Validate ValidateOpts
  | GenerateCBOR GenOpts
  | ValidateCBOR ValidateCBOROpts

newtype ValidateOpts = ValidateOpts {vNoPrelude :: Bool}

pValidateOpts :: Parser ValidateOpts
pValidateOpts =
  ValidateOpts
    <$> switch
      ( long "no-prelude"
          <> help "Do not include the CDDL prelude."
      )

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
  , outputTo :: Maybe String
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
    <*> optional
      ( strOption
          ( long "out-file"
              <> short 'o'
              <> help "Write to"
          )
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

data ValidateCBOROpts = ValidateCBOROpts
  { vcItemName :: T.Text
  , vcInput :: FilePath
  , vcNoPrelude :: Bool
  }

pValidateCBOROpts :: Parser ValidateCBOROpts
pValidateCBOROpts =
  ValidateCBOROpts
    <$> strOption
      ( long "rule"
          <> short 'r'
          <> metavar "RULE"
          <> help "Name of the CDDL rule to validate this file with"
      )
    <*> strOption
      ( long "cbor"
          <> short 'c'
          <> help "CBOR file"
      )
    <*> switch
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
          <> command
            "validate-cbor"
            ( info
                (ValidateCBOR <$> pValidateCBOROpts <**> helper)
                (progDesc "Validate a CBOR file against a schema")
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
  let
    mapRule ::
      ( IndexMappable XXType2 i j
      , IndexMappable XTerm i j
      , IndexMappable XRule i j
      ) =>
      TopLevel i -> [TopLevel j]
    mapRule (TopLevelRule r) = [TopLevelRule $ mapIndex r]
    mapRule (XXTopLevel _) = []
  parseFromFile pCDDL cddlFile >>= \case
    Left err -> do
      putStrLnErr $ errorBundlePretty err
      exitFailure
    Right res ->
      case cmd of
        Format fOpts ->
          let
            defs
              | sort fOpts = fromRules $ sortCDDL res
              | otherwise = res
           in
            putDocW 80 . pretty $ mapIndex @_ @_ @PrettyStage defs
        Validate vOpts ->
          let
            CDDL r tls _
              | vNoPrelude vOpts = res
              | otherwise = appendPostlude res
           in
            case fullResolveCDDL $ CDDL (mapIndex r) (foldMap mapRule tls) mempty of
              Left err -> putStrLnErr (show err) >> exitFailure
              Right _ -> exitSuccess
        (GenerateCBOR gOpts) ->
          let
            CDDL r tls _
              | gNoPrelude gOpts = res
              | otherwise = appendPostlude res
           in
            case fullResolveCDDL $ CDDL (mapIndex r) (foldMap mapRule tls) mempty of
              Left err -> putStrLnErr (show err) >> exitFailure
              Right mt -> do
                stdGen <- getStdGen
                let term = generateCBORTerm mt (Name $ itemName gOpts) stdGen
                 in case outputFormat gOpts of
                      AsTerm -> print term
                      AsFlatTerm -> print $ toFlatTerm (encodeTerm term)
                      AsCBOR -> case outputTo gOpts of
                        Nothing -> BSC.putStrLn . Base16.encode . toStrictByteString $ encodeTerm term
                        Just out -> BSC.writeFile out $ toStrictByteString $ encodeTerm term
                      AsPrettyCBOR -> putStrLn . prettyHexEnc $ encodeTerm term
        ValidateCBOR vcOpts ->
          let
            CDDL r tls _
              | vcNoPrelude vcOpts = res
              | otherwise = res
           in
            case fullResolveCDDL $ CDDL (mapIndex r) (foldMap mapRule tls) mempty of
              Left err -> putStrLnErr (show err) >> exitFailure
              Right mt -> do
                cbor <- BSC.readFile (vcInput vcOpts)
                validateCBOR cbor (Name $ vcItemName vcOpts) (mapIndex mt)

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

parseFromFile ::
  Parsec e T.Text a ->
  String ->
  IO (Either (ParseErrorBundle T.Text e) a)
parseFromFile p file = runParser p file <$> T.readFile file
