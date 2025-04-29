{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Test.Codec.CBOR.Cuddle.CDDL.Validator where

import Test.QuickCheck.Monadic
import Test.QuickCheck
import Data.ByteString qualified as BS
import System.Process
import System.IO.Temp
import System.FilePath
import Control.Exception
import System.Directory
import Codec.CBOR.Cuddle.CBOR.Validator
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Codec.CBOR.Cuddle.CDDL
import System.IO
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Text.Megaparsec
import Codec.CBOR.Cuddle.CDDL.Prelude (prependPrelude)
import Data.Either
import Codec.CBOR.Cuddle.CDDL.Resolve (
  fullResolveCDDL,
 )
import Debug.Trace

runTest :: T.Text -> PropertyM IO ()
runTest bs = do
  res <- run $ bracket
    ((,,) <$> emptySystemTempFile "cddl.cddl" <*> emptySystemTempFile "diag.diag" <*> emptySystemTempFile "cbor.cbor")
    (\(f1, f2, f3) -> mapM_ removeFile [f1, f2, f3])
    (\(fpcddl, fpdiag, fpcbor) -> do
        T.writeFile fpcddl bs
        out <- readProcess "ruby" ["C:/msys64/clang64/bin/cddl", fpcddl, "generate"] ""
        writeFile fpdiag out
        _ <- withBinaryFile fpcbor WriteMode $ \h ->
          withCreateProcess ((proc "ruby" ["C:/msys64/clang64/bin/diag2cbor.rb", fpdiag]){std_out = UseHandle h}) $ \_ _ _ -> waitForProcess
        cbor <- BS.readFile fpcbor
        tt <- T.readFile fpcddl
        let cddl = fromRight (error "impossible!") $ fullResolveCDDL $ prependPrelude $ fromRight (error "impossible!") $ runParser pCDDL fpcddl tt
        pure $ traceShowId $ validateCBOR' cbor (Name "foo") cddl
    )
  Test.QuickCheck.Monadic.assertWith (isRight res) "Failedd"

a :: IO ()
a = verboseCheck $ monadicIO $ runTest "foo = uint .gt 3\n"
