{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Test.Codec.CBOR.Cuddle.CDDL.Validator (spec) where

import Codec.CBOR.Cuddle.CBOR.Validator
import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.Prelude (prependPrelude)
import Codec.CBOR.Cuddle.CDDL.Resolve (
  fullResolveCDDL,
 )
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Control.Exception
import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.Either
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory
import System.IO
import System.IO.Temp
import System.Process
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Text.Megaparsec

spec :: Spec
spec = do
  describe "UInt" $ do
    describe "Type" $ genAndCounter "uint" "\"foo\""
    describe "Literal" $ genAndCounter "3" "4"
    describe "Ge" $ genAndCounter "uint .ge 3" "2"
    describe "Gt" $ genAndCounter "uint .gt 3" "3"
    describe "Le" $ genAndCounter "uint .le 3" "4"
    describe "Lt" $ genAndCounter "uint .lt 3" "3"
    describe "Eq" $ genAndCounter "uint .eq 3" "4"
    describe "Ne" $ genAndCounter "uint .ne 3" "3"
    describe "Size" $ genAndCounter "uint .size 1" "300"
    describe "Bits" $ genAndCounter "uint .bits (&(r: 2, w: 1, x: 0))" "8"
  describe "NInt" $ do
    describe "Type" $ genAndCounter "nint" "\"foo\""
    describe "Literal" $ genAndCounter "-3" "-4"
    describe "Ge" $ genAndCounter "uint .ge -3" "-4"
    describe "Gt" $ genAndCounter "uint .gt -3" "-3"
    describe "Le" $ xgenAndCounter "uint .le -3" "-2"
    describe "Lt" $ xgenAndCounter "uint .lt -3" "-3"
    describe "Eq" $ xgenAndCounter "uint .eq -3" "-4"
    describe "Ne" $ genAndCounter "uint .ne -3" "-3"
  describe "Float16" $ do
    describe "Type" $ genAndCounter "float16" "\"foo\""
    describe "Ge" $ genAndCounter "float16 .ge 3.5" "2.5"
    describe "Gt" $ genAndCounter "float16 .gt 3.5" "3.5"
    describe "Le" $ genAndCounter "float16 .le 3.5" "4.5"
    describe "Lt" $ genAndCounter "float16 .lt 3.5" "3.5"
    describe "Eq" $ genAndCounter "float16 .eq 3.5" "4.5"
    describe "Ne" $ genAndCounter "float16 .ne 3.5" "3.5"
  describe "Float32" $ do
    describe "Type" $ genAndCounter "float32" "\"foo\""
    describe "Ge" $ genAndCounter "float32 .ge 3.5" "2.5"
    describe "Gt" $ genAndCounter "float32 .gt 3.5" "3.5"
    describe "Le" $ genAndCounter "float32 .le 3.5" "4.5"
    describe "Lt" $ genAndCounter "float32 .lt 3.5" "3.5"
    describe "Eq" $ xgenAndCounter "float32 .eq 3.5" "4.5"
    describe "Ne" $ genAndCounter "float32 .ne 3.5" "3.5"
  describe "Float64" $ do
    describe "Type" $ genAndCounter "float64" "\"foo\""
    describe "Literal" $ genAndCounter "3.5" "4.5"
    describe "Ge" $ genAndCounter "float64 .ge 3.5" "2.5"
    describe "Gt" $ genAndCounter "float64 .gt 3.5" "3.5"
    describe "Le" $ genAndCounter "float64 .le 3.5" "4.5"
    describe "Lt" $ genAndCounter "float64 .lt 3.5" "3.5"
    describe "Eq" $ xgenAndCounter "float64 .eq 3.5" "4.5"
    describe "Ne" $ genAndCounter "float64 .ne 3.5" "3.5"
  describe "Bytestring" $ do
    describe "Type" $ genAndCounter "bytes" "3"
    describe "Literal" $ genAndCounter "h'FFFF'" "h'0E0E'"
    describe "Cbor" $ genAndCounter "bytes .cbor bar\nbar = int" "h'63666F6F'" -- cbor of "foo"
    describe "Cborseq" $ genAndCounter "bytes .cborseq bar\nbar = [* int]" "h'63666F6F'"
    xdescribe "Cbor" $ genAndCounter "bytes .cbor int" "h'63666F6F'" -- cbor of "foo"
    xdescribe "Cborseq" $ genAndCounter "bytes .cborseq [* int]" "h'63666F6F'"
    describe "Bits" $
      genAndCounter
        "bytes .bits tcpflags \
        \ tcpflags = &( \
        \  fin: 8, \
        \  syn: 9, \
        \  rst: 10, \
        \  psh: 11, \
        \  ack: 12, \
        \  urg: 13, \
        \  ece: 14, \
        \  cwr: 15, \
        \  ns: 0 \
        \  ) / (4..7)"
        "h'02'"
  describe "Text" $ do
    describe "Type" $ genAndCounter "text" "3"
    describe "Literal" $ genAndCounter "\"foo\"" "\"bar\""
    describe "Regexp" $
      genAndCounter "tstr .regexp \"[A-Za-z0-9]+@[A-Za-z0-9]+(_[A-Za-z0-9]+)+\"" "\"novalid\""
  where
    genAndCounter t c = do
      prop "Gen" $ withMaxSuccess 5 $ generatedCase t
      specify "counterexample" $ counterCase t c
    xgenAndCounter t c = do
      xprop "Gen" $ withMaxSuccess 5 $ generatedCase t
      specify "counterexample" $ counterCase t c

counterCase :: T.Text -> T.Text -> Expectation
counterCase cddl counter = do
  res <-
    bracket
      ((,) <$> emptySystemTempFile "diag.diag" <*> emptySystemTempFile "cbor.cbor")
      (\(f1, f2) -> mapM removeFile [f1, f2])
      ( \(fpdiag, fpcbor) -> do
          T.writeFile fpdiag counter
          _ <- withBinaryFile fpcbor WriteMode $ \h ->
            withCreateProcess
              ((proc "ruby" ["C:/msys64/clang64/bin/diag2cbor.rb", fpdiag]) {std_out = UseHandle h}) $ \_ _ _ -> waitForProcess
          cbor <- BS.readFile fpcbor
          let cddl' =
                fromRight (error "impossible!") $
                  fullResolveCDDL $
                    prependPrelude $
                      fromRight (error "impossible!") $
                        runParser pCDDL "" ("foo = " <> cddl <> "\n")
          pure $ validateCBOR' cbor (Name "foo") cddl'
      )
  res `shouldSatisfy` \case
    Left Right {} -> True
    _ -> False

generatedCase :: T.Text -> Property
generatedCase txt = monadicIO $ do
  res <-
    run $
      bracket
        (emptySystemTempFile "cddl.cddl")
        removeFile
        ( \fpcddl -> do
            T.writeFile fpcddl ("foo = " <> txt <> "\n")
            out <- readProcess "ruby" ["C:/msys64/clang64/bin/cddl", fpcddl, "generate"] ""
            forM (lines out) $ \l ->
              bracket
                ((,) <$> emptySystemTempFile "diag.diag" <*> emptySystemTempFile "cbor.cbor")
                (\(f1, f2) -> mapM removeFile [f1, f2])
                ( \(fpdiag, fpcbor) -> do
                    writeFile fpdiag out
                    _ <- withBinaryFile fpcbor WriteMode $ \h ->
                      withCreateProcess
                        ((proc "ruby" ["C:/msys64/clang64/bin/diag2cbor.rb", fpdiag]) {std_out = UseHandle h}) $ \_ _ _ -> waitForProcess
                    cbor <- BS.readFile fpcbor
                    tt <- T.readFile fpcddl
                    let cddl =
                          fromRight (error "impossible!") $
                            fullResolveCDDL $
                              prependPrelude $
                                fromRight (error "impossible!") $
                                  runParser pCDDL fpcddl tt
                    pure $ (l, validateCBOR' cbor (Name "foo") cddl)
                )
        )
  sequence_ $
    map
      ( \(diag, tc) -> do
          monitor (counterexample $ "CBOR Diag: " <> diag)
          case tc of
            Left (Right e) ->
              monitor (counterexample $ "Validation error: " <> errorBundlePretty e)
            Left (Left e) ->
              monitor (counterexample $ "CBOR parsing error: " <> show e)
            Right _ -> pure ()
          Test.QuickCheck.Monadic.assert $ isRight tc
      )
      res
