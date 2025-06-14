{-# LANGUAGE OverloadedStrings #-}

module Codec.CBOR.Cuddle.CDDL.Prelude (prependPrelude) where

import Codec.CBOR.Cuddle.CDDL (CDDL (..))
import Codec.CBOR.Cuddle.Parser (pCDDL)
import Text.Megaparsec (errorBundlePretty, parse)

-- TODO switch to quasiquotes
cddlPrelude :: CDDL
cddlPrelude =
  either (error . errorBundlePretty) id $
    parse
      pCDDL
      "<HARDCODED>"
      " any = # \
      \ uint = #0 \
      \ nint = #1 \
      \ int = uint / nint \
      \ \
      \ bstr = #2 \
      \ bytes = bstr \
      \ tstr = #3 \
      \ text = tstr \
      \ \
      \ tdate = #6.0(tstr) \
      \ time = #6.1(number) \
      \ number = int / float \
      \ biguint = #6.2(bstr) \
      \ bignint = #6.3(bstr) \
      \ bigint = biguint / bignint \
      \ integer = int / bigint \
      \ unsigned = uint / biguint \
      \ decfrac = #6.4([e10: int, m: integer]) \
      \ bigfloat = #6.5([e2: int, m: integer]) \
      \ eb64url = #6.21(any) \
      \ eb64legacy = #6.22(any) \
      \ eb16 = #6.23(any) \
      \ encoded-cbor = #6.24(bstr) \
      \ uri = #6.32(tstr) \
      \ b64url = #6.33(tstr) \
      \ b64legacy = #6.34(tstr) \
      \ regexp = #6.35(tstr) \
      \ mime-message = #6.36(tstr) \
      \ cbor-any = #6.55799(any) \
      \ float16 = #7.25 \
      \ float32 = #7.26 \
      \ float64 = #7.27 \
      \ float16-32 = float16 / float32 \
      \ float32-64 = float32 / float64 \
      \ float = float16-32 / float64 \
      \  \
      \ false = #7.20 \
      \ true = #7.21 \
      \ bool = false / true \
      \ nil = #7.22 \
      \ null = nil \
      \ undefined = #7.23"

prependPrelude :: CDDL -> CDDL
prependPrelude = (cddlPrelude <>)
