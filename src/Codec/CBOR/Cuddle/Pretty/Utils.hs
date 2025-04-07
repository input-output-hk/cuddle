module Codec.CBOR.Cuddle.Pretty.Utils where

import Data.Text qualified as T
import Prettyprinter (Doc, flatAlt, layoutCompact, space)
import Prettyprinter.Render.Text (renderStrict)

renderedLen :: Doc ann -> Int
renderedLen = T.length . renderStrict . layoutCompact

softspace :: Doc ann
softspace = flatAlt space mempty

spaces :: Int -> Doc ann
spaces i = mconcat $ replicate i softspace

fillLeft :: Int -> Doc ann -> Doc ann
fillLeft len doc = spaces (len - renderedLen doc) <> doc

fillRight :: Int -> Doc ann -> Doc ann
fillRight len doc = doc <> spaces (len - renderedLen doc)
