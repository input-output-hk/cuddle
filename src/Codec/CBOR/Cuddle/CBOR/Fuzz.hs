module Codec.CBOR.Cuddle.CBOR.Fuzz (
  zap,
) where

import Codec.CBOR.Cuddle.CBOR.Validator (ValidationTree)
import Codec.CBOR.Term (Term)

zap :: ValidationTree -> m Term
zap tree = undefined
