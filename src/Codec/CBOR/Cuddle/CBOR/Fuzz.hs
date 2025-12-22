{-# LANGUAGE TypeData #-}

module Codec.CBOR.Cuddle.CBOR.Fuzz (
  FuzzPhase,
  zap,
) where

import Codec.CBOR.Cuddle.CDDL (CDDL)
import Codec.CBOR.Term (Term)

type data FuzzPhase

zap :: CDDL FuzzPhase -> Term -> m Term
zap cddl term = undefined
