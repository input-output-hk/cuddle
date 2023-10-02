{-# LANGUAGE TypeFamilies #-}

module Codec.CBOR.Cuddle.Schema.Decode (toDecoder) where

import Codec.CBOR.Cuddle.Schema.Core (Schema (..))
import Codec.CBOR.Decoding (Decoder, decodeInt, decodeBytes, decodeString, decodeListLenOf)
import Control.Monad (when)

toDecoder :: Schema i a -> Decoder s a
toDecoder (PureP x) = pure x
toDecoder p@(ArrP _ _) = do
  case arrDecoder p of
    (e, n) 
      | n < 2     -> e
      | otherwise -> do
        decodeListLenOf n
        fst $ arrDecoder p
toDecoder p@(ChoiceP _ _) = do
  decodeListLenOf 2
  len <- decodeInt
  when (len < 0) $ fail "tag is negative"
  let 
    lookupDec 0 (d:_) = d
    lookupDec n (_:ds) = lookupDec (n - 1) ds
    lookupDec _ [] = fail "tag out of range"
  lookupDec len $ choiceDecoder p
toDecoder (NamedP _ x) = toDecoder x
toDecoder PrimNum = decodeInt
toDecoder PrimText = decodeString
toDecoder PrimBytes = decodeBytes

arrDecoder :: Schema i a -> (Decoder s a, Int)
arrDecoder (ArrP f x) = (fd <*> xd, fl + xl)
  where
    (fd, fl) = arrDecoder f
    (xd, xl) = arrDecoder x
arrDecoder (PureP x) = (pure x, 0)
arrDecoder p = (toDecoder p, 1)

choiceDecoder :: Schema i a -> [Decoder s a]
choiceDecoder (ChoiceP x y) = choiceDecoder x <> choiceDecoder y
choiceDecoder p = [toDecoder p]
