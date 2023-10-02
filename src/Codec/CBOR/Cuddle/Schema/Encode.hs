{-# LANGUAGE TypeFamilies #-}

module Codec.CBOR.Cuddle.Schema.Encode (toEncoder) where

import Codec.CBOR.Cuddle.Schema.Core (Schema (..))
import Codec.CBOR.Encoding (Encoding, encodeInt, encodeString, encodeBytes, encodeListLen)
import Test.QuickCheck (Gen, Arbitrary (..), elements)
import qualified Data.Text as T
import qualified Data.ByteString as BS

encodeArrP :: Schema i a -> Gen Encoding
encodeArrP x = do
  let
    flattenArr :: forall i a. Schema i a -> Gen [Encoding]
    flattenArr (ArrP f a) = (<>) <$> flattenArr f <*> flattenArr a
    flattenArr (PureP _) = pure []
    flattenArr p = (:[]) <$> toEncoder p
  es <- flattenArr x
  case es of
    [] -> pure mempty
    [e] -> pure e
    _ -> pure . mconcat $ encodeListLen (fromIntegral $ length es) : es

encodeChoiceP :: Schema i a -> Gen Encoding
encodeChoiceP p = do
  (i, e') <- elements $ flattenChoice 0 p
  e <- e'
  pure $ encodeListLen 2 <> encodeInt i <> e
  where
    flattenChoice :: forall i a. Int -> Schema i a -> [(Int, Gen Encoding)]
    flattenChoice n (ChoiceP a b) = l <> flattenChoice (n + length l) b
      where
        l = flattenChoice n a
    flattenChoice n a = [(n, toEncoder a)]

toEncoder :: Schema i a -> Gen Encoding
toEncoder (PureP _) = pure mempty
toEncoder p@(ArrP _ _) = encodeArrP p
toEncoder p@(ChoiceP _ _) = encodeChoiceP p
toEncoder (NamedP _ p) = toEncoder p
toEncoder PrimNum = encodeInt <$> arbitrary
toEncoder PrimText = encodeString . T.pack <$> arbitrary
toEncoder PrimBytes = encodeBytes . BS.pack <$> arbitrary
