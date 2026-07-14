module Test.Codec.CBOR.Cuddle.CBOR.Term (spec) where

import Codec.CBOR.Cuddle.CBOR.Term (
  CBORTerm (..),
  decodeCBORTerm,
  encodeCBORTerm,
  nintMin,
  toNInt,
 )
import Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Either (isLeft)
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

decode :: BSL.ByteString -> Either DeserialiseFailure CBORTerm
decode bs = do
  (rest, t) <- deserialiseFromBytes decodeCBORTerm bs
  if BSL.null rest
    then Right t
    else error $ "decode: leftover bytes " <> show (BSL.unpack rest)

encode :: CBORTerm -> BSL.ByteString
encode = toLazyByteString . encodeCBORTerm

spec :: Spec
spec = do
  describe "decode . encode" $ do
    prop "round-trips arbitrary terms" $ \t ->
      decode (encode t) === Right t

  describe "encode . decode" $ do
    it "reproduces minimal-width wire bytes" $
      mapM_ shouldRoundtripBytes byteVectors

  describe "wire-form fidelity" $ do
    it "decodes bignums as uninterpreted tags" $ do
      -- tag 2 with content 2^64, the smallest positive bignum
      decode (BSL.pack (0xc2 : 0x49 : 0x01 : replicate 8 0x00))
        `shouldBe` Right (TermTag 2 (TermBytes (BS.pack (0x01 : replicate 8 0x00))))
      decode (BSL.pack [0xc3, 0x41, 0x2a])
        `shouldBe` Right (TermTag 3 (TermBytes (BS.pack [0x2a])))

    it "decodes major type 1 down to -2^64" $
      decode (BSL.pack (0x3b : replicate 8 0xff))
        `shouldBe` Right (TermNInt (fromJust (toNInt nintMin)))

  describe "ill-formed input is rejected" $ do
    it "a lone break stop code" $
      decode (BSL.pack [0xff]) `shouldSatisfy` isLeft
    it "reserved additional information 28..30" $
      mapM_
        (\w -> decode (BSL.pack [w]) `shouldSatisfy` isLeft)
        [mt * 32 + ai | mt <- [0 .. 7], ai <- [28 .. 30]]
    it "indefinite-length integers and tags" $
      mapM_
        (\w -> decode (BSL.pack [w]) `shouldSatisfy` isLeft)
        [0x1f, 0x3f, 0xdf]
    it "an indefinite string with a non-string chunk" $
      decode (BSL.pack [0x7f, 0x41, 0x61, 0xff]) `shouldSatisfy` isLeft

shouldRoundtripBytes :: [Word8] -> Expectation
shouldRoundtripBytes ws =
  fmap encode (decode (BSL.pack ws)) `shouldBe` Right (BSL.pack ws)

-- | Wire encodings with minimal-width arguments, mostly from RFC 8949
-- Appendix A, covering every major type in definite and indefinite form.
byteVectors :: [[Word8]]
byteVectors =
  [ [0x00] -- 0
  , [0x18, 0x2a] -- 42
  , [0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] -- 2^64 - 1
  , [0x20] -- -1
  , [0x3b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff] -- -2^64
  , [0xc2, 0x49, 0x01, 0, 0, 0, 0, 0, 0, 0, 0] -- tag 2 bignum 2^64
  , [0xc3, 0x41, 0x2a] -- tag 3 bignum -43
  , [0xc1, 0x1a, 0x51, 0x4b, 0x67, 0xb0] -- tag 1 epoch time
  , [0x40] -- h''
  , [0x44, 0x01, 0x02, 0x03, 0x04] -- h'01020304'
  , [0x5f, 0x41, 0x61, 0x42, 0x62, 0x63, 0xff] -- (_ h'61', h'6263')
  , [0x5f, 0xff] -- (_ ), empty indefinite bytes
  , [0x60] -- ""
  , [0x64, 0x49, 0x45, 0x54, 0x46] -- "IETF"
  , [0x7f, 0x61, 0x61, 0xff] -- (_ "a")
  , [0x80] -- []
  , [0x83, 0x01, 0x02, 0x03] -- [1, 2, 3]
  , [0x9f, 0x01, 0x02, 0xff] -- [_ 1, 2]
  , [0x9f, 0xff] -- [_ ]
  , [0xa0] -- {}
  , [0xa1, 0x61, 0x61, 0x01] -- {"a": 1}
  , [0xbf, 0x61, 0x61, 0x01, 0xff] -- {_ "a": 1}
  , [0xf4] -- false
  , [0xf5] -- true
  , [0xf6] -- null
  , [0xf7] -- undefined
  , [0xf0] -- simple(16)
  , [0xf8, 0xff] -- simple(255)
  , [0xf9, 0x3c, 0x00] -- 1.0_1
  , [0xf9, 0x7c, 0x00] -- Infinity_1
  , [0xfa, 0x47, 0xc3, 0x50, 0x00] -- 100000.0_2
  , [0xfb, 0x40, 0x09, 0x21, 0xfb, 0x54, 0x44, 0x2d, 0x18] -- pi_3
  ]
