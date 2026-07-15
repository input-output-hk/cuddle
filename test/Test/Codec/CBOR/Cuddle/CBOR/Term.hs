module Test.Codec.CBOR.Cuddle.CBOR.Term (spec) where

import Codec.CBOR.Cuddle.CBOR.Term (
  ArgWidth (..),
  CBORTerm (..),
  decodeCBORTerm,
  encodeCBORTerm,
  fromNInt,
  isValidWidth,
  mkTermBytes,
  mkTermNInt,
  mkTermTag,
  nintMin,
  optimalWidth,
  toNInt,
 )
import Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Either (isLeft)
import Data.Maybe (fromJust)
import Data.Text qualified as T
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
  describe "NInt" $ do
    it "toNInt accepts exactly [-2^64, -1]" $ do
      toNInt 0 `shouldBe` Nothing
      toNInt 1 `shouldBe` Nothing
      toNInt (nintMin - 1) `shouldBe` Nothing
      toNInt nintMin `shouldNotBe` Nothing
      toNInt (-1) `shouldNotBe` Nothing

    prop "toNInt . fromNInt = Just" $ \n ->
      toNInt (fromNInt n) === Just n

    it "boundary values roundtrip" $ do
      fmap fromNInt (toNInt (-1)) `shouldBe` Just (-1)
      fmap fromNInt (toNInt nintMin) `shouldBe` Just nintMin

    prop "Ord on NInt agrees with Ord on the represented Integer" $ \a b ->
      compare a b === compare (fromNInt a) (fromNInt b)

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
        `shouldBe` Right (mkTermTag 2 (mkTermBytes (BS.pack (0x01 : replicate 8 0x00))))
      decode (BSL.pack [0xc3, 0x41, 0x2a])
        `shouldBe` Right (mkTermTag 3 (mkTermBytes (BS.pack [0x2a])))

    it "decodes major type 1 down to -2^64" $
      decode (BSL.pack (0x3b : replicate 8 0xff))
        `shouldBe` Right (mkTermNInt (fromJust (toNInt nintMin)))

  describe "ArgWidth" $ do
    it "optimalWidth picks the width at each boundary" $ do
      optimalWidth 23 `shouldBe` InlineArg
      optimalWidth 24 `shouldBe` OneByteArg
      optimalWidth 0xff `shouldBe` OneByteArg
      optimalWidth 0x100 `shouldBe` TwoByteArg
      optimalWidth 0xff_ff `shouldBe` TwoByteArg
      optimalWidth 0x1_00_00 `shouldBe` FourByteArg
      optimalWidth 0xff_ff_ff_ff `shouldBe` FourByteArg
      optimalWidth 0x1_00_00_00_00 `shouldBe` EightByteArg

    prop "optimalWidth yields a valid width" $ \v ->
      isValidWidth v (optimalWidth v)

    prop "no width below optimalWidth is valid" $ \v ->
      all (not . isValidWidth v) [w | w <- [InlineArg ..], w < optimalWidth v]

    it "encodes the argument at each width" $ do
      encode (TermUInt' InlineArg 5) `shouldBe` BSL.pack [0x05]
      encode (TermUInt' OneByteArg 5) `shouldBe` BSL.pack [0x18, 0x05]
      encode (TermUInt' TwoByteArg 5) `shouldBe` BSL.pack [0x19, 0x00, 0x05]
      encode (TermUInt' FourByteArg 5) `shouldBe` BSL.pack [0x1a, 0x00, 0x00, 0x00, 0x05]
      encode (TermUInt' EightByteArg 5)
        `shouldBe` BSL.pack (0x1b : replicate 7 0x00 <> [0x05])

    it "decodes non-minimal argument widths" $ do
      decode (BSL.pack [0x18, 0x05]) `shouldBe` Right (TermUInt' OneByteArg 5)
      decode (BSL.pack [0x38, 0x04])
        `shouldBe` Right (TermNInt' OneByteArg (fromJust (toNInt (-5))))
      decode (BSL.pack [0x58, 0x01, 0x00])
        `shouldBe` Right (TermBytes' OneByteArg (BS.pack [0x00]))
      decode (BSL.pack [0x78, 0x01, 0x61])
        `shouldBe` Right (TermString' OneByteArg (T.pack "a"))
      decode (BSL.pack [0x98, 0x02, 0x01, 0x02])
        `shouldBe` Right (TermArray' OneByteArg [TermUInt' InlineArg 1, TermUInt' InlineArg 2])
      decode (BSL.pack [0xb8, 0x01, 0x01, 0x02])
        `shouldBe` Right (TermMap' OneByteArg [(TermUInt' InlineArg 1, TermUInt' InlineArg 2)])
      decode (BSL.pack [0xd8, 0x2a, 0x00])
        `shouldBe` Right (TermTag' OneByteArg 42 (TermUInt' InlineArg 0))
      decode (BSL.pack [0x5f, 0x58, 0x01, 0x61, 0xff])
        `shouldBe` Right (TermBytesI' [(OneByteArg, BSL.pack [0x61])])

    it "round-trips non-minimal wire bytes exactly" $
      mapM_ shouldRoundtripBytes nonMinimalByteVectors

    it "refuses to encode an argument that does not fit the width" $
      evaluate (BSL.length (encode (TermUInt' InlineArg 24))) `shouldThrow` anyErrorCall

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

-- | Well-formed but non-preferred encodings: every argument is stored
-- wider than necessary and must survive a decode/encode round-trip.
nonMinimalByteVectors :: [[Word8]]
nonMinimalByteVectors =
  [ [0x18, 0x05] -- 5 with a one-byte argument
  , [0x19, 0x00, 0x05] -- 5 with a two-byte argument
  , [0x1a, 0x00, 0x00, 0x00, 0x05] -- 5 with a four-byte argument
  , [0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05] -- 5, eight bytes
  , [0x38, 0x00] -- -1 with a one-byte argument
  , [0x59, 0x00, 0x01, 0x2a] -- h'2a' with a two-byte length
  , [0x7a, 0x00, 0x00, 0x00, 0x01, 0x61] -- "a" with a four-byte length
  , [0x98, 0x01, 0x00] -- [0] with a one-byte length
  , [0xb8, 0x01, 0x00, 0x01] -- {0: 1} with a one-byte length
  , [0xd8, 0x01, 0x00] -- tag 1 with a one-byte argument
  , [0x5f, 0x59, 0x00, 0x01, 0x61, 0xff] -- indefinite bytes, wide chunk length
  , [0x7f, 0x78, 0x01, 0x61, 0xff] -- indefinite text, wide chunk length
  ]
