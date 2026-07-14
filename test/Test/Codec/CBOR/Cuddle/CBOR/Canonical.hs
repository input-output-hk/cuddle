{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CBOR.Canonical (spec) where

import Codec.CBOR.Cuddle.CBOR.Canonical (
  CanonicalTerm (..),
  toCanonical,
 )
import Codec.CBOR.Cuddle.CBOR.Term (
  CBORTerm (..),
  fromNInt,
  nintMin,
  toNInt,
  uintMax,
 )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Word (Word64)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

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

  describe "toCanonical" $ do
    describe "integer normalization" $ do
      prop "TermUInt maps to CTInt" $ \w ->
        toCanonical (TermUInt w) === CTInt w

      prop "TermNInt maps to CTNInt" $ \n ->
        toCanonical (TermNInt n) === CTNInt n

      prop "in-range positive bignum collapses to CTInt" $ \(w :: Word64) ->
        toCanonical (TermTag 2 (TermBytes (unsignedBE (toInteger w))))
          === toCanonical (TermUInt w)

      prop "in-range negative bignum collapses to CTNInt" $ \nint ->
        let n = fromNInt nint
         in toCanonical (TermTag 3 (TermBytes (unsignedBE (-1 - n))))
              === toCanonical (TermNInt nint)

      it "bignum with leading zeros canonicalizes" $
        toCanonical (TermTag 2 (TermBytes (BS.pack [0, 0, 5])))
          `shouldBe` CTInt 5

      prop "bignum from TermBytesI matches TermBytes" $ \(w :: Word64) ->
        let bs = unsignedBE (toInteger w)
         in toCanonical (TermTag 2 (TermBytesI [BSL.fromStrict bs]))
              === toCanonical (TermTag 2 (TermBytes bs))

      it "true bignum (above uintMax) stays tagged" $
        let n = uintMax + 1
         in toCanonical (TermTag 2 (TermBytes (unsignedBE n)))
              `shouldBe` CTTagged 2 (CTBytes (unsignedBE n))

      it "true bignum (below nintMin) stays tagged" $
        let n = nintMin - 1
         in toCanonical (TermTag 3 (TermBytes (unsignedBE (-1 - n))))
              `shouldBe` CTTagged 3 (CTBytes (unsignedBE (-1 - n)))

    describe "definite/indefinite variants merge" $ do
      it "TermBytes ≡ chunked TermBytesI" $
        toCanonical (TermBytes "abc")
          `shouldBe` toCanonical (TermBytesI ["a", "bc"])

      it "TermString ≡ chunked TermStringI" $
        toCanonical (TermString "abc")
          `shouldBe` toCanonical (TermStringI ["a", "bc"])

      it "TermArray ≡ TermArrayI" $
        toCanonical (TermArray [TermUInt 1, TermUInt 2])
          `shouldBe` toCanonical (TermArrayI [TermUInt 1, TermUInt 2])

      it "TermMap ≡ TermMapI" $
        toCanonical (TermMap [(TermUInt 1, TermUInt 2)])
          `shouldBe` toCanonical (TermMapI [(TermUInt 1, TermUInt 2)])

    describe "simple values" $ do
      prop "TermSimple maps to CTSimple" $ \w ->
        toCanonical (TermSimple w) === CTSimple w

    describe "maps" $ do
      it "key order is irrelevant" $
        toCanonical (TermMap [(TermUInt 1, TermUInt 10), (TermUInt 2, TermUInt 20)])
          `shouldBe` toCanonical (TermMap [(TermUInt 2, TermUInt 20), (TermUInt 1, TermUInt 10)])

    describe "floats stay distinct by width" $ do
      it "TermHalf 1.0 ≠ TermFloat 1.0" $
        toCanonical (TermHalf 1.0) `shouldNotBe` toCanonical (TermFloat 1.0)
      it "TermFloat 1.0 ≠ TermDouble 1.0" $
        toCanonical (TermFloat 1.0) `shouldNotBe` toCanonical (TermDouble 1.0)

-- | Encode a non-negative Integer as a big-endian byte string with no leading zeros.
unsignedBE :: Integer -> BS.ByteString
unsignedBE = BS.pack . reverse . go
  where
    go 0 = []
    go n = fromInteger (n `mod` 256) : go (n `div` 256)
