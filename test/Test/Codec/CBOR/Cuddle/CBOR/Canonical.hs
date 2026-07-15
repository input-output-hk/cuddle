{-# LANGUAGE OverloadedStrings #-}

module Test.Codec.CBOR.Cuddle.CBOR.Canonical (spec) where

import Codec.CBOR.Cuddle.CBOR.Canonical (
  CanonicalTerm (..),
  toCanonical,
 )
import Codec.CBOR.Cuddle.CBOR.NInt (fromNInt, nintMin, uintMax)
import Codec.CBOR.Cuddle.CBOR.Term (
  CBORTerm (..),
  mkTermArray,
  mkTermBytes,
  mkTermBytesI,
  mkTermMap,
  mkTermNInt,
  mkTermString,
  mkTermStringI,
  mkTermTag,
  mkTermUInt,
 )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Word (Word64)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "toCanonical" $ do
    describe "integer normalization" $ do
      prop "TermUInt maps to CTInt" $ \w ->
        toCanonical (mkTermUInt w) === CTInt w

      prop "TermNInt maps to CTNInt" $ \n ->
        toCanonical (mkTermNInt n) === CTNInt n

      prop "in-range positive bignum collapses to CTInt" $ \(w :: Word64) ->
        toCanonical (mkTermTag 2 (mkTermBytes (unsignedBE (toInteger w))))
          === toCanonical (mkTermUInt w)

      prop "in-range negative bignum collapses to CTNInt" $ \nint ->
        let n = fromNInt nint
         in toCanonical (mkTermTag 3 (mkTermBytes (unsignedBE (-1 - n))))
              === toCanonical (mkTermNInt nint)

      it "bignum with leading zeros canonicalizes" $
        toCanonical (mkTermTag 2 (mkTermBytes (BS.pack [0, 0, 5])))
          `shouldBe` CTInt 5

      prop "bignum from TermBytesI matches TermBytes" $ \(w :: Word64) ->
        let bs = unsignedBE (toInteger w)
         in toCanonical (mkTermTag 2 (mkTermBytesI [BSL.fromStrict bs]))
              === toCanonical (mkTermTag 2 (mkTermBytes bs))

      it "true bignum (above uintMax) stays tagged" $
        let n = uintMax + 1
         in toCanonical (mkTermTag 2 (mkTermBytes (unsignedBE n)))
              `shouldBe` CTTagged 2 (CTBytes (unsignedBE n))

      it "true bignum (below nintMin) stays tagged" $
        let n = nintMin - 1
         in toCanonical (mkTermTag 3 (mkTermBytes (unsignedBE (-1 - n))))
              `shouldBe` CTTagged 3 (CTBytes (unsignedBE (-1 - n)))

    describe "definite/indefinite variants merge" $ do
      it "TermBytes ≡ chunked TermBytesI" $
        toCanonical (mkTermBytes "abc")
          `shouldBe` toCanonical (mkTermBytesI ["a", "bc"])

      it "TermString ≡ chunked TermStringI" $
        toCanonical (mkTermString "abc")
          `shouldBe` toCanonical (mkTermStringI ["a", "bc"])

      it "TermArray ≡ TermArrayI" $
        toCanonical (mkTermArray [mkTermUInt 1, mkTermUInt 2])
          `shouldBe` toCanonical (TermArrayI [mkTermUInt 1, mkTermUInt 2])

      it "TermMap ≡ TermMapI" $
        toCanonical (mkTermMap [(mkTermUInt 1, mkTermUInt 2)])
          `shouldBe` toCanonical (TermMapI [(mkTermUInt 1, mkTermUInt 2)])

    describe "simple values" $ do
      prop "TermSimple maps to CTSimple" $ \w ->
        toCanonical (TermSimple w) === CTSimple w

    describe "maps" $ do
      it "key order is irrelevant" $
        toCanonical (mkTermMap [(mkTermUInt 1, mkTermUInt 10), (mkTermUInt 2, mkTermUInt 20)])
          `shouldBe` toCanonical (mkTermMap [(mkTermUInt 2, mkTermUInt 20), (mkTermUInt 1, mkTermUInt 10)])

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
