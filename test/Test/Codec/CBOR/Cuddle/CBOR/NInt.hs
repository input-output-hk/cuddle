module Test.Codec.CBOR.Cuddle.CBOR.NInt (spec) where

import Codec.CBOR.Cuddle.CBOR.NInt
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
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
