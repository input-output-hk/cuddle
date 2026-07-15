module Codec.CBOR.Cuddle.CBOR.NInt (
  NInt,
  fromNInt,
  toNInt,
  uintMax,
  nintMin,

  -- * Internal

  -- | These expose the raw representation and are intended for the
  -- encoder and decoder in "Codec.CBOR.Cuddle.CBOR.Term" only; use
  -- 'toNInt' and 'fromNInt' everywhere else.
  toWord64Raw,
  fromWord64Raw,
) where

import Codec.CBOR.Cuddle.Comments (CollectComments)
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Data.Word (Word64)
import Test.QuickCheck (Arbitrary (..))

-- | A negative integer in the range @[-2^64, -1]@: exactly the values
-- representable by CBOR major type 1 (RFC 8949 §3.1). The range is wider
-- than 'Int64' on the negative side, so it can't be stored as a plain
-- signed integer.
--
-- Internally, the value is stored as its wire argument @n@, denoting @-1 - n@
-- (RFC 8949 §3.1).
newtype NInt = NInt Word64
  deriving (Eq, Hashable, CollectComments)

instance Ord NInt where
  compare (NInt a) (NInt b) = compare b a

instance Bounded NInt where
  minBound = NInt maxBound
  maxBound = NInt minBound

instance Show NInt where
  showsPrec p x =
    showParen (p > 10) $ showString "toNInt " . showsPrec 11 (fromNInt x)

instance Arbitrary NInt where
  arbitrary = NInt <$> arbitrary
  shrink = mapMaybe toNInt . shrink . fromNInt

toNInt :: Integer -> Maybe NInt
toNInt x
  | x >= nintMin && x < 0 = Just . NInt . fromInteger $ -1 - x
  | otherwise = Nothing

fromNInt :: NInt -> Integer
fromNInt (NInt n) = -1 - toInteger n

toWord64Raw :: NInt -> Word64
toWord64Raw (NInt n) = n

fromWord64Raw :: Word64 -> NInt
fromWord64Raw = NInt

-- Bounds

uintMax :: Integer
uintMax = 2 ^ (64 :: Int) - 1

nintMin :: Integer
nintMin = -(2 ^ (64 :: Int))
