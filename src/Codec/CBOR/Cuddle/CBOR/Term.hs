module Codec.CBOR.Cuddle.CBOR.Term (
  CBORTerm (..),
  decodeCBORTerm,
  NInt,
  toNInt,
  fromNInt,
  uintMax,
  nintMin,
) where

import Codec.CBOR.Decoding (
  Decoder,
  TokenType (..),
  decodeBreakOr,
  decodeBytes,
  decodeBytesIndef,
  decodeDouble,
  decodeFloat,
  decodeListLen,
  decodeListLenIndef,
  decodeMapLen,
  decodeMapLenIndef,
  decodeNegWord64,
  decodeSimple,
  decodeString,
  decodeStringIndef,
  decodeTag64,
  decodeWord64,
  peekTokenType,
 )
import Control.Monad (replicateM)
import Data.Bits (complement)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Word (Word64, Word8)
import Numeric.Half (Half, toHalf)
import Test.QuickCheck (Arbitrary (..))
import Prelude hiding (decodeFloat)

-- | A negative integer in the range @[-2^64, -1]@: exactly the values
-- representable by CBOR major type 1 (RFC 8949 §3.1). The range is wider
-- than 'Int64' on the negative side, so it can't be stored as a plain
-- signed integer.
newtype NInt = NInt Word64
  deriving (Eq, Ord, Bounded)

instance Show NInt where
  showsPrec p x =
    showParen (p > 10) $ showString "toNInt " . showsPrec 11 (fromNInt x)

instance Arbitrary NInt where
  arbitrary = NInt <$> arbitrary

toNInt :: Integer -> Maybe NInt
toNInt x
  | x >= nintMin && x < 0 = Just . NInt . fromInteger $ x - nintMin
  | otherwise = Nothing

fromNInt :: NInt -> Integer
fromNInt (NInt n) = nintMin + toInteger n

data CBORTerm
  = -- | Major type 0
    TermUInt Word64
  | -- | Major type 1
    TermNInt NInt
  | -- | Major type 2 (definite)
    TermBytes BS.ByteString
  | -- | Major type 2 (indefinite)
    TermBytesI [LBS.ByteString]
  | -- | Major type 3 (definite)
    TermString T.Text
  | -- | Major type 3 (indefinite)
    TermStringI [LT.Text]
  | -- | Major type 4 (definite)
    TermArray [CBORTerm]
  | -- | Major type 4 (indefinite)
    TermArrayI [CBORTerm]
  | -- | Major type 5 (definite)
    TermMap [(CBORTerm, CBORTerm)]
  | -- | Major type 5 (indefinite)
    TermMapI [(CBORTerm, CBORTerm)]
  | -- | Major type 6
    TermTag Word64 CBORTerm
  | -- | Major type 7 (0..24)
    TermSimple Word8
  | -- | Major type 7 (25)
    TermHalf Half
  | -- | Major type 7 (26)
    TermFloat Float
  | -- | Major type 7 (27)
    TermDouble Double
  deriving (Eq, Show)

decodeCBORTerm :: Decoder s CBORTerm
decodeCBORTerm = do
  tokType <- peekTokenType
  case tokType of
    TypeUInt -> TermUInt <$> decodeWord64
    TypeUInt64 -> TermUInt <$> decodeWord64
    -- 'decodeNegWord64' yields the argument @n@ of the wire encoding, which
    -- denotes the value @-1 - n@, i.e. @nintMin + complement n@.
    TypeNInt -> TermNInt . NInt . complement <$> decodeNegWord64
    TypeNInt64 -> TermNInt . NInt . complement <$> decodeNegWord64
    -- 'peekTokenType' classifies tags 2 and 3 (bignums) as 'TypeInteger'
    -- so that 'Integer' decoders know to take the bignum path. We want the
    -- uninterpreted term, and 'decodeTag64' accepts these headers like any
    -- other tag.
    TypeInteger -> decodeTagged
    -- Half NaNs with non-canonical payloads may not survive the round-trip
    -- through 'Float': cborg only exposes the half as a widened 'Float'.
    TypeFloat16 -> TermHalf . toHalf <$> decodeFloat
    TypeFloat32 -> TermFloat <$> decodeFloat
    TypeFloat64 -> TermDouble <$> decodeDouble
    TypeBytes -> TermBytes <$> decodeBytes
    TypeBytesIndef ->
      decodeBytesIndef
        >> TermBytesI <$> decodeChunks (LBS.fromStrict <$> decodeBytes)
    TypeString -> TermString <$> decodeString
    TypeStringIndef ->
      decodeStringIndef
        >> TermStringI <$> decodeChunks (LT.fromStrict <$> decodeString)
    TypeListLen -> decodeArray
    TypeListLen64 -> decodeArray
    TypeListLenIndef ->
      decodeListLenIndef >> TermArrayI <$> decodeChunks decodeCBORTerm
    TypeMapLen -> decodeMap
    TypeMapLen64 -> decodeMap
    TypeMapLenIndef ->
      decodeMapLenIndef >> TermMapI <$> decodeChunks decodeKeyValue
    TypeTag -> decodeTagged
    TypeTag64 -> decodeTagged
    -- Bools, null and undefined are simple values like any other;
    -- 'decodeSimple' accepts the whole simple range.
    TypeBool -> TermSimple <$> decodeSimple
    TypeNull -> TermSimple <$> decodeSimple
    TypeSimple -> TermSimple <$> decodeSimple
    TypeBreak -> fail "decodeCBORTerm: unexpected break"
    TypeInvalid -> fail "decodeCBORTerm: invalid token"
  where
    decodeTagged = do
      tag <- decodeTag64
      TermTag tag <$> decodeCBORTerm
    decodeArray = do
      n <- decodeListLen
      TermArray <$> replicateM n decodeCBORTerm
    decodeMap = do
      n <- decodeMapLen
      TermMap <$> replicateM n decodeKeyValue
    decodeKeyValue = (,) <$> decodeCBORTerm <*> decodeCBORTerm

-- | Decode items with @dec@ until hitting (and consuming) a break stop code.
decodeChunks :: Decoder s a -> Decoder s [a]
decodeChunks dec = go
  where
    go = do
      done <- decodeBreakOr
      if done then pure [] else (:) <$> dec <*> go

-- Bounds

uintMax :: Integer
uintMax = 2 ^ (64 :: Int) - 1

nintMin :: Integer
nintMin = -(2 ^ (64 :: Int))
