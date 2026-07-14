module Codec.CBOR.Cuddle.CBOR.Term (
  CBORTerm (..),
  decodeCBORTerm,
  encodeCBORTerm,
  NInt,
  toNInt,
  fromNInt,
  bytesToUnsigned,
  unsignedToBytes,
  uintMax,
  nintMin,
  unwrapBytes,
  unwrapString,
  unwrapArray,
  unwrapMap,
) where

import Codec.CBOR.Cuddle.Comments (CollectComments)
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
import Codec.CBOR.Encoding (
  Encoding,
  encodeBreak,
  encodeBytes,
  encodeBytesIndef,
  encodeDouble,
  encodeFloat,
  encodeFloat16,
  encodeInteger,
  encodeListLen,
  encodeListLenIndef,
  encodeMapLen,
  encodeMapLenIndef,
  encodeSimple,
  encodeString,
  encodeStringIndef,
  encodeTag64,
  encodeWord64,
 )
import Control.Monad (replicateM)
import Data.Bits (complement)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import Numeric.Half (Half, fromHalf, toHalf)
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  chooseInt,
  oneof,
  sized,
  suchThat,
  vectorOf,
 )
import Prelude hiding (decodeFloat, encodeFloat)

-- | A negative integer in the range @[-2^64, -1]@: exactly the values
-- representable by CBOR major type 1 (RFC 8949 §3.1). The range is wider
-- than 'Int64' on the negative side, so it can't be stored as a plain
-- signed integer.
newtype NInt = NInt Word64
  deriving (Eq, Ord, Bounded, Hashable, CollectComments)

instance Show NInt where
  showsPrec p x =
    showParen (p > 10) $ showString "toNInt " . showsPrec 11 (fromNInt x)

instance Arbitrary NInt where
  arbitrary = NInt <$> arbitrary
  shrink = mapMaybe toNInt . shrink . fromNInt

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
  deriving (Generic, Eq, Ord, Show)

-- | Floats never include NaN: the many NaN bit patterns don't survive
-- round-trips through 'Float' (see 'decodeCBORTerm') and compare unequal
-- to themselves anyway.
instance Arbitrary CBORTerm where
  arbitrary = sized genTerm
    where
      genTerm :: Int -> Gen CBORTerm
      genTerm n
        | n <= 0 = leaf
        | otherwise = oneof [leaf, branch]
        where
          leaf =
            oneof
              [ TermUInt <$> arbitrary
              , TermNInt <$> arbitrary
              , TermBytes . BS.pack <$> arbitrary
              , TermBytesI . fmap LBS.pack <$> arbitrary
              , TermString . T.pack <$> arbitrary
              , TermStringI . fmap LT.pack <$> arbitrary
              , TermSimple <$> arbitrary
              , TermHalf . toHalf <$> nonNaN
              , TermFloat <$> nonNaN
              , TermDouble <$> nonNaN
              ]
          nonNaN :: (RealFloat a, Arbitrary a) => Gen a
          nonNaN = arbitrary `suchThat` (not . isNaN)
          branch =
            oneof
              [ TermArray <$> subterms
              , TermArrayI <$> subterms
              , TermMap <$> subpairs
              , TermMapI <$> subpairs
              , TermTag <$> arbitrary <*> genTerm (n - 1)
              ]
          subterms = do
            k <- chooseInt (0, 5)
            vectorOf k . genTerm $ n `div` (k + 1)
          subpairs = do
            k <- chooseInt (0, 5)
            let sub = genTerm $ n `div` (2 * k + 1)
            vectorOf k $ (,) <$> sub <*> sub

  shrink term = case term of
    TermUInt w -> TermUInt <$> shrink w
    TermNInt n -> TermNInt <$> shrink n
    TermBytes bs -> TermBytes . BS.pack <$> shrink (BS.unpack bs)
    TermBytesI chunks -> TermBytesI . fmap LBS.pack <$> shrink (LBS.unpack <$> chunks)
    TermString s -> TermString . T.pack <$> shrink (T.unpack s)
    TermStringI chunks -> TermStringI . fmap LT.pack <$> shrink (LT.unpack <$> chunks)
    TermArray ts -> ts <> (TermArray <$> shrink ts)
    TermArrayI ts -> ts <> (TermArrayI <$> shrink ts)
    TermMap kvs -> subterms kvs <> (TermMap <$> shrink kvs)
    TermMapI kvs -> subterms kvs <> (TermMapI <$> shrink kvs)
    TermTag tag t -> t : (TermTag <$> shrink tag <*> pure t) <> (TermTag tag <$> shrink t)
    TermSimple w -> TermSimple <$> shrink w
    TermHalf _ -> []
    TermFloat f -> TermFloat <$> shrink f
    TermDouble d -> TermDouble <$> shrink d
    where
      subterms = concatMap $ \(k, v) -> [k, v]

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

-- | Encode a term back to CBOR. Integer, length and tag arguments are
-- emitted with minimal width, so @encodeCBORTerm@ after 'decodeCBORTerm'
-- reproduces the input bytes only if the input used minimal-width
-- arguments; the definite\/indefinite structure is always preserved.
encodeCBORTerm :: CBORTerm -> Encoding
encodeCBORTerm term = case term of
  TermUInt w -> encodeWord64 w
  -- For values in @[-2^64, -1]@ 'encodeInteger' always emits major type 1
  -- with argument @-1 - n@, never a bignum.
  TermNInt n -> encodeInteger $ fromNInt n
  TermBytes bs -> encodeBytes bs
  TermBytesI chunks ->
    encodeBytesIndef
      <> foldMap (encodeBytes . LBS.toStrict) chunks
      <> encodeBreak
  TermString s -> encodeString s
  TermStringI chunks ->
    encodeStringIndef
      <> foldMap (encodeString . LT.toStrict) chunks
      <> encodeBreak
  TermArray ts ->
    encodeListLen (fromIntegral $ length ts) <> foldMap encodeCBORTerm ts
  TermArrayI ts ->
    encodeListLenIndef <> foldMap encodeCBORTerm ts <> encodeBreak
  TermMap kvs ->
    encodeMapLen (fromIntegral $ length kvs) <> foldMap encodeKeyValue kvs
  TermMapI kvs ->
    encodeMapLenIndef <> foldMap encodeKeyValue kvs <> encodeBreak
  TermTag tag t -> encodeTag64 tag <> encodeCBORTerm t
  TermSimple w -> encodeSimple w
  TermHalf h -> encodeFloat16 $ fromHalf h
  TermFloat f -> encodeFloat f
  TermDouble d -> encodeDouble d
  where
    encodeKeyValue (k, v) = encodeCBORTerm k <> encodeCBORTerm v

bytesToUnsigned :: ByteString -> Integer
bytesToUnsigned = BS.foldl' (\acc b -> acc * 256 + toInteger b) 0

unsignedToBytes :: Integer -> ByteString
unsignedToBytes = BS.pack . reverse . go
  where
    go 0 = []
    go n = let (d, r) = divMod n 256 in fromInteger r : go d

unwrapBytes :: CBORTerm -> Maybe ByteString
unwrapBytes (TermBytes bs) = Just bs
unwrapBytes (TermBytesI bss) = Just . LBS.toStrict $ mconcat bss
unwrapBytes _ = Nothing

unwrapString :: CBORTerm -> Maybe Text
unwrapString (TermString t) = Just t
unwrapString (TermStringI ts) = Just . LT.toStrict $ mconcat ts
unwrapString _ = Nothing

unwrapArray :: CBORTerm -> Maybe [CBORTerm]
unwrapArray (TermArray xs) = Just xs
unwrapArray (TermArrayI xs) = Just xs
unwrapArray _ = Nothing

unwrapMap :: CBORTerm -> Maybe [(CBORTerm, CBORTerm)]
unwrapMap (TermMap kvs) = Just kvs
unwrapMap (TermMapI kvs) = Just kvs
unwrapMap _ = Nothing

-- Bounds

uintMax :: Integer
uintMax = 2 ^ (64 :: Int) - 1

nintMin :: Integer
nintMin = -(2 ^ (64 :: Int))
