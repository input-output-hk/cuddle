{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.CBOR.Cuddle.CBOR.Term (
  CBORTerm (
    ..,
    TermUInt,
    TermNInt,
    TermBytes,
    TermBytesI,
    TermString,
    TermStringI,
    TermArray,
    TermMap,
    TermTag
  ),
  mkTermUInt,
  mkTermNInt,
  mkTermBytes,
  mkTermBytesI,
  mkTermString,
  mkTermStringI,
  mkTermArray,
  mkTermMap,
  mkTermTag,
  unwrapBytes,
  unwrapString,
  unwrapArray,
  unwrapMap,
  decodeCBORTerm,
  encodeCBORTerm,

  -- * Argument width
  ArgWidth (..),
  isValidWidth,
  optimalWidth,
  genValidWidth,
  textArg,

  -- * NInt
  NInt,
  toNInt,
  fromNInt,
  nintArg,

  -- * Utils
  bytesToUnsigned,
  unsignedToBytes,
  uintMax,
  nintMin,
) where

import Codec.CBOR.Cuddle.Comments (CollectComments)
import Codec.CBOR.Decoding (
  ByteOffset,
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
  peekByteOffset,
  peekTokenType,
 )
import Codec.CBOR.Encoding (
  Encoding,
  encodeBreak,
  encodeBytesIndef,
  encodeDouble,
  encodeFloat,
  encodeFloat16,
  encodeListLenIndef,
  encodeMapLenIndef,
  encodePreEncoded,
  encodeSimple,
  encodeStringIndef,
 )
import Control.Monad (forM, replicateM)
import Data.Bits (Bits (..), complement)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as LT
import Data.Text.Unsafe (lengthWord8)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import Numeric.Half (Half, fromHalf, toHalf)
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  choose,
  elements,
  listOf,
  oneof,
  scale,
  shrinkList,
  sized,
  suchThat,
 )
import Test.QuickCheck.GenT (MonadGen (liftGen))
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

data ArgWidth
  = InlineArg
  | OneByteArg
  | TwoByteArg
  | FourByteArg
  | EightByteArg
  deriving (Eq, Ord, Enum, Show)

isValidWidth :: Word64 -> ArgWidth -> Bool
isValidWidth i = \case
  InlineArg -> i < 24
  OneByteArg -> i <= 0xff
  TwoByteArg -> i <= 0xff_ff
  FourByteArg -> i <= 0xff_ff_ff_ff
  EightByteArg -> i <= 0xff_ff_ff_ff_ff_ff_ff_ff

optimalWidth :: Word64 -> ArgWidth
optimalWidth i
  | i < 24 = InlineArg
  | i <= 0xff = OneByteArg
  | i <= 0xff_ff = TwoByteArg
  | i <= 0xff_ff_ff_ff = FourByteArg
  | otherwise = EightByteArg

-- | All well-formed widths strictly smaller than the given one.
shrinkWidth :: Word64 -> ArgWidth -> [ArgWidth]
shrinkWidth v w = [w' | w' <- [InlineArg ..], w' < w, isValidWidth v w']

-- | The wire argument of a major type 1 head encoding the value @v@ is
-- @-1 - v@ (RFC 8949 §3.1), which is the complement of the offset stored
-- in 'NInt'.
nintArg :: NInt -> Word64
nintArg (NInt n) = complement n

-- | The length argument of a text string head counts UTF-8 bytes, not
-- characters.
textArg :: T.Text -> Word64
textArg = fromIntegral . lengthWord8

data CBORTerm
  = -- | Major type 0
    TermUInt' ArgWidth Word64
  | -- | Major type 1
    TermNInt' ArgWidth NInt
  | -- | Major type 2 (definite)
    TermBytes' ArgWidth BS.ByteString
  | -- | Major type 2 (indefinite)
    TermBytesI' [(ArgWidth, LBS.ByteString)]
  | -- | Major type 3 (definite)
    TermString' ArgWidth T.Text
  | -- | Major type 3 (indefinite)
    TermStringI' [(ArgWidth, LT.Text)]
  | -- | Major type 4 (definite)
    TermArray' ArgWidth [CBORTerm]
  | -- | Major type 4 (indefinite)
    TermArrayI [CBORTerm]
  | -- | Major type 5 (definite)
    TermMap' ArgWidth [(CBORTerm, CBORTerm)]
  | -- | Major type 5 (indefinite)
    TermMapI [(CBORTerm, CBORTerm)]
  | -- | Major type 6
    TermTag' ArgWidth Word64 CBORTerm
  | -- | Major type 7 (0..24)
    TermSimple Word8
  | -- | Major type 7 (25)
    TermHalf Half
  | -- | Major type 7 (26)
    TermFloat Float
  | -- | Major type 7 (27)
    TermDouble Double
  deriving (Generic, Eq, Ord, Show)

pattern TermUInt :: Word64 -> CBORTerm
pattern TermUInt v <- TermUInt' _ v

pattern TermNInt :: NInt -> CBORTerm
pattern TermNInt v <- TermNInt' _ v

pattern TermBytes :: BS.ByteString -> CBORTerm
pattern TermBytes bs <- TermBytes' _ bs

pattern TermBytesI :: [LBS.ByteString] -> CBORTerm
pattern TermBytesI bss <- TermBytesI' (fmap snd -> bss)

pattern TermString :: T.Text -> CBORTerm
pattern TermString t <- TermString' _ t

pattern TermStringI :: [LT.Text] -> CBORTerm
pattern TermStringI ts <- TermStringI' (fmap snd -> ts)

pattern TermArray :: [CBORTerm] -> CBORTerm
pattern TermArray es <- TermArray' _ es

pattern TermMap :: [(CBORTerm, CBORTerm)] -> CBORTerm
pattern TermMap kvs <- TermMap' _ kvs

pattern TermTag :: Word64 -> CBORTerm -> CBORTerm
pattern TermTag t v <- TermTag' _ t v

{-# COMPLETE
  TermUInt
  , TermNInt
  , TermBytes
  , TermBytesI
  , TermString
  , TermStringI
  , TermArray
  , TermArrayI
  , TermMap
  , TermMapI
  , TermTag
  , TermSimple
  , TermHalf
  , TermFloat
  , TermDouble
  #-}

mkTermUInt :: Word64 -> CBORTerm
mkTermUInt v = TermUInt' (optimalWidth v) v

mkTermNInt :: NInt -> CBORTerm
mkTermNInt v = TermNInt' (optimalWidth $ nintArg v) v

mkTermBytes :: BS.ByteString -> CBORTerm
mkTermBytes bs = TermBytes' (optimalWidth . fromIntegral $ BS.length bs) bs

mkTermBytesI :: [LBS.ByteString] -> CBORTerm
mkTermBytesI bss = TermBytesI' $ (\bs -> (optimalWidth . fromIntegral $ LBS.length bs, bs)) <$> bss

mkTermString :: T.Text -> CBORTerm
mkTermString t = TermString' (optimalWidth $ textArg t) t

mkTermStringI :: [LT.Text] -> CBORTerm
mkTermStringI ts = TermStringI' $ (\t -> (optimalWidth . textArg $ LT.toStrict t, t)) <$> ts

mkTermArray :: [CBORTerm] -> CBORTerm
mkTermArray es = TermArray' (optimalWidth . fromIntegral $ length es) es

mkTermMap :: [(CBORTerm, CBORTerm)] -> CBORTerm
mkTermMap kvs = TermMap' (optimalWidth . fromIntegral $ length kvs) kvs

mkTermTag :: Word64 -> CBORTerm -> CBORTerm
mkTermTag t = TermTag' (optimalWidth t) t

genValidWidth :: MonadGen m => Word64 -> m ArgWidth
genValidWidth v = liftGen $ elements [x | x <- [InlineArg .. EightByteArg], isValidWidth v x]

-- | Floats never include NaN: the many NaN bit patterns don't survive
-- round-trips through 'Float' (see 'decodeCBORTerm') and compare unequal
-- to themselves anyway.
instance Arbitrary CBORTerm where
  arbitrary = sized $ \sz ->
    if sz <= 0
      then leaf
      else oneof [leaf, branch]
    where
      leaf =
        oneof
          [ do
              v <- arbitrary
              w <- genValidWidth v
              pure $ TermUInt' w v
          , do
              v <- arbitrary
              w <- genValidWidth $ nintArg v
              pure $ TermNInt' w v
          , do
              bs <- BS.pack <$> arbitrary
              w <- genValidWidth . fromIntegral $ BS.length bs
              pure $ TermBytes' w bs
          , do
              bss <- listOf $ LBS.pack <$> arbitrary
              bsw <- forM bss $ \bs -> do
                w <- genValidWidth . fromIntegral $ LBS.length bs
                pure (w, bs)
              pure $ TermBytesI' bsw
          , do
              t <- T.pack <$> arbitrary
              w <- genValidWidth $ textArg t
              pure $ TermString' w t
          , do
              tss <- listOf $ LT.pack <$> arbitrary
              tsw <- forM tss $ \t -> do
                w <- genValidWidth . textArg $ LT.toStrict t
                pure (w, t)
              pure $ TermStringI' tsw
          , TermSimple <$> arbitrary
          , TermHalf . toHalf <$> nonNaN
          , TermFloat <$> nonNaN
          , TermDouble <$> nonNaN
          ]
      nonNaN :: (RealFloat a, Arbitrary a) => Gen a
      nonNaN = arbitrary `suchThat` (not . isNaN)
      branch =
        sized $ \sz ->
          oneof
            [ do
                n <- choose (0, max 0 sz)
                es <- replicateM n $ scale (`div` n) arbitrary
                w <- genValidWidth . fromIntegral $ length es
                pure $ TermArray' w es
            , do
                n <- choose (0, max 0 sz)
                es <- replicateM n $ scale (`div` n) arbitrary
                pure $ TermArrayI es
            , do
                n <- choose (0, max 0 sz)
                es <- replicateM n $ scale (`div` n) arbitrary
                w <- genValidWidth . fromIntegral $ length es
                pure $ TermMap' w es
            , do
                n <- choose (0, max 0 sz)
                es <- replicateM n $ scale (`div` n) arbitrary
                pure $ TermMapI es
            , do
                t <- arbitrary
                v <- scale (\x -> if x > 0 then x - 1 else x) arbitrary
                w <- genValidWidth t
                pure $ TermTag' w t v
            ]

  shrink term = case term of
    TermUInt' w v ->
      [TermUInt' w' v | w' <- shrinkWidth v w]
        <> [TermUInt' w v' | v' <- shrink v]
    TermNInt' w n ->
      [TermNInt' w' n | w' <- shrinkWidth (nintArg n) w]
        <> [TermNInt' w n' | n' <- shrink n]
    TermBytes' w bs ->
      [TermBytes' w' bs | w' <- shrinkWidth (fromIntegral $ BS.length bs) w]
        <> [TermBytes' w (BS.pack bs') | bs' <- shrink (BS.unpack bs)]
    TermBytesI' chunks -> TermBytesI' <$> shrinkList shrinkBytesChunk chunks
    TermString' w s ->
      [TermString' w' s | w' <- shrinkWidth (textArg s) w]
        <> [TermString' w (T.pack s') | s' <- shrink (T.unpack s)]
    TermStringI' chunks -> TermStringI' <$> shrinkList shrinkStringChunk chunks
    TermArray' w ts ->
      ts
        <> [TermArray' w' ts | w' <- shrinkWidth (fromIntegral $ length ts) w]
        <> (TermArray' w <$> shrink ts)
    TermArrayI ts -> ts <> (TermArrayI <$> shrink ts)
    TermMap' w kvs ->
      subterms kvs
        <> [TermMap' w' kvs | w' <- shrinkWidth (fromIntegral $ length kvs) w]
        <> (TermMap' w <$> shrink kvs)
    TermMapI kvs -> subterms kvs <> (TermMapI <$> shrink kvs)
    TermTag' w tag t ->
      t
        : [TermTag' w' tag t | w' <- shrinkWidth tag w]
          <> (TermTag' w <$> shrink tag <*> pure t)
          <> (TermTag' w tag <$> shrink t)
    TermSimple v -> TermSimple <$> shrink v
    TermHalf _ -> []
    TermFloat f -> TermFloat <$> shrink f
    TermDouble d -> TermDouble <$> shrink d
    where
      subterms = concatMap $ \(k, v) -> [k, v]
      shrinkBytesChunk (w, c) =
        [(w', c) | w' <- shrinkWidth (fromIntegral $ LBS.length c) w]
          <> [(w, LBS.pack c') | c' <- shrink (LBS.unpack c)]
      shrinkStringChunk (w, c) =
        [(w', c) | w' <- shrinkWidth (textArg $ LT.toStrict c) w]
          <> [(w, LT.pack c') | c' <- shrink (LT.unpack c)]

widthFromOffset :: ByteOffset -> ArgWidth
widthFromOffset = \case
  0 -> InlineArg
  1 -> OneByteArg
  2 -> TwoByteArg
  4 -> FourByteArg
  8 -> EightByteArg
  n -> error $ "Impossible head size: " <> show n

decodeWithWidth :: Decoder s a -> Decoder s (ArgWidth, a)
decodeWithWidth = decodeWithLengthWidth $ const 0

-- | Like 'decodeWithWidth', but for decoders that consume both the head and
-- its content: the content's byte count is discounted from the measured
-- head size.
decodeWithLengthWidth :: (a -> Word64) -> Decoder s a -> Decoder s (ArgWidth, a)
decodeWithLengthWidth contentBytes decoder = do
  o1 <- peekByteOffset
  x <- decoder
  o2 <- peekByteOffset
  pure (widthFromOffset (o2 - o1 - 1 - fromIntegral (contentBytes x)), x)

decodeCBORTerm :: Decoder s CBORTerm
decodeCBORTerm = do
  tokType <- peekTokenType
  case tokType of
    TypeUInt -> decodeUInt
    TypeUInt64 -> decodeUInt
    -- 'decodeNegWord64' yields the argument @n@ of the wire encoding, which
    -- denotes the value @-1 - n@, i.e. @nintMin + complement n@.
    TypeNInt -> decodeNInt
    TypeNInt64 -> decodeNInt
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
    TypeBytes -> uncurry TermBytes' <$> decodeBytesWithWidth
    TypeBytesIndef -> do
      decodeBytesIndef
      bsw <- decodeChunks $ do
        (w, bs) <- decodeBytesWithWidth
        pure (w, LBS.fromStrict bs)
      pure $ TermBytesI' bsw
    TypeString -> uncurry TermString' <$> decodeStringWithWidth
    TypeStringIndef -> do
      decodeStringIndef
      tsw <- decodeChunks $ do
        (w, t) <- decodeStringWithWidth
        pure (w, LT.fromStrict t)
      pure $ TermStringI' tsw
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
    decodeBytesWithWidth = decodeWithLengthWidth (fromIntegral . BS.length) decodeBytes
    decodeStringWithWidth = decodeWithLengthWidth textArg decodeString
    decodeUInt = do
      (w, v) <- decodeWithWidth decodeWord64
      pure $ TermUInt' w v
    decodeNInt = do
      (w, v) <- decodeWithWidth decodeNegWord64
      pure . TermNInt' w . NInt $ complement v
    decodeTagged = do
      (w, tag) <- decodeWithWidth decodeTag64
      TermTag' w tag <$> decodeCBORTerm
    decodeArray = do
      (w, n) <- decodeWithWidth decodeListLen
      TermArray' w <$> replicateM n decodeCBORTerm
    decodeMap = do
      (w, n) <- decodeWithWidth decodeMapLen
      TermMap' w <$> replicateM n decodeKeyValue
    decodeKeyValue = (,) <$> decodeCBORTerm <*> decodeCBORTerm

-- | Decode items with @dec@ until hitting (and consuming) a break stop code.
decodeChunks :: Decoder s a -> Decoder s [a]
decodeChunks dec = go
  where
    go = do
      done <- decodeBreakOr
      if done then pure [] else (:) <$> dec <*> go

data MajorType = M0 | M1 | M2 | M3 | M4 | M5 | M6 | M7
  deriving (Enum, Eq, Ord, Show)

majorTypeMask :: MajorType -> Word8
majorTypeMask = (`shiftL` 5) . fromIntegral . fromEnum

argWidthMask :: ArgWidth -> Word8
argWidthMask = \case
  InlineArg -> 0x00
  OneByteArg -> 0x18
  TwoByteArg -> 0x19
  FourByteArg -> 0x1a
  EightByteArg -> 0x1b

encodeRawDataItem :: MajorType -> ArgWidth -> Word64 -> Encoding
encodeRawDataItem major width arg
  | isValidWidth arg width = encodePreEncoded . BS.pack $ headerByte : argBytes
  | otherwise = error $ "Argument is too large to be encoded as " <> show width <> ": " <> show arg
  where
    headerByte = majorTypeMask major .|. argWidthMask width .|. inlineArgMask
    argBytes = case width of
      InlineArg -> mempty
      OneByteArg -> beBytes 1
      TwoByteArg -> beBytes 2
      FourByteArg -> beBytes 4
      EightByteArg -> beBytes 8
    beBytes n = [fromIntegral (arg `shiftR` (8 * i)) | i <- [n - 1, n - 2 .. 0]]
    inlineArgMask
      | InlineArg <- width = fromIntegral arg
      | otherwise = 0x00

-- | Encode a term back to CBOR. Integer, length and tag arguments are
-- emitted with minimal width, so @encodeCBORTerm@ after 'decodeCBORTerm'
-- reproduces the input bytes only if the input used minimal-width
-- arguments; the definite\/indefinite structure is always preserved.
encodeCBORTerm :: CBORTerm -> Encoding
encodeCBORTerm term = case term of
  TermUInt' w v -> encodeRawDataItem M0 w v
  -- For values in @[-2^64, -1]@ 'encodeInteger' always emits major type 1
  -- with argument @-1 - n@, never a bignum.
  TermNInt' w v -> encodeRawDataItem M1 w (nintArg v)
  TermBytes' w bs -> encodeBytesW w bs
  TermBytesI' chunks ->
    encodeBytesIndef <> foldMap (\(w, bs) -> encodeBytesW w $ LBS.toStrict bs) chunks <> encodeBreak
  TermString' w s -> encodeStringW w s
  TermStringI' chunks ->
    encodeStringIndef <> foldMap (\(w, t) -> encodeStringW w $ LT.toStrict t) chunks <> encodeBreak
  TermArray' w ts -> encodeRawDataItem M4 w (fromIntegral $ length ts) <> foldMap encodeCBORTerm ts
  TermArrayI ts -> encodeListLenIndef <> foldMap encodeCBORTerm ts <> encodeBreak
  TermMap' w kvs -> encodeRawDataItem M5 w (fromIntegral $ length kvs) <> encodeKVs kvs
  TermMapI kvs -> encodeMapLenIndef <> encodeKVs kvs <> encodeBreak
  TermTag' w tag t -> encodeRawDataItem M6 w tag <> encodeCBORTerm t
  TermSimple v -> encodeSimple v
  TermHalf h -> encodeFloat16 $ fromHalf h
  TermFloat f -> encodeFloat f
  TermDouble d -> encodeDouble d
  where
    encodeBytesW w bs =
      encodeRawDataItem M2 w (fromIntegral $ BS.length bs) <> encodePreEncoded bs
    encodeStringW w t =
      let bs = TE.encodeUtf8 t
       in encodeRawDataItem M3 w (fromIntegral $ BS.length bs) <> encodePreEncoded bs
    encodeKVs = foldMap (\(k, v) -> encodeCBORTerm k <> encodeCBORTerm v)

bytesToUnsigned :: ByteString -> Integer
bytesToUnsigned = BS.foldl' (\acc b -> acc * 256 + toInteger b) 0

unsignedToBytes :: Integer -> ByteString
unsignedToBytes = BS.pack . reverse . go
  where
    go 0 = []
    go n = let (d, r) = divMod n 256 in fromInteger r : go d

unwrapBytes :: CBORTerm -> Maybe ByteString
unwrapBytes (TermBytes' _ bs) = Just bs
unwrapBytes (TermBytesI' bss) = Just . LBS.toStrict . mconcat $ snd <$> bss
unwrapBytes _ = Nothing

unwrapString :: CBORTerm -> Maybe Text
unwrapString (TermString' _ t) = Just t
unwrapString (TermStringI' ts) = Just . LT.toStrict . mconcat $ snd <$> ts
unwrapString _ = Nothing

unwrapArray :: CBORTerm -> Maybe [CBORTerm]
unwrapArray (TermArray' _ xs) = Just xs
unwrapArray (TermArrayI xs) = Just xs
unwrapArray _ = Nothing

unwrapMap :: CBORTerm -> Maybe [(CBORTerm, CBORTerm)]
unwrapMap (TermMap' _ kvs) = Just kvs
unwrapMap (TermMapI kvs) = Just kvs
unwrapMap _ = Nothing

-- Bounds

uintMax :: Integer
uintMax = 2 ^ (64 :: Int) - 1

nintMin :: Integer
nintMin = -(2 ^ (64 :: Int))
