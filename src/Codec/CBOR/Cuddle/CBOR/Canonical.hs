{-# LANGUAGE LambdaCase #-}

module Codec.CBOR.Cuddle.CBOR.Canonical (
  CanonicalTerm (..),
  NInt,
  toCanonical,
) where

import Codec.CBOR.Cuddle.CBOR.Term (
  CBORTerm (..),
  NInt,
  bytesToUnsigned,
  fromNInt,
  toNInt,
  uintMax,
  unsignedToBytes,
 )
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Lazy qualified as LT
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import Numeric.Half (Half)

-- | Convert a `cborg` @Term@ to a @CanonicalTerm@.
--
-- A CBOR map with duplicate keys is well-formed but invalid (RFC 8949 §5.6).
-- Canonicalizing such a map is not currently supported and raises an 'error'.
-- This only arises in the niche case of a duplicate-keyed map nested in a
-- position the validator does not otherwise reject (e.g. a map used as a map
-- key). If you hit this, please open an issue.
toCanonical :: CBORTerm -> CanonicalTerm
toCanonical = \case
  TermUInt i -> integerToCanonical $ toInteger i
  TermNInt i -> integerToCanonical $ fromNInt i
  TermBytes bs -> CTBytes bs
  TermBytesI bss -> CTBytes . mconcat $ LBS.toStrict <$> bss
  TermString t -> CTString t
  TermStringI ts -> CTString . mconcat $ LT.toStrict <$> ts
  TermArray xs -> mkList xs
  TermArrayI xs -> mkList xs
  TermMap kvs -> mkMap kvs
  TermMapI kvs -> mkMap kvs
  TermTag 2 inner
    | Just bs <- tagBytes inner -> integerToCanonical $ bytesToUnsigned bs
  TermTag 3 inner
    | Just bs <- tagBytes inner -> integerToCanonical $ -1 - bytesToUnsigned bs
  TermTag t inner -> CTTagged t $ toCanonical inner
  TermSimple i -> CTSimple i
  TermHalf x -> CTHalf x
  TermFloat x -> CTFloat x
  TermDouble x -> CTDouble x
  where
    mkMap kvs =
      let pairs = bimap toCanonical toCanonical <$> kvs
          m = Map.fromList pairs
       in if Map.size m == length pairs
            then CTMap m
            else
              error
                "toCanonical: encountered a map with duplicate keys, which is \
                \not currently supported. Please open an issue at \
                \https://github.com/input-output-hk/cuddle/issues"
    mkList ts = CTList $ toCanonical <$> ts
    tagBytes (TermBytes bs) = Just bs
    tagBytes (TermBytesI bs) = Just . BSL.toStrict $ mconcat bs
    tagBytes _ = Nothing

integerToCanonical :: Integer -> CanonicalTerm
integerToCanonical n
  | n >= 0, n <= uintMax = CTInt $ fromInteger n
  | n < 0, Just nn <- toNInt n = CTNInt nn
  | n > uintMax = CTTagged 2 . CTBytes $ unsignedToBytes n
  | otherwise = CTTagged 3 . CTBytes . unsignedToBytes $ -1 - n

-- | A canonical representation of CBOR data items. Two 'CanonicalTerm's
-- compare equal exactly when the underlying CBOR items are equivalent
-- under the /extended generic data model/ of RFC 8949 §3.4.3 — the same
-- notion of equality that determines whether two map keys are duplicates
-- (RFC 8949 §3.1, §5.6).
--
-- Differences from 'Codec.CBOR.Term.Term':
--
-- * Major types 0 and 1 are split into 'CTInt' and 'CTNInt' (matching
--   the on-wire structure) instead of overlapping in 'TInt'/'TInteger'.
-- * Bignums (tags 2 and 3) whose value fits in @[-2^64, 2^64 - 1]@
--   are normalized into 'CTInt' / 'CTNInt' (RFC 8949 §3.4.3).
-- * Definite- and indefinite-length variants are merged: there is no
--   separate constructor for what 'Codec.CBOR.Term.TBytesI',
--   'Codec.CBOR.Term.TStringI', 'Codec.CBOR.Term.TListI',
--   'Codec.CBOR.Term.TMapI' represent.
-- * Maps use 'Data.Map.Strict.Map' rather than a list of pairs, so
--   key order is irrelevant.
-- * 'TBool' and 'TNull' are folded into 'CTSimple' (values 20, 21, 22
--   per RFC 8949 §3.3).
-- * 'THalf', 'TFloat', 'TDouble' remain distinct constructors: the
--   generic data model treats different float widths as distinct items
--   (RFC 8949 §2).
data CanonicalTerm
  = CTInt !Word64
  | CTNInt !NInt
  | CTBytes !ByteString
  | CTString !Text
  | CTList ![CanonicalTerm]
  | CTMap !(Map CanonicalTerm CanonicalTerm)
  | CTTagged !Word64 !CanonicalTerm
  | CTSimple !Word8
  | CTHalf !Half
  | CTFloat !Float
  | CTDouble !Double
  deriving (Generic, Eq, Ord, Show)
