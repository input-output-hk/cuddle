{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns -Wno-unused-matches -Wno-unused-imports #-}

module Codec.CBOR.Cuddle.CBOR.Validator where

import Codec.CBOR.Cuddle.CDDL hiding (CDDL, Group, Rule)
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.CDDL.Postlude
import Codec.CBOR.Cuddle.CDDL.Resolve
import Codec.CBOR.Cuddle.Pretty
import Codec.CBOR.FlatTerm
import Codec.CBOR.Magic
import Codec.CBOR.Read
import Codec.CBOR.Term
import Control.Applicative.Permutations
import Control.Monad (guard)
import Control.Monad.ST
import Data.Bifunctor
import Data.Bits hiding (And)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.Either
import Data.Function ((&))
import Data.Functor.Identity
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Void
import Debug.Trace
import GHC.Float
import Prettyprinter
import Prettyprinter.Render.String
import Prettyprinter.Util (putDocW)
import System.Exit
import Text.Megaparsec
import Text.Regex.TDFA

type CDDL = CTreeRoot' Identity MonoRef
type Rule = Node MonoRef
type ResolvedRule = CTree MonoRef

instance VisualStream [TermToken] where
  showTokens _ = show

instance TraversableStream [TermToken] where
  reachOffset i s = (Nothing, s {pstateInput = drop (i - pstateOffset s) (pstateInput s)})

resolveIfRef :: CDDL -> Rule -> ResolvedRule
resolveIfRef _ (MIt aa) = aa
resolveIfRef ct@(CTreeRoot cddl) (MRuleRef n) = do
  case Map.lookup n cddl of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val -> resolveIfRef ct $ runIdentity val

token' :: MonadParsec e s m => (Token s -> Maybe a) -> m a
token' a = token a Set.empty

validateCBOR' ::
  BS.ByteString ->
  Name ->
  CDDL ->
  Either (Either DeserialiseFailure (ParseErrorBundle [TermToken] Void)) Term
validateCBOR' bs rule cddl@(CTreeRoot tree) =
  case decodeFlatTerm bs of
    Left e -> Left $ Left e
    Right fterm -> bimap Right id $ parse (mkParser cddl $ runIdentity $ tree Map.! rule) "" fterm

validateCBOR :: BS.ByteString -> Name -> CDDL -> IO ()
validateCBOR bs rule cddl =
  case validateCBOR' bs rule cddl of
    Left (Left e) -> print e
    Left (Right e) -> putStrLn (errorBundlePretty e)
    Right _ -> putStrLn "Valid"

decodeFlatTerm :: BS.ByteString -> Either DeserialiseFailure FlatTerm
decodeFlatTerm bs =
  runST $
    go =<< deserialiseIncremental decodeTermToken
  where
    go :: forall s. IDecode s TermToken -> ST s (Either DeserialiseFailure FlatTerm)
    go = \case
      Partial next -> go =<< next (Just bs)
      Done nextbs _ tt ->
        if BS.null nextbs
          then pure $ Right [tt]
          else case decodeFlatTerm nextbs of
            Left e -> pure $ Left e
            Right ft -> pure $ Right (tt : ft)
      Fail _ _ e -> pure $ Left e

-- | A type can be just a single value (such as 1 or "icecream" or h'0815'),
-- which matches only a data item with that specific value (no conversions
-- defined),
parseLiteral :: Value -> Parsec Void [TermToken] Term
parseLiteral = \case
  VUInt (fromIntegral -> i) ->
    token'
      ( \case
          TkInt i' -> if i == i' then Just (TInt i') else Nothing
          _ -> Nothing
      )
      <?> ("a literal int " <> show i)
  VNInt (negate . fromIntegral -> i) ->
    token'
      ( \case
          TkInt i' -> if i == i' then Just (TInt i') else Nothing
          _ -> Nothing
      )
      <?> ("a literal int " <> show i)
  VBignum i ->
    token'
      ( \case
          TkInteger i' -> if i == i' then Just (TInteger i') else Nothing
          TkInt i' -> if i == toInteger i' then Just (TInt i') else Nothing
          _ -> Nothing
      )
      <?> ("a literal int " <> show i)
  VFloat16 i ->
    token'
      ( \case
          TkFloat16 i' -> if i == i' then Just (THalf i') else Nothing
          _ -> Nothing
      )
      <?> ("a literal float16 " <> show i)
  VFloat32 i ->
    token'
      ( \case
          TkFloat16 i' -> if i == i' then Just (THalf i') else Nothing
          TkFloat32 i' -> if i == i' then Just (TFloat i') else Nothing
          _ -> Nothing
      )
      <?> ("a literal float32 " <> show i)
  VFloat64 i ->
    token'
      ( \case
          TkFloat16 i' -> if i == float2Double i' then Just (THalf i') else Nothing
          TkFloat32 i' -> if i == float2Double i' then Just (TFloat i') else Nothing
          TkFloat64 i' -> if i == i' then Just (TDouble i') else Nothing
          _ -> Nothing
      )
      <?> ("a literal float64 " <> show i)
  VText i ->
    let tst = \case
          TkString i' -> if i == i' then Just i else Nothing
          _ -> Nothing
     in ( (TString <$> token' tst)
            <|> ( between
                    (satisfy ((==) TkStringBegin))
                    (satisfy ((==) TkBreak))
                    (TStringI . TL.fromStrict <$> token' tst)
                )
        )
          <?> ("a literal text " <> show i)
  VBytes (either (error "Impossible!") id . Base16.decode -> i) ->
    let tst = \case
          TkBytes i' -> if i == i' then Just i' else Nothing
          _ -> Nothing
     in ( (TBytes <$> token' tst)
            <|> ( between
                    (satisfy ((==) TkBytesBegin))
                    (satisfy ((==) TkBreak))
                    (TBytesI . BSL.fromStrict <$> token' tst)
                )
        )
          <?> ("a literal bytestring " <> show i)
  VBool i ->
    token'
      ( \case
          TkBool i' -> if i == i' then Just (TBool i') else Nothing
          _ -> Nothing
      )
      <?> ("a literal bool " <> show i)

-- | or be defined by a rule giving a meaning to a name (possibly after
-- supplying generic arguments as required by the generic parameters),
parseType :: PTerm -> Parsec Void [TermToken] Term
parseType = \case
  PTBool ->
    token'
      ( \case
          TkBool i -> Just $ TBool i
          _ -> Nothing
      )
      <?> "a boolean"
  PTUInt ->
    token'
      ( \case
          TkInt i -> if i >= 0 then Just (TInt i) else Nothing
          _ -> Nothing
      )
      <?> "an unsigned integer"
  PTNInt ->
    token'
      ( \case
          TkInt i -> if i < 0 then Just (TInt i) else Nothing
          _ -> Nothing
      )
      <?> "a negative integer"
  PTInt ->
    token'
      ( \case
          TkInt i -> Just (TInt i)
          _ -> Nothing
      )
      <?> "an integer"
  PTHalf ->
    token'
      ( \case
          TkFloat16 i -> Just (THalf i)
          _ -> Nothing
      )
      <?> "a float16"
  PTFloat ->
    token'
      ( \case
          TkFloat32 i -> Just (TFloat i)
          _ -> Nothing
      )
      <?> "a float32"
  PTDouble ->
    token'
      ( \case
          TkFloat64 i -> Just (TDouble i)
          _ -> Nothing
      )
      <?> "a float64"
  PTBytes ->
    let tst = \case
          TkBytes b -> Just b
          _ -> Nothing
     in ( (TBytes <$> token' tst)
            <|> ( between
                    (satisfy ((==) TkBytesBegin))
                    (satisfy ((==) TkBreak))
                    (TBytesI . BSL.fromStrict <$> token' tst)
                )
        )
          <?> "a  bytestring"
  PTText ->
    let tst = \case
          TkString i' -> Just i'
          _ -> Nothing
     in ( (TString <$> token' tst)
            <|> ( between
                    (satisfy ((==) TkStringBegin))
                    (satisfy ((==) TkBreak))
                    (TStringI . TL.fromStrict <$> token' tst)
                )
        )
          <?> "a text"
  PTNil ->
    token'
      ( \case
          TkNull -> Just TNull
          _ -> Nothing
      )
      <?> "a nil"
  PTUndefined ->
    token'
      ( \case
          TkSimple 23 -> Just (TSimple 23)
          _ -> Nothing
      )
      <?> "an undefined"

-- | a map expression, which matches a valid CBOR map the key/value pairs of
-- which can be ordered in such a way that the resulting sequence matches the
-- group expression
parseMap ::
  CDDL ->
  [Rule] ->
  ParsecT Void [TermToken] Identity Term
parseMap cddl nodes =
  parseCollection
    cddl
    nodes
    True
    TkMapBegin
    TMapI
    TMap
    (\case TkMapLen w -> Just (fromIntegral w); _ -> Nothing)
    (mkGroupParser parseKeyValue)
    <?> "a proper map"

-- | an array expression, which matches a CBOR array the elements of which --
-- when taken as values and complemented by a wildcard (matches anything) key
-- each -- match the group
parseArray ::
  CDDL ->
  [Rule] ->
  ParsecT Void [TermToken] Identity Term
parseArray cddl nodes =
  parseCollection
    cddl
    nodes
    False
    TkListBegin
    TListI
    TList
    (\case TkListLen w -> Just (fromIntegral w); _ -> Nothing)
    (mkGroupParser mkParser)
    <?> "a proper list"

parseCollection ::
  CDDL ->
  [Rule] ->
  Bool ->
  TermToken ->
  ([a] -> Term) ->
  ([a] -> Term) ->
  (TermToken -> Maybe Int) ->
  (CDDL -> Rule -> Parsec Void [TermToken] [a]) ->
  Parsec Void [TermToken] Term
parseCollection cddl nodes allowPermutations beginToken indefConstructor defConstructor parseLength groupParser =
  ( between
      (satisfy ((==) beginToken))
      (satisfy ((==) TkBreak))
      ( indefConstructor . concat
          <$> if allowPermutations
            then runPermutation (traverse (toPermutation . groupParser cddl) (flattenGroup cddl nodes))
            else traverse (groupParser cddl) (flattenGroup cddl nodes)
      )
      <|> ( do
              llen <- token' parseLength
              elems <-
                concat
                  <$> if allowPermutations
                    then runPermutation (traverse (toPermutation . groupParser cddl) (flattenGroup cddl nodes))
                    else traverse (groupParser cddl) (flattenGroup cddl nodes)
              if length elems == llen
                then pure $ defConstructor elems
                else fail "Different number of elements!"
          )
  )

-- | A range operator can be used to join two type expressions that stand for
-- either two integer values or two floating-point values; it matches any value
-- that is between the two values, where the first value is always included in
-- the matching set and the second value is included for ".." and excluded for
-- "...".
parseRange ::
  CDDL ->
  Rule ->
  Rule ->
  RangeBound ->
  ParsecT Void [TermToken] Identity Term
parseRange cddl low high inc =
  case (resolveIfRef cddl low, resolveIfRef cddl high) of
    (Literal (VUInt low'), Literal (VUInt high')) -> do
      TInt i <- mkParser cddl (MIt (Postlude PTInt))
      ( guard $
          fromIntegral low' <= i
            && ( case inc of
                  ClOpen -> (<)
                  Closed -> (<=)
               )
              i
              (fromIntegral high')
        )
        <?> "in range [" <> show low' <> ", " <> show high' <> (case inc of ClOpen -> ")"; Closed -> "]")
      pure $ TInt i
    (Literal (VNInt low'), Literal (VUInt high')) -> do
      TInt i <- mkParser cddl (MIt (Postlude PTInt))
      ( guard $
          -(fromIntegral low') <= i
            && ( case inc of
                  ClOpen -> (<)
                  Closed -> (<=)
               )
              i
              (fromIntegral high')
        )
        <?> "in range ["
          <> show (-fromIntegral low' :: Int)
          <> ", "
          <> show high'
          <> (case inc of ClOpen -> ")"; Closed -> "]")
      pure $ TInt i
    (Literal (VNInt low'), Literal (VNInt high')) -> do
      TInt i <- mkParser cddl (MIt (Postlude PTInt))
      ( guard $
          -(fromIntegral low') <= i
            && ( case inc of
                  ClOpen -> (<)
                  Closed -> (<=)
               )
              i
              (-fromIntegral high')
        )
        <?> "in range ["
          <> show (-fromIntegral low' :: Int)
          <> ", "
          <> show (-fromIntegral high' :: Int)
          <> (case inc of ClOpen -> ")"; Closed -> "]")
      pure $ TInt i
    (Literal (VFloat16 low'), Literal (VFloat16 high')) -> do
      THalf i <- mkParser cddl (MIt (Postlude PTHalf))
      ( guard $
          low' <= i
            && ( case inc of
                  ClOpen -> (<)
                  Closed -> (<=)
               )
              i
              high'
        )
        <?> "in range [" <> show low' <> ", " <> show high' <> (case inc of ClOpen -> ")"; Closed -> "]")
      pure $ THalf i
    (Literal (VFloat32 low'), Literal (VFloat32 high')) -> do
      TFloat i <- mkParser cddl (MIt (Postlude PTFloat))
      ( guard $
          low' <= i
            && ( case inc of
                  ClOpen -> (<)
                  Closed -> (<=)
               )
              i
              high'
        )
        <?> "in range [" <> show low' <> ", " <> show high' <> (case inc of ClOpen -> ")"; Closed -> "]")
      pure $ TFloat i
    (Literal (VFloat64 low'), Literal (VFloat64 high')) -> do
      TDouble i <- mkParser cddl (MIt (Postlude PTDouble))
      ( guard $
          low' <= i
            && ( case inc of
                  ClOpen -> (<)
                  Closed -> (<=)
               )
              i
              high'
        )
        <?> "in range [" <> show low' <> ", " <> show high' <> (case inc of ClOpen -> ")"; Closed -> "]")
      pure $ TDouble i

mkParser :: CDDL -> Rule -> Parsec Void [TermToken] Term
mkParser cddl rule = case resolveIfRef cddl rule of
  Literal l -> parseLiteral l
  Postlude p -> parseType p
  Control op tgt ctrl ->
    lookAhead (mkParser cddl tgt) *> mkOpParser cddl op tgt ctrl
  Map nodes ->
    parseMap cddl nodes
  Array nodes ->
    parseArray cddl nodes
  Choice nodes ->
    L.foldl1' (<|>) [mkParser cddl n | n <- NE.toList nodes]
      <?> ("something in the choice " <> show (NE.toList nodes))
  Range low high inc ->
    parseRange cddl low high inc
  Group {} -> fail "Found lone group!"

mkGroupParser ::
  (CDDL -> Node MonoRef -> Parsec Void [TermToken] a) ->
  CDDL ->
  Rule ->
  ParsecT Void [TermToken] Identity [a]
mkGroupParser p cddl rule = case resolveIfRef cddl rule of
  Occur o OIOptional -> maybeToList <$> optional (p cddl o)
  Occur o OIZeroOrMore -> many (p cddl o)
  Occur o OIOneOrMore -> some (p cddl o)
  Occur o (OIBounded low high) -> case (low, high) of
    (Nothing, Nothing) -> many (p cddl o)
    (Just low', Nothing) -> do
      n <- many (p cddl o)
      guard $ length n >= fromIntegral low'
      pure n
    (Just low', Just high') -> count' (fromIntegral low') (fromIntegral high') (p cddl o)
    (Nothing, Just high') -> do
      n <- many (p cddl o)
      guard $ length n <= fromIntegral high'
      pure n
  _ -> (: []) <$> p cddl rule

parseKeyValue :: CDDL -> Rule -> ParsecT Void [TermToken] Identity (Term, Term)
parseKeyValue cddl rule = case resolveIfRef cddl rule of
  KV k v _ -> (,) <$> mkParser cddl k <*> mkParser cddl v

flattenGroup :: CDDL -> [Rule] -> [Rule]
flattenGroup cddl nodes =
  mconcat
    [ case resolveIfRef cddl rule of
        Literal {} -> [rule]
        Postlude {} -> [rule]
        Map {} -> [rule]
        Array {} -> [rule]
        Choice {} -> [rule]
        KV {} -> [rule]
        Occur o i -> case resolveIfRef cddl o of
          Group g -> map (MIt . (`Occur` i)) $ flattenGroup cddl g
          Map g -> map (MIt . (`Occur` i)) $ flattenGroup cddl g
          Array g -> map (MIt . (`Occur` i)) $ flattenGroup cddl g
          _ -> [rule]
        Range {} -> [rule]
        Control {} -> [rule]
        Enum e -> case resolveIfRef cddl e of
          Group g -> flattenGroup cddl g
          _ -> error "Malformed cddl"
        Unwrap g -> case resolveIfRef cddl g of
          Map n -> flattenGroup cddl n
          Array n -> flattenGroup cddl n
          Tag _ n -> [n]
          _ -> error "Malformed cddl"
        Tag {} -> [rule]
        Group g -> flattenGroup cddl g
    | rule <- nodes
    ]

mkOpParser :: CDDL -> CtlOp -> Rule -> Rule -> Parsec Void [TermToken] Term
mkOpParser cddl Size tgt ctrl
  | Just name <- supportsSize cddl tgt =
      case resolveIfRef cddl ctrl of
        Literal (VUInt (fromIntegral -> sz)) ->
          let tst = \case
                TkBytes s -> if sz == BS.length s then Just (TBytes s) else Nothing
                TkString s -> if sz == T.length s then Just (TString s) else Nothing
                _ -> Nothing
           in ( token' tst
                  <|> between
                    ( satisfy
                        ( \case
                            TkStringBegin -> True
                            TkBytesBegin -> True
                            _ -> False
                        )
                    )
                    (satisfy ((==) TkBreak))
                    (token' tst)
                  <|> token'
                    ( \case
                        TkInt i -> if 0 <= i && i < 256 ^ sz then Just (TInt i) else Nothing
                        TkInteger i -> if 0 <= i && i < 256 ^ sz then Just (TInteger i) else Nothing
                        _ -> Nothing
                    )
              )
                <?> (name <> " of size " <> show sz)
  | otherwise = error $ "Malformed cddl: " <> show tgt
mkOpParser cddl Bits tgt ctrl
  | Just name <- supportsBits cddl tgt =
      let indices :: [Int]
          indices = map fromIntegral $ case resolveIfRef cddl ctrl of
            Literal (VUInt idx) -> [idx]
            Choice nodes -> getIndicesOfChoice nodes
            Range ff tt incl -> getIndicesOfRange ff tt incl
            Enum g -> getIndicesOfEnum g
       in token'
            ( \case
                TkBytes bs -> if bitsControlCheck bs indices then Just (TBytes bs) else Nothing
                TkInt i -> if bitsControlCheckInteger i indices then Just (TInt i) else Nothing
                TkInteger i -> if bitsControlCheckInteger i indices then Just (TInteger i) else Nothing
            )
            <?> ("something that validates the .bits")
  | otherwise = error $ "Malformed cddl: " <> show tgt
  where
    -- Implements `.bits` control for ByteStrings:
    -- All set bits must be among the allowed bit indices.
    bitsControlCheck :: BS.ByteString -> [Int] -> Bool
    bitsControlCheck bs allowedBits =
      let allowedSet = IS.fromList allowedBits
          totalBits = BS.length bs * 8
          isAllowedBit n =
            let byteIndex = n `shiftR` 3
                bitIndex = n .&. 7
             in case BS.indexMaybe bs byteIndex of
                  Just byte -> not (testBit byte bitIndex) || IS.member n allowedSet
                  Nothing -> True -- Treat missing bytes as unset (safe)
       in all isAllowedBit [0 .. totalBits - 1]

    -- Implements `.bits` control for unsigned integers:
    -- All set bits must be among the allowed bit indices.
    bitsControlCheckInteger :: (Num a, Bits a) => a -> [Int] -> Bool
    bitsControlCheckInteger x allowedBits = go x 0
      where
        allowedSet = IS.fromList allowedBits

        go 0 _ = True
        go n i =
          let bitSet = testBit n 0
              allowed = not bitSet || IS.member i allowedSet
           in allowed && go (shiftR n 1) (i + 1)

    getIndicesOfChoice nodes =
      mconcat $
        NE.toList $
          NE.map
            ( \x -> case resolveIfRef cddl x of
                Literal (VUInt v) -> [fromIntegral v]
                KV _ v _ -> case resolveIfRef cddl v of
                  Literal (VUInt v') -> [fromIntegral v']
                  somethingElse -> error $ "Malformed value in KV in choice in .bits: " <> show somethingElse
                Range ff tt incl -> getIndicesOfRange ff tt incl
                Enum g -> getIndicesOfEnum g
                somethingElse -> error $ "Malformed alternative in choice in .bits: " <> show somethingElse
            )
            nodes

    getIndicesOfRange ff tt incl =
      case (resolveIfRef cddl ff, resolveIfRef cddl tt) of
        (Literal (VUInt ff'), Literal (VUInt tt')) ->
          [ff' .. tt'] & case incl of
            ClOpen -> init
            Closed -> id
        somethingElse -> error $ "Malformed range in .bits: " <> show somethingElse

    getIndicesOfEnum g =
      case resolveIfRef cddl g of
        Group g' -> getIndicesOfChoice (fromJust $ NE.nonEmpty g')
        somethingElse -> error $ "Malformed enum in .bits: " <> show somethingElse
mkOpParser cddl Regexp tgt ctrl
  | Just name <- supportsRegexp cddl tgt =
      case resolveIfRef cddl ctrl of
        Literal (VText s') ->
          let tst = \case
                TkString s -> case s =~ s' :: (T.Text, T.Text, T.Text) of
                  ("", s'', "") -> if s == s'' then Just (TString s) else Nothing
                  _ -> Nothing
                _ -> Nothing
           in ( token' tst
                  <|> between
                    ( satisfy
                        ( \case
                            TkStringBegin -> True
                            _ -> False
                        )
                    )
                    (satisfy ((==) TkBreak))
                    (token' tst)
              )
                <?> (" something that completely matches the regex " <> show s')
  | otherwise = error $ "Malformed cddl: " <> show tgt
mkOpParser cddl Cbor tgt ctrl
  | Just name <- supportsCbor cddl tgt =
      let tst n = \case
            TkBytes bs -> case validateCBOR' bs n cddl of
              Left {} -> Nothing
              Right t -> Just t
       in case ctrl of
            MRuleRef n ->
              ( token' (tst n)
                  <|> between
                    (satisfy ((==) TkStringBegin))
                    (satisfy ((==) TkBreak))
                    (token' (tst n))
              )
                <?> " cbor "
  | otherwise = error $ "Malformed cddl: " <> show tgt
mkOpParser cddl Cborseq tgt ctrl
  | Just name <- supportsCborseq cddl tgt =
      let tst n = \case
            TkBytes bs -> case validateCBOR' (BS.snoc (BS.cons 0x9f bs) 0xff) n cddl of
              Left {} -> Nothing
              Right t -> Just t
       in case ctrl of
            MRuleRef n ->
              ( token' (tst n)
                  <|> between
                    (satisfy ((==) TkStringBegin))
                    (satisfy ((==) TkBreak))
                    (token' (tst n))
              )
                <?> " cborseq "
  | otherwise = error $ "Malformed cddl: " <> show tgt
mkOpParser cddl Within _ ctrl =
  mkParser cddl ctrl
mkOpParser cddl And _ ctrl =
  mkParser cddl ctrl
mkOpParser cddl Lt tgt ctrl
  | Just name <- supportsIntAlgebra cddl tgt =
      let lt = case resolveIfRef cddl ctrl of
            Literal (VUInt lt') -> fromIntegral lt'
            Literal (VNInt lt') -> -fromIntegral lt'
       in token'
            ( \case
                TkInt i -> if fromIntegral i < lt then Just (TInt i) else Nothing
                TkInteger i -> if i < lt then Just (TInteger i) else Nothing
                _ -> Nothing
            )
            <?> (name <> " < " <> show lt)
  | Just name <- supportsFloatAlgebra cddl tgt =
      let lt = case resolveIfRef cddl ctrl of
            Literal (VFloat16 lt') -> float2Double lt'
            Literal (VFloat32 lt') -> float2Double lt'
            Literal (VFloat64 lt') -> lt'
       in token'
            ( \case
                TkFloat16 i -> if float2Double i < lt then Just (THalf i) else Nothing
                TkFloat32 i -> if float2Double i < lt then Just (TFloat i) else Nothing
                TkFloat64 i -> if i < lt then Just (TDouble i) else Nothing
                _ -> Nothing
            )
            <?> (name <> " < " <> show lt)
  | otherwise = error $ "Malformed cddl: " <> show tgt
mkOpParser cddl Le tgt ctrl
  | Just name <- supportsIntAlgebra cddl tgt =
      let lt = case resolveIfRef cddl ctrl of
            Literal (VUInt lt') -> fromIntegral lt'
            Literal (VNInt lt') -> -fromIntegral lt'
       in token'
            ( \case
                TkInt i -> if fromIntegral i <= lt then Just (TInt i) else Nothing
                TkInteger i -> if i <= lt then Just (TInteger i) else Nothing
                _ -> Nothing
            )
            <?> (name <> " <= " <> show lt)
  | Just name <- supportsFloatAlgebra cddl tgt =
      let lt = case resolveIfRef cddl ctrl of
            Literal (VFloat16 lt') -> float2Double lt'
            Literal (VFloat32 lt') -> float2Double lt'
            Literal (VFloat64 lt') -> lt'
       in token'
            ( \case
                TkFloat16 i -> if float2Double i <= lt then Just (THalf i) else Nothing
                TkFloat32 i -> if float2Double i <= lt then Just (TFloat i) else Nothing
                TkFloat64 i -> if i <= lt then Just (TDouble i) else Nothing
                _ -> Nothing
            )
            <?> (name <> " <= " <> show lt)
  | otherwise = error $ "Malformed cddl: " <> show tgt
mkOpParser cddl Gt tgt ctrl
  | Just name <- supportsIntAlgebra cddl tgt =
      let lt = case resolveIfRef cddl ctrl of
            Literal (VUInt lt') -> fromIntegral lt'
            Literal (VNInt lt') -> -fromIntegral lt'
       in token'
            ( \case
                TkInt i -> if fromIntegral i > lt then Just (TInt i) else Nothing
                TkInteger i -> if i > lt then Just (TInteger i) else Nothing
                _ -> Nothing
            )
            <?> (name <> " > " <> show lt)
  | Just name <- supportsFloatAlgebra cddl tgt =
      let lt = case resolveIfRef cddl ctrl of
            Literal (VFloat16 lt') -> float2Double lt'
            Literal (VFloat32 lt') -> float2Double lt'
            Literal (VFloat64 lt') -> lt'
       in token'
            ( \case
                TkFloat16 i -> if float2Double i > lt then Just (THalf i) else Nothing
                TkFloat32 i -> if float2Double i > lt then Just (TFloat i) else Nothing
                TkFloat64 i -> if i > lt then Just (TDouble i) else Nothing
                _ -> Nothing
            )
            <?> (name <> " > " <> show lt)
  | otherwise = error $ "Malformed cddl: " <> show tgt
mkOpParser cddl Ge tgt ctrl
  | Just name <- supportsIntAlgebra cddl tgt =
      let lt = case resolveIfRef cddl ctrl of
            Literal (VUInt lt') -> fromIntegral lt'
            Literal (VNInt lt') -> -fromIntegral lt'
       in token'
            ( \case
                TkInt i -> if fromIntegral i >= lt then Just (TInt i) else Nothing
                TkInteger i -> if i >= lt then Just (TInteger i) else Nothing
                _ -> Nothing
            )
            <?> (name <> " >= " <> show lt)
  | Just name <- supportsFloatAlgebra cddl tgt =
      let lt = case resolveIfRef cddl ctrl of
            Literal (VFloat16 lt') -> float2Double lt'
            Literal (VFloat32 lt') -> float2Double lt'
            Literal (VFloat64 lt') -> lt'
       in token'
            ( \case
                TkFloat16 i -> if float2Double i >= lt then Just (THalf i) else Nothing
                TkFloat32 i -> if float2Double i >= lt then Just (TFloat i) else Nothing
                TkFloat64 i -> if i >= lt then Just (TDouble i) else Nothing
                _ -> Nothing
            )
            <?> (name <> " >= " <> show lt)
  | otherwise = error $ "Malformed cddl: " <> show tgt
mkOpParser cddl Eq tgt ctrl
  | Just name <- supportsIntAlgebra cddl tgt =
      let lt = case resolveIfRef cddl ctrl of
            Literal (VUInt lt') -> fromIntegral lt'
            Literal (VNInt lt') -> -fromIntegral lt'
       in token'
            ( \case
                TkInt i -> if fromIntegral i == lt then Just (TInt i) else Nothing
                TkInteger i -> if i == lt then Just (TInteger i) else Nothing
                _ -> Nothing
            )
            <?> (name <> " == " <> show lt)
  | Just name <- supportsFloatAlgebra cddl tgt =
      let lt = case resolveIfRef cddl ctrl of
            Literal (VFloat16 lt') -> float2Double lt'
            Literal (VFloat32 lt') -> float2Double lt'
            Literal (VFloat64 lt') -> lt'
       in token'
            ( \case
                TkFloat16 i -> if float2Double i == lt then Just (THalf i) else Nothing
                TkFloat32 i -> if float2Double i == lt then Just (TFloat i) else Nothing
                TkFloat64 i -> if i == lt then Just (TDouble i) else Nothing
                _ -> Nothing
            )
            <?> (name <> " == " <> show lt)
  | otherwise = error $ "Malformed cddl: " <> show tgt
mkOpParser cddl Ne tgt ctrl
  | Just name <- supportsIntAlgebra cddl tgt =
      let lt = case resolveIfRef cddl ctrl of
            Literal (VUInt lt') -> fromIntegral lt'
            Literal (VNInt lt') -> -fromIntegral lt'
       in token'
            ( \case
                TkInt i -> if fromIntegral i /= lt then Just (TInt i) else Nothing
                TkInteger i -> if i /= lt then Just (TInteger i) else Nothing
                _ -> Nothing
            )
            <?> (name <> " /= " <> show lt)
  | Just name <- supportsFloatAlgebra cddl tgt =
      let lt = case resolveIfRef cddl ctrl of
            Literal (VFloat16 lt') -> float2Double lt'
            Literal (VFloat32 lt') -> float2Double lt'
            Literal (VFloat64 lt') -> lt'
       in token'
            ( \case
                TkFloat16 i -> if float2Double i /= lt then Just (THalf i) else Nothing
                TkFloat32 i -> if float2Double i /= lt then Just (TFloat i) else Nothing
                TkFloat64 i -> if i /= lt then Just (TDouble i) else Nothing
                _ -> Nothing
            )
            <?> (name <> " /= " <> show lt)
  | otherwise = error $ "Malformed cddl: " <> show tgt

supportsSize :: CDDL -> Rule -> Maybe String
supportsSize cddl rule = case resolveIfRef cddl rule of
  Postlude PTUInt -> Just "uint"
  Postlude PTBytes -> Just "bytes"
  Postlude PTText -> Just "text"
  _ -> Nothing

supportsBits :: CDDL -> Rule -> Maybe String
supportsBits cddl rule = case resolveIfRef cddl rule of
  Postlude PTUInt -> Just "uint"
  Postlude PTBytes -> Just "bytes"
  _ -> Nothing

supportsRegexp :: CDDL -> Rule -> Maybe String
supportsRegexp cddl rule = case resolveIfRef cddl rule of
  Postlude PTText -> Just "text"
  _ -> Nothing

supportsCbor :: CDDL -> Rule -> Maybe String
supportsCbor cddl rule = case resolveIfRef cddl rule of
  Postlude PTBytes -> Just "bytes"
  _ -> Nothing

supportsCborseq :: CDDL -> Rule -> Maybe String
supportsCborseq cddl rule = case resolveIfRef cddl rule of
  Postlude PTBytes -> Just "bytes"
  _ -> Nothing

supportsIntAlgebra :: CDDL -> Rule -> Maybe String
supportsIntAlgebra cddl rule = case resolveIfRef cddl rule of
  Postlude PTUInt -> Just "uint"
  Postlude PTNInt -> Just "nint"
  Postlude PTInt -> Just "int"
  _ -> Nothing

supportsFloatAlgebra :: CDDL -> Rule -> Maybe String
supportsFloatAlgebra cddl rule = case resolveIfRef cddl rule of
  Postlude PTHalf -> Just "half"
  Postlude PTFloat -> Just "float"
  Postlude PTDouble -> Just "double"
  _ -> Nothing
