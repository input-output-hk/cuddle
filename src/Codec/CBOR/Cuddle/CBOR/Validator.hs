{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns -Wno-unused-matches -Wno-unused-imports #-}
-- |

module Codec.CBOR.Cuddle.CBOR.Validator where

import Control.Monad (guard)
import Data.Maybe
import Text.Megaparsec
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.Resolve
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.CDDL.Postlude
import Codec.CBOR.FlatTerm
import Codec.CBOR.Term
import Data.Functor.Identity
import Data.Void
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Codec.CBOR.Cuddle.CDDL hiding (CDDL, Rule, Group)
import Data.Text.Lazy qualified as TL
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.ByteString qualified as BS
import Data.Either
import Data.List.NonEmpty qualified as NE
import Data.List qualified as L
import Codec.CBOR.Cuddle.Pretty
import Prettyprinter
import Prettyprinter.Util (putDocW)
import Prettyprinter.Render.String

type CDDL = CTreeRoot' Identity MonoRef
type Rule = Node MonoRef
type ResolvedRule = CTree MonoRef

instance VisualStream [TermToken] where
  showTokens _ = show

instance TraversableStream [TermToken] where
  reachOffset i s = (Nothing, s { pstateInput = drop (i - pstateOffset s) (pstateInput s) })

resolveIfRef :: CDDL -> Rule -> ResolvedRule
resolveIfRef _ (MIt aa) = aa
resolveIfRef ct@(CTreeRoot cddl) (MRuleRef n) = do
  case Map.lookup n cddl of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val -> resolveIfRef ct $ runIdentity val

token' :: MonadParsec e s m => (Token s -> Maybe a) -> m a
token' a = token a Set.empty

-- | A type can be just a single value (such as 1 or "icecream" or h'0815'),
-- which matches only a data item with that specific value (no conversions
-- defined),
parseLiteral :: Value -> Parsec Void [TermToken] Term
parseLiteral = \case
  VUInt (fromIntegral -> i) ->
    token' (\case
               TkInt i' -> if i == i' then Just (TInt i') else Nothing
               _ -> Nothing
           ) <?> ("a literal int " <> show i)
  VNInt (negate . fromIntegral -> i) ->
    token' (\case
                TkInt i' -> if i == i' then Just (TInt i') else Nothing
                _ -> Nothing
            ) <?> ("a literal int " <> show i)
  VBignum i ->
    token' (\case
               TkInteger i' -> if i == i' then Just (TInteger i') else Nothing
               TkInt i' -> if i == toInteger i' then Just (TInt i') else Nothing
               _ -> Nothing
           ) <?> ("a literal int " <> show i)

  VFloat16 i ->
    token' (\case
               TkFloat16 i' -> if i == i' then Just (THalf i') else Nothing
               _ -> Nothing
           ) <?> ("a literal float " <> show i)
  VFloat32 i ->
    token' (\case
               TkFloat32 i' -> if i == i' then Just (TFloat i') else Nothing
               _ -> Nothing
           ) <?> ("a literal float " <> show i)
  VFloat64 i ->
    token' (\case
               TkFloat64 i' -> if i == i' then Just (TDouble i') else Nothing
               _ -> Nothing
           ) <?> ("a literal float " <> show i)

  VText i ->
    let tst = \case
                TkString i' -> if i == i' then Just i else Nothing
                _ -> Nothing
    in
    (    ( TString <$> token' tst )
      <|> ( between
              (satisfy ((==) TkStringBegin))
              (satisfy ((==) TkBreak))
              (TStringI . TL.fromStrict <$> token' tst)
          )
    ) <?> ("a literal text " <> show i)
  VBytes i ->
    let tst = \case
                TkBytes i' -> if i == i' then Just i' else Nothing
                _ -> Nothing
    in
    (     ( TBytes <$> token' tst )
      <|> ( between
                (satisfy ((==) TkBytesBegin))
                (satisfy ((==) TkBreak))
                (TBytesI . BSL.fromStrict <$> token' tst)
          )
    ) <?> ("a literal bytestring " <> show i)

  VBool i ->
    token' (\case
               TkBool i' -> if i == i' then Just (TBool i') else Nothing
               _ -> Nothing
           ) <?> ("a literal bool " <> show i)

-- | or be defined by a rule giving a meaning to a name (possibly after
-- supplying generic arguments as required by the generic parameters),
parseType :: PTerm -> Parsec Void [TermToken] Term
parseType = \case
  PTBool ->
    token' (\case
              TkBool i -> Just $ TBool i
              _ -> Nothing
          ) <?> "a boolean"

  PTUInt ->
    token' (\case
              TkInt i -> if i >= 0 then Just (TInt i) else Nothing
              _ -> Nothing
          ) <?> "an unsigned integer"

  PTNInt ->
    token' (\case
              TkInt i -> if i < 0 then Just (TInt i) else Nothing
              _ -> Nothing
          ) <?> "a negative integer"

  PTInt ->
    token' (\case
               TkInt i -> Just (TInt i)
               _ -> Nothing
           ) <?> "an integer"

  PTHalf ->
    token' (\case
               TkFloat16 i -> Just (THalf i)
               _ -> Nothing
           ) <?> "a float16"

  PTFloat ->
    token' (\case
               TkFloat32 i  -> Just (TFloat i)
               _ -> Nothing
           ) <?> "a float32"

  PTDouble ->
    token' (\case
               TkFloat64 i -> Just (TDouble i)
               _ -> Nothing
           ) <?> "a float64"

  PTBytes ->
    let tst = \case
                TkBytes b -> Just b
                _ -> Nothing
    in
    (     ( TBytes <$> token' tst )
      <|> ( between
                (satisfy ((==) TkBytesBegin))
                (satisfy ((==) TkBreak))
                (TBytesI . BSL.fromStrict <$> token' tst)
          )
    ) <?> "a  bytestring"

  PTText ->
    let tst = \case
                TkString i' -> Just i'
                _ -> Nothing
    in
    (    ( TString <$> token' tst )
      <|> ( between
              (satisfy ((==) TkStringBegin))
              (satisfy ((==) TkBreak))
              (TStringI . TL.fromStrict <$> token' tst)
          )
    ) <?> "a text"

  PTNil ->
    token' (\case
               TkNull -> Just TNull
               _ -> Nothing
           ) <?> "a nil"
  PTUndefined ->
    token' (\case
               TkSimple 23 -> Just (TSimple 23)
               _ -> Nothing
           ) <?> "an undefined"

-- | a map expression, which matches a valid CBOR map the key/value pairs of
-- which can be ordered in such a way that the resulting sequence matches the
-- group expression
--
-- TODO: It is unclear to me how to do this "ordered in such a way that"
parseMap :: CDDL
         -> [Rule]
         -> ParsecT Void [TermToken] Identity Term
parseMap cddl nodes =
  parseCollection
    cddl
    nodes
    TkMapBegin
    TMapI
    TMap
    (\case TkMapLen w -> Just (fromIntegral w); _ -> Nothing)
    (mkGroupParser parseKeyValue)
  <?> "a proper map"

-- | an array expression, which matches a CBOR array the elements of which --
-- when taken as values and complemented by a wildcard (matches anything) key
-- each -- match the group
parseArray :: CDDL
         -> [Rule]
         -> ParsecT Void [TermToken] Identity Term
parseArray cddl nodes =
  parseCollection
    cddl
    nodes
    TkListBegin
    TListI
    TList
    (\case TkListLen w -> Just (fromIntegral w); _ -> Nothing)
    (mkGroupParser mkParser)
  <?> "a proper list"

parseCollection :: CDDL
                -> [Rule]
                -> TermToken
                -> ([a] -> Term)
                -> ([a] -> Term)
                -> (TermToken -> Maybe Int)
                -> (CDDL -> Rule -> Parsec Void [TermToken] [a])
                -> Parsec Void [TermToken] Term
parseCollection cddl nodes beginToken indefConstructor defConstructor parseLength groupParser =
  (    between
           (satisfy ((==) beginToken))
           (satisfy ((==) TkBreak))
           (indefConstructor . concat <$> traverse (groupParser cddl) (flattenGroup cddl nodes))
     <|> ( do
             llen <- token' parseLength
             elems <- concat <$> traverse (groupParser cddl) (flattenGroup cddl nodes)
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
parseRange :: CDDL
           -> Rule
           -> Rule
           -> RangeBound
           -> ParsecT Void [TermToken] Identity Term
parseRange cddl low high inc =
  case (resolveIfRef cddl low, resolveIfRef cddl high) of
      (Literal (VUInt low'), Literal (VUInt high')) -> do
        TInt i <- mkParser cddl (MIt (Postlude PTInt))
        (guard $ fromIntegral low' <= i && (case inc of
                                ClOpen -> (<)
                                Closed -> (<=)
                             ) i (fromIntegral high')
          ) <?> "in range [" <> show low' <> ", " <> show high' <> (case inc of ClOpen -> ")"; Closed -> "]")
        pure $ TInt i
      (Literal (VNInt low'), Literal (VUInt high')) -> do
        TInt i <- mkParser cddl (MIt (Postlude PTInt))
        (guard $ - (fromIntegral low') <= i && (case inc of
                                ClOpen -> (<)
                                Closed -> (<=)
                             ) i (fromIntegral high')
          ) <?> "in range [" <> show (- fromIntegral low' :: Int) <> ", " <> show high' <> (case inc of ClOpen -> ")"; Closed -> "]")
        pure $ TInt i
      (Literal (VNInt low'), Literal (VNInt high')) -> do
        TInt i <- mkParser cddl (MIt (Postlude PTInt))
        (guard $ - (fromIntegral low') <= i && (case inc of
                                ClOpen -> (<)
                                Closed -> (<=)
                             ) i (- fromIntegral high')
          ) <?> "in range [" <> show (- fromIntegral low' :: Int) <> ", " <> show (- fromIntegral high' :: Int) <> (case inc of ClOpen -> ")"; Closed -> "]")
        pure $ TInt i

      (Literal (VFloat16 low'), Literal (VFloat16 high')) -> do
        THalf i <- mkParser cddl (MIt (Postlude PTHalf))
        (guard $ low' <= i && (case inc of
                                ClOpen -> (<)
                                Closed -> (<=)
                             ) i high'
          ) <?> "in range [" <> show low' <> ", " <> show high' <> (case inc of ClOpen -> ")"; Closed -> "]")
        pure $ THalf i

      (Literal (VFloat32 low'), Literal (VFloat32 high')) -> do
        TFloat i <- mkParser cddl (MIt (Postlude PTFloat))
        (guard $ low' <= i && (case inc of
                                ClOpen -> (<)
                                Closed -> (<=)
                             ) i high'
          ) <?> "in range [" <> show low' <> ", " <> show high' <> (case inc of ClOpen -> ")"; Closed -> "]")
        pure $ TFloat i

      (Literal (VFloat64 low'), Literal (VFloat64 high')) -> do
        TDouble i <- mkParser cddl (MIt (Postlude PTDouble))
        (guard $ low' <= i && (case inc of
                                ClOpen -> (<)
                                Closed -> (<=)
                             ) i high'
          ) <?> "in range [" <> show low' <> ", " <> show high' <> (case inc of ClOpen -> ")"; Closed -> "]")
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
    L.foldl1' (<|>) [ mkParser cddl n | n <- NE.toList nodes ]
    <?> ("something in the choice " <> show (NE.toList nodes))

  Range low high inc ->
    parseRange cddl low high inc

  Group{} -> fail "Found lone group!"

mkGroupParser ::
     (CDDL -> Node MonoRef -> Parsec Void [TermToken] a)
  -> CDDL
  -> Rule
  -> ParsecT Void [TermToken] Identity [a]
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
  _ -> (:[]) <$> p cddl rule

parseKeyValue :: CDDL -> Rule -> ParsecT Void [TermToken] Identity (Term, Term)
parseKeyValue cddl rule = case resolveIfRef cddl rule of
  KV k v _ -> (,) <$> mkParser cddl k <*> mkParser cddl v

flattenGroup :: CDDL -> [Rule] -> [Rule]
flattenGroup cddl nodes =
  mconcat [ case resolveIfRef cddl rule of
      Literal{} -> [rule]
      Postlude{} -> [rule]
      Map{} -> [rule]
      Array{} -> [rule]
      Choice{} -> [rule]
      KV{} -> [rule]
      Occur o i -> case resolveIfRef cddl o of
        Group g -> map (MIt . (`Occur` i)) $ flattenGroup cddl g
        Map g -> map (MIt . (`Occur` i)) $ flattenGroup cddl g
        Array g -> map (MIt . (`Occur` i)) $ flattenGroup cddl g
        _ -> [rule]
      Range{} -> [rule]
      Control{} -> [rule]
      Enum e -> case resolveIfRef cddl e of
        Group g -> flattenGroup cddl g
        _ -> error "Malformed cddl"
      Unwrap g -> case resolveIfRef cddl g of
        Map n -> flattenGroup cddl n
        Array n -> flattenGroup cddl n
        Tag _ n -> [n]
        _ -> error "Malformed cddl"
      Tag{} -> [rule]
  | rule <- nodes
  ]

mkOpParser :: CDDL -> CtlOp -> Rule -> Rule -> Parsec Void [TermToken] Term
mkOpParser cddl Size tgt ctrl
  | Just name <- supportsSize cddl tgt = case resolveIfRef cddl ctrl of
      Literal (VUInt (fromIntegral -> sz)) ->
        let tst = \case
                    TkBytes s -> if sz == BS.length s then Just (TBytes s) else Nothing
                    TkString s -> if sz == T.length s then Just (TString s) else Nothing
                    _ -> Nothing
        in (     token' tst
             <|> between
                   (satisfy ((==) TkStringBegin))
                   (satisfy ((==) TkBreak))
                   (token' tst)
             <|> token' (\case
                            TkInt i -> if 0 <= i && i < 256 ^ sz then Just (TInt i) else Nothing
                            TkInteger i -> if 0 <= i && i < 256 ^ sz then Just (TInteger i) else Nothing
                            _ -> Nothing
                        )
           ) <?> (name <> " of size " <> show sz)
  | otherwise = error $ "Malformed cddl: " <> show tgt
mkOpParser cddl Bits tgt ctrl
  | Just name <- supportsBits cddl tgt = case resolveIfRef cddl tgt of
      _ -> undefined
  | otherwise = error $ "Malformed cddl: " <> show tgt
mkOpParser cddl Regexp tgt ctrl
  | Just name <- supportsRegexp cddl tgt = case resolveIfRef cddl tgt of
      _ -> undefined
  | otherwise = error $ "Malformed cddl: " <> show tgt
mkOpParser cddl Cbor tgt ctrl
  | Just name <- supportsCbor cddl tgt = case resolveIfRef cddl tgt of
      _ -> undefined
  | otherwise = error $ "Malformed cddl: " <> show tgt
mkOpParser cddl Cborseq tgt ctrl
  | Just name <- supportsCborseq cddl tgt = case resolveIfRef cddl tgt of
      _ -> undefined
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

aCDDL :: IO ()
aCDDL =
  sequence_ (map (uncurry parseTest)
    [ foo (Literal (VUInt 1)) [TkInt 1]
    , foo (Literal (VNInt 1)) [TkInt (-1)]
    , foo (Literal (VBignum 1)) [TkInteger 1]
    , foo (Literal (VFloat16 1.5)) [TkFloat16 1.5]
    , foo (Literal (VFloat32 1.5)) [TkFloat32 1.5]
    , foo (Literal (VFloat64 1.5)) [TkFloat64 1.5]
    , foo (Literal (VText "hi")) [TkString "hi"]
    , foo (Literal (VText "hi")) [TkStringBegin, TkString "hi", TkBreak]
    , foo (Literal (VBytes "hi")) [TkBytes "hi"]
    , foo (Literal (VBytes "hi")) [TkBytesBegin, TkBytes "hi", TkBreak]
    , foo (Literal (VBool True)) [TkBool True]
    , foo (Postlude PTBool) [TkBool True]
    , foo (Postlude PTUInt) [TkInt 1]
    , foo (Postlude PTNInt) [TkInt (-1)]
    , foo (Postlude PTInt) [TkInt 1]
    , foo (Postlude PTHalf) [TkFloat16 1.5]
    , foo (Postlude PTFloat) [TkFloat32 1.5]
    , foo (Postlude PTDouble) [TkFloat64 1.5]
    , foo (Postlude PTBytes) [TkBytes "hi"]
    , foo (Postlude PTBytes) [TkBytesBegin, TkBytes "hi", TkBreak]
    , foo (Postlude PTText) [TkString "hi"]
    , foo (Postlude PTText) [TkStringBegin, TkString "hi", TkBreak]
    , foo (Postlude PTNil) [TkNull]
    , foo (Postlude PTUndefined) [TkSimple 23]
    , foo (Control Size (MIt (Postlude PTUInt)) (MIt (Literal (VUInt 1)))) [TkInt 100]
    , foo (Control Size (MIt (Postlude PTBytes)) (MIt (Literal (VUInt 5)))) [TkBytes "12345"]
    , foo (Array [MIt (Postlude PTUInt), MIt (Postlude PTUInt)]) [TkListBegin, TkInt 1, TkInt 2, TkBreak]
    , foo (Array [MIt (Postlude PTUInt), MIt (Postlude PTUInt)]) [TkListLen 2, TkInt 1, TkInt 2]
    , foo (Map [MIt (KV (MIt (Postlude PTUInt)) (MIt (Postlude PTUInt)) False)]) [TkMapBegin, TkInt 1, TkInt 2, TkBreak]
    , foo (Map [MIt (KV (MIt (Postlude PTUInt)) (MIt (Postlude PTUInt)) False)]) [TkMapLen 1, TkInt 1, TkInt 2]
    , foo (Map [MIt (Occur (MIt (KV (MIt (Postlude PTUInt)) (MIt (Postlude PTUInt)) False)) OIZeroOrMore)]) [TkMapLen 2, TkInt 1, TkInt 2, TkInt 3, TkInt 4]
    , foo (Map [
                MIt (Occur (MIt (KV (MIt (Postlude PTUInt)) (MIt (Postlude PTUInt)) False)) OIZeroOrMore)
              , MIt (KV (MIt (Postlude PTUInt)) (MIt (Postlude PTUInt)) False)
              ]) [TkMapLen 2, TkInt 1, TkInt 2, TkInt 3, TkInt 4]
    , foo (Range (MIt (Literal (VUInt 0))) (MIt (Literal (VUInt 2))) Closed) [TkInt 1]
    , foo (Choice (MIt
                     (Map [MIt (KV (MIt (Postlude PTUInt)) (MIt (Postlude PTUInt)) False)])
                   NE.:|
                    [ MIt ((Array [MIt (Postlude PTUInt), MIt (Postlude PTUInt)])) ]
                  ))
                     [TkMapLen 1, TkInt 1, TkInt 2]
    , foo (Choice (MIt
                     (Map [MIt (KV (MIt (Postlude PTUInt)) (MIt (Postlude PTUInt)) False)])
                   NE.:|
                    [ MIt ((Array [MIt (Postlude PTUInt), MIt (Postlude PTUInt)])) ]
                  ))
                     [TkListLen 2, TkInt 1, TkInt 2]
    ])

foo :: CTree MonoRef -> [TermToken] -> (Parsec Void [TermToken] Term, [TermToken])
foo a b = (
        mkParser
          (CTreeRoot (Map.singleton (Name "foo") (Identity (MIt a))))
          (MRuleRef (Name "foo"))
     ,b
     )
