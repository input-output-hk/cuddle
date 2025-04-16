{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-incomplete-patterns -Wno-unused-matches #-}
-- |

module Parser where

import Text.Megaparsec
import Codec.CBOR.Cuddle.CDDL.CTree
import Codec.CBOR.Cuddle.CDDL.Resolve
import Codec.CBOR.Cuddle.CDDL.CtlOp
import Codec.CBOR.Cuddle.CDDL.Postlude
import Codec.CBOR.Term
import Data.Functor.Identity
import Data.Void
import Data.Map.Strict qualified as Map
import Codec.CBOR.Cuddle.CDDL hiding (CDDL, Rule, Group)
import Data.Text.Lazy qualified as TL
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.ByteString qualified as BS
import Data.Either

type CDDL = CTreeRoot' Identity MonoRef
type Rule = Node MonoRef
type ResolvedRule = CTree MonoRef

instance VisualStream [Term] where
  showTokens _ = show

instance TraversableStream [Term] where
  reachOffset i s = (Nothing, s { pstateInput = drop (i - pstateOffset s) (pstateInput s) })

resolveIfRef :: CDDL -> Rule -> ResolvedRule
resolveIfRef _ (MIt aa) = aa
resolveIfRef ct@(CTreeRoot cddl) (MRuleRef n) = do
  case Map.lookup n cddl of
    Nothing -> error $ "Unbound reference: " <> show n
    Just val -> resolveIfRef ct $ runIdentity val


mkParser :: CDDL -> Rule -> Parsec Void [Term] Term
mkParser cddl rule = case resolveIfRef cddl rule of
  Literal (VUInt i) -> satisfy (\case
                                   TInt i' -> fromIntegral i == i'
                                   _ -> False
                               ) <?> ("a literal int " <> show i)
  Literal (VNInt (negate . fromIntegral -> i)) -> satisfy (\case
                                   TInt i' -> i == i'
                                   _ -> False
                               ) <?> ("a literal int " <> show i)
  Literal (VBignum i) -> satisfy (\case
                                   TInteger i' -> i == i'
                                   TInt i' -> i == toInteger i'
                                   _ -> False
                               ) <?> ("a literal int " <> show i)

  Literal (VFloat16 i) -> satisfy (\case
                                   THalf i' -> i == i'
                                   _ -> False
                               ) <?> ("a literal float " <> show i)
  Literal (VFloat32 i) -> satisfy (\case
                                   TFloat i' -> i == i'
                                   _ -> False
                               ) <?> ("a literal float " <> show i)
  Literal (VFloat64 i) -> satisfy (\case
                                   TDouble i' -> i == i'
                                   _ -> False
                               ) <?> ("a literal float " <> show i)
  Literal (VText i) -> satisfy (\case
                                   TString i' -> i == i'
                                   TStringI (TL.toStrict -> i') -> i == i'
                                   _ -> False
                               ) <?> ("a literal text " <> show i)
  Literal (VBytes i) -> satisfy (\case
                                   TBytes i' -> i == i'
                                   TBytesI (BSL.toStrict -> i') -> i == i'
                                   _ -> False
                               ) <?> ("a literal bytes " <> show i)
  Literal (VBool i) -> satisfy (\case
                                   TBool i' -> i == i'
                                   _ -> False
                               ) <?> ("a literal bool " <> show i)

  Postlude PTBool -> satisfy (\case
                                TBool{} -> True
                                _ -> False
                            ) <?> "a boolean"

  Postlude PTUInt -> satisfy (\case
                                TInt i -> i >= 0
                                _ -> False
                            ) <?> "an unsigned integer"

  Postlude PTNInt -> satisfy (\case
                                TInt i -> i < 0
                                _ -> False
                            ) <?> "a negative integer"
  Postlude PTInt -> satisfy (\case
                                TInt{} -> True
                                _ -> False
                            ) <?> "an integer"
  Postlude PTHalf -> satisfy (\case
                                THalf{} -> True
                                _ -> False
                            ) <?> "a float16"
  Postlude PTFloat -> satisfy (\case
                                TFloat{} -> True
                                _ -> False
                            ) <?> "a float32"
  Postlude PTDouble -> satisfy (\case
                                TDouble{} -> True
                                _ -> False
                            ) <?> "a float64"
  Postlude PTBytes -> satisfy (\case
                                TBytes{} -> True
                                TBytesI{} -> True
                                _ -> False
                            ) <?> "a bytestring"
  Postlude PTText -> satisfy (\case
                                TString{} -> True
                                TStringI{} -> True
                                _ -> False
                            ) <?> "a textstring"
  Postlude PTNil -> satisfy (\case
                                TNull{} -> True
                                _ -> False
                            ) <?> "a nil"
  Postlude PTUndefined -> satisfy (\case
                                TSimple 23 -> True
                                _ -> False
                            ) <?> "an undefined"

  Control op tgt ctrl ->
    lookAhead (mkParser cddl tgt) *> mkOpParser cddl op tgt ctrl

  Map nodes -> undefined

  Array nodes -> satisfy (\case
                             TList nodes' ->
                               isRight $ parse (traverse (mkParser cddl) $ flattenGroup cddl nodes) "" nodes'
                             TListI nodes' ->
                               isRight $ parse (traverse (mkParser cddl) $ flattenGroup cddl nodes) "" nodes'
                             _ -> False
                         ) <?> "a list"

  Choice nodes -> undefined

  Group nodes -> undefined

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

mkOpParser :: CDDL -> CtlOp -> Rule -> Rule -> Parsec Void [Term] Term
mkOpParser cddl Size tgt ctrl
  | Just name <- supportsSize cddl tgt = case resolveIfRef cddl ctrl of
      Literal (VUInt (fromIntegral -> sz)) ->
        satisfy (\case
                    TBytesI (BSL.length -> sz') -> sz == sz'
                    TBytes (fromIntegral . BS.length -> sz') -> sz == sz'
                    TString (fromIntegral . T.length -> sz') -> sz == sz'
                    TStringI (TL.length -> sz') -> sz == sz'
                    TInt i -> 0 <= i && i < 256 ^ sz
                    TInteger i -> 0 <= i && i < 256 ^ sz
                    _ -> False
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
    [ foo (Literal (VUInt 1)) (TInt 1)
    , foo (Literal (VNInt 1)) (TInt (-1))
    , foo (Literal (VBignum 1)) (TInteger 1)
    , foo (Literal (VFloat16 1.5)) (THalf 1.5)
    , foo (Literal (VFloat32 1.5)) (TFloat 1.5)
    , foo (Literal (VFloat64 1.5)) (TDouble 1.5)
    , foo (Literal (VText "hi")) (TString "hi")
    , foo (Literal (VText "hi")) (TStringI "hi")
    , foo (Literal (VBytes "hi")) (TBytes "hi")
    , foo (Literal (VBytes "hi")) (TBytesI "hi")
    , foo (Literal (VBool True)) (TBool True)
    , foo (Postlude PTBool) (TBool True)
    , foo (Postlude PTUInt) (TInt 1)
    , foo (Postlude PTNInt) (TInt (-1))
    , foo (Postlude PTInt) (TInt 1)
    , foo (Postlude PTHalf) (THalf 1.5)
    , foo (Postlude PTFloat) (TFloat 1.5)
    , foo (Postlude PTDouble) (TDouble 1.5)
    , foo (Postlude PTBytes) (TBytes "hi")
    , foo (Postlude PTBytes) (TBytesI "hi")
    , foo (Postlude PTText) (TString "hi")
    , foo (Postlude PTText) (TStringI "hi")
    , foo (Postlude PTNil) TNull
    , foo (Postlude PTUndefined) (TSimple 23)
    , foo (Control Size (MIt (Postlude PTUInt)) (MIt (Literal (VUInt 1)))) (TInt 100)
    , foo (Control Size (MIt (Postlude PTBytes)) (MIt (Literal (VUInt 5)))) (TBytes "12345")
    , foo (Array [MIt (Postlude PTUInt), MIt (Postlude PTUInt)]) (TList [TInt 1, TInt 2])
    , foo (Array [MIt (Postlude PTUInt), MIt (Postlude PTUInt)]) (TList [TBytes "AA", TInt 2])
    ])

foo :: CTree MonoRef -> Term -> (Parsec Void [Term] Term, [Term])
foo a b = (
        mkParser
          (CTreeRoot (Map.singleton (Name "foo") (Identity (MIt a))))
          (MRuleRef (Name "foo"))
     ,[b]
     )
