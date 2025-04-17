{-# LANGUAGE OverloadedStrings #-}

module Codec.CBOR.Cuddle.Parser.Lexer (
  Parser,
  charInRange,
  space,
  pComment,
  sameLineComment,
  (|||),
  pCommentBlock,
) where

import Codec.CBOR.Cuddle.Comments (Comment (..))
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.Foldable1 (Foldable1 (..))
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (
  MonadParsec (..),
  Parsec,
  sepEndBy,
  (<|>),
 )
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char qualified as L

type Parser = Parsec Void Text

charInRange :: Char -> Char -> Char -> Bool
charInRange lb ub x = lb <= x && x <= ub

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(x ||| y) a = x a || y a

pComment :: Parser Comment
pComment =
  Comment <$> label "comment" (char ';' *> takeWhileP Nothing validChar <* eol)
  where
    validChar = charInRange '\x20' '\x7e' ||| charInRange '\x80' '\x10fffd'

pCommentBlock :: Parser Comment
pCommentBlock = fold1 <$> NE.some (L.hspace *> pComment)

space :: Parser Comment
space = mconcat <$> (L.space *> sepEndBy pComment L.space)

sameLineComment :: Parser Comment
sameLineComment =
  try ((<>) <$> (L.hspace *> pComment) <*> space) <|> (L.space $> mempty)
