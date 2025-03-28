{-# LANGUAGE OverloadedStrings #-}

module Codec.CBOR.Cuddle.Parser.Lexer (
  Parser,
  charInRange,
  space,
  space_,
  pComment,
  sameLineComment,
  (|||),
  (<*!),
  (!*>),
  pCommentBlock,
) where

import Codec.CBOR.Cuddle.CDDL (Comment, WithComments (..), comment)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.Foldable1 (Foldable1 (..))
import Data.Functor (void, ($>))
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
  comment <$> label "comment" (char ';' *> takeWhileP Nothing validChar <* eol)
  where
    validChar = charInRange '\x20' '\x7e' ||| charInRange '\x80' '\x10fffd'

pCommentBlock :: Parser Comment
pCommentBlock = fold1 <$> NE.some (L.hspace *> pComment)

space :: Parser (Maybe Comment)
space = foldMap Just <$> (L.space *> sepEndBy pComment L.space)

space_ :: Parser ()
space_ = void space

sameLineComment :: Parser (Maybe Comment)
sameLineComment =
  try ((<>) <$> (L.hspace *> fmap Just pComment) <*> space) <|> (L.space $> Nothing)

(<*!) :: Parser a -> Parser (Maybe Comment) -> Parser (WithComments a)
x <*! c = WithComments <$> x <*> c

(!*>) :: Parser (Maybe Comment) -> Parser a -> Parser (WithComments a)
c !*> x = do
  c' <- c
  (`WithComments` c') <$> x
