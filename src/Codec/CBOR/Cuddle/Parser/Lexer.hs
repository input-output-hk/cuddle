{-# LANGUAGE OverloadedStrings #-}

module Codec.CBOR.Cuddle.Parser.Lexer (
  Parser,
  charInRange,
  (|||),
  space,
  (<*!),
  (!*>),
  space_,
) where

import Codec.CBOR.Cuddle.CDDL (Comment (..), WithComments (..))
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (..), Parsec, sepEndBy)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char qualified as L

type Parser = Parsec Void Text

charInRange :: Char -> Char -> Char -> Bool
charInRange lb ub x = lb <= x && x <= ub

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(x ||| y) a = x a || y a

pComment :: Parser Comment
pComment =
  Comment . NE.singleton
    <$> label "comment" (char ';' *> takeWhileP Nothing validChar <* eol)
  where
    validChar = charInRange '\x20' '\x7e' ||| charInRange '\x80' '\x10fffd'

space :: Parser (Maybe Comment)
space = foldMap Just <$> (L.space *> sepEndBy pComment L.space)

space_ :: Parser ()
space_ =
  -- TODO handle this more gracefully
  maybe
    ()
    ( \(Comment e) -> error $ "Could not attach the comments to a term:\n" <> unlines (T.unpack <$> toList e)
    )
    <$> space

(<*!) :: Parser a -> Parser (Maybe Comment) -> Parser (WithComments a)
x <*! c = WithComments <$> x <*> c

(!*>) :: Parser (Maybe Comment) -> Parser a -> Parser (WithComments a)
c !*> x = do
  c' <- c
  (`WithComments` c') <$> x
