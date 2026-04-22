{-# LANGUAGE OverloadedStrings #-}

module Codec.CBOR.Cuddle.Parser.Lexer (
  Parser,
  charInRange,
  space,
  trailingSpace,
  pComment,
  (|||),
  pCommentBlock,
) where

import Codec.CBOR.Cuddle.Comments (Comment (..))
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.Foldable1 (Foldable1 (..))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (
  MonadParsec (..),
  Parsec,
  many,
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
  Comment . (<> "\n") <$> label "comment" (char ';' *> takeWhileP Nothing validChar <* eol)
  where
    validChar = charInRange '\x20' '\x7e' ||| charInRange '\x80' '\x10fffd'

pCommentBlock :: Parser Comment
pCommentBlock = fold1 <$> NE.some (L.hspace *> pComment)

space :: Parser Comment
space = mconcat <$> (L.space *> sepEndBy pComment L.space)

-- | Non-greedy space consumer for use at construct boundaries.
-- Consumes a same-line comment and any immediately adjacent comment lines
-- (no blank line gap), then eats trailing blank lines.
-- Does NOT cross blank-line boundaries to consume further comment blocks.
trailingSpace :: Parser Comment
trailingSpace = do
  _ <- L.hspace
  comments <- sameLineComments
  L.space
  pure $ mconcat comments
  where
    sameLineComments =
      ( do
          c <- pComment
          rest <- adjacentComments
          pure (c : rest)
      )
        <|> (eol *> adjacentComments)
        <|> pure []

    adjacentComments = many (try (L.hspace *> pComment))
