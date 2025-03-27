module Codec.CBOR.Cuddle.Parser.Lexer (
  Parser,
  charInRange,
  (|||),
  space,
  lexeme,
  symbol,
) where

import Codec.CBOR.Cuddle.CDDL (Comment (..))
import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (..), Parsec)
import Text.Megaparsec.Char (char, eol, space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

charInRange :: Char -> Char -> Char -> Bool
charInRange lb ub x = lb <= x && x <= ub

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(x ||| y) a = x a || y a

pComment :: Parser Comment
pComment =
  Comment
    <$> (char ';' *> takeWhileP Nothing validChar <* eol)
  where
    validChar = charInRange '\x20' '\x7e' ||| charInRange '\x80' '\x10fffd'

space :: Parser ()
space = L.space space1 (void pComment) (fail "No block comments")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space
