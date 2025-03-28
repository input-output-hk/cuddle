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

import Codec.CBOR.Cuddle.CDDL (Comment, WithComments (..), comment, unComment)
import Data.Foldable (Foldable (..))
import Data.Text (Text)
import Data.Text qualified as T
import Text.Megaparsec (MonadParsec (..), Parsec, ShowErrorComponent (..), customFailure, sepEndBy)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char qualified as L

newtype CDDLParserFailure
  = UnattachableComment Comment
  deriving (Eq, Ord)

instance ShowErrorComponent CDDLParserFailure where
  showErrorComponent (UnattachableComment cmt) =
    "Could not attach the comments to a term:\n" <> unlines (T.unpack <$> toList (unComment cmt))

type Parser = Parsec CDDLParserFailure Text

charInRange :: Char -> Char -> Char -> Bool
charInRange lb ub x = lb <= x && x <= ub

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(x ||| y) a = x a || y a

pComment :: Parser Comment
pComment =
  comment <$> label "comment" (char ';' *> takeWhileP Nothing validChar <* eol)
  where
    validChar = charInRange '\x20' '\x7e' ||| charInRange '\x80' '\x10fffd'

space :: Parser (Maybe Comment)
space = foldMap Just <$> (L.space *> sepEndBy pComment L.space)

space_ :: Parser ()
space_ = do
  spc <- space
  case spc of
    Nothing -> pure ()
    Just cmt -> customFailure (UnattachableComment cmt)

(<*!) :: Parser a -> Parser (Maybe Comment) -> Parser (WithComments a)
x <*! c = WithComments <$> x <*> c

(!*>) :: Parser (Maybe Comment) -> Parser a -> Parser (WithComments a)
c !*> x = do
  c' <- c
  (`WithComments` c') <$> x
