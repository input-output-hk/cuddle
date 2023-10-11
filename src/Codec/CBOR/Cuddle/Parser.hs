{-# LANGUAGE OverloadedStrings #-}

module Codec.CBOR.Cuddle.Parser where

import Codec.CBOR.Cuddle.CDDL
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

pRule :: Parser Rule
pRule =
  choice
    [ Rule
        <$> pName
        <*> optional pGenericParam
        <*> pAssignG
        <*> (TOGGroup <$> pGroup),
      Rule
        <$> pName
        <*> optional pGenericParam
        <*> pAssignT
        <*> (TOGType <$> pType0)
    ]

pName :: Parser Name
pName = do
  fc <- firstChar
  rest <- optional (mappend <$> many midChar <*> fmap (: []) lastChar)
  pure $ Name . T.pack $ (fc : fromMaybe mempty rest)
  where
    firstChar = letterChar <|> char '@' <|> char '_' <|> char '$'
    midChar = alphaNumChar <|> char '-' <|> char '_' <|> char '@' <|> char '.' <|> char '$'
    lastChar = alphaNumChar <|> char '@' <|> char '_' <|> char '$'

pAssignT :: Parser Assign
pAssignT =
  choice
    [ AssignEq <$ char '=',
      AssignExt <$ string "/="
    ]

pAssignG :: Parser Assign
pAssignG =
  choice
    [ AssignEq <$ char '=',
      AssignExt <$ string "//="
    ]

pGenericParam :: Parser GenericParam
pGenericParam =
  GenericParam
    <$> between
      (char '<')
      (char '>')
      (NE.sepBy1 pName (space <* char ',' <* space))

pGenericArg :: Parser GenericArg
pGenericArg =
  GenericArg
    <$> between
      (char '<')
      (char '>')
      (NE.sepBy1 pType1 (space <* char ',' <* space))

pType0 :: Parser Type0
pType0 = Type0 <$> NE.sepBy1 pType1 (space <* char ',' <* space)

pType1 :: Parser Type1
pType1 = Type1 <$> pType2 <*> optional ((,) <$> pTyOp <*> pType2)

pType2 :: Parser Type2
pType2 =
  choice
    [ T2Value <$> pValue,
      T2Name <$> pName <*> optional pGenericArg,
      T2Group <$> pType0,
      T2Map <$> pGroup
    ]

pGroup :: Parser Group
pGroup = Group <$> NE.sepBy1 pGrpChoice (space <* string "//" *> space)

pGrpChoice :: Parser GrpChoice
pGrpChoice = many (pGrpEntry <* optional (space *> char ',' *> space))

pGrpEntry :: Parser GroupEntry
pGrpEntry =
  choice
    [ GEType <$> optional pOccur <*> optional pMemberKey <*> pType0,
      GERef <$> optional pOccur <*> pName <*> optional pGenericArg,
      GEGroup <$> optional pOccur <*> pGroup
    ]

pMemberKey :: Parser MemberKey
pMemberKey =
  choice
    [ MKType <$> pType1,
      MKBareword <$> pName,
      MKValue <$> pValue
    ]

pTyOp :: Parser TyOp
pTyOp =
  choice
    [ RangeOp <$> pRangeBound,
      CtrlOp <$> (char '.' *> pName)
    ]
  where
    pRangeBound :: Parser RangeBound
    pRangeBound = (string ".." $> Closed) <|> (string ".." $> ClOpen)

pOccur :: Parser OccurrenceIndicator
pOccur =
  choice
    [ char '+' $> OIOneOrMore,
      char '?' $> OIOptional,
      mkBounded <$> optional L.decimal <*> (char '*' *> optional L.decimal)
    ]
  where
    mkBounded mlb mub = case (mlb, mub) of
      (Nothing, Nothing) -> OIZeroOrMore
      (x, y) -> OIBounded x y

pValue :: Parser Value
pValue =
  choice
    [ pNumber,
      pText,
      pBytes
    ]
  where
    pNumber = VNum <$> L.decimal
    pText = VText <$> (char '"' *> pSChar <* char '"')
    -- Currently this doesn't allow string escaping
    pSChar :: Parser Text
    pSChar =
      T.pack
        <$> many
          ( satisfy
              ( charInRange '\x20' '\x21'
                  ||| charInRange '\x23' '\x5b'
                  ||| charInRange '\x5d' '\x7e'
                  ||| charInRange '\x80' '\x10fffd'
              )
          )
    pBytes = VBytes <$> (optional (string "h" <|> string "b64") $> mempty)

-- pBChar :: Parser B.ByteString
-- pBChar =
--   B.pack
--     <$> many
--       ( satisfy
--           ( charInRange '\x20' '\x26'
--               ||| charInRange '\x28' '\x5b'
--               ||| charInRange '\x5d' '\x10fffd'
--           )
--           <|> crlf
--       )

charInRange :: Char -> Char -> Char -> Bool
charInRange lb ub x = lb <= x && x <= ub

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(x ||| y) a = x a || y a
