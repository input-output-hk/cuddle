{-# LANGUAGE OverloadedStrings #-}

module Codec.CBOR.Cuddle.Parser where

import Codec.CBOR.Cuddle.CDDL
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.Functor (void, ($>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

pCDDL :: Parser CDDL
pCDDL = space *> NE.some (pRule <* space) <* eof

pRule :: Parser Rule
pRule =
  choice
    [ try $
        Rule
          <$> pName
          <*> optcomp pGenericParam
          <*> (space *> pAssignT <* space)
          <*> (TOGType <$> pType0),
      Rule
        <$> pName
        <*> optcomp pGenericParam
        <*> (space *> pAssignG <* space)
        <*> (TOGGroup <$> pGrpEntry)
    ]

pName :: Parser Name
pName = do
  fc <- firstChar
  rest <- many midChar
  pure $ Name . T.pack $ (fc : rest)
  where
    firstChar = letterChar <|> char '@' <|> char '_' <|> char '$'
    midChar =
      alphaNumChar
        <|> char '@'
        <|> char '_'
        <|> char '$'
        <|> ( (char '.' <|> char '-')
                <* notFollowedBy (space1 <|> eof <|> void eol)
            )

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
      (NE.sepBy1 (space *> pName <* space) (char ','))

pGenericArg :: Parser GenericArg
pGenericArg =
  GenericArg
    <$> between
      (char '<')
      (char '>')
      (NE.sepBy1 (space *> pType1 <* space) (char ','))

pType0 :: Parser Type0
pType0 = Type0 <$> NE.sepBy1 (space *> pType1 <* space) (char ',')

pType1 :: Parser Type1
pType1 = Type1 <$> pType2 <*> optcomp ((,) <$> pTyOp <*> pType2)

pType2 :: Parser Type2
pType2 =
  choice
    [ try $ T2Value <$> pValue,
      try $ T2Name <$> pName <*> optcomp pGenericArg,
      try $ T2Group <$> between (char '(') (char ')') (space *> pType0 <* space),
      try $ T2Map <$> between (char '{') (char '}') (space *> pGroup <* space),
      try $ T2Array <$> between (char '[') (char ']') (space *> pGroup <* space),
      try $ T2Unwrapped <$> (char '~' *> space *> pName) <*> optcomp pGenericArg,
      try $
        T2Enum
          <$> ( char '&'
                  *> space
                  *> between
                    (char '(')
                    (char ')')
                    (space *> pGroup <* space)
              ),
      try $ T2EnumRef <$> (char '&' *> space *> pName) <*> optcomp pGenericArg,
      try $
        T2Tag
          <$> (string "#6" *> optcomp (char '.' *> L.decimal))
          <*> between (char '(') (char ')') (space *> pType0 <* space),
      try $ T2DataItem <$> (char '#' *> L.decimal) <*> optcomp (char '.' *> L.decimal),
      T2Any <$ char '#'
    ]

pGroup :: Parser Group
pGroup = Group <$> NE.sepBy1 (space *> pGrpChoice <* space) (string "//")

pGrpChoice :: Parser GrpChoice
pGrpChoice = many ((space *> pGrpEntry <* space) <* optional (char ','))

pGrpEntry :: Parser GroupEntry
pGrpEntry =
  choice
    [ try $ GEType <$> optcomp pOccur <*> optcomp pMemberKey <*> pType0,
      try $ GERef <$> optcomp pOccur <*> pName <*> optcomp pGenericArg,
      GEGroup
        <$> optcomp pOccur
        <*> between
          (char '(')
          (char ')')
          (space *> pGroup <* space)
    ]

pMemberKey :: Parser MemberKey
pMemberKey =
  choice
    [ try $ MKType <$> pType1 <* space <* optional (char '^' <* space) <* string "=>",
      try $ MKBareword <$> pName <* space <* char ':' <* space,
      MKValue <$> pValue <* space <* char ':' <* space
    ]

pTyOp :: Parser TyOp
pTyOp =
  choice
    [ try $ RangeOp <$> pRangeBound,
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
      pText
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

-- pBytes = VBytes <$> (optional (string "h" <|> string "b64") $> mempty)

space :: Parser ()
space = L.space space1 (void pComment) (fail "No block comments")

pComment :: Parser Comment
pComment =
  Comment . T.pack
    <$> (char ';' *> many pChar <* eol)
  where
    pChar = satisfy (charInRange '\x20' '\x7e' ||| charInRange '\x80' '\x10fffd')

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

-- | A variant of 'optional' for composite parsers, which will consume no input
-- if it fails.
optcomp :: MonadParsec e s f => f a -> f (Maybe a)
optcomp = optional . try

{-
Appendix B.  ABNF Grammar

   This appendix is normative.

   The following is a formal definition of the CDDL syntax in ABNF
   [RFC5234].  Note that, as is defined in ABNF, the quote-delimited
   strings below are case insensitive (while string values and names are
   case sensitive in CDDL).

     cddl = S 1*(rule S)
     rule = typename [genericparm] S assignt S type
          / groupname [genericparm] S assigng S grpent

     typename = id
     groupname = id

     assignt = "=" / "/="
     assigng = "=" / "//="

     genericparm = "<" S id S *("," S id S ) ">"
     genericarg = "<" S type1 S *("," S type1 S ) ">"

     type = type1 *(S "/" S type1)

     type1 = type2 [S (rangeop / ctlop) S type2]
     ; space may be needed before the operator if type2 ends in a name

     type2 = value
           / typename [genericarg]
           / "(" S type S ")"
           / "{" S group S "}"
           / "[" S group S "]"
           / "~" S typename [genericarg]
           / "&" S "(" S group S ")"
           / "&" S groupname [genericarg]
           / "#" "6" ["." uint] "(" S type S ")"
           / "#" DIGIT ["." uint]                ; major/ai
           / "#"                                 ; any

     rangeop = "..." / ".."

     ctlop = "." id

     group = grpchoice *(S "//" S grpchoice)

     grpchoice = *(grpent optcom)

Birkholz, et al.             Standards Track                   [Page 45]
RFC 8610                          CDDL                         June 2019

     grpent = [occur S] [memberkey S] type
            / [occur S] groupname [genericarg]  ; preempted by above
            / [occur S] "(" S group S ")"

     memberkey = type1 S ["^" S] "=>"
               / bareword S ":"
               / value S ":"

     bareword = id

     optcom = S ["," S]

     occur = [uint] "*" [uint]
           / "+"
           / "?"

     uint = DIGIT1 *DIGIT
          / "0x" 1*HEXDIG
          / "0b" 1*BINDIG
          / "0"

     value = number
           / text
           / bytes

     int = ["-"] uint

     ; This is a float if it has fraction or exponent; int otherwise
     number = hexfloat / (int ["." fraction] ["e" exponent ])
     hexfloat = ["-"] "0x" 1*HEXDIG ["." 1*HEXDIG] "p" exponent
     fraction = 1*DIGIT
     exponent = ["+"/"-"] 1*DIGIT

     text = %x22 *SCHAR %x22
     SCHAR = %x20-21 / %x23-5B / %x5D-7E / %x80-10FFFD / SESC
     SESC = "\" (%x20-7E / %x80-10FFFD)

     bytes = [bsqual] %x27 *BCHAR %x27
     BCHAR = %x20-26 / %x28-5B / %x5D-10FFFD / SESC / CRLF
     bsqual = "h" / "b64"

Birkholz, et al.             Standards Track                   [Page 46]
RFC 8610                          CDDL                         June 2019

     id = EALPHA *(*("-" / ".") (EALPHA / DIGIT))
     ALPHA = %x41-5A / %x61-7A
     EALPHA = ALPHA / "@" / "_" / "$"
     DIGIT = %x30-39
     DIGIT1 = %x31-39
     HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
     BINDIG = %x30-31

     S = *WS
     WS = SP / NL
     SP = %x20
     NL = COMMENT / CRLF
     COMMENT = ";" *PCHAR CRLF
     PCHAR = %x20-7E / %x80-10FFFD
     CRLF = %x0A / %x0D.0A

-}
