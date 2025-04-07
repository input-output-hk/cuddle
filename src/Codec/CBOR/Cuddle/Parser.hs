{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Codec.CBOR.Cuddle.Parser where

import Codec.CBOR.Cuddle.CDDL
import Codec.CBOR.Cuddle.CDDL.CtlOp (CtlOp)
import Codec.CBOR.Cuddle.CDDL.CtlOp qualified as COp
import Codec.CBOR.Cuddle.Parser.Lexer (
  Parser,
  charInRange,
  pComment,
  pCommentBlock,
  sameLineComment,
  space,
  (!*>),
  (//?),
  (<*!),
  (|||),
 )
import Control.Applicative (liftA2)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Data.ByteString qualified as B
import Data.Foldable1 (Foldable1 (..))
import Data.Functor (void, ($>))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Scientific qualified as Sci
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Void (Void)
import GHC.Word (Word64, Word8)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Foldable (Foldable(..))

pCDDL :: Parser CDDL
pCDDL = do
  initialComments <- many pCommentBlock <* notFollowedBy pRule <* C.space
  initialRuleComment <- optional pCommentBlock
  initialRule <- pRule
  cddlTail <- many $ pTopLevel <* C.space
  eof $> CDDL initialComments (initialRule //? initialRuleComment) cddlTail

pTopLevel :: Parser TopLevel
pTopLevel = try tlRule <|> tlComment
  where
    tlRule = do
      mCmt <- optional pCommentBlock
      rule <- pRule
      pure . TopLevelRule $ rule //? mCmt
    tlComment = TopLevelComment <$> pCommentBlock

pRule :: Parser Rule
pRule = do
  name <- pName
  genericParam <- optcomp pGenericParam
  cmt <- space
  (assign, typeOrGrp) <-
    choice
      [ try $
          (,)
            <$> pAssignT
            <* space
            <*> (TOGType <$> pType0 <* notFollowedBy (space >> (":" <|> "=>")))
      , (,) <$> pAssignG <* space <*> (TOGGroup <$> pGrpEntry)
      ]
  pure $ Rule name genericParam assign typeOrGrp cmt

pName :: Parser Name
pName = label "name" $ do
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
        <|> ((char '.' <|> char '-') <* notFollowedBy (space1 <|> eof <|> void eol))

pAssignT :: Parser Assign
pAssignT =
  choice
    [ AssignEq <$ "="
    , AssignExt <$ "/="
    ]

pAssignG :: Parser Assign
pAssignG =
  choice
    [ AssignEq <$ "="
    , AssignExt <$ "//="
    ]

pGenericParam :: Parser GenericParam
pGenericParam =
  GenericParam
    <$> between "<" ">" (NE.sepBy1 (space !*> pName <*! space) ",")

pGenericArg :: Parser GenericArg
pGenericArg =
  GenericArg
    <$> between "<" ">" (NE.sepBy1 (space !*> pType1 <*! space) ",")

pType0 :: Parser Type0
pType0 = Type0 <$> sepBy1' (space !*> pType1 <*! space) (try "/")

pType1 :: Parser Type1
pType1 = do
  -- Type1 <$> pType2 <*> optcomp (space !*> ((,) <$> pTyOp <*> (space !*> pType2)))
  v <- pType2
  rest <- optcomp $ do
    cmtFst <- space
    tyOp <- pTyOp
    cmtSnd <- space
    w <- pType2
    pure (cmtFst, tyOp, cmtSnd, w)
  case rest of
    Just (cmtFst, tyOp, cmtSnd, w) ->
      pure $ Type1 v (Just (tyOp, w //- cmtSnd)) //- cmtFst
    Nothing -> pure $ Type1 v Nothing

pType2 :: Parser Type2
pType2 =
  choice
    [ T2Value <$> pValue
    , T2Name <$> pName <*> optional pGenericArg
    , T2Group <$> ("(" *> space !*> pType0 <*! space <* ")")
    , T2Map <$> ("{" *> space !*> pGroup <*! space <* "}")
    , T2Array <$> ("[" *> space !*> pGroup <*! space <* "]")
    , T2Unwrapped <$> ("~" *> space !*> pName) <*> optional pGenericArg
    , "&"
        <*! space
        *> choice
          [ T2Enum <$> ("(" *> space !*> pGroup <*! space <* ")")
          , T2EnumRef <$> pName <*> optional pGenericArg
          ]
    , "#" *> do
        mmajor :: Maybe Word8 <- optional L.decimal
        case mmajor of
          Just major -> do
            mminor <- optional ("." *> L.decimal)
            let
              pTag
                | major == 6 = T2Tag mminor <$> ("(" *> space !*> pType0 <*! space <* ")")
                | otherwise = empty
            pTag <|> pure (T2DataItem major mminor)
          Nothing -> pure T2Any
    ]

pHeadNumber :: Parser Word64
pHeadNumber = L.decimal

pRangeOp :: Parser RangeBound
pRangeOp = try ("..." $> ClOpen) <|> (".." $> Closed)

pCtlOp :: Parser CtlOp
pCtlOp =
  label "Control operator" $
    "."
      *> choice
        ( try
            <$> [ "cborseq" $> COp.Cborseq
                , "cbor" $> COp.Cbor
                , "size" $> COp.Size
                , "bits" $> COp.Bits
                , "within" $> COp.Within
                , "and" $> COp.And
                , "lt" $> COp.Lt
                , "le" $> COp.Le
                , "gt" $> COp.Gt
                , "ge" $> COp.Ge
                , "eq" $> COp.Eq
                , "ne" $> COp.Ne
                , "default" $> COp.Default
                , "regexp" $> COp.Regexp
                ]
        )

pGroup :: Parser Group
pGroup = Group <$> NE.sepBy1 (space !*> pGrpChoice <*! space) "//"

pGrpChoice :: Parser GrpChoice
pGrpChoice = many (pGrpEntry <*! pOptCom)

pGrpEntry :: Parser GroupEntry
pGrpEntry = do
  occur <- optcomp pOccur
  variant <- space !*> choice
    [ try $ GEType <$> optcomp (pMemberKey <*! space) <*> pType0
    , try $ GERef <$> pName <*> optional pGenericArg
    , GEGroup <$> ("(" *> space !*> pGroup <*! space <* ")")
    ]
  pure $ GroupEntry undefined undefined undefined

pMemberKey :: Parser MemberKey
pMemberKey =
  choice
    [ try $ MKType <$> pType1 <*! space <*! (fold <$> optcomp ("^" >> space)) <* "=>"
    , try $ MKBareword <$> pName <*! space <* ":"
    , MKValue <$> pValue <*! space <* ":"
    ]

pOptCom :: Parser Comment
pOptCom = space <*! (fold <$> optional ("," >> space))

pOccur :: Parser OccurrenceIndicator
pOccur =
  label "occurrence indicator" $
    choice
      [ char '+' $> OIOneOrMore
      , char '?' $> OIOptional
      , pBounded
      ]

pValue :: Parser Value
pValue =
  label "value" $
    choice
      [ try pFloat
      , try pInt
      , try pBytes
      , pText
      ]
  where
    pSignedNum :: Num a => Parser a -> Parser (Bool, a)
    pSignedNum valParser = do
      sign <- optional "-"
      val <- valParser <* notFollowedBy "*"
      pure (isJust sign, val)
    -- Need to ensure that number values are not actually bounds on a later
    -- value.
    pInt =
      pSignedNum L.decimal >>= \case
        (False, val) -> pure $ VUInt val
        (True, val) -> pure $ VNInt val
    pFloat =
      pSignedNum L.float >>= \case
        (False, val) -> pure $ VFloat64 val
        (True, val) -> pure . VFloat64 $ negate val
    -- Currently this doesn't allow string escaping
    pSChar :: Parser Text
    pSChar = takeWhileP (Just "character") $ \x ->
      or $
        [ charInRange '\x20' '\x21'
        , charInRange '\x23' '\x5b'
        , charInRange '\x5d' '\x7e'
        , charInRange '\x80' '\x10fffd'
        ]
          <*> pure x
    pText = VText <$> ("\"" *> pSChar <* "\"")
    pSByte = takeWhileP (Just "byte character") $ \x ->
      or $
        [ charInRange '\x20' '\x26'
        , charInRange '\x28' '\x5b'
        , charInRange '\x5d' '\x10fffd'
        ]
          <*> pure x
    pBytes = do
      _qualifier <- optional ("h" <|> "b64")
      between "'" "'" $ VBytes . encodeUtf8 <$> pSByte

pTyOp :: Parser TyOp
pTyOp =
  choice
    [ try $ RangeOp <$> pRangeOp
    , CtrlOp <$> pCtlOp
    ]

pBounded :: Parser OccurrenceIndicator
pBounded = do
  lo <- optional L.decimal
  _ <- char '*'
  hi <- optional L.decimal
  pure $ case (lo, hi) of
    (Nothing, Nothing) -> OIZeroOrMore
    (x, y) -> OIBounded x y

-- | A variant of 'optional' for composite parsers, which will consume no input
-- if it fails.
optcomp :: MonadParsec e s f => f a -> f (Maybe a)
optcomp = optional . try

{-
cddl = S *(rule S)
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
      / "#" "6" ["." head-number] "(" S type S ")"
      / "#" "7" ["." head-number]
      / "#" DIGIT ["." uint]                ; major/ai
      / "#"                                 ; any
head-number = uint / ("<" type ">")

rangeop = "..." / ".."

ctlop = "." id

group = grpchoice *(S "//" S grpchoice)

grpchoice = *(grpent optcom)

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
SCHAR = %x20-21 / %x23-5B / %x5D-7E / NONASCII / SESC

SESC = "\" ( %x22 / "/" / "\" /                 ; \" \/ \\
             %x62 / %x66 / %x6E / %x72 / %x74 / ; \b \f \n \r \t
             (%x75 hexchar) )                   ; \uXXXX

hexchar = "{" (1*"0" [ hexscalar ] / hexscalar) "}" /
          non-surrogate / (high-surrogate "\" %x75 low-surrogate)
non-surrogate = ((DIGIT / "A"/"B"/"C" / "E"/"F") 3HEXDIG) /
                ("D" %x30-37 2HEXDIG )
high-surrogate = "D" ("8"/"9"/"A"/"B") 2HEXDIG
low-surrogate = "D" ("C"/"D"/"E"/"F") 2HEXDIG
hexscalar = "10" 4HEXDIG / HEXDIG1 4HEXDIG
          / non-surrogate / 1*3HEXDIG

bytes = [bsqual] %x27 *BCHAR %x27
BCHAR = %x20-26 / %x28-5B / %x5D-7E / NONASCII / SESC / "\'" / CRLF
bsqual = "h" / "b64"

id = EALPHA *(*("-" / ".") (EALPHA / DIGIT))
ALPHA = %x41-5A / %x61-7A
EALPHA = ALPHA / "@" / "_" / "$"
DIGIT = %x30-39
DIGIT1 = %x31-39
HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
HEXDIG1 = DIGIT1 / "A" / "B" / "C" / "D" / "E" / "F"
BINDIG = %x30-31

S = *WS
WS = SP / NL
SP = %x20
NL = COMMENT / CRLF
COMMENT = ";" *PCHAR CRLF
PCHAR = %x20-7E / NONASCII
NONASCII = %xA0-D7FF / %xE000-10FFFD
CRLF = %x0A / %x0D.0A
-}

-- | Variant on 'NE.sepEndBy1' which doesn't consume the separator
sepBy1' :: MonadParsec e s m => m a -> m sep -> m (NonEmpty a)
sepBy1' p sep = NE.fromList <$> go
  where
    go = liftA2 (:) p (many (try $ sep *> p))
