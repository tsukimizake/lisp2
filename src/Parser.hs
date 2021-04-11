{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Parser where

import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Token
import Types

type Parser a = Parsec Text () a

tp :: GenTokenParser Text () Identity
tp =
  makeTokenParser $
    LanguageDef
      { commentStart = "/*",
        commentEnd = "*/",
        commentLine = "//",
        nestedComments = False,
        identStart = letter,
        identLetter = letter <|> char '-',
        opStart = oneOf "!#$%&|*+-/<=>?@^_~:",
        opLetter = oneOf "!#$%&|*+-/<=>?@^_~:",
        reservedNames = [],
        reservedOpNames = [":"],
        caseSensitive = True
      }

expr :: Parser Expr
expr =
  try (parens tp do val <- expr; reservedOp tp ":"; type_ <- expr; pure val) -- typed expr parser (not used yet)
    <|> Atom . T.pack <$> identifier tp
    <|> Atom . T.pack <$> operator tp
    <|> Constant . Num . fromIntegral <$> integer tp
    <|> Constant . Str . T.pack <$> stringLiteral tp
    <|> parens tp (List <$> sepBy1 expr spaces)

parseExpr :: Text -> Either ParseError Expr
parseExpr = runParser (do res <- expr; eof; pure res) () ""
