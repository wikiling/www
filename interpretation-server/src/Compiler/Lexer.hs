module Compiler.Lexer where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Tok
import qualified Text.ParserCombinators.Parsec.Char as CTok
import Text.Parsec.Language (haskellStyle)

reservedNames :: [String]
reservedNames = [
    "exists",
    "forall"
  ]

reservedOps :: [String]
reservedOps = [
    "->",
    "\\",
    "+",
    "*",
    "-",
    "="
  ]

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser $ Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = reservedNames
  , Tok.reservedOpNames = reservedOps
  , Tok.caseSensitive   = True
  }
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

natural :: Parser Integer
natural = Tok.natural lexer