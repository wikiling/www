module Lang.Lexer where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Tok
import qualified Text.ParserCombinators.Parsec.Char as CTok
import Text.Parsec.Language (haskellStyle)

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["->", "\\", "="]
        names = ["True", "False"]
        style = haskellStyle {Tok.identStart = CTok.lower,
                              Tok.reservedOpNames = ops,
                              Tok.reservedNames = names,
                              Tok.commentLine = "#"}

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