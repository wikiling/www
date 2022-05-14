module Lang.Parser
  (parseExpr)
where

import Lang.Syntax (Expr(..))
import Text.Parsec (parse, letter, alphaNum, oneOf, eof, (<|>))
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Data.Functor.Identity (Identity)

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = []
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

table :: Ex.OperatorTable String () Identity Expr
table = [
    [
      prefixOp "succ" Succ
    , prefixOp "pred" Pred
    , prefixOp "iszero" IsZero
    ]
  ]

-- Ternary Conditional
ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

-- Constants
true, false, zero :: Parser Expr
true  = reserved "true"  >> return Tr
false = reserved "false" >> return Fl
zero  = reservedOp "0"   >> return Zero

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor =
      true
  <|> false
  <|> zero
  <|> ifthen
  <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr s = parse (contents expr) "<stdin>" s