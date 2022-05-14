module Lang.Parser (
  parseExpr
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import Text.ParserCombinators.Parsec.Combinator (choice)

import Lang.Lexer
import Lang.Syntax

import Debug.Trace(trace)
import Data.Functor.Identity

println msg = trace (show msg) $ return ()
seeNext :: Int -> ParsecT String u Identity ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  println out

-------------------------------------------------------------------------------
-- Expression
-------------------------------------------------------------------------------

bool :: Parser Expr
bool =  (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

number :: Parser Expr
number = do
  n <- natural
  return (Lit (LInt (fromIntegral n)))

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  x <- identifier
  reservedOp ":"
  t <- type'
  reservedOp "."
  e <- expr
  return (Lam x t e)

parsePred :: Parser FOLExpr
parsePred = do
    x  <- capsName
    ts <- parens (parseTerm `sepBy` char ',')
    return $ Pred x ts

parseForAll :: Parser FOLExpr
parseForAll = do
    string "forall" >> spaces
    x <- name
    char '.' >> spaces
    e <- expr
    return $ ForAll x e

parseExists :: Parser FOLExpr
parseExists = do
    string "exists" >> spaces
    x <- name
    char '.' >> spaces
    e <- expr
    return $ Exists x e

capsName :: Parser String
capsName = do
    c  <- upper
    cs <- many alphaNum
    return (c:cs)

factor :: Parser Expr
factor = parens expr
      <|> bool
      <|> number
      <|> variable
      <|> lambda

binOp :: String -> (Expr -> Expr -> Expr) -> Ex.Assoc -> Ex.Operator String () Identity Expr
binOp name fun assoc = Ex.Infix (do{ reservedOp name; return fun }) assoc

unOp :: String -> (Expr -> Expr) -> Ex.Operator String () Identity Expr
unOp name fun = Ex.Prefix (reservedOp s >> return f)

table :: Ex.OperatorTable String () Identity Expr
table = [ [ binOp "*" Mul Ex.AssocLeft
          , binOp "/" Div Ex.AssocLeft ]
        , [ binOp "+" Add Ex.AssocLeft
          , binOp "-" Sub Ex.AssocLeft ]
        , [binOp "==" Eq Ex.AssocNone]
        , [unOp "~" Not]
        , [ binary "&" Conj Ex.AssocLeft
        , , binary "|" Disj Ex.AssocLeft ]
        , [binary "=>" Impl Ex.AssocRight]
        ]

term :: Parser Expr
term = Ex.buildExpressionParser table factor

expr :: Parser Expr
expr = do
  seeNext 1
  es <- many1 term
  return (foldl1 App es)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

tyatom :: Parser Type
tyatom = tylit <|> (parens type')

tylit :: Parser Type
tylit = (reservedOp "Bool" >> return TBool) <|> (reservedOp "Int" >> return TInt)

type' :: Parser Type
type' = Ex.buildExpressionParser tyops tyatom
  where
    infixOp x f = Ex.Infix (reservedOp x >> return f)
    tyops = [
        [infixOp "->" TArr Ex.AssocRight]
      ]

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

parseExpr :: String -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input