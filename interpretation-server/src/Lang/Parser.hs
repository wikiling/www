module Lang.Parser (
  parseExpr
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import Text.ParserCombinators.Parsec.Combinator (choice)

import Lang.Lexer
import qualified Lang.Syntax as S

import Debug.Trace(trace)
import Data.Functor.Identity

println msg = trace (show msg) $ return ()
seeNext :: Int -> ParsecT String u Identity ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  println out

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

tyatom :: Parser S.Type
tyatom = tylit <|> (parens parseType)

tylit :: Parser S.Type
tylit = (reservedOp "Bool" >> return S.TBool)
     <|> (reservedOp "Int" >> return S.TInt)
     <|> (reservedOp "Ent" >> return S.TEnt)

parseType :: Parser S.Type
parseType = Ex.buildExpressionParser tyops tyatom
  where
    infixOp x f = Ex.Infix (reservedOp x >> return f)
    tyops = [
        [infixOp "->" S.TFunc Ex.AssocRight]
      ]

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

parseBool :: Parser S.Expr
parseBool = (reserved "True" >> return (S.Lit (S.LBool True)))
         <|> (reserved "False" >> return (S.Lit (S.LBool False)))

parseNumber :: Parser S.Expr
parseNumber = do
  n <- natural
  return (S.Lit (S.LInt (fromIntegral n)))

parseVariable :: Parser S.Expr
parseVariable = do
  x <- identifier
  return (S.Var x)

parseBinder :: Parser (String, S.Type, S.Expr)
parseBinder = do
  x <- identifier
  reservedOp ":"
  t <- parseType
  reservedOp "."
  e <- parseExpr'
  return (x,t,e)

parseLambda :: Parser S.Expr
parseLambda = do
  reservedOp "\\"
  (x,t,e) <- parseBinder
  return (S.Lam x t e)

parseUnivQ :: Parser S.Expr
parseUnivQ = do
  reservedOp "forall"
  (x,t,e) <- parseBinder
  return (S.UnivQ x t e)

parseExisQ :: Parser S.Expr
parseExisQ = do
  reservedOp "exists"
  (x,t,e) <- parseBinder
  return (S.ExisQ x t e)

title :: Parser String
title = do
  c  <- upper
  cs <- many alphaNum
  return (c:cs)

parsePred :: Parser S.Expr
parsePred = do
  x  <- title
  ts <- parens ((spaces *> identifier <* spaces) `sepBy` char ',')
  return $ S.Pred x ts

factor :: Parser S.Expr
factor = parens parseExpr'
      <|> parseBool
      <|> parseNumber
      <|> parseVariable
      <|> parseLambda
      <|> parsePred
      <|> parseUnivQ
      <|> parseExisQ

binOp :: String -> (S.Expr -> S.Expr -> S.Expr) -> Ex.Assoc -> Ex.Operator String () Identity S.Expr
binOp name fun assoc = Ex.Infix (do{ reservedOp name; return fun }) assoc

unOp :: String -> (S.Expr -> S.Expr) -> Ex.Operator String () Identity S.Expr
unOp name fun = Ex.Prefix (reservedOp name >> return fun)

table :: Ex.OperatorTable String () Identity S.Expr
table = [ [ binOp "*" S.Mul Ex.AssocLeft
          , binOp "/" S.Div Ex.AssocLeft ]
        , [ binOp "+" S.Add Ex.AssocLeft
          , binOp "-" S.Sub Ex.AssocLeft ]
        , [binOp "==" S.Eq Ex.AssocNone]
        , [unOp "~" S.Neg]
        , [ binOp "&" S.Conj Ex.AssocLeft
          , binOp "|" S.Disj Ex.AssocLeft ]
        , [binOp "=>" S.Impl Ex.AssocRight]
        ]

term :: Parser S.Expr
term = Ex.buildExpressionParser table factor

parseExpr' :: Parser S.Expr
parseExpr' = do
  seeNext 1
  es <- many1 term
  return (foldl1 S.App es)

-------------------------------------------------------------------------------
-- Entrypoint
-------------------------------------------------------------------------------

parseExpr :: String -> Either ParseError S.Expr
parseExpr input = parse (contents parseExpr') "<stdin>" input