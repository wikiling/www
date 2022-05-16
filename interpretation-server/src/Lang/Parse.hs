module Lang.Parse (
  parseExpr
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import Text.ParserCombinators.Parsec.Combinator (choice)

import Lang.Lex
import qualified Lang.Syn as S

import Debug.Trace(trace)
import Data.Functor.Identity

println msg = trace (show msg) $ pure ()
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
tylit = (reservedOp "Bool" >> pure S.TyBool)
     <|> (reservedOp "Int" >> pure S.TyInt)
     <|> (reservedOp "Ent" >> pure S.TyEnt)

parseType :: Parser S.Type
parseType = Ex.buildExpressionParser tyops tyatom
  where
    infixOp x f = Ex.Infix (reservedOp x >> pure f)
    tyops = [
        [infixOp "->" S.TyFunc Ex.AssocRight]
      ]

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------
parseTitleIdentifier :: Parser String
parseTitleIdentifier = lookAhead upper >> identifier

parseBool :: Parser S.Expr
parseBool = (reserved "True" >> pure (S.ELit (S.LBool True)))
         <|> (reserved "False" >> pure (S.ELit (S.LBool False)))

parseNumber :: Parser S.Expr
parseNumber = do
  n <- natural
  pure (S.ELit (S.LInt (fromIntegral n)))

parseTVar :: Parser S.Term
parseTVar = do
  lookAhead lower
  n <- identifier
  pure (S.TVar n)

parseTConst :: Parser S.Term
parseTConst = do
  c <- parseTitleIdentifier
  notFollowedBy $ char '(' -- brittle not to derive this constraint from `parsePred` conditions
  pure (S.TConst c)

parseTerm :: Parser S.Term
parseTerm = try parseTVar <|> parseTConst

parseVariable :: Parser S.Expr
parseVariable = parseTVar >>= \v -> pure (S.ETerm v)

parseConst :: Parser S.Expr
parseConst = parseTConst >>= \c -> pure (S.ETerm c)

parseBinder :: Parser (String, S.Type, S.Expr)
parseBinder = do
  x <- identifier
  reservedOp ":"
  t <- parseType
  reservedOp "."
  e <- parseExpr'
  pure (x,t,e)

parseLambda :: Parser S.Expr
parseLambda = do
  reservedOp "\\"
  (x,t,e) <- parseBinder
  pure (S.Lam x t e)

parseUnivQ :: Parser S.Expr
parseUnivQ = do
  reservedOp "forall"
  (x,t,e) <- parseBinder
  pure (S.UnivQ x t e)

parseExisQ :: Parser S.Expr
parseExisQ = do
  reservedOp "exists"
  (x,t,e) <- parseBinder
  pure (S.ExisQ x t e)

parsePred :: Parser S.Expr
parsePred = do
  n  <- parseTitleIdentifier
  ts <- parens ((spaces *> parseTerm <* spaces) `sepBy` char ',')
  pure $ S.Pred n ts

factor :: Parser S.Expr
factor = parens parseExpr'
      <|> parseBool
      <|> parseNumber
      <|> try parseConst
      <|> parseVariable
      <|> parseLambda
      <|> try parsePred
      <|> parseUnivQ
      <|> parseExisQ

binOp :: String -> (S.Expr -> S.Expr -> S.Expr) -> Ex.Assoc -> Ex.Operator String () Identity S.Expr
binOp name fun assoc = Ex.Infix (do{ reservedOp name; pure fun }) assoc

unOp :: String -> (S.Expr -> S.Expr) -> Ex.Operator String () Identity S.Expr
unOp name fun = Ex.Prefix (reservedOp name >> pure fun)

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
  -- seeNext 1
  es <- many1 term
  pure (foldl1 S.App es)

-------------------------------------------------------------------------------
-- Entrypoint
-------------------------------------------------------------------------------

parseExpr :: String -> Either ParseError S.Expr
parseExpr input = parse (contents parseExpr') "<stdin>" input