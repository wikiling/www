module Compile.Parse (
  parseExpr
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import Text.ParserCombinators.Parsec.Combinator (choice)

import Compile.Lex
import qualified Compile.Syn as S
import Compile.Pretty

import Debug.Trace (trace, traceM)
import Data.List (intercalate)
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
    tyops = [ [infixOp "->" S.TyFunc Ex.AssocRight]
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

parseSVar :: Parser S.Sym
parseSVar = do
  lookAhead lower
  n <- identifier
  pure (S.SVar n)

parseSConst :: Parser S.Sym
parseSConst = do
  c <- parseTitleIdentifier
  notFollowedBy $ char '(' -- brittle not to derive this constraint from `parsePred` conditions
  pure (S.SConst c)

parseSym :: Parser S.Sym
parseSym = try parseSVar <|> parseSConst

parseVar :: Parser S.Expr
parseVar = parseSVar >>= pure . S.ESym

parseConst :: Parser S.Expr
parseConst = parseSConst >>= pure . S.ESym

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

parseApp :: Parser S.Expr
parseApp = do
  es <- many1 parseTerm
  pure (foldl1 S.App es)

parseUnivQ :: Parser S.Expr
parseUnivQ = do
  reservedOp "\\forall"
  (x,t,e) <- parseBinder
  pure (S.UnivQ x t e)

parseExisQ :: Parser S.Expr
parseExisQ = do
  reservedOp "\\exists"
  (x,t,e) <- parseBinder
  pure (S.ExisQ x t e)

parsePred :: Parser S.Expr
parsePred = do
  n  <- parseTitleIdentifier
  ts <- parens ((spaces *> parseSym <* spaces) `sepBy` char ',')
  pure $ S.Pred n ts

completeParse p = traceM ("ok, parsed " ++ p)
tryParse p = traceM ("parsing " ++ p ++ "...")
debugParse :: String -> Parser S.Expr -> Parser S.Expr
debugParse s p = (tryParse s) *> p <* (completeParse s)

factor :: Parser S.Expr
factor = debugParse "app"   (parens parseExpr') <|>
         debugParse "bool"   (parseBool) <|>
         debugParse "number" (parseNumber) <|>
         debugParse "const"  (parseConst) <|>
         debugParse "var"    (parseVar) <|>
         debugParse "lambda" (parseLambda) <|>
         debugParse "pred"   (parsePred) <|>
         debugParse "univq"  (parseUnivQ) <|>
         debugParse "exisq"  (parseExisQ)

binOp :: String -> (S.Expr -> S.Expr -> S.Expr) -> Ex.Assoc -> Ex.Operator String () Identity S.Expr
binOp name fun assoc = Ex.Infix (do{ reservedOp name; pure fun }) assoc

unOp :: String -> (S.Expr -> S.Expr) -> Ex.Operator String () Identity S.Expr
unOp name fun = Ex.Prefix (reservedOp name >> pure fun)

opTable :: Ex.OperatorTable String () Identity S.Expr
opTable = [ [ binOp "*" S.Mul Ex.AssocLeft
            , binOp "/" S.Div Ex.AssocLeft ]
          , [ binOp "+" S.Add Ex.AssocLeft
            , binOp "-" S.Sub Ex.AssocLeft ]
          , [binOp "==" S.Eq Ex.AssocNone]
          , [unOp "~" S.Neg]
          , [ binOp "&" S.Conj Ex.AssocLeft
            , binOp "|" S.Disj Ex.AssocLeft ]
          , [binOp "=>" S.Impl Ex.AssocRight]
          ]

parseTerm :: Parser S.Expr
parseTerm = Ex.buildExpressionParser opTable factor

parseExpr' :: Parser S.Expr
parseExpr' = parseApp

-------------------------------------------------------------------------------
-- Entrypoint
-------------------------------------------------------------------------------

parseExpr :: String -> Either ParseError S.Expr
parseExpr input = parse (contents parseExpr') "<stdin>" input