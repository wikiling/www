module Compiler.Parser (
  parseExpr,
  parseFrag,
  ParseError
) where

import System.IO (IO, FilePath)

import Text.Parsec
import Text.Parsec.Token (brackets)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Text.Parsec.Expr as Ex
import Text.ParserCombinators.Parsec.Combinator (choice)

import Compiler.Lexer
import qualified Compiler.Syntax as Syn
import Compiler.Pretty

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

tyatom :: Parser Syn.Type
tyatom = tylit <|> (parens parseType)

tylit :: Parser Syn.Type
tylit = (reservedOp "Bool" >> pure Syn.TyBool)
     <|> (reservedOp "Int" >> pure Syn.TyInt)
     <|> (reservedOp "Ent" >> pure Syn.TyEnt)

parseType :: Parser Syn.Type
parseType = Ex.buildExpressionParser tyops tyatom
  where
    infixOp x f = Ex.Infix (reservedOp x >> pure f)
    tyops = [ [infixOp "->" Syn.TyFunc Ex.AssocRight]
            ]

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------
parseTitleIdentifier :: Parser String
parseTitleIdentifier = lookAhead upper >> identifier

parseBool :: Parser Syn.Expr
parseBool = (reserved "True" >> pure (Syn.ELit (Syn.LBool True)))
         <|> (reserved "False" >> pure (Syn.ELit (Syn.LBool False)))

parseNumber :: Parser Syn.Expr
parseNumber = do
  n <- natural
  pure (Syn.ELit (Syn.LInt (fromIntegral n)))

lIdentifier = (lookAhead lower) >> identifier

parseSVar :: Parser Syn.Sym
parseSVar = Syn.SVar <$> lIdentifier

parseSConst :: Parser Syn.Sym
parseSConst = do
  c <- parseTitleIdentifier
  notFollowedBy $ char '(' -- brittle not to derive this constraint from `parsePred` conditions
  pure (Syn.SConst c)

parseSym :: Parser Syn.Sym
parseSym = try parseSVar <|> parseSConst

parseVar :: Parser Syn.Expr
parseVar = parseSVar >>= pure . Syn.ESym

parseConst :: Parser Syn.Expr
parseConst = parseSConst >>= pure . Syn.ESym

parseBinder :: Parser (Syn.Name, Syn.Type, Syn.Expr)
parseBinder = do
  x <- identifier
  reservedOp ":"
  t <- parseType
  reservedOp "."
  e <- parseExpr'
  pure (x,t,e)

parseLambda :: Parser Syn.Expr
parseLambda = do
  reservedOp "\\"
  (x,t,e) <- parseBinder
  pure (Syn.Lam x t e)

parseApp :: Parser Syn.Expr
parseApp = do
  es <- many1 parseTerm
  pure (foldl1 Syn.App es)

parseUnivQ :: Parser Syn.Expr
parseUnivQ = do
  reservedOp "\\forall"
  (x,t,e) <- parseBinder
  pure (Syn.UnivQ x t e)

parseExisQ :: Parser Syn.Expr
parseExisQ = do
  reservedOp "\\exists"
  (x,t,e) <- parseBinder
  pure (Syn.ExisQ x t e)

parsePred :: Parser Syn.Expr
parsePred = do
  n  <- parseTitleIdentifier
  ts <- parens ((spaces *> parseSym <* spaces) `sepBy` char ',')
  pure $ Syn.Pred n ts

parseLet :: Parser Syn.Decl
parseLet = do
  string "["
  name <- lIdentifier
  reservedOp "="
  expr <- parseExpr'
  string "]"
  pure $ (name, expr)

completeParse p = traceM ("ok, parsed " ++ p)
tryParse p = traceM ("parsing " ++ p ++ "...")
debugParse :: String -> Parser Syn.Expr -> Parser Syn.Expr
debugParse s p = (tryParse s) *> p <* (completeParse s)

factor :: Parser Syn.Expr
factor = debugParse "app"    (parens parseExpr') <|>
         debugParse "bool"   (parseBool) <|>
         debugParse "number" (parseNumber) <|>
         debugParse "const"  (parseConst) <|>
         debugParse "var"    (parseVar) <|>
         debugParse "lambda" (parseLambda) <|>
         debugParse "pred"   (parsePred) <|>
         debugParse "univq"  (parseUnivQ) <|>
         debugParse "exisq"  (parseExisQ)

binOp :: String -> (Syn.Expr -> Syn.Expr -> Syn.Expr) -> Ex.Assoc -> Ex.Operator String () Identity Syn.Expr
binOp name fun assoc = Ex.Infix (do{ reservedOp name; pure fun }) assoc

unOp :: String -> (Syn.Expr -> Syn.Expr) -> Ex.Operator String () Identity Syn.Expr
unOp name fun = Ex.Prefix (reservedOp name >> pure fun)

opTable :: Ex.OperatorTable String () Identity Syn.Expr
opTable = [ [ binOp "*" Syn.Mul Ex.AssocLeft
            , binOp "/" Syn.Div Ex.AssocLeft ]
          , [ binOp "+" Syn.Add Ex.AssocLeft
            , binOp "-" Syn.Sub Ex.AssocLeft ]
          , [binOp "==" Syn.Eq Ex.AssocNone]
          , [unOp "~" Syn.Neg]
          , [ binOp "&" Syn.Conj Ex.AssocLeft
            , binOp "|" Syn.Disj Ex.AssocLeft ]
          , [binOp "=>" Syn.Impl Ex.AssocRight]
          ]

parseTerm :: Parser Syn.Expr
parseTerm = Ex.buildExpressionParser opTable factor

parseExpr' :: Parser Syn.Expr
parseExpr' = parseApp

parseFrag' :: Parser [Syn.Decl]
parseFrag' = many parseLet

-------------------------------------------------------------------------------
-- Entrypoints
-------------------------------------------------------------------------------

type ExprParse = Either ParseError Syn.Expr
type FragParse = Either ParseError [Syn.Decl]

parseExpr :: String -> ExprParse
parseExpr input = parse (contents parseExpr') "<stdin>" input

parseFrag :: FilePath -> IO FragParse
parseFrag fp = parseFromFile parseFrag' fp
