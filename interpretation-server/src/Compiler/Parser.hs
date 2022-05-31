module Compiler.Parser (
  parseExpr,
  parseFrag,
  parseFragS,
  parseDecl,
  ParseError
) where

import System.IO (IO, FilePath)
import qualified Data.Map as Map
import Debug.Trace (trace, traceM)
import Data.List (intercalate)
import Data.Functor.Identity
import Control.Monad (void)
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.ParserCombinators.Parsec.Combinator (choice)

import Compiler.Lexer
import qualified Compiler.Syntax as Syn
import Compiler.Pretty

println msg = trace (show msg) $ pure ()
seeNext :: Int -> ParsecT String u Identity ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  println out

debugParse :: String -> Parser a -> Parser a
debugParse s p = (tryParse s) *> p <* (completeParse s)
  where
    tryParse p = parserTrace ("parsing " ++ p ++ "...")
    completeParse p = parserTrace ("ok, parsed " ++ p)

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

tyatom :: Parser Syn.Type
tyatom = tylit <|> (angles parseType)

tylit :: Parser Syn.Type
tylit = (reservedOp "t" >> pure Syn.tyBool)
     <|> (reservedOp "n" >> pure Syn.tyInt)
     <|> (identifier >>= \t -> pure $ Syn.TyCon t)

parseType :: Parser Syn.Type
parseType = Ex.buildExpressionParser tyops tyatom
  where
    infixOp x f = Ex.Infix (reservedOp x >> pure f)
    tyops = [ [infixOp "," Syn.TyFunc Ex.AssocRight]
            ]

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

parseTypeAssignment :: Parser Syn.Type
parseTypeAssignment = reservedOp ":" >> parseType

parseBool :: Parser Syn.Expr
parseBool = debugParse "bool" (reserved "True" >> pure (Syn.ELit (Syn.LBool True)))
         <|> (reserved "False" >> pure (Syn.ELit (Syn.LBool False)))

parseNumber :: Parser Syn.Expr
parseNumber = debugParse "number" $ do
  n <- natural
  pure (Syn.ELit (Syn.LInt (fromIntegral n)))

titularIdentifier = (lookAhead upper) >> identifier
lIdentifier = (lookAhead lower) >> identifier

parseVar :: Parser Syn.Expr
parseVar = debugParse "var" $ do
  x <- lIdentifier
  s <- getState
  t <- parseTypeAssignment <|> (pure $ s Map.! x)
  pure $ Syn.ESym (Syn.SVar x) t

parseConst :: Parser Syn.Expr
parseConst = debugParse "const" $ do
  c <- titularIdentifier
  t <- parseTypeAssignment <|> (pure $ Syn.TyVar (Syn.TV "A"))
  notFollowedBy $ char '(' -- brittle not to derive this constraint from `parsePred` conditions
  pure $ Syn.ESym (Syn.SConst c) t

parseBinder :: Parser (Syn.Name, Syn.Type, Syn.Expr)
parseBinder = debugParse "binder" $ do
  x <- identifier
  t <- parseTypeAssignment
  modifyState (Map.insert x t)
  reservedOp "."
  e <- parseExpr'
  pure (x,t,e)

parseLambda :: Parser Syn.Expr
parseLambda = debugParse "lambda" $ do
  reservedOp "\\"
  (x,t,e) <- parseBinder
  pure (Syn.Lam x t e)

parseApp :: Parser Syn.Expr
parseApp = do
  es <- many1 parseTerm
  pure (foldl1 Syn.App es)

parseUnivQ :: Parser Syn.Expr
parseUnivQ = debugParse "univq" $ do
  reservedOp "forall"
  (n,t,e) <- parseBinder
  pure (Syn.UnivQ (Syn.ESym (Syn.SVar n) t) e)

parseExisQ :: Parser Syn.Expr
parseExisQ = debugParse "exisq" $ do
  reservedOp "exists"
  (n,t,e) <- parseBinder
  pure (Syn.ExisQ (Syn.ESym (Syn.SVar n) t) e)

parsePred :: Parser Syn.Expr
parsePred = debugParse "pred" $ do
  n  <- identifier
  ts <- parens ((spaces *> parseExpr' <* spaces) `sepBy` char ',')
  pure $ Syn.Pred n ts

parseSet :: Parser Syn.Expr
parseSet = brackets ((spaces *> parseExpr' <* spaces) `sepBy` char ',') >>= (pure . Syn.mkset)

parseLet :: Parser Syn.Decl
parseLet = do
  string "["
  name <- identifier
  string "]"
  spaces
  reservedOp "="
  spaces
  expr <- parseExpr'
  pure $ (name, expr)

factor :: Parser Syn.Expr
factor = (parens parseExpr') <|>
         (parseBool)         <|>
         (parseNumber)       <|>
         (try parseConst)    <|>
         (parseUnivQ)        <|>
         (parseExisQ)        <|>
         (parseVar)          <|>
         (parseLambda)       <|>
         (parsePred)         <|>
         (parseSet)    

binOp :: String -> (Syn.Expr -> Syn.Expr -> Syn.BinOp) -> Ex.Assoc -> Ex.Operator String SymTypeState Identity Syn.Expr
binOp name fun assoc = Ex.Infix (reservedOp name >> (pure $ \e0 -> \e1 -> Syn.EBinOp $ fun e0 e1)) assoc

unOp :: String -> (Syn.Expr -> Syn.UnOp) -> Ex.Operator String SymTypeState Identity Syn.Expr
unOp name fun = Ex.Prefix (reservedOp name >> (pure $ \e -> Syn.EUnOp $ fun e))

opTable :: Ex.OperatorTable String SymTypeState Identity Syn.Expr
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
parseExpr' = debugParse "app" parseApp

parseFrag' :: Parser [Syn.Decl]
parseFrag' = whitespace >> many parseLet

-------------------------------------------------------------------------------
-- Entrypoints
-------------------------------------------------------------------------------

type ExprParse = Either ParseError Syn.Expr
type FragParse = Either ParseError [Syn.Decl]

parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- readFile fname
         return (runP p Map.empty fname input)

parseDecl :: String -> Either ParseError Syn.Decl
parseDecl input = runP (contents parseLet) Map.empty "<stdin>" input

parseExpr :: String -> ExprParse
parseExpr input = runP (contents parseExpr') Map.empty "<stdin>" input

parseFrag :: FilePath -> IO FragParse
parseFrag fp = parseFromFile parseFrag' fp

parseFragS :: String -> FragParse
parseFragS input = runP (contents parseFrag') Map.empty "<stdin>" input
