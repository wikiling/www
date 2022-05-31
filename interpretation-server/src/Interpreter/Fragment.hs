module Interpreter.Fragment where

import System.IO
import qualified Data.Either as E
import qualified Data.Map as Map
import qualified Compiler.Types as Ty
import qualified Compiler.Syntax as Syn
import qualified Compiler.Parser as Parse

type LexicalEntry = (Syn.Expr, Syn.Type)
type Fragment = Map.Map String LexicalEntry

data LoadError = LParError Parse.ParseError
               | LTyError [Ty.TypeError]
instance Show LoadError where
  show (LParError e) = show e
  show (LTyError e) = show e

typeCheck :: Syn.Decl -> E.Either Ty.TypeError (String, LexicalEntry)
typeCheck decl = case decl of
  Syn.Let (name, expr) -> case Ty.checkTop [] expr of
    Right ty -> Right (name, (expr, ty))
    Left err -> Left err

loadDecls :: [Syn.Decl] -> E.Either LoadError Fragment
loadDecls decls = do
  let (errs, tyCheckedDecls) = E.partitionEithers $ map typeCheck decls
  if null errs
    then Right (Map.fromList tyCheckedDecls)
    else Left (LTyError errs)

loadFragment :: FilePath -> IO (E.Either LoadError Fragment)
loadFragment fp = do
  fragIO <- Parse.parseFrag fp
  case fragIO of
    Left parErr -> pure $ Left (LParError parErr)
    Right decls -> pure $ loadDecls decls
