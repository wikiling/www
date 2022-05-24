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

typeCheck :: Syn.Decl -> E.Either Ty.TypeError (String, LexicalEntry)
typeCheck (name, expr) = case Ty.checkTop [] expr of
  Right ty -> Right (name, (expr, ty))
  Left err -> Left err

loadFragment :: FilePath -> IO (E.Either LoadError Fragment)
loadFragment fp = do
  fragIO <- Parse.parseFrag fp
  case fragIO of
    Left parErr -> pure $ Left (LParError parErr)
    Right decls -> do
      let (errs, tyCheckedDecls) = E.partitionEithers $ map typeCheck decls
      if null errs
        then pure $ Right (Map.fromList tyCheckedDecls)
        else pure $ Left (LTyError errs)