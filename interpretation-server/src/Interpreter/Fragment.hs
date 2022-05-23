module Interpreter.Fragment where

import System.IO
import Control.Monad.Except
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Compiler.Types as Ty
import qualified Compiler.Syntax as Syn
import qualified Compiler.Parser as Parse

type LexicalEntry = (Syn.Expr, Syn.Type)
type Fragment = Map.Map String LexicalEntry

data LoadError = LParError Parse.ParseError
               | LTyError [Ty.TypeError]

typeCheck :: Syn.Decl -> Either.Either Ty.TypeError (String, LexicalEntry)
typeCheck (name, expr) = case Ty.checkTop [] expr of
  Right ty -> Right (name, (expr, ty))
  Left err -> Left err

loadFragment :: FilePath -> ExceptT LoadError IO (Either.Either LoadError Fragment)
loadFragment fp = runExceptT $ do
  fragIO <- liftIO $ Parse.parseFrag fp
  case fragIO of
    Left parErr -> throwError (LParError parErr)
    Right decls -> do
      let (errs, tyCheckedDecls) = Either.partitionEithers $ map typeCheck decls
      if null errs
        then pure $ Map.fromList tyCheckedDecls
        else throwError (LTyError errs)