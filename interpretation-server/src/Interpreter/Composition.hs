module Interpreter.Composition where

import System.IO
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader

import qualified Compiler.Syntax as Syn
import qualified Compiler.Types as Ty
import qualified Compiler.Parser as Parse
import qualified Interpreter.Fragment as Frag
import qualified Interpreter.Semantics as Sem

type LexicalEntry = (Syn.Expr, Syn.Type)
type Fragment = Map.Map String LexicalEntry

data BTree a b = Branch a (BTree a b, BTree a b) | Leaf b | Ep

data SemNode = SemNode Syn.Type Syn.Expr Sem.Value
type SemTree = BTree (Maybe SemNode) Syn.Sym
type SynTree = BTree String String

type Ctx = Reader Fragment
type CompositionTree = Ctx SemTree

checkLexicon :: Syn.Name -> Ctx (Maybe LexicalEntry)
checkLexicon name = asks (Map.lookup name)

compose :: SynTree -> CompositionTree
compose synTree = case synTree of
  Ep       -> _
  Leaf   l -> _
  Branch b cs -> _

runComposition :: Fragment -> SynTree -> SemTree
runComposition frag synTree = runReader (compose synTree) frag

data LoadError = LParError Parse.ParseError
               | LTyError Ty.TypeError

loadFragment :: FilePath -> ExceptT LoadError IO (Either LoadError Fragment)
loadFragment fp = runExceptT $ do
  fragIO <- liftIO $ Parse.parseFrag fp
  case fragIO of
    Left parErr -> throwError (LParError parErr)
    Right decls -> pure $ Map.fromList $ mapError typeCheck decls
  where
    typeCheck (name, expr) = case Ty.checkTop [] expr of
      Left tyErr -> throwError $ LTyError tyErr
      Right ty   -> (name, (expr, ty))

      
       
  