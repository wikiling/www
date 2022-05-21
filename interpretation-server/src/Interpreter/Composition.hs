module Interpreter.Composition where

import System.IO
import qualified Data.Map as Map
import qualified Data.Either as Either
import Control.Monad.Except
import Control.Monad.Reader

import qualified Compiler.Syntax as Syn
import qualified Compiler.Types as Ty
import qualified Compiler.Parser as Parse
import qualified Interpreter.Fragment as Frag
import qualified Interpreter.Semantics as Sem

type LexicalEntry = (Syn.Expr, Syn.Type)
type Fragment = Map.Map String LexicalEntry

data BTree a = Ep | Branch a BTree a BTree a
type Leaf a = Branch a Ep Ep

data SemNode = SemNode Syn.Expr Syn.Type Sem.Value
type SemTree = BTree (Maybe SemNode)
type SynTree = BTree String

type FragmentCtx = Reader Fragment
type CompositionTree = FragmentCtx SemTree

checkLexicon :: Syn.Name -> FragmentCtx (Maybe LexicalEntry)
checkLexicon name = asks (Map.lookup name)

compose :: SynTree -> CompositionTree
compose synTree = case synTree of
  Leaf a -> case checkLexicon a of
    Nothing -> Nothing
    Just (LexicalEntry (expr, ty)) -> Just Leaf SemNode expr ty (Sem.runEval expr)
  Branch b (Branch b1) Ep -> preTerm b1
  Branch b Ep (Branch b2) -> preTerm b2
  Branch b (Branch b1) (Branch b2) -> do
    l1 <- compose b1
    l2 <- compose b2
    if isNothing l1 || isNothing l2
      then Nothing
      else 
  where
    preTerm b = case compose b of
      Nothing -> Nothing
      Just (Leaf SemNode Syn.SConst const)
      Just (Leaf SemNode expr ty v) -> 

  
  Branch b (Leaf l1 Leaf l2) ->
  Branch b (d1,d2) -> do
    let (l1, l2) = (checkLexicon d1, checkLexicon d2)


runComposition :: Fragment -> SynTree -> SemTree
runComposition frag synTree = runReader (compose synTree) frag

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



      
       
  