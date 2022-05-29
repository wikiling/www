module REPL (main) where

import Compiler.Syntax
import Compiler.Parser
import Compiler.Types
import Compiler.Pretty

import Interpreter.Evaluation

import Control.Monad.Trans
import System.Console.Haskeline

process :: String ->  IO ()
process line = do
  let expr = parseExpr line
  case expr of
    Left err -> print err
    Right ex -> do
      let chk = checkTop [] ex
      case chk of
        Left tyerr -> print tyerr
        Right ty    -> do
          print $ show ex
          print $ runEval ex


main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    mInput <- getInputLine "λ> "
    case mInput of
      Nothing -> outputStrLn "Goodbye."
      Just input | length input > 0 -> (liftIO $ process input) >> loop
                 | otherwise        -> loop