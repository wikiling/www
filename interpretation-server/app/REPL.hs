module REPL (main) where

import Compile.Syn
import Compile.Parse
import Compile.Type
import Compile.Pretty

import Interpret.Sem

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> do
      let chk = checkTop [] ex
      case chk of
        Left tyerr -> print tyerr
        Right _ -> print $ runEval ex

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "λ> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input | length input > 0 -> (liftIO $ process input) >> loop
                 | otherwise        -> loop