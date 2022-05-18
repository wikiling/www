module Interpreter.Fragment where

import Text.Parsec.String
import System.Exit
import System.IO

import qualified Data.Map as Map
import qualified Compiler.Syntax as Syn
import qualified Compiler.Parser as Parse

{-
loadFragment :: FilePath -> Either ParseError Fragment
loadFragment fp = do
  frag <- Parse.parseFrag fp
  case frag of
    Left err -> err
    Right frag -> Fragment Map.fromList frag
-}
  
  
