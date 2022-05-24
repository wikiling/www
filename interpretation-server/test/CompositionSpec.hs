module Interpreter.CompositionSpec where

import Control.Monad (void)
import Test.Hspec
import Interpreter.Composition
import Interpreter.Fragment
import Compiler.Syntax
import Compiler.Parser

leaf n = Node n Leaf Leaf

main :: IO ()
main = do
  let synTree = Node "VP" (Node "NP" (leaf "Brutus") Leaf) (Node "V'" (Node "V" (leaf "stab") Leaf) (Node "NP" (leaf "Caesar") Leaf))
  let fragE = parseFragS "[V] = \\y:Ent . \\x:Ent . \\e:V . Stab(e,y,x) \n [NP] = Noun"
  print (show synTree)

  case fragE of
    Right decls -> do
      print "1"
      print (show decls)
      case loadDecls decls of
        Right frag -> do
          print "2"
          print $ show $ runComposition frag synTree
          print "3"
