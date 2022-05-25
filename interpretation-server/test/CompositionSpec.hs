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

  case fragE of
    Right decls -> case loadDecls decls of
      Right frag -> printTree $ runComposition frag synTree
{-
       ┌Just Noun e "Brutus"┐
       │                    └Just Brutus e "Brutus"
Nothing┤
       │       ┌Just λy:e -> (λx:e -> (λe:v -> Stab(e, y, x))) e -> e -> v -> t "stab"┐
       │       │                                                                      └Just stab e "stab"
       └Nothing┤
               └Just Noun e "Caesar"┐
                                    └Just Caesar e "Caesar"
-}