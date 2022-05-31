module Interpreter.CompositionSpec where

import Control.Monad (void)
-- import Test.Hspec
import Interpreter.Composition
import Interpreter.Fragment
import Compiler.Syntax
import Compiler.Parser

node :: String -> String -> ConstituencyTree -> ConstituencyTree -> ConstituencyTree
node s pos c1 c2 = Node (CNodeLabel s pos) c1 c2
leaf n pos = node n pos Leaf Leaf

main :: IO ()
main = do
  let vp = (node "VP" "-1" (node "NP" "0" (leaf "Brutus" "00") Leaf) (node "V'" "1" (node "V" "10" (leaf "stab" "100") Leaf) (node "NP" "11" (leaf "Caesar" "110") Leaf)))
  let asp = (node "Asp" "110" (leaf "PF" "1100") (leaf "t" "1101"))
  let asp' = node "AspP'" "11" asp vp
  let aspP = (node "AspP" "1" (leaf "" "10") asp')
  let s = node "S" "-1" (leaf "bindt" "0") aspP

  let fragE = parseFragS "[V] = \\y:<e> . \\x:<e> . \\e:<v> . V(e,y,x) \n [NP] = NP:e \n [PF] = \\t:<i> . \\p:<v,t> . exists e:<v> . T(e) & p e \n [t] = T:i"

  case fragE of
    Left e -> print e
    Right decls -> do
      print decls
      case loadDecls decls of
        Left e -> print e
        Right frag -> printTree $ runComposition frag s
      
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