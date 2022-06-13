module Interpreter.CompositionSpec where
import Data.Foldable (toList)
import Control.Monad (void)
-- import Test.Hspec
import Interpreter.Compose
import Interpreter.Fragment
import Compiler.Syntax
import Compiler.Parser
import Compiler.Inference
import Compiler.Pretty
import qualified Compiler.TypeEnv as TE


node :: String -> String -> ConstituencyTree -> ConstituencyTree -> ConstituencyTree
node s pos c1 c2 = Node (CLabel s pos) c1 c2
leaf n pos = node n pos Leaf Leaf

isLet decl = case decl of
  Let{} -> True
  _ -> False

toTup (Let n e) = (n,e)

ppInferDecls :: [Decl] -> IO ()
ppInferDecls ds = case inferTop TE.empty $ map toTup (filter isLet ds) of
  Right env -> print env
  Left err -> print err

main :: IO ()
main = do
  let vp = (node "VP" "-1" (node "NP" "0" (leaf "Brutus" "00") Leaf) (node "V'" "1" (node "V" "10" (leaf "stab" "100") Leaf) (node "NP" "11" (leaf "Caesar" "110") Leaf)))
  let asp = (node "Asp" "110" (leaf "PF" "1100") (leaf "t" "1101"))
  let asp' = node "AspP'" "11" asp vp
  let aspP = (node "AspP" "1" (leaf "id" "10") asp')
  let s = node "S" "-1" (leaf "bindt" "0") aspP

  -- let fragE = parseFragS "[V] = \\y:<e> . \\x:<e> . \\e:<v> . V:<v,<e,<e,t>>>(e)(y)(x); [NP] = NP:<e>; Duration:<v,i>; [PF] = \\t:<i> . \\p:<v,t> . exists e:<v> . ((Duration e) subs t) & (p e); [t] = T:<i>"
  -- let fragE = parseFragS "[bindt] = \\t:<i>; \n [V] = \\y:<e> . \\x:<e> . \\e:<v> . V:<v,<e,<e,t>>>(e)(y)(x); \n [NP] = NP:<e>; \n Runtime:<v,i>; \n [PF] = \\t:<i> . \\p:<v,t> . exists e:<v> . ((Runtime e) subs t) & (p e); \n [t] = T:<i>; \n [id] = \\x:<A> . x"
  let fragE = parseFragS "[V] = \\y:<e> . \\x:<e> . \\e:<v> . V:<v,<e,<e,t>>>(e)(y)(x); [NP] = NP:<e>; Runtime:<v,i>; [PF] = \\t:<i> . \\p:<v,t> . exists e:<v> . ((Runtime e) subs t) & (p e); [t] = t; [id] = \\x:<A> . x; [bindt] = \\t:<i>"
  
  case fragE of
    Left e -> print e
    Right decls -> do
      print decls
      ppInferDecls decls
      case loadDecls decls of
        Left e -> print e
        Right frag -> print $ show (head $ toList $ compose frag s)