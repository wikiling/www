module Interpreter.CompositionSpec where
import Data.Foldable (toList)
import Control.Monad (void)
-- import Test.Hspec
import Interpreter.Compose
import Interpreter.Fragment
import Compiler.Core.Syntax
import Compiler.Core.Parser
import Compiler.Core.Inference
import Compiler.Core.Pretty
import qualified Compiler.Core.TypeEnv as TE
import Compiler.Tree.Syntax


node :: Label -> Int -> ConstituencyTree -> ConstituencyTree -> ConstituencyTree
node l pos c1 c2 = Node (CLabel l pos) c1 c2

cat l = node (CatLabel l)
leaf n pos = node (LexLabel n) pos Leaf Leaf

main :: IO ()
main = do
  let v = (cat "V" 10 (leaf "stab" 100) Leaf)
  let v' = (cat "V'" 1 v (cat "NP" 11 (leaf "Caesar" 110) Leaf))
  let vp = (cat "VP" 1 (cat "NP" 0 (leaf "Brutus" 00) Leaf) v')
  let asp = (cat "Asp" 110 (leaf "PF" 1100) (leaf "z" 1101))
  let asp' = cat "AspP'" 11 asp vp
  let aspP = (cat "AspP" 1 (leaf "id" 10) asp')
  let s = cat "S" 1 (leaf "bindt" 0) aspP

  -- let fragE = parseFragS "[V] = \\y:<e> . \\x:<e> . \\e:<v> . V:<v,<e,<e,t>>>(e)(y)(x); [NP] = NP:<e>; Duration:<v,i>; [PF] = \\t:<i> . \\p:<v,t> . exists e:<v> . ((Duration e) subs t) & (p e); [t] = T:<i>"
  -- let fragE = parseFragS "[bindt] = \\t:<i>; \n [V] = \\y:<e> . \\x:<e> . \\e:<v> . V:<v,<e,<e,t>>>(e)(y)(x); \n [NP] = NP:<e>; \n Runtime:<v,i>; \n [PF] = \\t:<i> . \\p:<v,t> . exists e:<v> . ((Runtime e) subs t) & (p e); \n [t] = T:<i>; \n [id] = \\x:<A> . x"
  let fragE = parseFragS "[V] = \\y:<e> . \\x:<e> . \\e:<v> . V:<v,<e,<e,t>>>(e)(y)(x); [NP] = NP:<e>; Runtime:<v,i>; [PF] = \\t:<i> . \\p:<v,t> . exists e:<v> . ((Runtime e) subs t) & (p e); [z] = z; [id] = \\x:<A> . x; [bindt] = \\z:<i>"
  
  case fragE of
    Left e -> print e
    Right decls -> print $ head $ toList $ compose (mkFragment decls) s