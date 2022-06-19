module Compiler.Tree.Parser (
  parseConstituencyTree
) where

import Data.Foldable                 (toList)
import Data.Char                     (digitToInt)
import Data.List                     (foldl')
import           Control.Monad.State
import           Text.Megaparsec     ((<|>))
import qualified Text.Megaparsec  as P

import Compiler.Tree.Lexer        as L
import Compiler.Tree.Syntax       as S

type CTreeParser = L.Parser (S.Tree S.Label)

lexNode :: CTreeParser
lexNode = do
  label <- identifier
  pure $ mkLexNode label

unCatNode :: CTreeParser
unCatNode = do
  cat <- identifier
  c   <- P.try lexNode <|> cTree

  pure $ mkUnCatNode cat c

biCatNode :: CTreeParser
biCatNode = do
  cat <- identifier
  c0  <- P.try lexNode <|> cTree
  c1  <- P.try lexNode <|> cTree

  pure $ mkBiCatNode cat c0 c1

cTree :: CTreeParser
cTree = P.try (brackets unCatNode) <|> (brackets biCatNode)

positionCTree :: S.Tree S.Label -> S.ConstituencyTree
positionCTree = S.fromList . fst . (foldl' position ([],-1)) . toList
  where
    position (ls, pos) l = let pos' = pos + 1 in (ls ++ [S.CLabel l pos'], pos')

parseConstituencyTree s = case P.runParser cTree "<input>" s of
  Right tree -> Right $ positionCTree tree
  Left err -> Left err
