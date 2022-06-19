{-# LANGUAGE DeriveGeneric #-}

module Compiler.Tree.Syntax (
  T.Tree(..), TreePosition, T.fromList,
  Label(..), ConstituencyLabel(..), ConstituencyTree(..),
  mkBiCatNode, mkUnCatNode, mkLexNode
) where

import GHC.Generics
import qualified Data.Tree.Binary.Preorder as T

type TreePosition = Int
type Category = String
type Lexeme = String

data Label
  = CatLabel Category
  | LexLabel Lexeme deriving (Show, Generic)

data ConstituencyLabel = CLabel Label TreePosition deriving (Show, Generic)

type ConstituencyTree = T.Tree ConstituencyLabel

mkLexNode lex = T.Node (LexLabel lex) T.Leaf T.Leaf
mkBiCatNode cat c0 c1 = T.Node (CatLabel cat) c0 c1
mkUnCatNode cat c = T.Node (CatLabel cat) c T.Leaf
