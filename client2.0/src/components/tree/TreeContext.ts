import React from "react";
import { SemanticTree, Example, SyntaxTree } from "types";

export interface TreeContextInterface {
  example: Example
  interpretTree: (tree: SyntaxTree) => Promise<SemanticTree>
}
  
export const TreeContext = React.createContext<TreeContextInterface | null>(null);

export const useTreeContext = () => React.useContext(TreeContext);