import React from "react";
import { SemanticTree, SyntaxTree } from "types";

export interface TreeContextInterface {
  interpretTree: (tree: SyntaxTree) => Promise<SemanticTree>
}
  
export const TreeContext = React.createContext<TreeContextInterface | null>(null);

export const useTreeContext = () => React.useContext(TreeContext);