import React from "react";
import EditableSyntaxNode from "./EditableSyntaxNode";
import SyntaxNode from "./SyntaxNode";
import Tree, { TreeProps } from "./Tree";

type SyntaxTreeProps = Omit<TreeProps , 'nodeComponent' | 'editableNodeComponent'> & {
}

const SyntaxTree: React.FC<SyntaxTreeProps> = ({ ...treeProps }) => {
  return <Tree
    nodeComponent={(props) => <SyntaxNode {...props}/>}
    editableNodeComponent={(props) => <EditableSyntaxNode {...props}/>}
    {...treeProps}
  />
};

export default SyntaxTree