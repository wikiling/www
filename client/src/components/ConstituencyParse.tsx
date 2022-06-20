import "./ConstituencyParse.scss";
import React, { useState } from 'react';
import { CoordinatedConstituencyParse, TreeID } from 'types';
import SyntaxTree from './trees/SyntaxTree';
import { CoordinatedTreeNode, EditableSyntaxNodeValues } from './trees/types';
import Button from "./Button";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import { toJS } from "mobx";
import Menu from "./Menu";
import useConstituencyParseOps from "hooks/useConstituencyParseOps";

type ConstituencyParseProps = {
  constituencyParse: CoordinatedConstituencyParse
  onRemove?: () => any
}

const ConstituencyParse: React.FC<ConstituencyParseProps> = ({ constituencyParse, onRemove }) => {
  const { fragmentStore: fs } = useStores();
  const [initialEditNode, setInitialEditNode] = useState<CoordinatedTreeNode | null>(null);
  const [treeEditCount, setTreeEditCount] = useState<number>(0);
  const incrTreeEditCount = () => setTreeEditCount(prev => prev + 1);

  return (
    <div className="constituency-parse">
      <div className="constituency-parse-tree">
        <SyntaxTree
          id={constituencyParse.id}
          // key={`${constituencyParse.id}-${treeEditCount}}`}
          tree={constituencyParse.coordinated_syntax_tree}
          nodeLabel={(node) => node.data.label}
          onNodeAdd={(nodeId: TreeID) => {
            const node = fs.addConstituencyParseNode(constituencyParse.id, nodeId);
            node && setInitialEditNode(node); // smells
            incrTreeEditCount();
          }}
          onNodeEdit={(values: EditableSyntaxNodeValues) => {
            fs.updateConstituencyParseNode(constituencyParse.id, {
              id: values.id,
              label: values.label
            });
            setInitialEditNode(null); // smells
            incrTreeEditCount();
          }}
          onNodeRemove={(nodeId: TreeID) => {
            fs.removeConstituencyParseNode(constituencyParse.id, nodeId);
            incrTreeEditCount();
          }}
          onNodeMove={(nodeId: TreeID, targetParentId: TreeID) => {
            fs.moveConstituencyParseNode(constituencyParse.id, nodeId, targetParentId);
            incrTreeEditCount();
          }}
          initialEditNode={initialEditNode}
        />
      </div>
    </div>
  );
};

export default observer(ConstituencyParse);