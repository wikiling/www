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
  onRemove: () => any
}

const ConstituencyParse: React.FC<ConstituencyParseProps> = ({ constituencyParse, onRemove }) => {
  const { fragmentStore: fs } = useStores();
  const [initialEditNode, setInitialEditNode] = useState<CoordinatedTreeNode | null>(null);
  const {
    handleInterpret,
    handleRemove,
    handleSave,

    treeEditCount,
    incrTreeEditCount,
    isLoading,
  } = useConstituencyParseOps(constituencyParse);

  return (
    <div className="constituency-parse">
      <div className="constituency-parse-tree">
        <SyntaxTree
          id={constituencyParse.id}
          key={`${constituencyParse.id}-${treeEditCount}`}
          tree={toJS(constituencyParse.coordinated_syntax_tree)}
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

      <Menu isLoading={isLoading}>
        <Button mode="menu" onClick={handleInterpret}>interpret</Button>
        <Button mode="menu" onClick={() => handleRemove(onRemove)}>remove</Button>
        <Button mode="menu" onClick={handleSave}>save</Button>
      </Menu>
    </div>
  );
};

export default observer(ConstituencyParse);