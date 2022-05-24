import "./ConstituencyParse.scss";
import React, { useState } from 'react';
import { CoordinatedConstituencyParse, TreeID } from 'types';
import Tree from './tree/Tree';
import { EditableNodeValues } from './tree/types';
import Button from "./Button";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import { toJS } from "mobx";
import Menu from "./Menu";
import useLoadWhile from "hooks/useLoadWhile";

type ConstituencyParseProps = {
  constituencyParse: CoordinatedConstituencyParse
  onRemove: () => any
}

const ConstituencyParse: React.FC<ConstituencyParseProps> = ({ constituencyParse, onRemove }) => {
  const { fragmentStore: fs } = useStores();
  const [treeEditCount, setTreeEditCount] = useState<number>(0);
  const incrTreeEditCount = () => setTreeEditCount(prev => prev + 1);
  const { isLoading, loadWhile } = useLoadWhile();

  const handleInterpret = () => loadWhile(
    () => fs.dispatchInterpretConstituencyParse(constituencyParse)
  );

  const handleRemove = () => loadWhile(
    async () => {
      await fs.dispatchDeleteConstituencyParse(constituencyParse.id);
      onRemove();
    }
  );

  const handleSave = () => loadWhile(
    async () => {
      await fs.dispatchUpdateConstituencyParse(constituencyParse.id, {
        parse_string: constituencyParse.coordinated_syntax_tree.parseString()
      });
      setTreeEditCount(0);
    }
  );

  return (
    <div className="constituency-parse">
      <div className="constituency-parse-tree">
        <Tree
          id={constituencyParse.id}
          key={`${constituencyParse.id}-${treeEditCount}`}
          syntaxTree={toJS(constituencyParse.coordinated_syntax_tree)}
          onNodeAdd={(nodeId: TreeID) => {
            const node = fs.addConstituencyParseNode(constituencyParse.id, nodeId);
            incrTreeEditCount();
            return node;
          }}
          onNodeEdit={(values: EditableNodeValues) => {
            fs.updateConstituencyParseNode(constituencyParse.id, {
              id: values.id,
              label: values.label
            });
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
        />
      </div>

      <Menu isLoading={isLoading}>
        <Button mode="menu" onClick={handleInterpret}>interpret</Button>
        <Button mode="menu" onClick={handleRemove}>remove</Button>
        <Button mode="menu" onClick={handleSave}>save</Button>
      </Menu>
    </div>
  );
};

export default observer(ConstituencyParse);