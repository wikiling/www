import "./ConstituencyParse.scss";
import React, { useState } from 'react';
import { CoordinatedConstituencyParse, SyntaxTreeID } from 'types';
import Tree from './tree/Tree';
import { EditableNodeValues } from './tree/types';
import Button from "./Button";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import { toJS } from "mobx";

type ConstituencyParseProps = {
  constituencyParse: CoordinatedConstituencyParse
  onRemove: () => any
}

const ConstituencyParse: React.FC<ConstituencyParseProps> = ({ constituencyParse, onRemove }) => {
  const { fragmentStore: fs } = useStores();
  const [treeEditCount, setTreeEditCount] = useState<number>(0);
  const incrTreeEditCount = () => setTreeEditCount(prev => prev + 1);

  const handleInterpret = () => fs.dispatchInterpretConstituencyParse(constituencyParse)

  const handleRemove = async () => {
    await fs.dispatchDeleteConstituencyParse(constituencyParse.id);
    onRemove();
  }

  const handleSave = async () => {
    await fs.dispatchUpdateConstituencyParse(constituencyParse.id, {
      parse_string: constituencyParse.coordinated_syntax_tree.parseString()
    });
    setTreeEditCount(0);
  };

  return (
    <div className="constituency-parse">
      <div className="constituency-parse-tree">
        <Tree
          id={constituencyParse.id}
          key={`${constituencyParse.id}-${treeEditCount}`}
          syntaxTree={toJS(constituencyParse.coordinated_syntax_tree)}
          onNodeAdd={(nodeId: SyntaxTreeID) => {
            const node = fs.addConstituencyParseNode(constituencyParse.id, nodeId);
            incrTreeEditCount();
            return node;
          }}
          onNodeEdit={(values: EditableNodeValues) => {
            fs.updateConstituencyParseNode(constituencyParse.id, {
              nodeId: values.id,
              nodeText: values.text
            });
            incrTreeEditCount();
          }}
          onNodeRemove={(nodeId: SyntaxTreeID) => {
            fs.removeConstituencyParseNode(constituencyParse.id, nodeId);
            incrTreeEditCount();
          }}
          onNodeMove={(nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => {
            fs.moveConstituencyParseNode(constituencyParse.id, nodeId, targetParentId);
            incrTreeEditCount();
          }}
        />
      </div>
      <div className="constituency-parse-toolbar">
        <Button onClick={handleInterpret}>interpret</Button>
        <Button onClick={handleRemove}>remove</Button>
        <Button onClick={handleSave}>save</Button>
      </div>
    </div>
  );
};

export default observer(ConstituencyParse);