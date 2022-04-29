import "./Example.scss";
import React, { useState } from 'react';
import { ConstituencyParse, CoordinatedConstituencyParse, Example as ExampleModel, ID, SyntaxTreeID } from 'types';
import Tree from './tree/Tree';
import { toJS } from 'mobx';
import { EditableNodeValues } from './tree/types';
import Button from "./Button";

type ExampleProps = {
  example: ExampleModel
  constituencyParses: CoordinatedConstituencyParse[]
  onConstituencyParseNodeAdd: (ConstituencyParseId: ID, nodeId: SyntaxTreeID) => void
  onConstituencyParseNodeEdit: (ConstituencyParseId: ID, values: EditableNodeValues) => void
  onConstituencyParseNodeRemove: (ConstituencyParseId: ID, nodeId: SyntaxTreeID) => void
  onConstituencyParseNodeMove: (ConstituencyParseId: ID, nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => void
  onConstituencyParseInterpret: (constituencyParse: ConstituencyParse) => void
}

const Example: React.FC<ExampleProps> = ({
  example,
  constituencyParses,
  onConstituencyParseNodeAdd,
  onConstituencyParseNodeEdit,
  onConstituencyParseNodeRemove,
  onConstituencyParseNodeMove,
  onConstituencyParseInterpret
}) => {
  const [treeEditCountMap, setTreeEditCountMap] = useState<{[key: ID]: number}>({});
  const [treeExpansionMap, setTreeExpansionMap] = useState<{[key: ID]: boolean}>({});

  const incrTreeEditCount = (constituencyParseId: ID) => setTreeEditCountMap(
    prev => ({ ...prev, [constituencyParseId]: (prev[constituencyParseId] ?? 0) + 1 })
  );

  const handleExpandButtonClick = (example: ExampleModel) => setTreeExpansionMap(
    prev => ({ ...prev, [example.id]: !prev[example.id] })
  );

  return (
    <div className="example">
      <div className="example-header">
        <div>({example.label}) {example.content}</div>
        <Button onClick={() => handleExpandButtonClick(example)} trans>
          {!treeExpansionMap[example.id] ? 'expand' : 'collapse'}
        </Button>
      </div>

      {constituencyParses.map((constituencyParse) => !!treeExpansionMap[example.id] && (
        <div key={`${example.id}-${treeEditCountMap[example.id]}`}>
          <Tree
            id={example.id}
            // FIXME: mobx shouldn't be leaked here
            syntaxTree={toJS(constituencyParse.coordinated_syntax_tree)}
            onNodeAdd={(nodeId: SyntaxTreeID) => {
              onConstituencyParseNodeAdd(constituencyParse.id, nodeId);
              incrTreeEditCount(constituencyParse.id);
            }}
            onNodeEdit={(values: EditableNodeValues) => {
              onConstituencyParseNodeEdit(constituencyParse.id, values);
              incrTreeEditCount(constituencyParse.id);
            }}
            onNodeRemove={(nodeId: SyntaxTreeID) => {
              onConstituencyParseNodeRemove(constituencyParse.id, nodeId);
              incrTreeEditCount(constituencyParse.id);
            }}
            onNodeMove={(nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => {
              onConstituencyParseNodeMove(constituencyParse.id, nodeId, targetParentId);
              incrTreeEditCount(constituencyParse.id);
            }}
          />
          <Button trans onClick={() => onConstituencyParseInterpret(constituencyParse)}>
            interpret
          </Button>
        </div>
      ))}
    </div>
  )
};

export default Example