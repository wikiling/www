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
  onConstituencyParseNodeAdd: (nodeId: SyntaxTreeID) => void
  onConstituencyParseNodeEdit: (values: EditableNodeValues) => void
  onConstituencyParseNodeRemove: (nodeId: SyntaxTreeID) => void
  onConstituencyParseNodeMove: (nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => void
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

  const incrTreeEditCount = (constituencyParseId: ID) => setTreeEditCountMap(
    prev => Object.assign(prev, { [constituencyParseId]: (prev[constituencyParseId] ?? 0) + 1 })
  );

  return (
    <div className="example">
      <div className="example-header">
        <div>({example.id})</div>
      </div>

      {constituencyParses.map((constituencyParse) => (
        <div>
          <Tree
            id={example.id}
            key={`${example.id}-${treeEditCountMap[example.id]}`}
            // FIXME: mobx shouldn't be leaked here
            syntaxTree={toJS(constituencyParse.coordinated_syntax_tree)}
            onNodeAdd={(nodeId: SyntaxTreeID) => {
              onConstituencyParseNodeAdd(nodeId);
              incrTreeEditCount(constituencyParse.id);
            }}
            onNodeEdit={(values: EditableNodeValues) => {
              onConstituencyParseNodeEdit(values);
              incrTreeEditCount(constituencyParse.id);
            }}
            onNodeRemove={(nodeId: SyntaxTreeID) => {
              onConstituencyParseNodeRemove(nodeId);
              incrTreeEditCount(constituencyParse.id);
            }}
            onNodeMove={(nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => {
              onConstituencyParseNodeMove(nodeId, targetParentId);
              incrTreeEditCount(constituencyParse.id);
            }}
          />
          <Button
            className="interpret-button"
            onClick={() => onConstituencyParseInterpret(constituencyParse)}>
            interpret
          </Button>
        </div>
      ))}
    </div>
  )
};

export default Example