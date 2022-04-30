import "./Example.scss";
import React, { useState } from 'react';
import { ConstituencyParse, CoordinatedConstituencyParse, EditableConstituencyParseValues, Example as ExampleModel, ID, SyntaxTreeID } from 'types';
import Tree from './tree/Tree';
import { toJS } from 'mobx';
import { EditableNodeValues } from './tree/types';
import Button from "./Button";

type ExampleProps = {
  example: ExampleModel
  constituencyParses: CoordinatedConstituencyParse[]
  onConstituencyParseNodeAdd: (constituencyParseId: ID, nodeId: SyntaxTreeID) => void
  onConstituencyParseNodeEdit: (constituencyParseId: ID, values: EditableConstituencyParseValues) => void
  onConstituencyParseNodeRemove: (constituencyParseId: ID, nodeId: SyntaxTreeID) => void
  onConstituencyParseNodeMove: (constituencyParseId: ID, nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => void
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

  console.log(treeEditCountMap)

  return (
    <div className="example">
      <div className="example-header example-row">
        <div>({example.label}) {example.content}</div>
        <Button onClick={() => handleExpandButtonClick(example)} trans>
          {!treeExpansionMap[example.id] ? 'expand' : 'collapse'}
        </Button>
      </div>

      <div className="example-body example-row">
        {constituencyParses.map((constituencyParse) => !!treeExpansionMap[example.id] && (
          <div className="example-constituency-parse" key={`${example.id}-${treeEditCountMap[example.id]}`}>
            <div className="tree-wrapper">
              <Tree
                id={example.id}
                syntaxTree={constituencyParse.coordinated_syntax_tree}
                onNodeAdd={(nodeId: SyntaxTreeID) => {
                  onConstituencyParseNodeAdd(constituencyParse.id, nodeId);
                  incrTreeEditCount(constituencyParse.id);
                }}
                onNodeEdit={(values: EditableNodeValues) => {
                  onConstituencyParseNodeEdit(constituencyParse.id, {
                    nodeId: values.id,
                    nodeText: values.text
                  });
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
            </div>
            <Button trans onClick={() => onConstituencyParseInterpret(constituencyParse)}>
              interpret
            </Button>
          </div>
        ))}
      </div>
    </div>
  )
};

export default Example