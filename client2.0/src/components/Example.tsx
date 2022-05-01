import "./Example.scss";
import React, { useRef, useState } from 'react';
import { ConstituencyParse, CoordinatedConstituencyParse, EditableConstituencyParseValues, EditableExampleValues, Example as ExampleModel, ID, SyntaxTreeID } from 'types';
import Tree from './tree/Tree';
import { EditableNodeValues } from './tree/types';
import Button from "./Button";
import { useForm } from "react-hook-form";

type ExampleProps = {
  example: ExampleModel
  constituencyParses: CoordinatedConstituencyParse[]
  onConstituencyParseInterpret: (constituencyParse: ConstituencyParse) => void
  onConstituencyParseApproximate: (exampleId: ID) => Promise<ConstituencyParse>
  onConstituencyParseNodeAdd: (constituencyParseId: ID, nodeId: SyntaxTreeID) => void
  onConstituencyParseNodeEdit: (constituencyParseId: ID, values: EditableConstituencyParseValues) => void
  onConstituencyParseNodeRemove: (constituencyParseId: ID, nodeId: SyntaxTreeID) => void
  onConstituencyParseNodeMove: (constituencyParseId: ID, nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => void
  onConstituencyParseRemove: (constituencyParseId: ID) => Promise<void>
  onSave: (example: EditableExampleValues) => void
}

const Example: React.FC<ExampleProps> = ({
  example,
  constituencyParses,
  onConstituencyParseNodeAdd,
  onConstituencyParseNodeEdit,
  onConstituencyParseNodeRemove,
  onConstituencyParseNodeMove,
  onConstituencyParseInterpret,
  onConstituencyParseApproximate,
  onConstituencyParseRemove,
  onSave
}) => {
  const [isInEdit, setIsInEdit] = useState<boolean>(false);
  const [treeEditCountMap, setTreeEditCountMap] = useState<{[key: ID]: number}>({});
  const [treeExpansionMap, setTreeExpansionMap] = useState<{[key: ID]: boolean}>({});
  const formRef = useRef<HTMLFormElement>(null);
  const {
    register,
    handleSubmit,
    formState
  } = useForm<EditableExampleValues>({
    defaultValues: {
      label: example.label,
      content: example.content
    }
  });
  const [inputWidthMap, setInputWidthMap] = useState<{[field: string]: number}>({
    label: example.label.length,
    content: example.content.length
  });

  const registerOnInputChange = (field: string) =>
    (e: any) => setInputWidthMap(prev => ({ ...prev, [field]: e.target.value.length }));

  const incrTreeEditCount = (constituencyParseId: ID) => setTreeEditCountMap(
    prev => ({ ...prev, [constituencyParseId]: (prev[constituencyParseId] ?? 0) + 1 })
  );

  const handleExpandButtonClick = (example: ExampleModel) => setTreeExpansionMap(
    prev => ({ ...prev, [example.id]: !prev[example.id] })
  );

  const handleApproximateSyntax = async () => {
    await onConstituencyParseApproximate(example.id);
    treeExpansionMap[example.id] = true;
  }

  const renderInput = (field: keyof EditableExampleValues) => <input
    spellCheck={false}
    style={{ width: `${inputWidthMap[field]}ch` }}
    {...register(field, { onChange: registerOnInputChange(field)})}
  />

  return (
    <div className="example">
      <div className="example-header example-row">
        <form className="example-form" onClick={() => !isInEdit && setIsInEdit(true)} onSubmit={handleSubmit(onSave)} ref={formRef}>
          <fieldset disabled={!isInEdit}>
            {renderInput('label')}
            {renderInput('content')}
          </fieldset>
        </form>

        <div className="example-text-toolbar">
          {formState.isDirty && <Button onClick={handleSubmit(onSave)}>save</Button>}
          <Button onClick={handleApproximateSyntax}>approximate syntax</Button>
          <Button onClick={() => handleExpandButtonClick(example)}>
            {!treeExpansionMap[example.id] ? 'expand' : 'collapse'}
          </Button>
        </div>
      </div>

      <div className="example-body example-row">
        {constituencyParses.map((constituencyParse) => !!treeExpansionMap[example.id] && (
          <div key={`${constituencyParse.id}-${treeEditCountMap[constituencyParse.id]}`}>
          <div>{constituencyParse.coordinated_syntax_tree.parseString()}</div>
          <div className="example-constituency-parse" >
            
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
            <div className="example-syntax-toolbar">
              <Button trans onClick={() => onConstituencyParseInterpret(constituencyParse)}>
                interpret
              </Button>
              <Button trans onClick={() => onConstituencyParseRemove(constituencyParse.id)}>
                remove
              </Button>
            </div>
          </div>
          </div>
        ))}
      </div>
    </div>
  )
};

export default Example