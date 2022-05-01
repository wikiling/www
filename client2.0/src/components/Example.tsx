import "./Example.scss";
import React, { useRef, useState } from 'react';
import { CoordinatedConstituencyParse, EditableExampleValues } from 'types';
import Button from "./Button";
import { useForm } from "react-hook-form";
import { Example as ExampleT } from "types";
import ConstituencyParse from "./ConstituencyParse";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";

type ExampleProps = {
  example: ExampleT
}

const Example: React.FC<ExampleProps> = ({ example }) => {
  const { fragmentStore: fs } = useStores();
  const constituencyParses = fs.exampleConstituencyParses(example.id);
  const [isInEdit, setIsInEdit] = useState<boolean>(false);
  const [isExpanded, setIsExpanded] = useState<boolean>(false);
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

  const formHandler = (values: EditableExampleValues) => fs.dispatchUpdateExample(example.id, values);
  const [inputWidthMap, setInputWidthMap] = useState<{[field: string]: number}>({
    label: example.label.length,
    content: example.content.length
  });

  const registerOnInputChange = (field: string) =>
    (e: any) => setInputWidthMap(prev => ({ ...prev, [field]: e.target.value.length }));

  const handleExpand = () => setIsExpanded(!isExpanded);

  const handleApproximateSyntax = async () => {
    await fs.dispatchApproximateExampleConstituency(example.id);
    setIsExpanded(true);
  }

  const handleConstituencyParseRemove = (constituencyParse: CoordinatedConstituencyParse) => {
    // collapse if this was the last cp
    if (constituencyParses.filter(({ id }) => id !== constituencyParse.id).length === 0) {
      setIsExpanded(false);
    }
  }

  const renderInput = (field: keyof EditableExampleValues) => <input
    spellCheck={false}
    style={{ width: `${inputWidthMap[field]}ch` }}
    {...register(field, { onChange: registerOnInputChange(field) })}
  />

  return (
    <div className="example">
      <div className="example-header example-row">
        <form
          className="example-form"
          onClick={() => !isInEdit && setIsInEdit(true)}
          onSubmit={handleSubmit(formHandler)}
          ref={formRef}
        >
          <fieldset disabled={!isInEdit}>
            {renderInput('label')}
            {renderInput('content')}
          </fieldset>
        </form>

        <div className="example-text-toolbar">
          {formState.isDirty && <Button onClick={handleSubmit(formHandler)}>save</Button>}
          <Button onClick={handleApproximateSyntax}>approximate syntax</Button>
          <Button onClick={handleExpand}>
            {isExpanded ? 'collapse' : 'expand'}
          </Button>
        </div>
      </div>

      <div className="example-body example-row">
        {isExpanded && constituencyParses.map((constituencyParse) =>
          <ConstituencyParse
            key={constituencyParse.id}
            constituencyParse={constituencyParse}
            onRemove={() => handleConstituencyParseRemove(constituencyParse)}
          />
        )}
      </div>
    </div>
  )
};

export default observer(Example);