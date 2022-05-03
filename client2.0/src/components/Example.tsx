import "./Example.scss";
import React, { useState } from 'react';
import { CoordinatedConstituencyParse, ExampleEditValues } from 'types';
import Button from "./Button";
import { useForm, UseFormSetFocus } from "react-hook-form";
import { Example as ExampleT } from "types";
import ConstituencyParse from "./ConstituencyParse";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import Field from "./forms/Field";

type ExampleProps = {
  example: ExampleT
}

export type ExampleRef = {
  setFocus: UseFormSetFocus<ExampleEditValues>
}

const Example: React.FC<ExampleProps> = ({ example }) => {
  const { fragmentStore: fs } = useStores();
  const constituencyParses = fs.exampleConstituencyParses(example.id);
  const [isInEdit, setIsInEdit] = useState<boolean>(false);
  const [isExpanded, setIsExpanded] = useState<boolean>(false);
  const [isUpdateLoading, setIsUpdateLoading] = useState<boolean>(false);
  const {
    register,
    handleSubmit,
  } = useForm<ExampleEditValues>({
    defaultValues: {
      label: example.label,
      content: example.content
    }
  });

  const formHandler = (values: ExampleEditValues) => {
    setIsUpdateLoading(true);
    fs.dispatchUpdateExample(example.id, values);
    setIsUpdateLoading(false);
  }

  const handleExpand = () => setIsExpanded(!isExpanded);

  const handleRemove = () => fs.dispatchDeleteExample(example.id);

  const handleApproximateSyntax = async () => {
    await fs.dispatchApproximateExampleConstituency(example.id);
    setIsExpanded(true);
  };

  const handleConstituencyParseRemove = (constituencyParse: CoordinatedConstituencyParse) => {
    // collapse if this was the last cp
    if (constituencyParses.filter(({ id }) => id !== constituencyParse.id).length === 0) {
      setIsExpanded(false);
    }
  };

  return (
    <div className="example">
      <div className="example-header example-row">
        <form
          className="example-form"
          onClick={() => !isInEdit && setIsInEdit(true)}
          onSubmit={handleSubmit(formHandler)}
        >
          <fieldset>
            <Field initialValue={example.label} {...register('label')}/>
            <Field className="example-form-field-content" initialValue={example.content} {...register('content')}/>
            <Field type="submit"/>
          </fieldset>
        </form>

        <div className="example-text-toolbar">
          <Button onClick={handleApproximateSyntax}>approximate syntax</Button>
          <Button onClick={handleExpand}>
            {isExpanded ? 'collapse' : 'expand'}
          </Button>
          <Button onClick={handleRemove}>remove</Button>
          <Button onClick={handleSubmit(formHandler)} loading={isUpdateLoading}>save</Button>
        </div>
      </div>

      <div className="example-body">
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