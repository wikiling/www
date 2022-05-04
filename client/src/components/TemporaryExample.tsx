import "./Example.scss";
import React, { useState } from 'react';
import { ExampleCreateValues, TemporaryExample as TemporaryExampleT } from 'types';
import Button from "./Button";
import { UseFormReturn } from "react-hook-form";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import ExampleForm, { ExampleFormContext } from "./ExampleForm";

type TemporaryExampleProps = {
  example: TemporaryExampleT
  onCreateSuccess?: () => void
}

const TemporaryExample: React.FC<TemporaryExampleProps> = ({ example }) => {
  const { fragmentStore: fs } = useStores();
  // this should be done with a ref instead?
  const [formCtx, setFormCtx] = useState<ExampleFormContext | null>(null);

  const handleFormInit = (formCtx: UseFormReturn<ExampleCreateValues>) => {
    setFormCtx(formCtx);

    if (example.label.length > 0) formCtx.setFocus('content');
    else formCtx.setFocus('label');
  };

  const handleFormSubmit = (values: ExampleCreateValues) =>
    fs.dispatchCreateExample(example.temp_id, values);

  const handleSave = () => formCtx?.handleSubmit(handleFormSubmit)();

  return (
    <div className="example">
      <div className="example-header example-row">
        <ExampleForm example={example} onSubmit={handleFormSubmit} onInit={handleFormInit}/>

        <div className="example-text-toolbar">
          <Button onClick={handleSave}>save</Button>
        </div>
      </div>
    </div>
  )
};

export default observer(TemporaryExample);