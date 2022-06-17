import "./Example.scss";
import React, { useRef } from 'react';
import { ExampleCreateValues, TemporaryExample as TemporaryExampleT } from 'types';
import Button from "./Button";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import ExampleForm, { ExampleFormContext } from "./ExampleForm";

type TemporaryExampleProps = {
  example: TemporaryExampleT
}

const TemporaryExample: React.FC<TemporaryExampleProps> = ({ example }) => {
  const { fragmentStore: fs } = useStores();
  const formCtxRef = useRef<ExampleFormContext | null>(null);
  const formCtx = formCtxRef.current;

  // bit of a hack
  let prevFormCtx;
  if (!prevFormCtx && formCtx) {
    if (example.label.length > 0) formCtx.setFocus('content');
    else formCtx.setFocus('label');
    prevFormCtx = formCtx;
  }

  const handleFormSubmit = (values: ExampleCreateValues) =>
    fs.dispatchCreateExample(example.temp_id, values);

  const handleSave = () => {
    console.log(formCtx);
    formCtx?.handleSubmit(handleFormSubmit)();
  };

  return (
    <div className="example">
      <div className="example-header example-row">
        <ExampleForm example={example} onSubmit={handleFormSubmit} ctxRef={formCtxRef}/>

        <div className="example-text-toolbar">
          <Button onClick={handleSave}>save</Button>
        </div>
      </div>
    </div>
  )
};

export default observer(TemporaryExample);