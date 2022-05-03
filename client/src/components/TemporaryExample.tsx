import "./Example.scss";
import React, { useEffect, useState } from 'react';
import { ExampleCreateValues, TemporaryExample as TemporaryExampleT } from 'types';
import Button from "./Button";
import { useForm } from "react-hook-form";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import Field from "./forms/Field";

type TemporaryExampleProps = {
  example: TemporaryExampleT
}

const TemporaryExample: React.FC<TemporaryExampleProps> = ({ example }) => {
  const { fragmentStore: fs } = useStores();
  const [isInEdit, setIsInEdit] = useState<boolean>(true);
  const {
    register,
    handleSubmit,
    setFocus,
  } = useForm<ExampleCreateValues>({
    defaultValues: {
      label: example.label,
      content: example.content,
      fragment_id: example.fragment_id
    }
  });

  useEffect(() => {
    if (example.label.length > 0) setFocus('content');
    else setFocus('label');
  }, []);

  const formHandler = (values: ExampleCreateValues) => fs.dispatchCreateExample(example.temp_id, values);

  return (
    <div className="example">
      <div className="example-header example-row">
        <form
          className="example-form"
          onClick={() => !isInEdit && setIsInEdit(true)}
          onSubmit={handleSubmit(formHandler)}
        >
          <fieldset disabled={!isInEdit}>
            <Field initialValue={example.label} {...register('label')}/>
            <Field className="example-form-field-content" initialValue={example.content} {...register('content')}/>
            <Field type="submit"/>
          </fieldset>
        </form>
        <div className="example-text-toolbar">
          <Button onClick={handleSubmit(formHandler)}>save</Button>
        </div>
      </div>
    </div>
  )
};

export default observer(TemporaryExample);