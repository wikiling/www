import "./ExampleForm.scss";
import React, { MutableRefObject } from 'react';
import { Example, ExampleBase, TemporaryExample } from 'types';
import { UseFormReturn } from "react-hook-form";
import Field from "./forms/Field";
import useYupResolver from "hooks/useYupResolver";
import * as yup from "yup"
import Fieldset from "./forms/Fieldset";
import Form, { FormDblClickHandler } from "./forms/Form";

export type ExampleFormContext = UseFormReturn<ExampleBase>

type ExampleFormProps = {
  example: TemporaryExample | Example
  onSubmit: (values: ExampleBase) => void
  focusOnDblClick?: boolean
  onClick?: React.MouseEventHandler<HTMLFormElement>
  onDblClick?: FormDblClickHandler<ExampleBase>
  ctxCb?: (ctx: ExampleFormContext) => void
}

const ExampleForm: React.FC<ExampleFormProps> = ({ example, onSubmit, onClick, onDblClick, ctxCb }) => {
  return (
    <Form<ExampleBase>
      className="example-form"
      options={{
        defaultValues: {
          label: example.label,
          content: example.content,
          fragment_id: example.fragment_id
        },
        resolver: useYupResolver({
          label: yup.string().min(1).required("a label is required"),
          content: yup.string().min(1).required("enter some text")
        }),
        mode: 'onSubmit',
        reValidateMode: 'onChange'
      }}
      onSubmit={onSubmit}
      onClick={onClick}
      onDblClick={onDblClick}
    >
      {(ctx) => {
        ctxCb && ctxCb(ctx);
        return <Fieldset>
          <Field
            name="label"
            matchTextWidth
            className="example-form-field-label"
            initialValue={example.label}/>

          <Field
            autoFocus
            name="content"
            className="example-form-field-content"
            initialValue={example.content}/>
        </Fieldset>
      }}
    </Form>
  );
};

export default ExampleForm;