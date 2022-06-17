import "./ExampleForm.scss";
import React, { MutableRefObject, useRef, useState } from 'react';
import { Example, ExampleBase, TemporaryExample } from 'types';
import { useForm, UseFormReturn } from "react-hook-form";
import Field from "./forms/Field";
import useYupResolver from "hooks/useYupResolver";
import * as yup from "yup"
import Fieldset from "./forms/Fieldset";
import { useEffectOnce } from "react-use";
import useTwoClicks from "hooks/useTwoClicks";
import Form from "./forms/Form";

export type ExampleFormContext = UseFormReturn<ExampleBase>

type ExampleFormProps = {
  example: TemporaryExample | Example
  onSubmit: (values: ExampleBase) => void
  ctxRef?: MutableRefObject<ExampleFormContext | null>
  focusOnDblClick?: boolean
  onClick?: React.MouseEventHandler<HTMLFormElement>
}

const ExampleForm: React.FC<ExampleFormProps> = ({ example, onSubmit, onClick }) => {
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
    >
      {() => (
        <Fieldset>
          <Field
            name="label"
            matchTextWidth
            className="example-form-field-label"
            initialValue={example.label}/>

          <Field
            name="content"
            className="example-form-field-content"
            initialValue={example.content}/>
        </Fieldset>
      )}
    </Form>
  );
};

export default ExampleForm;