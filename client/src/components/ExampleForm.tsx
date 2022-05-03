import "./ExampleForm.scss";
import React, { useState } from 'react';
import { Example, ExampleBase, TemporaryExample } from 'types';
import { useForm, UseFormReturn } from "react-hook-form";
import Field from "./forms/Field";
import useYupResolver from "hooks/useYupResolver";
import * as yup from "yup"
import Fieldset from "./forms/Fieldset";
import { useEffectOnce } from "react-use";

export type ExampleFormContext = UseFormReturn<ExampleBase>

type ExampleFormProps = {
  example: TemporaryExample | Example
  onSubmit: (values: ExampleBase) => void
  onInit?: (formCtx: ExampleFormContext) => void
}

const ExampleForm: React.FC<ExampleFormProps> = ({ example, onSubmit, onInit }) => {
  const [isInEdit, setIsInEdit] = useState<boolean>(true);
  const formCtx = useForm<ExampleBase>({
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
  });

  const {
    register,
    handleSubmit,
    formState: { errors }
  } = formCtx;

  useEffectOnce(() => onInit && onInit(formCtx));

  return (
    <form
      className="example-form"
      onClick={() => !isInEdit && setIsInEdit(true)}
      onSubmit={handleSubmit(onSubmit)}
    >
      <Fieldset errors={errors}>
        <Field matchTextWidth error={errors.label} className="example-form-field-label" initialValue={example.label} {...register('label')}/>
        <Field error={errors.content} className="example-form-field-content" initialValue={example.content} {...register('content')}/>
      </Fieldset>
    </form>
  )
};

export default ExampleForm;