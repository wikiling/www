import "./ExampleForm.scss";
import React, { useRef, useState } from 'react';
import { Example, ExampleBase, TemporaryExample } from 'types';
import { useForm, UseFormReturn } from "react-hook-form";
import Field from "./forms/Field";
import useYupResolver from "hooks/useYupResolver";
import * as yup from "yup"
import Fieldset from "./forms/Fieldset";
import { useEffectOnce } from "react-use";
import useTwoClicks from "hooks/useTwoClicks";

export type ExampleFormContext = UseFormReturn<ExampleBase>

const noop = () => {};

type ExampleFormProps = {
  example: TemporaryExample | Example
  onSubmit: (values: ExampleBase) => void
  onInit?: (formCtx: ExampleFormContext) => void
  focusOnDblClick?: boolean
  onClick?: React.MouseEventHandler<HTMLFormElement>
}

const ExampleForm: React.FC<ExampleFormProps> = ({ example, onSubmit, onInit, focusOnDblClick = false, onClick }) => {
  const formRef = useRef<HTMLFormElement>(null);
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
    formState: { errors },
    setFocus
  } = formCtx;

  const handleFormClick = useTwoClicks<HTMLFormElement>({
    onSingleClick: (e) => {
      console.log('single click')
      onClick && onClick(e);
      focusOnDblClick && e.preventDefault();
    },
    onDoubleClick: () => {
      console.log('double click')
      focusOnDblClick && setFocus('content')
    }
  });

  useEffectOnce(() => onInit && onInit(formCtx));

  return (
    <form
      ref={formRef}
      className="example-form"
      onClick={handleFormClick}
      onSubmit={handleSubmit(onSubmit)}
    >
      <Fieldset errors={errors}>
        <Field matchTextWidth error={errors.label} className="example-form-field-label" initialValue={example.label} {...register('label')}/>
        <Field noFocus={focusOnDblClick} error={errors.content} className="example-form-field-content" initialValue={example.content} {...register('content')}/>
      </Fieldset>
    </form>
  )
};

export default ExampleForm;