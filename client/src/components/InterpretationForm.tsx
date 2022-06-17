import "./InterpretationForm.scss";
import React, { useRef, useState } from 'react';
import { Interpretation, InterpretationBase, TemporaryInterpretation } from 'types';
import { useForm, UseFormReturn } from "react-hook-form";
import Field from "./forms/Field";
import useYupResolver from "hooks/useYupResolver";
import * as yup from "yup"
import Fieldset from "./forms/Fieldset";
import { useEffectOnce } from "react-use";
import useTwoClicks from "hooks/useTwoClicks";

export type InterpretationFormContext = UseFormReturn<InterpretationBase>

type InterpretationFormProps = {
  interpretation: TemporaryInterpretation | Interpretation
  onSubmit: (values: InterpretationBase) => void
  onInit?: (formCtx: InterpretationFormContext) => void
  focusOnDblClick?: boolean
  onClick?: React.MouseEventHandler<HTMLFormElement>
}

const InterpretationForm: React.FC<InterpretationFormProps> = ({ interpretation, onSubmit, onInit, focusOnDblClick = false, onClick }) => {
  const formRef = useRef<HTMLFormElement>(null);
  const formCtx = useForm<InterpretationBase>({
    defaultValues: {
      content: interpretation.content,
      paraphrase: interpretation.paraphrase
    },
    resolver: useYupResolver({
      content: yup.string().min(1).required("enter an interpretation"),
      paraphrase:  yup.string().optional(),
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
      className="interpretation-form"
      onClick={handleFormClick}
      onSubmit={handleSubmit(onSubmit)}
    >
      <Fieldset errors={errors}>
        <Field
          noFocus={focusOnDblClick}
          error={errors.content}
          className="interpretation-form-field-content"
          initialValue={interpretation.content}
          placeholder="possible interpretation"
          {...register('content')}/>

        <Field
          noFocus={focusOnDblClick}
          error={errors.paraphrase}
          className="interpretation-form-field-paraphrase"
          initialValue={interpretation.paraphrase}
          placeholder="optional paraphrase"
          {...register('paraphrase')}/>
      </Fieldset>
    </form>
  )
};

export default InterpretationForm;