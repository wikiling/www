import "./InterpretationForm.scss";
import React, { MutableRefObject, useRef, useState } from 'react';
import { Interpretation, InterpretationBase, TemporaryInterpretation } from 'types';
import { useForm, UseFormReturn } from "react-hook-form";
import Field from "./forms/Field";
import useYupResolver from "hooks/useYupResolver";
import * as yup from "yup"
import Fieldset from "./forms/Fieldset";
import Form from "./forms/Form";
import Button from "./Button";
import Check from "./icons/Check";
import Cross from "./icons/Cross";

export type InterpretationFormContext = UseFormReturn<InterpretationBase>

type InterpretationFormProps = {
  interpretation: TemporaryInterpretation | Interpretation
  onSubmit: (values: InterpretationBase) => void
  onDelete?: () => void
  ctxRef?: MutableRefObject<InterpretationFormContext | null>
  toolbar?: (ctx: InterpretationFormContext) => React.ReactNode;
  // onClick?: React.MouseEventHandler<HTMLFormElement>
}

const InterpretationForm: React.FC<InterpretationFormProps> = ({ interpretation, onSubmit, onDelete, toolbar }) => {
  return (
    <Form<InterpretationBase>
      className="interpretation-form"
      onSubmit={onSubmit}
      options={{
        defaultValues: {
          content: interpretation.content,
          paraphrase: interpretation.paraphrase,
          example_id: interpretation.example_id
        },
        resolver: useYupResolver({
          content: yup.string().min(1).required("enter an interpretation"),
          paraphrase:  yup.string().optional(),
        }),
        mode: 'onSubmit',
        reValidateMode: 'onChange'
      }}
    >
      {formCtx => <>
        <Fieldset>
          <Field
            name="content"
            className="interpretation-form-field-content"
            initialValue={interpretation.content}
            placeholder="possible interpretation"/>

          <Field
            name="paraphrase"
            className="interpretation-form-field-paraphrase"
            initialValue={interpretation.paraphrase}
            placeholder="optional paraphrase"/>
        </Fieldset>

        {toolbar && toolbar(formCtx)}
      </>}

    </Form>
  )
};

export default InterpretationForm;