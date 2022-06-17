import "./Interpretation.scss";
import React, { useState } from 'react';
import { InterpretationCreateValues, TemporaryInterpretation as TemporaryInterpretationT } from 'types';
import Button from "./Button";
import { UseFormReturn } from "react-hook-form";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import InterpretationForm, { InterpretationFormContext } from "./InterpretationForm";
import Cross from "./icons/Cross";
import Check from "./icons/Check";

type TemporaryInterpretationProps = {
  interpretation: TemporaryInterpretationT
  onCreateSuccess?: () => void
}

const TemporaryInterpretation: React.FC<TemporaryInterpretationProps> = ({ interpretation }) => {
  const { fragmentStore: fs } = useStores();
  // this should be done with a ref instead?
  const [formCtx, setFormCtx] = useState<InterpretationFormContext | null>(null);

  const handleFormInit = (formCtx: UseFormReturn<InterpretationCreateValues>) => {
    setFormCtx(formCtx);
  };

  const handleFormSubmit = (values: InterpretationCreateValues) =>
    fs.dispatchCreateInterpretation(interpretation.temp_id, values);

  const handleSave = () => formCtx?.handleSubmit(handleFormSubmit)();
  const handleRemove = () => fs.removeTemporaryInterpretation(interpretation.temp_id);

  return (
    <div className="interpretation interpretation--temporary">
      <div className="interpretation-header interpretation-row">
        <InterpretationForm
          interpretation={interpretation}
          onSubmit={handleFormSubmit}
          onInit={handleFormInit}/>

        <div className="interpretation-toolbar">
          <Button onClick={handleSave}><Check/></Button>
          <Button onClick={handleRemove}><Cross/></Button>
        </div>
      </div>
    </div>
  )
};

export default observer(TemporaryInterpretation);