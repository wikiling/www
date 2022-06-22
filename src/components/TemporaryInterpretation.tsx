import "./Interpretation.scss";
import React, { useRef } from 'react';
import { InterpretationCreateValues, TemporaryInterpretation as TemporaryInterpretationT } from 'types';
import Button from "./Button";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import InterpretationForm, { InterpretationFormContext } from "./InterpretationForm";
import Cross from "./icons/Cross";
import Check from "./icons/Check";
import Plus from "./icons/PlusIcon";

type TemporaryInterpretationProps = {
  interpretation: TemporaryInterpretationT
}

const TemporaryInterpretation: React.FC<TemporaryInterpretationProps> = ({ interpretation }) => {
  const { fragmentStore: fs } = useStores();

  const handleFormSubmit = (values: InterpretationCreateValues) =>
    fs.dispatchCreateInterpretation(interpretation.temp_id, values);

  const handleDelete = () => fs.removeTemporaryInterpretation(interpretation.temp_id);

  return (
    <div className="interpretation interpretation--temporary">
      <div className="interpretation-header interpretation-row">
        <InterpretationForm
          interpretation={interpretation}
          onSubmit={handleFormSubmit}
          onDelete={handleDelete}
          toolbar={
            ({ handleSubmit }) => (
              <div className="interpretation-toolbar">
                <Button mode="clear" onClick={handleSubmit(handleFormSubmit)}><Plus height={13} fill="green"/></Button>
                <Button mode="clear" onClick={handleDelete}><Cross height={12}/></Button>
              </div>
            )
          }
          />
      </div>
    </div>
  )
};

export default observer(TemporaryInterpretation);