import "./Example.scss";
import React, { useRef, useState } from 'react';
import { ExampleEditValues } from 'types';
import Button from "./Button";
import { UseFormSetFocus } from "react-hook-form";
import { Example as ExampleT } from "types";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import Menu from "./Menu";
import useLoadWhile from "hooks/useLoadWhile";
import ExampleForm, { ExampleFormContext } from "./ExampleForm";
import classNames from "classnames";
import Interpretation from "./Interpretation";
import TemporaryInterpretation from "./TemporaryInterpretation";
import useTwoClicks from "hooks/useTwoClicks";

type ExampleProps = {
  example: ExampleT
  onSaveSuccess?: () => void
}

export type ExampleRef = {
  setFocus: UseFormSetFocus<ExampleEditValues>
}

const Example: React.FC<ExampleProps> = ({ example }) => {
  const { fragmentStore: fs } = useStores();
  const interpretations = fs.exampleInterpretations(example.id);
  const temporaryInterpretations = fs.exampleTemporaryInterpretations(example.id);
  const [isExpanded, setIsExpanded] = useState<boolean>(false);
  const {isLoading, loadWhile} = useLoadWhile();
  const [isInEdit, setIsInEdit] = useState<boolean>(false);
  const formCtxRef = useRef<ExampleFormContext | null>(null);

  const handleFormSubmit = (values: ExampleEditValues) => loadWhile(
    () => fs.dispatchUpdateExample(example.id, values)
  );

  const handleRemove = () => loadWhile(
    () => fs.dispatchDeleteExample(example.id)
  );

  const handleSave = () => {
    console.log(formCtxRef.current);
    formCtxRef.current?.handleSubmit(handleFormSubmit)()
  };

  const handleNewInterpretation = () => fs.createTemporaryInterpretation(example.id);

  const handleFormClick = useTwoClicks<HTMLFormElement>({
    onDoubleClick: () => {
      setIsExpanded(!isExpanded);
    }
  });

  console.log(temporaryInterpretations);

  return (
    <div className={classNames("example", { "example--in-edit": isInEdit })}>
      <div className="example-header example-row">
        <ExampleForm
          onClick={handleFormClick}
          example={example}
          onSubmit={handleFormSubmit}
          ctxRef={formCtxRef}/>

        <Menu isLoading={isLoading}>
          <Button mode="menu" onClick={handleNewInterpretation}>add an interpretation</Button>
          <Button mode="menu" onClick={handleSave} isLoading={isLoading}>save</Button>
          <Button mode="menu" onClick={handleRemove}>delete</Button>
        </Menu>
      </div>

      {isExpanded && <div className="example-body">
        {temporaryInterpretations.map(i => 
          <TemporaryInterpretation interpretation={i} key={i.temp_id}/>
        )}
        {interpretations.map(i => 
          <Interpretation interpretation={i} key={i.id}/>
        )}
      </div>}
    </div>
  );
};

export default observer(Example);