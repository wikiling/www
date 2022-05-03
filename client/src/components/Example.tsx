import "./Example.scss";
import React, { useState } from 'react';
import { CoordinatedConstituencyParse, ExampleEditValues } from 'types';
import Button from "./Button";
import { UseFormSetFocus } from "react-hook-form";
import { Example as ExampleT } from "types";
import ConstituencyParse from "./ConstituencyParse";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import Menu from "./Menu";
import useLoadWhile from "hooks/useLoadWhile";
import ExampleForm, { ExampleFormContext } from "./ExampleForm";

type ExampleProps = {
  example: ExampleT
  onSaveSuccess?: () => void
}

export type ExampleRef = {
  setFocus: UseFormSetFocus<ExampleEditValues>
}

const Example: React.FC<ExampleProps> = ({ example }) => {
  const { fragmentStore: fs } = useStores();
  const constituencyParses = fs.exampleConstituencyParses(example.id);
  const [isExpanded, setIsExpanded] = useState<boolean>(false);
  const {isLoading, loadWhile} = useLoadWhile();
  // this should be done with a ref instead?
  const [formCtx, setFormCtx] = useState<ExampleFormContext | null>(null);

  const handleFormInit = (formCtx: ExampleFormContext) => setFormCtx(formCtx)
  const handleExpand = () => setIsExpanded(!isExpanded);
  const handleFormSubmit = (values: ExampleEditValues) => loadWhile(
    () => fs.dispatchUpdateExample(example.id, values)
  );

  const handleRemove = () => loadWhile(
    () => fs.dispatchDeleteExample(example.id)
  );

  const handleSave = () => loadWhile(
    async () => formCtx?.handleSubmit(handleFormSubmit)()
  )

  const handleApproximateSyntax = () => loadWhile(
    async () => {
      await fs.dispatchApproximateExampleConstituency(example.id);
      setIsExpanded(true);
    }
  );

  const handleConstituencyParseRemove = (constituencyParse: CoordinatedConstituencyParse) => {
    // collapse if this was the last cp
    if (constituencyParses.filter(({ id }) => id !== constituencyParse.id).length === 0) {
      setIsExpanded(false);
    }
  };

  return (
    <div className="example">
      <div className="example-header example-row">
        <ExampleForm example={example} onSubmit={handleFormSubmit} onInit={handleFormInit}/>

        <Menu isLoading={isLoading}>
          <Button mode="menu" onClick={handleApproximateSyntax}>approximate syntax</Button>
          <Button mode="menu" onClick={handleExpand}>{isExpanded ? 'collapse' : 'expand'}</Button>
          <Button mode="menu" onClick={handleRemove}>remove</Button>
          <Button mode="menu" onClick={handleSave} isLoading={isLoading}>save</Button>
        </Menu>
      </div>

      <div className="example-body">
        {isExpanded && constituencyParses.map((constituencyParse) =>
          <ConstituencyParse
            key={constituencyParse.id}
            constituencyParse={constituencyParse}
            onRemove={() => handleConstituencyParseRemove(constituencyParse)}
          />
        )}
      </div>
    </div>
  )
};

export default observer(Example);