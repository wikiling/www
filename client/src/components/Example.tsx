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
import classNames from "classnames";

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
  const [isInEdit, setIsInEdit] = useState<boolean>(false);
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
  };;

  const handleFormClick = () => setIsExpanded(!isExpanded);

  return (
    <div className={classNames("example", { "example--in-edit": isInEdit })}>
      <div className="example-header example-row">
        <ExampleForm focusOnDblClick onClick={handleFormClick} example={example} onSubmit={handleFormSubmit} onInit={handleFormInit}/>

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