import "./LogicalForm.scss";
import React, { useState } from 'react';
import { CoordinatedConstituencyParse, CoordinatedSemanticTree, TreeID } from 'types';
import SyntaxTree from './trees/SyntaxTree';
import Button from "./Button";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import { toJS } from "mobx";
import Menu from "./Menu";
import useLoadWhile from "hooks/useLoadWhile";
import SemanticTree from "./trees/SemanticTree";
import useConstituencyParseOps from "hooks/useConstituencyParseOps";

type LogicalFormProps = {
  constituencyParse: CoordinatedConstituencyParse
  semanticTree: CoordinatedSemanticTree
}

const LogicalForm: React.FC<LogicalFormProps> = ({ semanticTree, constituencyParse }) => {
  const {
    handleInterpret: onInterpret,
    handleRemove,
    handleSave,

    treeEditCount,
    incrTreeEditCount,
    isLoading,
  } = useConstituencyParseOps(constituencyParse);

  const handleInterpret = async () => {
    await onInterpret();
    incrTreeEditCount();
  };

  return (
    <div className="logical-form">
      <div className="logical-form-tree">
        <SemanticTree
          id={constituencyParse.id}
          key={`${constituencyParse.id}-${treeEditCount}`}
          tree={toJS(semanticTree)}
        />
      </div>

      <Menu isLoading={isLoading}>
        <Button mode="menu" onClick={handleInterpret}>interpret</Button>
        <Button mode="menu" onClick={() => handleRemove()}>remove</Button>
        <Button mode="menu" onClick={handleSave}>save</Button>
      </Menu>
    </div>
  );
};

export default observer(LogicalForm);