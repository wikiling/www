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
  semanticTree: CoordinatedSemanticTree
  onClick: React.MouseEventHandler<HTMLDivElement>
}

const LogicalForm: React.FC<LogicalFormProps> = ({ semanticTree, onClick }) => {

  return (
    <div className="logical-form" onClick={onClick}>
      {semanticTree.data.value}
    </div>
  );
};

export default observer(LogicalForm);