import { useStores } from "hooks";
import React, { useState } from "react";
import useLoadWhile from "hooks/useLoadWhile";
import { CoordinatedConstituencyParse, Interpretation as InterpretationT, InterpretationEditValues } from "types";
import ConstituencyParse from "./ConstituencyParse";
import "./Interpretation.scss";
import LogicalForm from "./LogicalForm";
import InterpretationForm from "./InterpretationForm";
import Menu from "./Menu";
import Button from "./Button";
import { observer } from "mobx-react-lite";
import Bracketing from "./trees/Bracketing";
import EditableBracketing from "./trees/EditableBracketing";
import useTwoClicks from "hooks/useTwoClicks";
import SemanticTree from "./trees/SemanticTree";
import { toJS } from "mobx";
import InterpretationSyntax from "./InterpretationSyntax";
import InterpretationSemantics from "./InterpretationSemantics";

type InterpretationProps = {
  interpretation: InterpretationT
}

const Interpretation: React.FC<InterpretationProps> = ({ interpretation }) => {
  const { fragmentStore: fs } = useStores();
  const { id, constituency_parse: cp, example_id, content, paraphrase } = interpretation;
  const ccp = cp ? fs.constituencyParseMap[cp.id] : undefined;
  const semanticTree = fs.semanticTreeMap[interpretation.id];
  const [isExpanded, setIsExpanded] = useState<boolean>(false);
  const { isLoading, loadWhile } = useLoadWhile();

  const handleFormSubmit = (values: InterpretationEditValues) => loadWhile(
    () => fs.dispatchUpdateInterpretation(id, values)
  );

  const handleInterpret = () => ccp && loadWhile(
    () => fs.dispatchInterpretConstituencyParse(ccp)
  );

  const handleApproximateSyntax = () => loadWhile(
    async () => {
      await fs.dispatchApproximateExampleConstituency(example_id);
      setIsExpanded(true);
    }
  );

  const handleExpand = () => setIsExpanded(!isExpanded);

  return (
    <div className="interpretation">
      <div className="interpretation-header interpretation-row">
        <InterpretationForm interpretation={interpretation} onSubmit={handleFormSubmit}/>

        <Button mode="trans" onClick={handleInterpret}>interpret</Button>
      </div>

      <div className="interpretation-body">
        {ccp && <InterpretationSyntax constituencyParse={ccp}/>}
        {semanticTree && <InterpretationSemantics semanticTree={semanticTree}/>}
      </div>
    </div>
  );
};


export default observer(Interpretation);