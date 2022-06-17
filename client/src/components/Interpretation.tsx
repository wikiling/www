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

type InterpretationProps = {
  interpretation: InterpretationT
}

const Interpretation: React.FC<InterpretationProps> = ({ interpretation }) => {
  const { fragmentStore: fs } = useStores();
  const { id, constituency_parse: cp, example_id, content, paraphrase } = interpretation;
  const ccp = cp ? fs.constituencyParseMap[cp.id] : undefined;
  const [isExpanded, setIsExpanded] = useState<boolean>(false);
  const { isLoading, loadWhile } = useLoadWhile();

  const handleFormSubmit = (values: InterpretationEditValues) => loadWhile(
    () => fs.dispatchUpdateInterpretation(id, values)
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

        <Menu className="interpretation-menu" isLoading={isLoading}>
          <Button mode="menu" onClick={handleExpand}>{isExpanded ? 'collapse' : 'expand'}</Button>
        </Menu>
      </div>
      <div className="interpretation-body">
        {ccp && <>
          <Bracketing tree={ccp.coordinated_syntax_tree}/>

          {isExpanded && (
            fs.semanticTreeMap[ccp.id]
              ? <LogicalForm
                  key={ccp.id}
                  semanticTree={fs.semanticTreeMap[ccp.id]}
                  constituencyParse={ccp}
                />
              : <ConstituencyParse
                  key={ccp.id}
                  constituencyParse={ccp}
                />
          )}
        </>}
      </div>
    </div>
  );
};

export default observer(Interpretation);