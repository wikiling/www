import "./InterpretationSemantics.scss";
import useTwoClicks from "hooks/useTwoClicks";
import { toJS } from "mobx";
import { useState } from "react";
import { CoordinatedSemanticTree } from "types"
import LogicalForm from "./LogicalForm";
import SemanticTree from "./trees/SemanticTree";

type InterpretationSemanticsProps = {
  semanticTree: CoordinatedSemanticTree
}
const InterpretationSemantics: React.FC<InterpretationSemanticsProps> = ({ semanticTree }) => {
  const [isExpanded, setIsExpanded] = useState<boolean>(false);
  const handleLogicalFormClick = useTwoClicks<HTMLDivElement>({
    onDoubleClick: () => {
      console.log('?')
      setIsExpanded(!isExpanded);
    }
  });
  return (
    <div className="interpretation-semantics">
      <div className="interpretation-semantics-header">
        <LogicalForm semanticTree={semanticTree} onClick={handleLogicalFormClick}/>
      </div>
      <div className="interpretation-semantics-body">
        {isExpanded &&
          <SemanticTree
            id={semanticTree.data.id}
            tree={toJS(semanticTree)}/>
        }
      </div>
    </div>
  )
};

export default InterpretationSemantics;