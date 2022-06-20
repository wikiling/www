import "./InterpretationSyntax.scss"
import useTwoClicks from "hooks/useTwoClicks";
import { toJS } from "mobx";
import { useState } from "react";
import { CoordinatedConstituencyParse, CoordinatedSyntaxTree } from "types"
import ConstituencyParse from "./ConstituencyParse";
import LogicalForm from "./LogicalForm";
import EditableBracketing from "./trees/EditableBracketing";
import SyntaxTree from "./trees/SyntaxTree";

type InterpretationSyntaxProps = {
  constituencyParse: CoordinatedConstituencyParse
}
const InterpretationSyntax: React.FC<InterpretationSyntaxProps> = ({ constituencyParse }) => {
  const [isExpanded, setIsExpanded] = useState<boolean>(false);

  const handleBracketingClick = useTwoClicks<HTMLDivElement>({
    onDoubleClick: () => {
      console.log('?')
      setIsExpanded(!isExpanded);
    }
  });

  return (
    <div className="interpretation-syntax">
      <div className="interpretation-syntax-header">
        <EditableBracketing
          onClick={handleBracketingClick}
          tree={constituencyParse.coordinated_syntax_tree}/>
      </div>
      <div className="interpretation-syntax-body">
        {isExpanded &&
          <ConstituencyParse
            key={constituencyParse.id}
            constituencyParse={constituencyParse}/>
        }
      </div>
    </div>
  )
};

export default InterpretationSyntax;