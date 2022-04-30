import { EditableNodeValues } from "components/tree/types";
import React from "react";
import { SemanticTree, Example, SyntaxTree, CoordinatedConstituencyParse, ID, SyntaxTreeID, ConstituencyParse } from "types";

export interface ExampleContextInterface {
  example: Example
  constituencyParses: CoordinatedConstituencyParse[]
  onConstituencyParseNodeAdd: (ConstituencyParseId: ID, nodeId: SyntaxTreeID) => void
  onConstituencyParseNodeEdit: (ConstituencyParseId: ID, values: EditableNodeValues) => void
  onConstituencyParseNodeRemove: (ConstituencyParseId: ID, nodeId: SyntaxTreeID) => void
  onConstituencyParseNodeMove: (ConstituencyParseId: ID, nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => void
  onConstituencyParseInterpret: (constituencyParse: ConstituencyParse) => void
}

export const ExampleContext = React.createContext<ExampleContextInterface | null>(null);

export const useExampleContext = () => React.useContext(ExampleContext);