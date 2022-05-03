import React from "react";
import { Example, CoordinatedConstituencyParse, ID, SyntaxTreeID, ConstituencyParse, ExampleEditValues, ConstituencyParseNodeEditValues, ConstituencyParseEditValues } from "types";

export type ExampleContextT = {
  example: Example
  onExampleSave: (example: ExampleEditValues) => void
  constituencyParses: CoordinatedConstituencyParse[]
  onConstituencyParseInterpret: (constituencyParse: ConstituencyParse) => void
  onConstituencyParseApproximate: (exampleId: ID) => Promise<CoordinatedConstituencyParse>
  onConstituencyParseNodeAdd: (constituencyParseId: ID, nodeId: SyntaxTreeID) => void
  onConstituencyParseNodeEdit: (constituencyParseId: ID, values: ConstituencyParseNodeEditValues) => void
  onConstituencyParseNodeRemove: (constituencyParseId: ID, nodeId: SyntaxTreeID) => void
  onConstituencyParseNodeMove: (constituencyParseId: ID, nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => void
  onConstituencyParseRemove: (constituencyParseId: ID) => Promise<void>
  onConstituencyParseSave: (constituencyParseId: ID, values: ConstituencyParseEditValues) => Promise<CoordinatedConstituencyParse>
}

export const ExampleContext = React.createContext<ExampleContextT | null>(null);

export function withExampleContext<OuterProps extends ExampleContextT> (Component: React.FC<OuterProps>) {
  return function injectExampleContext(props: OuterProps) {
    return ExampleContext.Consumer({
      children: (ctx) => Component({ ...ctx, ...props })
    });
  };
}

export const useExampleContext = () => React.useContext(ExampleContext);