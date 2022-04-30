import React, { useEffect, useState } from "react";
import "./FragmentDetailRoute.scss";
import { useParams } from "react-router-dom";
import MonacoEditor, { monaco } from "react-monaco-editor";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import { EditableNodeValues } from "components/tree/types";
import { FragmentDetailRouteParams, ID, SyntaxTreeID } from "types";
import { registerMonaco } from "utils/monaco";
import Header from "components/Header";
import Example from "components/Example";
import { toJS } from "mobx";

const FragmentDetailRoute: React.FC = () => {
  const { fragmentSlug } = useParams<FragmentDetailRouteParams>();
  const { fragmentStore: fs } = useStores();

  const uri = `file:///app/fragments/`;

  const options = {
    model: monaco.editor.getModel(monaco.Uri.parse(uri)) ||
      monaco.editor.createModel("-- write your fragment here...", "haskell", monaco.Uri.parse(uri))
  };
  const [exampleEditCountMap, setExampleEditCountMap] = useState<{[key: ID]: number}>({});
  const incrExampleEditCount = (exampleId: ID) => setExampleEditCountMap(
    prev => ({ ...prev, [exampleId]: (prev[exampleId] ?? 0) + 1 })
  );

  useEffect(() => {
    if (fragmentSlug) fs.dispatchFetchFragment(fragmentSlug);
  }, [fragmentSlug, fs])

  return (
    <div className="fragment-detail-route">
      <Header left={fs.fragment?.author?.full_name}/>
      <div className="fragment-detail-route-editor">
        <MonacoEditor
          width="100%"
          height="90vh"
          language="haskell"
          editorWillMount={registerMonaco}
          options={options}
          // theme="vs-dark"
          // onChange={::this.onChange}
          // editorDidMount={::this.editorDidMount}
        />
      </div>
      <div className="fragment-detail-route-examples">
        {fs.examples.map((example) =>
          <Example
            key={`${example.id}-${exampleEditCountMap[example.id]}`}
            example={example}
            constituencyParses={toJS(fs.exampleConstituencyParses(example.id))}
            onConstituencyParseNodeAdd={fs.addConstituencyParseNode}
            onConstituencyParseNodeEdit={fs.updateConstituencyParseNode}
            onConstituencyParseNodeMove={fs.moveConstituencyParseNode}
            onConstituencyParseNodeRemove={(cpid: ID, nodeId: SyntaxTreeID) => {
              fs.removeConstituencyParseNode(cpid, nodeId);
              incrExampleEditCount(example.id)
            }}
            onConstituencyParseInterpret={(constituencyParse) =>
              fs.fragment && fs.dispatchInterpretConstituencyParse(
                fs.fragment, constituencyParse
              )
            }
            />
        )}
      </div>
    </div>
  );
};

export default observer(FragmentDetailRoute);
