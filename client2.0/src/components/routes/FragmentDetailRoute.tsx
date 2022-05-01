import React, { useEffect } from "react";
import "./FragmentDetailRoute.scss";
import { useParams } from "react-router-dom";
import MonacoEditor, { monaco } from "react-monaco-editor";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import { FragmentDetailRouteParams } from "types";
import { registerMonaco } from "utils/monaco";
import Header from "components/Header";
import Example from "components/Example";


const FragmentDetailRoute: React.FC = () => {
  const { fragmentSlug } = useParams<FragmentDetailRouteParams>();
  const { fragmentStore: fs } = useStores();

  const uri = `file:///app/fragments/`;

  const options = {
    model: monaco.editor.getModel(monaco.Uri.parse(uri)) ||
      monaco.editor.createModel("-- write your fragment here...", "haskell", monaco.Uri.parse(uri))
  };

  useEffect(() => {
    if (fragmentSlug) fs.dispatchFetchFragment(fragmentSlug);
  }, [fragmentSlug, fs])

  return (
    <div className="fragment-detail-route">
      <Header left={`${fs.fragment?.author?.full_name}, ${fs.fragment?.title}`}/>
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
          <Example example={example} key={example.id}/>
        )}
      </div>
    </div>
  );
};

export default observer(FragmentDetailRoute);
