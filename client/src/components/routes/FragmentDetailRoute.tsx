import React, { useEffect, useState } from "react";
import "./FragmentDetailRoute.scss";
import { useParams } from "react-router-dom";
import MonacoEditor, { monaco } from "react-monaco-editor";
import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import { Fragment, FragmentDetailRouteParams, ID } from "types";
import { registerMonaco } from "utils/monaco";
import Header from "components/Header";
import Example, { ExampleRef } from "components/Example";
import PlusIcon from "components/icons/PlusIcon";
import Button from "components/Button";
import TemporaryExample from "components/TemporaryExample";
import Page from "components/Page";
import Route from "./Route";
import { toPascalCase } from "utils/string";
import { useKey } from "react-use";
import { updateFragmentGrammar } from "api";
import { fragmentGrammarFilename, fragmentGrammarURI } from "stores/FragmentStore";

const getMonacoModel = (fragment: Fragment) => monaco.editor.getModel(
  monaco.Uri.parse(fragmentGrammarURI(fragment))
);
const createMonacoModel = (fragment: Fragment, initialValue: string) => monaco.editor.createModel(
  initialValue,
  "haskell",
  monaco.Uri.parse(fragmentGrammarURI(fragment))
);

const getOrCreateMonacoModel = (fragment: Fragment, initialValue: string) =>
  getMonacoModel(fragment) ?? createMonacoModel(fragment, initialValue);

const FragmentDetailRoute: React.FC = () => {
  const { fragmentSlug } = useParams<FragmentDetailRouteParams>();
  const { fragmentStore: fs } = useStores();
  
  const handleNewExample = fs.createTemporaryExample;

  useEffect(() => {
    if (fragmentSlug) fs.dispatchFetchFragment(fragmentSlug);
  }, [fragmentSlug, fs])

  const saveKeyFilter = (e: KeyboardEvent) => e.key === "s" && e.metaKey;

  useKey(saveKeyFilter, (e: KeyboardEvent) => {
    if (!fs.fragment) return;
  
    const model = getMonacoModel(fs.fragment);
  
    if (!model) return;

    e.preventDefault();

    fs.dispatchUpdateFragmentGrammar(model.getValue());
  });

  return (
    <Route className="fragment-detail-route">
      <Header left={`${fs.fragment?.author?.full_name}, ${fs.fragment?.title}`}/>
      <Page>
        <div className="fragment-detail-route-editor">
          {fs.fragment && fs.initialGrammar && <MonacoEditor
            width="100%"
            height="90vh"
            language="haskell"
            editorWillMount={registerMonaco}
            options={{
              model: getOrCreateMonacoModel(fs.fragment, fs.initialGrammar),
              minimap: {
                enabled: false
              }
            }}
            // theme="vs-dark"
            // onChange={::this.onChange}
            // editorDidMount={::this.editorDidMount}
          />}
        </div>
        <div className="fragment-detail-route-examples">
          {fs.examples.map((example) =>
            <Example example={example} key={example.id}/>
          )}
          {fs.temporaryExamples.map((example) =>
            <TemporaryExample example={example} key={example.temp_id}/>
          )}
          <div className="fragment-detail-route-new-example-control">
            <Button mode="clear" onClick={handleNewExample}>
              <PlusIcon/><span className="fragment-detail-route-new-example-control-text" >add an example</span>
            </Button>
          </div>
        </div>
      </Page>
    </Route>
  );
};

export default observer(FragmentDetailRoute);
