import React, { useEffect } from "react";
import "./FragmentDetailRoute.scss";
import { useParams } from "react-router-dom";

import { useStores } from "hooks";
import { observer } from "mobx-react-lite";
import { FragmentDetailRouteParams } from "types";

import Header from "components/layout/Header";
import Example, { ExampleRef } from "components/Example";
import PlusIcon from "components/icons/PlusIcon";
import Button from "components/Button";
import TemporaryExample from "components/TemporaryExample";
import Page from "components/layout/Page";
import Route from "./Route";
import { toPascalCase } from "utils/string";
import { useKey } from "react-use";
import { updateFragmentGrammar } from "api";
import { fragmentGrammarFilename, fragmentGrammarURI } from "stores/FragmentStore";
import Editor from "components/Editor";
import YScrollable from "components/YScrollable";


const FragmentDetailRoute: React.FC = () => {
  const { fragmentSlug } = useParams<FragmentDetailRouteParams>();
  const { fragmentStore: fs } = useStores();
  
  const handleNewExample = fs.createTemporaryExample;

  useEffect(() => {
    if (fragmentSlug) fs.dispatchFetchFragment(fragmentSlug);
  }, [fragmentSlug, fs])

  return (
    <Route className="fragment-detail-route">
      <Page
        header={<Header left={`${fs.fragment?.author?.full_name}, ${fs.fragment?.title}`}/>}
        panes={[
          <div className="fragment-detail-route-editor">
            {fs.fragment && fs.initialGrammar && <Editor
              content={fs.initialGrammar}
              uri={fragmentGrammarURI(fs.fragment)}
              onSave={fs.dispatchUpdateFragmentGrammar}
            />}
          </div>,
          <YScrollable>
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
          </YScrollable>
        ]}
      />
    </Route>
  );
};

export default observer(FragmentDetailRoute);
