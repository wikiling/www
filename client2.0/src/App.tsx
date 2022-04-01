import React, { useEffect } from 'react';
import './App.scss';
import { useStores } from './hooks';
import Tree from 'tree/Tree';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import { EditableNodeValues } from 'tree/types';
import { ID, SyntaxTreeID } from 'types';

const App: React.FC = () => {
  const {
    centralStore, centralStore: {
      authors, textsByAuthor, sentenceStore
    }
  } = useStores();

  useEffect(() => {
    centralStore.dispatchFetchAuthors();
    centralStore.dispatchFetchTexts();
  }, []);

  const onTreeNodeAdd = () => {};
  const onTreeNodeRemove = () => {};

  return (
    <div className="app">
      {authors.map(author =>
        textsByAuthor(author.id).map(text =>
          <div key={author.id}>
            <div className="header">
              {author.full_name}, {text.title}
              <hr/>
            </div>
            {text.sentences.map(
              sentence => <div key={sentence.id}>
                <div>
                  ({sentence.id})
                </div>
                <Tree
                  data={toJS(sentence.syntax_tree)}
                  onNodeAdd={onTreeNodeAdd}
                  onNodeEdit={(values: EditableNodeValues) => {
                    sentenceStore.updateSentenceSyntaxTreeNodeText(
                      sentence.id, values.id, values.text
                    );
                  }}
                  onNodeRemove={(nodeId: SyntaxTreeID) => {
                    sentenceStore.removeSentenceSyntaxTreeNode(
                      sentence.id, nodeId
                    );
                  }}
                />
              </div>
            )}
          </div>
        )
      )}
    </div>
  );
};

export default observer(App)
