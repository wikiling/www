import React, { useEffect } from 'react';
import './App.scss';
import { useStores } from './hooks';
import Tree from 'components/tree/Tree';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import { EditableNodeValues } from 'components/tree/types';
import { ID, SyntaxTreeID } from 'types';
import { DndProvider } from 'react-dnd'
import { HTML5Backend } from 'react-dnd-html5-backend'


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

  return (
    <div className="app">
      {authors.map(author =>
        textsByAuthor(author.id).map(text =>
          <div key={author.id}>
            <div className="app-header">
              {author.full_name}, {text.title}
            </div>
            {text.sentences.map(
              sentence => <div key={sentence.id}>
                <div>
                  ({sentence.id})
                </div>
                <DndProvider backend={HTML5Backend}>
                  <Tree
                    syntaxTree={toJS(sentenceStore.sentenceMap[sentence.id].syntaxTree)}
                    onNodeAdd={(nodeId: SyntaxTreeID) => {
                      sentenceStore.addSentenceSyntaxTreeNode(
                        sentence.id, nodeId
                      );
                    }}
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
                </DndProvider>
              </div>
            )}
          </div>
        )
      )}
    </div>
  );
};

export default observer(App)
