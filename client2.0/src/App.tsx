import React, { useCallback, useEffect, useState } from 'react';
import './App.scss';
import { useStores } from './hooks';
import Tree from 'components/tree/Tree';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import { EditableNodeValues } from 'components/tree/types';
import { ID, NormalizedSyntaxTree, Sentence, SyntaxTree, SyntaxTreeID } from 'types';

const App: React.FC = () => {
  const {
    centralStore, centralStore: {
      authors, textsByAuthor, sentenceStore
    }
  } = useStores();
  const [treeEditCountMap, setTreeEditCountMap] = useState<{[key: ID]: number}>({})

  const incrTreeEditCount = (sentenceId: ID) => setTreeEditCountMap(
    prev => Object.assign(prev, { [sentenceId]: (prev[sentenceId] ?? 0) + 1 })
  );

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
              sentence => <div key={`${sentence.id}-${treeEditCountMap[sentence.id]}`}>
                <div>
                  ({sentence.id})
                </div>
                  <Tree
                    id={sentence.id}
                    syntaxTree={toJS(sentenceStore.sentenceMap[sentence.id].syntaxTree)}
                    onNodeAdd={(nodeId: SyntaxTreeID) => {
                      sentenceStore.addSentenceSyntaxTreeNode(
                        sentence.id, nodeId
                      );
                      incrTreeEditCount(sentence.id);
                    }}
                    onNodeEdit={(values: EditableNodeValues) => {
                      sentenceStore.updateSentenceSyntaxTreeNodeText(
                        sentence.id, values.id, values.text
                      );
                      incrTreeEditCount(sentence.id);
                    }}
                    onNodeRemove={(nodeId: SyntaxTreeID) => {
                      sentenceStore.removeSentenceSyntaxTreeNode(
                        sentence.id, nodeId
                      );
                      incrTreeEditCount(sentence.id);
                    }}
                    onNodeMove={(nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => {
                      sentenceStore.moveSentenceSyntaxTreeNode(
                        sentence.id, nodeId, targetParentId
                      );
                      incrTreeEditCount(sentence.id);
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
