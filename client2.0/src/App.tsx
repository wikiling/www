import React, { useCallback, useEffect } from 'react';
import './App.scss';
import { useStores } from './hooks';
import Tree from 'components/tree/Tree';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';
import { EditableNodeValues } from 'components/tree/types';
import { ID, NormalizedSyntaxTree, Sentence, SyntaxTreeID } from 'types';

const App: React.FC = () => {
  const {
    centralStore, centralStore: {
      authors, textsByAuthor, sentenceStore
    }
  } = useStores();

  console.log('APP RENDER', toJS(textsByAuthor(1)));

  // hack to force a re-render on tree node addition/subtraction/edit
  const nodeValue = (node: NormalizedSyntaxTree) => node.text.length + 1;
  const treeKey = (sentence: Sentence) => {
    const bar = sentence.syntaxTree.sum(nodeValue);
    const foo = `${sentence.id}${bar.value}`;
    console.log(foo, bar)
    return foo
  }

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
              sentence => <div key={treeKey(sentence)}>
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
                    onNodeMove={(nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => {
                      sentenceStore.moveSentenceSyntaxTreeNode(
                        sentence.id, nodeId, targetParentId
                      )
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
