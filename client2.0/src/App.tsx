import React, { useEffect } from 'react';
import './App.scss';
import { useStores } from './hooks';
import Tree from 'tree/Tree';
import { observer } from 'mobx-react-lite';
import { toJS } from 'mobx';

const App: React.FC = () => {
  const {
    centralStore, centralStore: {
      authors, textsByAuthor
    }
  } = useStores();

  useEffect(() => {
    centralStore.dispatchFetchAuthors();
    centralStore.dispatchFetchTexts();
  }, []);

  const onTreeNodeAdd = () => {};
  const onTreeNodeEdit = () => {};
  const onTreeNodeRemove = () => {};

  return (
    <div className="app">
      {authors.map(author =>
        textsByAuthor(author.id).map(text =>
          <>
            <div className="header">
              {author.full_name}, {text.title}
              <hr/>
            </div>
            {text.sentences.map(
              sentence => <>
                <div>
                  ({sentence.id})
                </div>
                <Tree
                  data={toJS(sentence.syntax_tree)}
                  onNodeAdd={onTreeNodeAdd}
                  onNodeEdit={onTreeNodeEdit}
                  onNodeRemove={onTreeNodeRemove}
                />
              </>
            )}
          </>
        )
      )}
    </div>
  );
};

export default observer(App)
