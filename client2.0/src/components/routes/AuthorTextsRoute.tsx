import React, { useState } from "react";
import "./AuthorTextsRoute.scss";
import { useParams } from "react-router-dom";
import MonacoEditor, { monaco } from "react-monaco-editor";
import { useStores } from "hooks";
import Tree from "components/tree/Tree";
import { observer } from "mobx-react-lite";
import { toJS } from "mobx";
import { EditableNodeValues } from "components/tree/types";
import { AuthorTextRouteParams, ID, SyntaxTree, SyntaxTreeID } from "types";
import { registerMonaco } from "utils/monaco";
import TreeHeader from "components/tree/TreeHeader";
import { TreeContext } from "components/tree/TreeContext";
import Header from "components/Header";

const AuthorTextsRoute: React.FC = () => {
  const { authorId } = useParams<AuthorTextRouteParams>();
  const {
    centralStore: {
      authors, textsByAuthor, sentenceStore, dispatchInterpretSentence
    }
  } = useStores();
  const [treeEditCountMap, setTreeEditCountMap] = useState<{[key: ID]: number}>({});

  const author = authors.find(({ id }) => id.toString() === authorId);

  const incrTreeEditCount = (sentenceId: ID) => setTreeEditCountMap(
    prev => Object.assign(prev, { [sentenceId]: (prev[sentenceId] ?? 0) + 1 })
  );

  const uri = `file:///app/fragments/`;

  const options = {
    model: monaco.editor.getModel(monaco.Uri.parse(uri)) ||
      monaco.editor.createModel("// bar", 'haskell', monaco.Uri.parse(uri))
  };

  return (
    <div className="author-texts-route">
      <Header left={author?.full_name}/>
      <div className="author-texts-route-editor">
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
      <div className="author-texts-route-trees">
        {author && textsByAuthor(author.id).map(text =>
          text.sentences.map(sentence =>
            <div key={`${sentence.id}-${treeEditCountMap[sentence.id]}`}>
              <TreeHeader
                sentence={sentence}
                onInterpretButtonClick={() => dispatchInterpretSentence(text, sentence.syntax_tree.data)}
              />
              <Tree
                id={sentence.id}
                syntaxTree={toJS(sentenceStore.sentenceMap[sentence.id].syntax_tree)}
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
          )
        )}
      </div>
    </div>
  );
};

export default observer(AuthorTextsRoute);
