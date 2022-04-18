import React, { useEffect, useState } from "react";
import "./AuthorTextsRoute.scss";
import { useParams } from "react-router-dom";
import Editor, { useMonaco } from "@monaco-editor/react";
// import * as monaco from "monaco-editor";
import { useStores } from "hooks";
import Tree from "components/tree/Tree";
import { observer } from "mobx-react-lite";
import { toJS } from "mobx";
import { EditableNodeValues } from "components/tree/types";
import { AuthorTextRouteParams, ID, SyntaxTreeID } from "types";
import { registerMonaco } from "utils/monaco";

// console.log(monaco.languages)
// loader.config({ monaco });

const AuthorTextsRoute: React.FC = () => {
  const { authorId } = useParams<AuthorTextRouteParams>();
  const {
    centralStore: {
      authors, textsByAuthor, sentenceStore
    }
  } = useStores();
  const [treeEditCountMap, setTreeEditCountMap] = useState<{[key: ID]: number}>({});

  const author = authors.find(({ id }) => id.toString() === authorId);

  const incrTreeEditCount = (sentenceId: ID) => setTreeEditCountMap(
    prev => Object.assign(prev, { [sentenceId]: (prev[sentenceId] ?? 0) + 1 })
  );

  const monaco = useMonaco();

  useEffect(() => {
    if (monaco) {
      console.log(monaco);
      registerMonaco(monaco);
    }
  })

  return (
    <div className="author-texts-route">
      <div className="author-texts-route-header">
        {author?.full_name}
      </div>
      <div className="author-texts-route-editor">
        <Editor
          height="90vh"
          language="json"
          defaultValue="// some comment"
        />
      </div>
      <div className="author-texts-route-trees">
        {author && textsByAuthor(author.id).map(text =>
          text.sentences.map(sentence =>
            <div key={`${sentence.id}-${treeEditCountMap[sentence.id]}`}>
              <div className="sentence-id">
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
          )
        )}
      </div>
    </div>
  );
};

export default observer(AuthorTextsRoute);
