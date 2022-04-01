
import { makeAutoObservable, set, has, remove } from 'mobx';
import { ID, Sentence, SyntaxTree } from 'types';

/**
 * See https://github.com/d3/d3-hierarchy/blob/v3.1.1/src/hierarchy/iterator.js
 * @param root SyntaxTree
 */
function* treeIterator (root: SyntaxTree) {
  let node: SyntaxTree | undefined = root, current, next = [node], children, i, n;
  do {
    current = next.reverse();
    next = [];
    while (node = current.pop()) {
      yield node;
      if (children = node.children) {
        for (i = 0, n = children.length; i < n; ++i) {
          next.push(children[i]);
        }
      }
    }
  } while (next.length);
}

const flattenTree = (root: SyntaxTree) => {
  return Array.from(treeIterator(root));
}

type SyntaxTreeNodeMap = {[key: ID]: {[key: string]: SyntaxTree}}

export class SentenceStore {
  sentences: Sentence[] = []

  // map from sentence ids to map from node ids to nodes
  syntaxTreeNodeMap: SyntaxTreeNodeMap = {}

  constructor() {
    makeAutoObservable(this);
  }

  setSentences = (sentences: Sentence[]) => {
    this.sentences = sentences;

    this.setSyntaxTreeNodeMap();
  }

  setSyntaxTreeNodeMap = () => {
    this.syntaxTreeNodeMap = this.sentences.reduce(
      (accum, { syntax_tree, id }) =>
        Object.assign(accum, {
          [id]: flattenTree(syntax_tree).reduce(
            (accum, node) => Object.assign(
              accum, {
                [node.id]: node
              }
            ), {}
          )
        }), {}
    );
  }

  updateSentenceSyntaxTreeNodeText = (sentenceId: ID, nodeId: string, text: string) => {
    const nodeMap = this.syntaxTreeNodeMap[sentenceId];

    if (!nodeMap) throw `No node map for sentence ${sentenceId}!`;

    nodeMap[nodeId].text = text;
  }

  removeSentenceSyntaxTreeNode = (sentenceId: ID, nodeId: string) => {
    const nodeMap = this.syntaxTreeNodeMap[sentenceId];

    if (!nodeMap) throw `No node map for sentence ${sentenceId}!`;
  
    remove(nodeMap, nodeId);
  }

  moveSentenceSyntaxTreeNode = (sentenceId: ID, sourceNodeId: string, targetNodeId: string) => {

  }

  addSentenceSyntaxTreeNode = (sentenceId: ID, targetNodeId: string) => {

  }
}