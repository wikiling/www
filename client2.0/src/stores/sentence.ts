
import { hierarchy } from 'd3-hierarchy';
import { remove } from 'lodash';
import { makeAutoObservable, set, has } from 'mobx';
import { ID, NormalizedSyntaxTree, Sentence, SyntaxTree, SyntaxTreeID } from 'types';

const { values, assign } = Object;

type SentenceMap = {[key: ID]: Sentence}

const SyntaxTreeNodeFactory = (parent: SyntaxTree) => {
  const { id: parentId, children } = parent.data;

  const incrChildId = (children: NormalizedSyntaxTree[]) => {
    const { id } = children[children.length - 1];
    return `${parentId}${parseInt(id[id.length - 1]) + 1}`;
  };
  
  return {
    id: children?.length ? incrChildId(children) : `${parentId}0`,
    text: ""
  };
}

export class SentenceStore {
  sentenceMap: SentenceMap = {}

  constructor() {
    makeAutoObservable(this);
  }

  get sentences () {
    return values(this.sentenceMap);
  }

  findSentenceNode = (sentenceId: ID, nodeId: SyntaxTreeID) => {
    const sentence = this.sentenceMap[sentenceId];
    const tree = sentence.syntaxTree.copy();

    if (!sentence) throw `No sentence with id:${sentenceId}!`;

    const node = tree.find(
      (node) => {
        return node.data.id === nodeId
      }
    )

    if (!node) throw `No node for sentence (${sentenceId}) with id:${nodeId}`;

    return { sentence, tree, node }
  }

  setSentences = (sentences: Sentence[]) => {
    this.sentenceMap = sentences.reduce((memo, sentence) => assign(
      memo, { [sentence.id]: sentence }
    ), {})
  }

  updateSentenceSyntaxTreeNodeText = (sentenceId: ID, nodeId: SyntaxTreeID, text: string) => {
    const { node } = this.findSentenceNode(sentenceId, nodeId);

    node.data.text = text;
  }

  // fixme: (a) move removal logic to hierarchy/node.prototype
  removeSentenceSyntaxTreeNode = (sentenceId: ID, nodeId: SyntaxTreeID) => {
    const { sentence, tree, node } = this.findSentenceNode(sentenceId, nodeId);

    if (!node.parent) throw "Can't remove the root of the tree!";

    // leaf
    if (node.parent.children!.length === 1) {
      node.parent.data.children = undefined
    // ancestor
    } else {
      const nodeIdx = node.parent.children!.indexOf(node);
      node.parent.data.children!.splice(nodeIdx, 1)
    }

    sentence.syntaxTree = hierarchy(tree.data);
  }

  addSentenceSyntaxTreeNode = (sentenceId: ID, parentNodeId: SyntaxTreeID) => {
    const { sentence, tree, node: parent } = this.findSentenceNode(sentenceId, parentNodeId);

    const newNode = SyntaxTreeNodeFactory(parent);

    if (!!parent.data.children) parent.data.children.push(newNode);
    else parent.data.children = [newNode];

    sentence.syntaxTree = hierarchy(tree.data);
  }

  translateSentenceSyntaxTreeNode = (sentenceId: ID, nodeId: SyntaxTreeID, dx: number, dy: number) => {
    const { sentence, tree, node } = this.findSentenceNode(sentenceId, nodeId);

    console.log(node);
    node.descendants().forEach((node) => {
      console.log(node);
    })

    sentence.syntaxTree = hierarchy(tree.data);
  }

  moveSentenceSyntaxTreeNode = (sentenceId: ID, nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => {

  }
}