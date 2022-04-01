
import { hierarchy } from 'd3-hierarchy';
import { remove } from 'lodash';
import { makeAutoObservable, set, has } from 'mobx';
import { ID, Sentence, SyntaxTree, SyntaxTreeID } from 'types';

const { values, assign } = Object;

type SentenceMap = {[key: ID]: Sentence}

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

    if (!sentence) throw `No sentence with id:${sentenceId}!`;

    const node = sentence.syntaxTree.find(
      (node) => {
        return node.data.id === nodeId
      }
    )

    if (!node) throw `No node for sentence (${sentenceId}) with id:${nodeId}`;

    return node
  }

  setSentences = (sentences: Sentence[]) => {
    this.sentenceMap = sentences.reduce((memo, sentence) => assign(
      memo, { [sentence.id]: sentence }
    ), {})
  }

  updateSentenceSyntaxTreeNodeText = (sentenceId: ID, nodeId: SyntaxTreeID, text: string) => {
    const node = this.findSentenceNode(sentenceId, nodeId);

    node.data.text = text;
  }

  // fixme: (a) move removal logic to hierarchy/node.prototype
  //        (b) recalculate e.g. tree height after removal
  removeSentenceSyntaxTreeNode = (sentenceId: ID, nodeId: string) => {
    const node = this.findSentenceNode(sentenceId, nodeId);

    if (!node.parent) throw "Can't remove the root of the tree!";

    if (node.parent.children!.length === 1) {
      node.parent.data.children = undefined
      node.parent.children = undefined
    } else {
      const nodeIdx = node.parent.children!.indexOf(node);

      node.parent.data.children!.splice(nodeIdx, 1)
      node.parent.children!.splice(nodeIdx, 1)
    }
  }

  addSentenceSyntaxTreeNode = (sentenceId: ID, targetNodeId: string) => {

  }

  moveSentenceSyntaxTreeNode = (sentenceId: ID, sourceNodeId: string, targetNodeId: string) => {

  }
}