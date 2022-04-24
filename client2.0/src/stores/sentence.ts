
import { hierarchy } from 'utils/hierarchy';
import { makeAutoObservable } from 'mobx';
import { ID, Sentence, SyntaxTreeID } from 'types';
import { interpret } from 'api';

const { values, assign } = Object;

type SentenceMap = {[key: ID]: Sentence}

const SyntaxTreeNodeFactory = () => ({
  id: "",
  text: ""
});

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
    const tree = sentence.syntax_tree.copy();
    const node = tree.findById(nodeId);

    if (!sentence) throw `No sentence with id:${sentenceId}!`;
    if (!node) throw `No node for sentence (${sentenceId}) with id:${nodeId}`;

    return { sentence, tree, node }
  }

  setSentences = (sentences: Sentence[]) => {
    this.sentenceMap = sentences.reduce((memo, sentence) => assign(
      memo, { [sentence.id]: sentence }
    ), {})
  }

  updateSentenceSyntaxTreeNodeText = (sentenceId: ID, nodeId: SyntaxTreeID, text: string) => {
    const { sentence, tree, node } = this.findSentenceNode(sentenceId, nodeId);

    if (!!node.data.pos) node.data.pos = text
    else node.data.token = text;

    sentence.syntax_tree = hierarchy(tree.data);
  }

  removeSentenceSyntaxTreeNode = (sentenceId: ID, nodeId: SyntaxTreeID) => {
    const { sentence, tree, node } = this.findSentenceNode(sentenceId, nodeId);

    tree.detach(node);

    sentence.syntax_tree = hierarchy(tree.data);
  }

  addSentenceSyntaxTreeNode = (sentenceId: ID, parentNodeId: SyntaxTreeID) => {
    const { sentence, tree, node: parent } = this.findSentenceNode(sentenceId, parentNodeId);
    const newNode = SyntaxTreeNodeFactory();

    tree.attach(parent, newNode);

    sentence.syntax_tree = hierarchy(tree.data);
  }

  moveSentenceSyntaxTreeNode = (sentenceId: ID, nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => {
    console.log('moving...', nodeId, targetParentId);

    const { sentence, tree, node: parent } = this.findSentenceNode(sentenceId, targetParentId);
    const child = tree.findById(nodeId);

    if (!child) throw new Error(`No node for sentence (${sentenceId}) with id:${nodeId}`);

    tree.detach(child);
    tree.attach(parent, child.data);

    sentence.syntax_tree = hierarchy(tree.data);
  }
}