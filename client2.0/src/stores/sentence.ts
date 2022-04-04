
import { hierarchy } from 'utils/hierarchy';
import { cloneDeep, remove } from 'lodash';
import { makeAutoObservable, set, has } from 'mobx';
import { ID, NormalizedSyntaxTree, Sentence, SyntaxTree, SyntaxTreeID } from 'types';
import { getNewChildId } from 'utils/hierarchy';

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
    const tree = sentence.syntaxTree.copy();
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

    node.data.text = text;

    sentence.syntaxTree = hierarchy(tree.data);
  }

  removeSentenceSyntaxTreeNode = (sentenceId: ID, nodeId: SyntaxTreeID) => {
    const { sentence, tree, node } = this.findSentenceNode(sentenceId, nodeId);

    tree.detach(node);

    sentence.syntaxTree = hierarchy(tree.data);
  }

  addSentenceSyntaxTreeNode = (sentenceId: ID, parentNodeId: SyntaxTreeID) => {
    const { sentence, tree, node: parent } = this.findSentenceNode(sentenceId, parentNodeId);
    const newNode = SyntaxTreeNodeFactory();

    tree.attach(parent, newNode);

    sentence.syntaxTree = hierarchy(tree.data);
  }

  moveSentenceSyntaxTreeNode = (sentenceId: ID, nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => {
    console.log('moving...', nodeId, targetParentId);

    const { sentence, tree, node: parent } = this.findSentenceNode(sentenceId, targetParentId);

    const child = tree.findById(nodeId);

    if (!child) throw `No node for sentence (${sentenceId}) with id:${nodeId}`;

    tree.detach(child);
    tree.attach(parent, child.data);

    console.log(cloneDeep(tree));

    sentence.syntaxTree = hierarchy(tree.data);
  }
}