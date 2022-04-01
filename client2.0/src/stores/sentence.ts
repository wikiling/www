
import { makeAutoObservable } from 'mobx'
import { ID, Sentence, SyntaxTree } from 'types'

export class SentenceStore {
  sentences: Sentence[] = []

  // map from sentence ids to map from node ids to nodes
  syntaxTreeNodeMap: Record<ID, Record<string, SyntaxTree>> = {}

  constructor() {
    makeAutoObservable(this);
  }

  setSentences = (sentences: Sentence[]) => {
    this.sentences = sentences;
  }

  setSyntaxTreeNodeMap = () => {
    
  }

  updateSentenceSyntaxTreeNode = (sentenceId: ID, nodeId: string, text: string) => {
    console.log(sentenceId, nodeId, text)
  }
}