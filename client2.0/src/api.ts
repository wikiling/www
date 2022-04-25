import axios from 'axios'
import { hierarchy } from 'd3-hierarchy';
import { IncomingText, Text, Author, SyntaxTree, SemanticTree } from './types'

const catalogueClient = axios.create({
  baseURL: 'http://localhost:8001/api/v1/',
  headers: {
    Accept: 'application/json',
  },
});

const interpreterClient = axios.create({
  baseURL: 'http://localhost:8080/',
  headers: {
    Accept: 'application/json',
  },
});

const TextFactory = ({sentences, ...text}: IncomingText): Text => ({
  ...text,
  sentences: sentences.map(({ syntax_tree, ...sent }) => ({
    ...sent,
    syntax_tree: hierarchy(syntax_tree)
  }))
});

const fetchTexts = (): Promise<Text[]> => catalogueClient
  .get('texts/')
  .then(({ data }: { data: IncomingText[] }) => data.map(TextFactory));

const fetchAuthors = (): Promise<Author[]> => catalogueClient
  .get('authors/')
  .then(({ data }) => data);


type InterpretationTree = {
  meta: string
  children: InterpretationTree[]
}

const toInterpretationTree = (syntaxTree: SyntaxTree): InterpretationTree => ({
  meta: syntaxTree.pos || syntaxTree.token || '',
  children: syntaxTree.children?.map(toInterpretationTree) ?? []
});

const interpret = (text: Text, syntaxTree: SyntaxTree): Promise<SemanticTree> => {
  console.log(toInterpretationTree(syntaxTree));
  return interpreterClient
  .post(`fragments/${text.id}/`, syntaxTree)
  .then(({ data }) => data);
}

export {
  fetchAuthors,
  fetchTexts,
  interpret
}