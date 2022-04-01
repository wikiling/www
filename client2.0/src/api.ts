import axios from 'axios'
import { IncomingSyntaxTree, SyntaxTree, IncomingText, Text, Author, NormalizedSyntaxTree } from './types'
import { hierarchy } from 'utils/tree';

const client = axios.create({
  baseURL: 'http://localhost:8001/api/v1/',
  headers: {
    Accept: 'application/json',
  },
});

const NormalizedSyntaxTreeFactory = ({ children = [], token, pos, id }: IncomingSyntaxTree): NormalizedSyntaxTree => {
  return {
    children: children.map(NormalizedSyntaxTreeFactory),
    text: pos ? pos : token ? token : '',
    id
  }
};

const TextFactory = ({sentences, ...text}: IncomingText): Text => ({
  ...text,
  sentences: sentences.map(({ syntax_tree, ...sent }) => ({
    ...sent,
    syntaxTree: hierarchy(
      NormalizedSyntaxTreeFactory(syntax_tree)
    )
  }))
});

const fetchTexts = (): Promise<Text[]> => client
  .get('texts/')
  .then(({ data }: { data: IncomingText[] }) => data.map(TextFactory));

const fetchAuthors = (): Promise<Author[]> => client
  .get('authors/')
  .then(({ data }) => data);

export {
  fetchAuthors,
  fetchTexts
}