import axios from 'axios'
import { IncomingSyntaxTree, SyntaxTree, IncomingText, Text, Author } from './types'

const client = axios.create({
  baseURL: 'http://localhost:8001/api/v1/',
  headers: {
    Accept: 'application/json',
  },
})

const SyntaxTreeFactory = ({ children = [], token, pos, id }: IncomingSyntaxTree): SyntaxTree => {
  return {
    children: children.map(SyntaxTreeFactory),
    text: pos ? pos : token ? token : '',
    id
  }
}


const TextFactory = ({sentences, ...text}: IncomingText): Text => ({
  ...text,
  sentences: sentences.map(sent => ({
    ...sent,
    syntax_tree: SyntaxTreeFactory(sent.syntax_tree)
  }))
})

const fetchTexts = (): Promise<Text[]> => client
  .get('texts/')
  .then(({ data }: { data: IncomingText[] }) => data.map(TextFactory))

const fetchAuthors = (): Promise<Author[]> => client
  .get('authors/')
  .then(({ data }) => data)


export {
  fetchAuthors,
  fetchTexts
}