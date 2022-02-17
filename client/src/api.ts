import axios from 'axios'
import { IncomingSyntaxTree, SyntaxTree, IncomingText, Text, Author } from './types'

const client = axios.create({
  baseURL: 'http://localhost:8001/api/v1/',
  headers: {
    Accept: 'application/json',
  },
})

const SyntaxTreeFactory = ({ children = [], token, pos }: IncomingSyntaxTree): SyntaxTree => {
  return {
    children: children.map(SyntaxTreeFactory),
    text: pos ? pos : token ? token : ''
  }
}


const TextFactory = (text: IncomingText): Text => ({
  ...text,
  syntax_trees: text.syntax_trees.map(SyntaxTreeFactory)
})

const getTexts = (): Promise<Text[]> => client
  .get('texts/')
  .then(({ data }: { data: IncomingText[] }) => data.map(TextFactory))

const getAuthors = (): Promise<Author[]> => client
  .get('authors/')
  .then(({ data }) => data)


export {
  getAuthors,
  getTexts
}