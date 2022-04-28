import axios from 'axios'
import { Fragment, Author, SyntaxTree, SemanticTree, Slug, Example, ID, ConstituencyParse } from './types'

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

const fetchAuthors = (): Promise<Author[]> => catalogueClient
  .get('authors/')
  .then(({ data }) => data);

const fetchFragment = (slug: Slug): Promise<Fragment> => catalogueClient
  .get(`fragments/${slug}`)
  .then(({ data }) => data);

const fetchExamples = (fragmentId: ID): Promise<Example[]> => catalogueClient
  .get(`examples?fragment=${fragmentId}`)
  .then(({ data }) => data);

const fetchConstituencyParses = (exampleId: ID): Promise<ConstituencyParse[]> => catalogueClient
  .get(`constituency-parses?example=${exampleId}`)
  .then(({ data }) => data);

const fetchInterpretation = (fragment: Fragment, syntaxTree: SyntaxTree): Promise<SemanticTree> => interpreterClient
  .post(`fragments/${fragment.id}/`, syntaxTree)
  .then(({ data }) => data);

export {
  fetchAuthors,
  fetchFragment,
  fetchExamples,
  fetchConstituencyParses,
  fetchInterpretation
}