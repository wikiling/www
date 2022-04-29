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

const fetchFragment = (slug: Slug): Promise<Fragment> => catalogueClient
  .get(`fragments/${slug}`)
  .then(({ data }) => data);

const fetchExamples = (fragment_id: ID): Promise<Example[]> => catalogueClient
  .get('examples/', {params: { fragment_id }})
  .then(({ data }) => data);

const fetchConstituencyParses = (example_id: ID): Promise<ConstituencyParse[]> => catalogueClient
  .get('constituency-parses/', {params: { example_id }})
  .then(({ data }) => data);

const fetchInterpretation = (fragment: Fragment, syntaxTree: SyntaxTree): Promise<SemanticTree> => interpreterClient
  .post(`fragments/${fragment.id}/`, syntaxTree)
  .then(({ data }) => data);

export {
  fetchFragment,
  fetchExamples,
  fetchConstituencyParses,
  fetchInterpretation
}