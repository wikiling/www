import axios from 'axios'
import { Fragment, Author, SyntaxTree, SemanticTree, Slug, Example, ID, ConstituencyParse, EditableExampleValues } from './types'

const catalogueClient = axios.create({
  baseURL: 'http://localhost:8001/api/v1/',
  headers: {
    Accept: 'application/json',
  },
});

const interpretationClient = axios.create({
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

const updateExample = (exampleId: ID, values: EditableExampleValues): Promise<Example> => catalogueClient
  .patch(`examples/${exampleId}/`, values)
  .then(({ data }) => data);

const fetchConstituencyParses = (example_id: ID): Promise<ConstituencyParse[]> => catalogueClient
  .get('constituency-parses/', {params: { example_id }})
  .then(({ data }) => data);

const createConstituencyParse = (example_id: ID): Promise<ConstituencyParse> => catalogueClient
  .post('constituency-parses/', { example_id })
  .then(({ data }) => data);

const deleteConstituencyParse = (constituencyParseId: ID): Promise<string | number> => catalogueClient
  .delete(`constituency-parses/${constituencyParseId}`)
  .then(({ status }) => status);

const fetchInterpretation = (fragment: Fragment, syntaxTree: SyntaxTree): Promise<SemanticTree> => interpretationClient
  .post(`fragments/${fragment.id}/`, syntaxTree)
  .then(({ data }) => data);

export {
  fetchFragment,
  fetchExamples,
  updateExample,
  fetchConstituencyParses,
  createConstituencyParse,
  deleteConstituencyParse,
  fetchInterpretation
}