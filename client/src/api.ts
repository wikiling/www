import axios from 'axios'
import { toPascalCase } from 'utils/string';
import { Fragment, SyntaxTree, SemanticTree, Slug, Example, ID, ConstituencyParse, ExampleEditValues, ConstituencyParseEditValues, ExampleCreateValues } from './types'

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

const languageServerClient = axios.create({
  baseURL: 'http://localhost:3003/',
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

const updateExample = (exampleId: ID, values: ExampleEditValues): Promise<Example> => catalogueClient
  .patch(`examples/${exampleId}/`, values)
  .then(({ data }) => data);

const createExample = (values: ExampleCreateValues): Promise<Example> => catalogueClient
  .post('examples/', values)
  .then(({ data }) => data);

const deleteExample = (exampleId: ID): Promise<string | number> => catalogueClient
  .delete(`examples/${exampleId}`)
  .then(({ status }) => status);

const fetchConstituencyParses = (example_id: ID): Promise<ConstituencyParse[]> => catalogueClient
  .get('constituency-parses/', {params: { example_id }})
  .then(({ data }) => data);

const createConstituencyParse = (example_id: ID): Promise<ConstituencyParse> => catalogueClient
  .post('constituency-parses/', { example_id })
  .then(({ data }) => data);

const deleteConstituencyParse = (constituencyParseId: ID): Promise<string | number> => catalogueClient
  .delete(`constituency-parses/${constituencyParseId}`)
  .then(({ status }) => status);

const updateConstituencyParse = (constituencyParseId: ID, values: ConstituencyParseEditValues): Promise<ConstituencyParse> => catalogueClient
  .patch(`constituency-parses/${constituencyParseId}/`, values)
  .then(({ data }) => data);

const fetchInterpretation = (fragment: Fragment, syntaxTree: SyntaxTree): Promise<SemanticTree> => interpretationClient
  .post(`fragments/${toPascalCase(fragment.slug)}/`, syntaxTree)
  .then(({ data }) => data);

const fetchFragmentGrammar = (filename: string): Promise<string> => languageServerClient
  .get(filename)
  .then(({ data }) => data);

const updateFragmentGrammar = (filename: string, content: string) => languageServerClient.post(filename, { content });

export {
  fetchFragment,
  fetchExamples,
  updateExample,
  createExample,
  deleteExample,
  fetchConstituencyParses,
  createConstituencyParse,
  deleteConstituencyParse,
  updateConstituencyParse,
  fetchInterpretation,
  fetchFragmentGrammar,
  updateFragmentGrammar
}