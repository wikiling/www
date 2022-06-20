
import { makeAutoObservable, ObservableMap, remove } from 'mobx';
import { ID, Author, Fragment, Slug, Example, CoordinatedConstituencyParse, ConstituencyParse, TreeID, ConstituencyParseNodeEditValues, TemporaryExample, ExampleEditValues, ConstituencyParseEditValues, UUID, ExampleCreateValues, CoordinatedSemanticTree, SemanticTree, Interpretation, TemporaryInterpretation, InterpretationEditValues, InterpretationCreateValues } from 'types';
import { fetchFragment, fetchSemanticTree, fetchExamples, updateExample, deleteExample, createConstituencyParse, deleteConstituencyParse, updateConstituencyParse, createExample, fetchFragmentGrammar, updateFragmentGrammar, fetchInterpretations, updateInterpretation, createInterpretation, deleteInterpretation } from 'api';
import { hierarchy } from 'utils/hierarchy';
import { createIdMap } from 'utils/store';
import { v4 as uuid } from 'uuid';
import { toPascalCase } from 'utils/string';
import { orderBy } from 'lodash';

type ExampleMap = {[key: ID]: Example}
type TemporaryExampleMap = {[key: UUID]: TemporaryExample}
type InterpretationMap = {[key: ID]: Interpretation}
type TemporaryInterpretationMap = {[key: UUID]: TemporaryInterpretation}
type ConstituencyParseMap = {[key: ID]: CoordinatedConstituencyParse}
type SemanticTreeMap = {[key: ID]: CoordinatedSemanticTree}

const { values } = Object;

export const fragmentGrammarFilename = (fragment: Fragment) => `${toPascalCase(fragment.slug)}.hs`;
export const fragmentGrammarURI = (fragment: Fragment) => `file:///app/fragments/${fragmentGrammarFilename(fragment)}`;

const labelTmpl = (inner: number) => `(${inner})`;

const getNextLabel = (example: Example | TemporaryExample | null) => {
  const label = example?.label;

  if (!label) return labelTmpl(1);

  switch (label) {
    case label.match(/\((.+)\)/)?.input:
      const stripped = label.replace(/[()]/g, '')
      const num = parseInt(stripped);
      if (isNaN(num)) return ''
      return labelTmpl(num + 1);
    default:
      return ''
  }
};

const SyntaxTreeNodeFactory = () => ({
  id: "",
  label: ""
});

const TemporaryExampleFactory = (
  fragment_id: ID,
  prevExample: Example | TemporaryExample | null
): TemporaryExample => ({
  fragment_id,
  content: '',
  label: getNextLabel(prevExample),
  description: '',
  temp_id: uuid()
});

const TemporaryInterpretationFactory = (example_id: ID): TemporaryInterpretation => ({
  example_id,
  content: '',
  paraphrase: '',
  temp_id: uuid()
});

const CoordinatedConstituencyParseFactory = (constituencyParse: ConstituencyParse): CoordinatedConstituencyParse => {
  return ({
    coordinated_syntax_tree: hierarchy(constituencyParse.syntax_tree),
    ...constituencyParse
  });
}

const CoordinatedSemanticTreeFactory = (semanticTree: SemanticTree): CoordinatedSemanticTree => hierarchy(semanticTree);

export class FragmentStore {
  authors: Author[] = []
  fragment: Fragment | null = null

  initialGrammar?: string

  exampleMap: ExampleMap = {}
  temporaryExampleMap: TemporaryExampleMap = {}
  interpretationMap: InterpretationMap = {}
  temporaryInterpretationMap: TemporaryInterpretationMap = {}
  constituencyParseMap: ConstituencyParseMap = {}
  semanticTreeMap: SemanticTreeMap = {}

  constructor() {
    makeAutoObservable(this);
  }

  get examples () {
    return values(this.exampleMap);
  }

  get temporaryExamples () {
    return values(this.temporaryExampleMap);
  }

  get interpretations () {
    return values(this.interpretationMap);
  }

  get temporaryInterpretations () {
    return values(this.temporaryInterpretationMap);
  }

  get constituencyParses () {
    return values(this.constituencyParseMap);
  }

  setExamples = (examples: Example[]) => {
    this.exampleMap = createIdMap(examples);
  }

  setInterpretation = (interpretation: Interpretation) => {
    this.interpretationMap[interpretation.id] = interpretation;

    if (interpretation.constituency_parse)
      this.setConstituencyParse(interpretation.constituency_parse);
  }

  setInterpretations = (interpretations: Interpretation[]) => {
    for (const i of interpretations) this.setInterpretation(i);
  }

  setConstituencyParse = (constituencyParse: ConstituencyParse) => {
    this.constituencyParseMap[constituencyParse.id] = CoordinatedConstituencyParseFactory(
      constituencyParse
    );
  }

  exampleInterpretations = (exampleId: ID) => {
    const interpretations = this.interpretations.filter(({ example_id }) => example_id === exampleId);
    return orderBy(interpretations, ["id"], ["desc"]);
  }

  exampleTemporaryInterpretations (exampleId: ID) {
    return this.temporaryInterpretations.filter(({ example_id }) => example_id === exampleId);
  }

  createTemporaryExample = () => {
    if (!this.fragment) throw new Error("Can't create an example without a fragment!");

    const prevExample = this.temporaryExamples.length
      ? this.temporaryExamples[this.temporaryExamples.length - 1]
      : this.examples[this.examples.length - 1];
  
    const temporaryExample = TemporaryExampleFactory(this.fragment.id, prevExample);
    this.temporaryExampleMap[temporaryExample.temp_id] = temporaryExample;

    return temporaryExample;
  }

  removeTemporaryExample = (exampleId: UUID) => {
    remove(this.temporaryExampleMap, exampleId);
  }

  createTemporaryInterpretation = (exampleId: ID) => {
    const temporaryInterpretation = TemporaryInterpretationFactory(exampleId);

    this.temporaryInterpretationMap[temporaryInterpretation.temp_id] = temporaryInterpretation;

    return temporaryInterpretation;
  }

  removeTemporaryInterpretation = (interpretationId: UUID) => {
    remove(this.temporaryInterpretationMap, interpretationId);
  }

  findConstituencyParseNode = (constituencyParseId: ID, nodeId: TreeID) => {
    const parse = this.constituencyParseMap[constituencyParseId];
    const tree = parse.coordinated_syntax_tree.copy();
    const node = tree.findById(nodeId);

    if (!parse) throw new Error(`No example with id:${constituencyParseId}!`);
    if (!node) throw new Error(`No node for example (${constituencyParseId}) with id:${nodeId}`);

    return { parse, tree, node };
  }

  updateConstituencyParseNode = (exampleId: ID, values: ConstituencyParseNodeEditValues) => {
    const { parse, tree, node } = this.findConstituencyParseNode(exampleId, values.id);

    node.data.label = values.label;

    parse.coordinated_syntax_tree = hierarchy(tree.data);
  }

  removeConstituencyParseNode = (exampleId: ID, nodeId: TreeID) => {
    const { parse, tree, node } = this.findConstituencyParseNode(exampleId, nodeId);

    tree.detach(node);

    parse.coordinated_syntax_tree = hierarchy(tree.data);
  }

  addConstituencyParseNode = (exampleId: ID, parentNodeId: TreeID) => {
    const { parse, tree, node: parent } = this.findConstituencyParseNode(exampleId, parentNodeId);
    const newNode = SyntaxTreeNodeFactory();

    const newCoordinatedNodeId = tree.attach(parent, newNode);

    parse.coordinated_syntax_tree = hierarchy(tree.data);

    return parse.coordinated_syntax_tree.findById(newCoordinatedNodeId);
  }

  moveConstituencyParseNode = (exampleId: ID, nodeId: TreeID, targetParentId: TreeID) => {
    const { parse, tree, node: parent } = this.findConstituencyParseNode(exampleId, targetParentId);
    const child = tree.findById(nodeId);

    if (!child) throw new Error(`No node for example (${exampleId}) with id:${nodeId}`);

    tree.detach(child);
    tree.attach(parent, child.data);

    parse.coordinated_syntax_tree = hierarchy(tree.data);
  }

  dispatchFetchFragment = async (fragmentSlug: Slug) => {
    this.fragment = await fetchFragment(fragmentSlug);

    if (!this.fragment) throw new Error(`Fragment ${fragmentSlug} not found!`);

    const [examples, grammar] = await Promise.all([
      fetchExamples(this.fragment.id),
      this.dispatchFetchFragmentGrammar(this.fragment)
    ]);

    this.initialGrammar = grammar ?? "-- write your fragment here...";
    
    if (!examples.length) return;

    this.setExamples(examples);

    for (const example of examples) {
      const interpretations = await fetchInterpretations(example.id);
      if (!interpretations) continue
      this.setInterpretations(interpretations);
    }

    return this.fragment;
  }

  dispatchFetchFragmentGrammar = async (fragment: Fragment) => {
    return fetchFragmentGrammar(fragmentGrammarFilename(fragment));
  }

  dispatchUpdateFragmentGrammar = async (value: string) => {
    if (!this.fragment) throw new Error("Can't update grammar of nonexistent fragment!");
  
    return updateFragmentGrammar(
      fragmentGrammarFilename(this.fragment),
      value
    );
  }

  dispatchUpdateExample = async (exampleId: ID, values: ExampleEditValues) => {
    const updatedExample = await updateExample(exampleId, values);
    this.exampleMap[exampleId] = updatedExample;
    return updatedExample;
  }

  dispatchCreateExample = async (temporaryExampleId: UUID, values: ExampleCreateValues) => {
    const example = await createExample(values);
    this.exampleMap[example.id] = example;
    this.removeTemporaryExample(temporaryExampleId);
    return example;
  }

  dispatchDeleteExample = async (exampleId: ID) => {
    await deleteExample(exampleId);
    // FIXME: TS: mobx defaults the key type to string and doesn't infer the
    // the type of the auto observed map.
    remove<ID, Example>(this.exampleMap as unknown as ObservableMap<ID, Example>, exampleId);
  }

  dispatchUpdateInterpretation = async (interpretationId: ID, values: InterpretationEditValues) => {
    const updatedInterpretation = await updateInterpretation(interpretationId, values);
    this.interpretationMap[interpretationId] = updatedInterpretation;
    return updatedInterpretation;
  }

  dispatchCreateInterpretation = async (temporaryInterpretationId: UUID, values: InterpretationCreateValues) => {
    const interpretation = await createInterpretation(values);
    this.interpretationMap[interpretation.id] = interpretation;
    this.removeTemporaryInterpretation(temporaryInterpretationId);
    return interpretation;
  }

  dispatchDeleteInterpretation = async (interpretationId: ID) => {
    await deleteInterpretation(interpretationId);
    // FIXME: ibid
    remove<ID, Interpretation>(this.interpretationMap as unknown as ObservableMap<ID, Interpretation>, interpretationId);
  }

  dispatchApproximateExampleConstituency = async (exampleId: ID) => {
    const constituencyParse = await createConstituencyParse(exampleId);
    this.setConstituencyParse(constituencyParse);
    return this.constituencyParses[constituencyParse.id];
  }

  dispatchDeleteConstituencyParse = async (constituencyParseId: ID) => {
    await deleteConstituencyParse(constituencyParseId);
    // FIXME: ibid
    remove<ID, ConstituencyParse>(this.constituencyParseMap as unknown as ObservableMap<ID, ConstituencyParse>, constituencyParseId);
  }

  dispatchUpdateConstituencyParse = async (constituencyParseId: ID, values: ConstituencyParseEditValues) => {
    const updatedConstituencyParse = await updateConstituencyParse(constituencyParseId, values);
    this.setConstituencyParse(updatedConstituencyParse);
    return this.constituencyParseMap[updatedConstituencyParse.id];
  }

  dispatchInterpretConstituencyParse = async (constituencyParse: CoordinatedConstituencyParse) => {
    if (!this.fragment) throw new Error("No fragment to interpret!");

    const semanticTree = await fetchSemanticTree(this.fragment, constituencyParse.parse_string);

    this.semanticTreeMap[constituencyParse.interpretation_id] = CoordinatedSemanticTreeFactory(semanticTree);

    return this.semanticTreeMap[constituencyParse.interpretation_id];
  }
}

