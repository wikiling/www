
import { makeAutoObservable, ObservableMap, remove } from 'mobx';
import { ID, Author, Fragment, Slug, Example, CoordinatedConstituencyParse, ConstituencyParse, SyntaxTreeID, ConstituencyParseNodeEditValues, TemporaryExample, ExampleEditValues, ConstituencyParseEditValues, UUID, ExampleCreateValues } from 'types';
import { fetchFragment, fetchInterpretation, fetchExamples, fetchConstituencyParses, updateExample, deleteExample, createConstituencyParse, deleteConstituencyParse, updateConstituencyParse, createExample } from 'api';
import { hierarchy } from 'utils/hierarchy';
import { createIdMap } from 'utils/store';
import { v4 as uuid } from 'uuid';

type ExampleMap = {[key: ID]: Example}
type TemporaryExampleMap = {[key: UUID]: TemporaryExample}
type ConstituencyParseMap = {[key: ID]: CoordinatedConstituencyParse}

const { values } = Object;

const SyntaxTreeNodeFactory = () => ({
  id: "",
  text: ""
});

const TemporaryExampleFactory = (fragment_id: ID): TemporaryExample => ({
  fragment_id,
  content: '',
  label: '',
  description: '',
  temp_id: uuid()
});

const CoordinatedConstituencyParseFactory = (constituencyParse: ConstituencyParse): CoordinatedConstituencyParse => {
  console.log(constituencyParse.syntax_tree);

  return ({
    coordinated_syntax_tree: hierarchy(constituencyParse.syntax_tree),
    ...constituencyParse
  });
}

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
}

export class FragmentStore {
  authors: Author[] = []
  fragment: Fragment | null = null

  exampleMap: ExampleMap = {}
  constituencyParseMap: ConstituencyParseMap = {}

  temporaryExampleMap: TemporaryExampleMap = {}

  constructor() {
    makeAutoObservable(this);
  }

  get examples () {
    return values(this.exampleMap);
  }

  get temporaryExamples () {
    return values(this.temporaryExampleMap);
  }

  get constituencyParses () {
    return values(this.constituencyParseMap);
  }

  setExamples = (examples: Example[]) => {
    this.exampleMap = createIdMap(examples);
  }

  setConstituencyParse = (constituencyParse: ConstituencyParse) => {
    this.constituencyParseMap[constituencyParse.id] = CoordinatedConstituencyParseFactory(
      constituencyParse
    );
  }

  setConstituencyParses = (constituencyParses: ConstituencyParse[]) => {
    for (const cp of constituencyParses) this.setConstituencyParse(cp);
  }

  exampleConstituencyParses = (exampleId: ID) => {
    return this.constituencyParses.filter(({ example_id }) => example_id === exampleId);
  }

  createTemporaryExample = () => {
    if (!this.fragment) throw new Error("Can't create an example without a fragment!");

    const temporaryExample = TemporaryExampleFactory(this.fragment.id);
    const lastExample = this.temporaryExamples.length
      ? this.temporaryExamples[this.temporaryExamples.length - 1]
      : this.examples[this.examples.length - 1]

    temporaryExample.label = getNextLabel(lastExample);

    this.temporaryExampleMap[temporaryExample.temp_id] = temporaryExample;

    return temporaryExample;
  }

  findConstituencyParseNode = (constituencyParseId: ID, nodeId: SyntaxTreeID) => {
    const parse = this.constituencyParseMap[constituencyParseId];
    const tree = parse.coordinated_syntax_tree.copy();
    const node = tree.findById(nodeId);

    if (!parse) throw new Error(`No example with id:${constituencyParseId}!`);
    if (!node) throw new Error(`No node for example (${constituencyParseId}) with id:${nodeId}`);

    return { parse, tree, node };
  }

  updateConstituencyParseNode = (exampleId: ID, values: ConstituencyParseNodeEditValues) => {
    const { parse, tree, node } = this.findConstituencyParseNode(exampleId, values.nodeId);

    if (!!node.data.pos) node.data.pos = values.nodeText
    else node.data.token = values.nodeText;

    parse.coordinated_syntax_tree = hierarchy(tree.data);
  }

  removeConstituencyParseNode = (exampleId: ID, nodeId: SyntaxTreeID) => {
    const { parse, tree, node } = this.findConstituencyParseNode(exampleId, nodeId);

    tree.detach(node);

    parse.coordinated_syntax_tree = hierarchy(tree.data);
  }

  addConstituencyParseNode = (exampleId: ID, parentNodeId: SyntaxTreeID) => {
    const { parse, tree, node: parent } = this.findConstituencyParseNode(exampleId, parentNodeId);
    const newNode = SyntaxTreeNodeFactory();

    const newCoordinatedNodeId = tree.attach(parent, newNode);

    parse.coordinated_syntax_tree = hierarchy(tree.data);

    return parse.coordinated_syntax_tree.findById(newCoordinatedNodeId);
  }

  moveConstituencyParseNode = (exampleId: ID, nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => {
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

    const examples = await fetchExamples(this.fragment.id);
    
    if (!examples.length) return;

    this.setExamples(examples);

    for (const example of examples) {
      const constituencyParses = await fetchConstituencyParses(example.id);
      if (!constituencyParses) continue
      this.setConstituencyParses(constituencyParses);
    }

    return this.fragment;
  }

  dispatchUpdateExample = async (exampleId: ID, values: ExampleEditValues) => {
    const updatedExample = await updateExample(exampleId, values);
    this.exampleMap[exampleId] = updatedExample;
    return updatedExample;
  }

  dispatchCreateExample = async (temporaryExampleId: UUID, values: ExampleCreateValues) => {
    const example = await createExample(values);
    this.exampleMap[example.id] = example;
    remove(this.temporaryExampleMap, temporaryExampleId);
    return example;
  }

  dispatchDeleteExample = async (exampleId: ID) => {
    await deleteExample(exampleId);
    // FIXME: mobx defaults the key type to string and doesn't infer the
    // the type of the auto observed map.
    remove<ID, Example>(this.exampleMap as unknown as ObservableMap<ID, Example>, exampleId);
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

  dispatchInterpretConstituencyParse = (constituencyParse: CoordinatedConstituencyParse) => {
    if (!this.fragment) throw new Error("No fragment to interpret!");

    return fetchInterpretation(this.fragment, constituencyParse.coordinated_syntax_tree.data);
  }
}

