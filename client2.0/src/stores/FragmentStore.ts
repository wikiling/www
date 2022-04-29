
import { makeAutoObservable } from 'mobx';
import { ID, Author, Fragment, Slug, Example, CoordinatedConstituencyParse, ConstituencyParse, SyntaxTreeID } from 'types';
import { fetchFragment, fetchInterpretation, fetchExamples, fetchConstituencyParses } from 'api';
import { hierarchy } from 'utils/hierarchy';
import { createIdMap } from 'utils/store';

type ExampleMap = {[key: ID]: Example}
type ConstituencyParseMap = {[key: ID]: CoordinatedConstituencyParse}

const { values } = Object;

const SyntaxTreeNodeFactory = () => ({
  id: "",
  text: ""
});

const CoordinatedConstituencyParseFactory = (constituencyParse: ConstituencyParse): CoordinatedConstituencyParse => ({
  coordinated_syntax_tree: hierarchy(constituencyParse.syntax_tree),
  ...constituencyParse
});

export class FragmentStore {
  authors: Author[] = []
  fragment: Fragment | null = null

  exampleMap: ExampleMap = {}
  constituencyParseMap: ConstituencyParseMap = {}

  constructor() {
    makeAutoObservable(this);
  }

  get examples () {
    return values(this.exampleMap);
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

  findConstituencyParseNode = (constituencyParseId: ID, nodeId: SyntaxTreeID) => {
    const parse = this.constituencyParseMap[constituencyParseId];
    const tree = parse.coordinated_syntax_tree.copy();
    const node = tree.findById(nodeId);

    if (!parse) throw new Error(`No example with id:${constituencyParseId}!`);
    if (!node) throw new Error(`No node for example (${constituencyParseId}) with id:${nodeId}`);

    return { parse, tree, node };
  }

  updateConstituencyParseNodeText = (exampleId: ID, nodeId: SyntaxTreeID, text: string) => {
    const { parse, tree, node } = this.findConstituencyParseNode(exampleId, nodeId);

    if (!!node.data.pos) node.data.pos = text
    else node.data.token = text;

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

    tree.attach(parent, newNode);

    parse.coordinated_syntax_tree = hierarchy(tree.data);
  }

  moveConstituencyParseNode = (exampleId: ID, nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => {
    console.log('moving...', nodeId, targetParentId);

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
  }

  dispatchInterpretConstituencyParse = (fragment: Fragment, constituencyParse: ConstituencyParse) => {
    return fetchInterpretation(fragment, constituencyParse.syntax_tree);
  }
}

