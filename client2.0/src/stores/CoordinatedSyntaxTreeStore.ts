
import { makeAutoObservable, ObservableMap, remove } from 'mobx';
import { ID, Author, Fragment, Slug, Example, CoordinatedConstituencyParse, ConstituencyParse, SyntaxTreeID, EditableConstituencyParseNodeValues, UnidentifiedExample, EditableExampleValues, EditableConstituencyParseValues } from 'types';
import { fetchFragment, fetchInterpretation, fetchExamples, fetchConstituencyParses, updateExample, createConstituencyParse, deleteConstituencyParse, updateConstituencyParse } from 'api';
import { hierarchy } from 'utils/hierarchy';
import { createIdMap } from 'utils/store';
import { CoordinatedTreeNode } from 'components/tree/types';

type ExampleMap = {[key: ID]: Example}
type ConstituencyParseMap = {[key: ID]: CoordinatedConstituencyParse}

const { values } = Object;

type MenuCoordinates = {
  left: string
  top: string
}

/*

export class CoordinatedSyntaxTreeStore {
  coordinatedRootNode: CoordinatedTreeNode | null = null
  menuCoordinates: MenuCoordinates | null = null;
  
  menuNode, setMenuNode] = useState<CoordinatedTreeNode | null>(null);
  editNode, setEditNode] = useState<CoordinatedTreeNode | null>(null);
  dragNode, setDragNode] = useState<CoordinatedTreeNode | null>(null);
  potentialParentNode, setPotentialParentNode] = useState<CoordinatedTreeNode | null>(null);
  const editNodeRef = useRef<HTMLFormElement>(null);
  const [groupTransform, setGroupTransform] = useState<string>(groupTransformTmpl());

  constructor() {
    makeAutoObservable(this);
  }
}

*/