import { HierarchyPointLink, HierarchyPointNode } from 'd3-hierarchy';
import { SyntaxTreeID } from 'types';

export type TreeData = {
  id: SyntaxTreeID
  text: string
  children?: TreeData[]
}

export type EditableNodeValues = {
  id: SyntaxTreeID
  text: string
}

export type TreeNode = HierarchyPointNode<TreeData>
export type TreeLink = HierarchyPointLink<TreeData>