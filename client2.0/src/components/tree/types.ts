import { D3DragEvent, SubjectPosition } from 'd3-drag';
import { HierarchyPointLink, HierarchyPointNode } from 'd3-hierarchy';
import { SyntaxTree, SyntaxTreeID } from 'types';

export type TreeData = {
  id: SyntaxTreeID
  text: string
  children?: TreeData[]
}

export type EditableNodeValues = {
  id: SyntaxTreeID
  text: string
}

export type CoordinatedTreeNode = HierarchyPointNode<TreeData>
export type CoordinatedTreeLink = HierarchyPointLink<TreeData>

export type NodeDragEvent = D3DragEvent<SVGGElement, SyntaxTree, SubjectPosition>
export type NodeDragHandler = (event: NodeDragEvent) => void