import { D3DragEvent, SubjectPosition } from 'd3-drag';
import { HierarchyPointLink, HierarchyPointNode } from 'd3-hierarchy';
import { SyntaxTree, SyntaxTreeID } from 'types';

export type TreeLinkData = {
  id: string
}

export type EditableNodeValues = {
  id: SyntaxTreeID
  text: string
}

export type CoordinatedTreeNode = HierarchyPointNode<SyntaxTree>
export type CoordinatedTreeLink = HierarchyPointLink<TreeLinkData>

export type NodeDragEvent = D3DragEvent<SVGGElement, SyntaxTree, SubjectPosition>
export type NodeDragHandler = (event: NodeDragEvent) => void