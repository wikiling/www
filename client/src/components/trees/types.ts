import { D3DragEvent, SubjectPosition } from 'd3-drag';
import { HierarchyNode, HierarchyPointLink, HierarchyPointNode } from 'd3-hierarchy';
import { SyntaxTree, TreeID } from 'types';

export type TreeLinkData = {
  id: string
}

export type BaseNodeData = {
  id: TreeID
  label: string
}

export type CoordinatedTree = HierarchyNode<BaseNodeData>

export type EditableSyntaxNodeValues = BaseNodeData

export type CoordinatedTreeNode = HierarchyPointNode<BaseNodeData>
export type CoordinatedTreeLink = HierarchyPointLink<TreeLinkData>

export type NodeDragEvent = D3DragEvent<SVGGElement, SyntaxTree, SubjectPosition>
export type NodeDragHandler = (event: NodeDragEvent) => void