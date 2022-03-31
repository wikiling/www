import { HierarchyPointLink, HierarchyPointNode } from 'd3-hierarchy';

export type TreeData = {
  id: string
  text: string
  children?: TreeData[]
}

export type TreeNode = HierarchyPointNode<TreeData>
export type TreeLink = HierarchyPointLink<TreeData>