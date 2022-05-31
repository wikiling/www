import { SubjectPosition } from 'd3-drag';
import { HierarchyNode, HierarchyPointNode, IdentifiableNodeDatum, tree as tidyTreeLayout } from 'd3-hierarchy';
import { getTextDimensions } from 'utils/document';
import { NODE_SEP_X, NODE_SEP_Y, NODE_WIDTH } from './config';
import { CoordinatedTreeNode } from './types';

type ComputeLayoutProps<TreeData extends IdentifiableNodeDatum> = {
  tree: HierarchyNode<TreeData>
  getLabel: (tree: HierarchyNode<TreeData>) => string
  nodeHeight: number
  nodeWidth?: number
}

export const computeLayout = <TreeData extends IdentifiableNodeDatum>({
  tree,
  getLabel,
  nodeHeight,
  nodeWidth = NODE_WIDTH
}: ComputeLayoutProps<TreeData>): HierarchyPointNode<TreeData> => {
  const createTreeLayout = tidyTreeLayout<TreeData>()
    .nodeSize([
      nodeWidth + NODE_SEP_X,
      nodeHeight + NODE_SEP_Y
    ])
    .separation((a, b) => {
      const halfWidthA = getTextDimensions(getLabel(a)).width / 2;
      const halfWidthB = getTextDimensions(getLabel(b)).width / 2;
      const diff = NODE_WIDTH + NODE_SEP_X - (halfWidthA + halfWidthB);

      return diff < 0 ? 1 + (-1 * diff / NODE_WIDTH) : 1;
    });

  return createTreeLayout(tree);
}

export const translateTree = (root: CoordinatedTreeNode, dx: number, dy: number) => {
  const translateCoordinates = (subject: SubjectPosition) => {
    subject.x += dx;
    subject.y += dy;
  };

  root.descendants().forEach(translateCoordinates);
}
