import { SubjectPosition } from 'd3-drag';
import { tree as tidyTreeLayout } from 'd3-hierarchy';
import { CoordinatedSyntaxTree, SyntaxTree } from 'types';
import { getTextDimensions } from 'utils/document';
import { NODE_HEIGHT, NODE_SEP_X, NODE_SEP_Y, NODE_WIDTH } from './config';
import { CoordinatedTreeNode } from './types';

type ComputeLayout = (syntaxTree: CoordinatedSyntaxTree) => CoordinatedTreeNode

export const computeLayout: ComputeLayout = (syntaxTree) => {
  const createTreeLayout = tidyTreeLayout<SyntaxTree>()
    .nodeSize([
      NODE_WIDTH + NODE_SEP_X,
      NODE_HEIGHT + NODE_SEP_Y
    ])
    .separation((a, b) => {
      const halfWidthA = getTextDimensions(a.data.label).width / 2;
      const halfWidthB = getTextDimensions(b.data.label).width / 2;
      const diff = NODE_WIDTH + NODE_SEP_X - (halfWidthA + halfWidthB);

      return diff < 0 ? 1 + (-1 * diff / NODE_WIDTH) : 1;
    });

  return createTreeLayout(syntaxTree);
}

export const translateTree = (root: CoordinatedTreeNode, dx: number, dy: number) => {
  const translateCoordinates = (subject: SubjectPosition) => {
    subject.x += dx;
    subject.y += dy;
  };

  root.descendants().forEach(translateCoordinates);
}
