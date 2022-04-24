import { SubjectPosition } from 'd3-drag';
import { tree as tidyTreeLayout } from 'd3-hierarchy';
import { CoordinatedSyntaxTree, SyntaxTree } from 'types';
import { getTextDimensions } from 'utils/document';
import { NODE_HEIGHT, NODE_SEP_X, NODE_SEP_Y, NODE_WIDTH } from './config';
import { CoordinatedTreeNode } from './types';

type ComputeLayout = (syntaxTree: CoordinatedSyntaxTree) => CoordinatedTreeNode

export const discriminateNodeTextField = (node: CoordinatedTreeNode) => node.data.pos ? 'pos' : 'token';

export const nodeText = (node: CoordinatedTreeNode) => node.data.pos
  ? node.data.pos
  : node.data.token
    ? node.data.token
    : "";

export const computeLayout: ComputeLayout = (syntaxTree) => {
  const createTreeLayout = tidyTreeLayout<SyntaxTree>()
    .nodeSize([
      NODE_WIDTH + NODE_SEP_X,
      NODE_HEIGHT + NODE_SEP_Y
    ])
    .separation((a, b) => {
      const halfWidthA = getTextDimensions(nodeText(a)).width / 2;
      const halfWidthB = getTextDimensions(nodeText(b)).width / 2;
      
      if (halfWidthA + halfWidthB > NODE_WIDTH + NODE_SEP_X) {
        return 1.5;
      }

      return a.parent === b.parent ? 1 : 1.25;
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
