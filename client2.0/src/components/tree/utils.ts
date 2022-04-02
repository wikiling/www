import { SubjectPosition } from 'd3-drag';
import { HierarchyPointLink, HierarchyPointNode, tree as tidyTreeLayout } from 'd3-hierarchy';
import { useEffect, useState } from 'react';
import { SyntaxTree } from 'types';
import { getTextWidth } from 'utils/document';
import { NODE_HEIGHT, NODE_SEP_X, NODE_SEP_Y, NODE_WIDTH } from './config';
import { TreeData, CoordinatedTreeLink, CoordinatedTreeNode } from './types';

type ComputeLayout = (syntaxTree: SyntaxTree) => CoordinatedTreeNode

export const computeLayout: ComputeLayout = (syntaxTree: SyntaxTree) => {
  const createTreeLayout = tidyTreeLayout<TreeData>()
    .nodeSize([
      NODE_WIDTH + NODE_SEP_X,
      NODE_HEIGHT + NODE_SEP_Y
    ])
    .separation((a, b) => {
      const halfWidthA = getTextWidth(a.data.text) / 2;
      const halfWidthB = getTextWidth(b.data.text) / 2;
      
      if (halfWidthA + halfWidthB > NODE_WIDTH + NODE_SEP_X) {
        return 1.5;
      }

      return a.parent === b.parent ? 1 : 1.25;
    });

  return createTreeLayout(syntaxTree);
}

export const translateSubtree = (root: CoordinatedTreeNode, dx: number, dy: number) => {
  const translateSubjectPosition = (subject: SubjectPosition) => {
    subject.x += dx;
    subject.y += dy;

    console.log('--->', subject.x, subject.y)
  };

  root.descendants().forEach(translateSubjectPosition);
  root.links().forEach((link) => {
    translateSubjectPosition(link.source);
    translateSubjectPosition(link.target);
  })

  return root
}

export const replaceSubtree = (tree: CoordinatedTreeNode, newSubtree: CoordinatedTreeNode) => {
  const copy = tree.copy();

  const currentSubtree = copy.find((node) => node.data.id === newSubtree.data.id);

  if (!currentSubtree) throw `Can't find subtree (${newSubtree.data.id}) to replace!`;
  if (!currentSubtree.parent) throw `Can't replace the root!`;

  const nodeIdx = currentSubtree.parent.children!.indexOf(currentSubtree);
  currentSubtree.parent.children!.splice(nodeIdx, 1, newSubtree);

  return tree;
}
