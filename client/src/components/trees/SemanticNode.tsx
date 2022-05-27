import React, { forwardRef, useState, useEffect } from 'react';
import { drag } from 'd3-drag';
import { select } from 'd3-selection';
import { ID, SemanticTree, TreeID } from 'types';
import { getTextDimensions } from 'utils/document';
import { NodeDragHandler, CoordinatedTreeNode, NodeDragEvent } from './types';
import { NODE_RADIUS } from './config';
import { HierarchyPointNode } from 'd3-hierarchy';

export type CoordinatedSemanticTreeNode = HierarchyPointNode<SemanticTree>

export type SemanticNodeProps = {
  treeId: TreeID
  node: CoordinatedSemanticTreeNode
  className?: string
  width: number
  height: number
}

const SemanticNode = forwardRef<
  SVGGElement, SemanticNodeProps
>(({ treeId, node, className = "" }, ref) => {
  const { id: nodeId, value: nodeLabel } = node.data;
  const id = `${treeId}-${nodeId}`;
  const { width: labelWidth, height: labelHeight } = getTextDimensions(nodeLabel);
  const labelX = node.x - labelWidth / 2, labelY = node.y + labelHeight / 2;

  return (
    <g ref={ref} className={`node ${className}`} data-id={id}>
      <circle className="node-circle" cx={node.x} cy={node.y} r={NODE_RADIUS} fill="white" strokeWidth="1"/>
      <text x={labelX} y={labelY} data-id={labelWidth}>
        {nodeLabel}
      </text>
    </g>
  );
});

export default SemanticNode