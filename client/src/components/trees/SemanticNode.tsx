import React, { forwardRef } from 'react';
import { SemanticTree, TreeID } from 'types';
import { getTextDimensions } from 'utils/document';
import { NODE_LINE_HEIGHT, SEM_NODE_HEIGHT } from './config';
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
  const { id: nodeId, value: nodeValue, constituencyLabel: nodeConstituencyLabel } = node.data;
  const id = `${treeId}-${nodeId}`;

  const { width: constituencyLabelWidth } = getTextDimensions(nodeConstituencyLabel);

  const offset = 5;

  const constituencyLabelX = node.x - constituencyLabelWidth / 2,
        constituencyLabelY = node.y - NODE_LINE_HEIGHT + offset;

  const { width: typeWidth } = getTextDimensions(node.data.type);
  const typeX = node.x - typeWidth / 2,
        typeY = node.y + offset;

  const { width: valueWidth } = getTextDimensions(nodeValue);
  const valueX = node.x - valueWidth / 2,
        valueY = node.y + NODE_LINE_HEIGHT + offset;

  return (
    <g ref={ref} className={`node ${className}`} data-id={id}>
      {nodeValue === nodeConstituencyLabel
        ? <text x={valueX} y={node.y}>
            {nodeValue}
          </text>
        : <>
            <text x={constituencyLabelX} y={constituencyLabelY}>
              {nodeConstituencyLabel}
            </text>
            <text x={typeX} y={typeY} fontWeight="500">
              {node.data.type}
            </text>
            <text x={valueX} y={valueY} fontWeight="500">
              {nodeValue}
            </text>
          </>
      }
    </g>
  );
});

export default SemanticNode