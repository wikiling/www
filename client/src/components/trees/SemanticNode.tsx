import React, { forwardRef } from 'react';
import { SemanticTree, TreeID } from 'types';
import { getTextDimensions } from 'utils/document';
import { NODE_HEIGHT } from './config';
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

  const {
    width: constituencyLabelWidth,
    height: constituencyLabelHeight
  } = getTextDimensions(nodeConstituencyLabel);

  const constituencyLabelX = node.x - constituencyLabelWidth / 2,
        constituencyLabelY = node.y - constituencyLabelHeight * 0.75;

  const { width: valueWidth, height: valueHeight } = getTextDimensions(nodeValue);
  const valueX = node.x - valueWidth / 2,
        valueY = node.y + valueHeight / 2;

  return (
    <g ref={ref} className={`node ${className}`} data-id={id}>
      {nodeValue === nodeConstituencyLabel
        ? <text x={valueX} y={valueY - valueHeight}>
            {nodeValue}
          </text>
        : <>
            <text x={constituencyLabelX} y={constituencyLabelY}>
              {nodeConstituencyLabel}
            </text>
            <text x={valueX} y={valueY} fontWeight="500">
              {nodeValue}
            </text>
            <text x={valueX} y={valueY + 20} fontWeight="500">
              {node.data.type}
            </text>
          </>
      }
    </g>
  );
});

export default SemanticNode