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
  const data = node.data;
  const id = `${treeId}-${data.id}`;

  const { width: syntaxLabelWidth } = getTextDimensions(data.syntaxLabel);

  const offset = 5;

  const syntaxLabelX = node.x - syntaxLabelWidth / 2,
        syntaxLabelY = node.y - NODE_LINE_HEIGHT + offset;

  const { width: typeWidth } = getTextDimensions(data.type);
  const typeX = node.x - typeWidth / 2,
        typeY = node.y + offset;

  const { width: valueWidth } = getTextDimensions(data.value);
  const valueX = node.x - valueWidth / 2,
        valueY = node.y + NODE_LINE_HEIGHT + offset;

  const { width: valuationErrorWidth } = getTextDimensions(data.valuationError);
  const valuationErrorX = node.x - valuationErrorWidth / 2;

  const { width: typeErrorWidth } = getTextDimensions(data.typeError);
  const typeErrorX = node.x - typeErrorWidth / 2;

  const { width: exprWidth } = getTextDimensions(data.expr);
  const exprX = node.x - exprWidth / 2;

  return (
    <g ref={ref} className={`node ${className}`} data-id={id}>
      <text x={syntaxLabelX} y={syntaxLabelY}>
        {data.syntaxLabel}
      </text>
      {data.type &&
        <text x={typeX} y={typeY} fontWeight="500">
          {data.type}
        </text>}
      {data.typeError &&
        <text x={typeErrorX} y={typeY} fill="red" fontWeight="500">
          {data.typeError}
        </text>}
      {data.value &&
        <text x={valueX} y={valueY} fontWeight="500">
          {data.value}
        </text>}
      {data.valuationError &&
        <text x={valuationErrorX} y={valueY} fill="red" fontWeight="500">
          {data.valuationError}
        </text>}

      {(data.typeError || data.valuationError) &&
        <text x={exprX} y={valueY + NODE_LINE_HEIGHT} fontWeight="500" fill="green">
          {data.expr}
        </text>
      }
    </g>
  );
});

export default SemanticNode