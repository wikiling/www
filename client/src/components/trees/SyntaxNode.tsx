import React, { forwardRef, useState, useEffect } from 'react';
import { drag } from 'd3-drag';
import { select } from 'd3-selection';
import { ID } from 'types';
import { getTextDimensions } from 'utils/document';
import { NodeDragHandler, CoordinatedTreeNode, NodeDragEvent } from './types';
import { NODE_RADIUS } from './config';

type SyntaxNodeProps = {
  treeId: ID
  node: CoordinatedTreeNode
  className?: string
  width: number
  height: number
  onClick: React.MouseEventHandler
  onDragProceed: NodeDragHandler
  onDragEnd: NodeDragHandler
}

const SyntaxNode = forwardRef<
  SVGGElement, SyntaxNodeProps
>(({ treeId, node, onClick, onDragProceed, onDragEnd, className = "" }, ref) => {
  const { id: nodeId } = node.data;
  const id = `${treeId}-${nodeId}`;
  const { width: labelWidth, height: labelHeight } = getTextDimensions(node.data.id);
  const labelX = node.x - labelWidth / 2, labelY = node.y + labelHeight / 2;
  const [latestDragEvent, setLatestDragEvent] = useState<NodeDragEvent | null>(null);

  const dragHandler = drag();

  useEffect(() => {
    const selection = select<Element, any>(`g[data-id="${id}"]`);

    dragHandler(selection);
  
    dragHandler.on("drag", (e) => {
      onDragProceed(e);
      setLatestDragEvent(e);
    });
  }, []);

  // see https://github.com/d3/d3-drag#drag_on for why this contortion is necessary
  // briefly: the callback needs to be re-registered on each event in order to be fresh
  useEffect(() => {
    latestDragEvent?.on("end", onDragEnd);
  }, [onDragEnd, latestDragEvent])

  return (
    <g ref={ref} onClick={onClick} className={`node ${className}`} data-id={id}>
      <text x={labelX} y={labelY} data-id={labelWidth}>
        {node.data.id}
      </text>
    </g>
  );
});

export default SyntaxNode