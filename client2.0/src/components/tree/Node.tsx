import { drag, DragContainerElement } from 'd3-drag';
import { select } from 'd3-selection';
import React, { forwardRef, useState } from 'react';
import { useEffect } from 'react';
import { useForm } from 'react-hook-form';
import { ID } from 'types';
import { getTextDimensions } from 'utils/document';
import Menu from './Menu';
import { NodeDragHandler, CoordinatedTreeNode } from './types';

type NodeProps = {
  treeId: ID
  node: CoordinatedTreeNode
  onClick: React.MouseEventHandler
  width: number
  height: number
  onDragStart: NodeDragHandler
  onDragProceed: NodeDragHandler
  onDragEnd: NodeDragHandler
}

const NODE_RADIUS = 15;

const Node = forwardRef<
  SVGGElement, NodeProps
>(({ treeId, node, onClick, height, width, onDragStart, onDragProceed, onDragEnd }, forwardRef) => {
  const { id: nodeId, text } = node.data;
  const id = `${treeId}-${nodeId}`;
  const { width: textWidth, height: textHeight } = getTextDimensions(text);
  const textX = node.x - textWidth / 2, textY = node.y + textHeight /2;

  const dragHandler = drag();

  useEffect(() => {
    const selection = select<Element, any>(`g[data-id="${id}"]`);

    dragHandler(selection);
  
    dragHandler.on("start", onDragStart)
               .on("drag", onDragProceed)
               .on("end", onDragEnd);
  }, []);

  return (
    <g ref={forwardRef} onClick={onClick} className="node" data-id={id}>
      <circle cx={node.x} cy={node.y} r={NODE_RADIUS} fill="white"/>
      <text x={textX} y={textY} data-id={textWidth}>
        {text}
      </text>
    </g>
  );
});

export default Node