import { drag, DragContainerElement } from 'd3-drag';
import { select } from 'd3-selection';
import React, { forwardRef, useState } from 'react';
import { useEffect } from 'react';
import { useForm } from 'react-hook-form';
import { ID } from 'types';
import { getTextWidth } from 'utils/document';
import Menu from './Menu';
import { NodeDragHandler, CoordinatedTreeNode } from './types';

type NodeProps = {
  sentenceId: ID
  node: CoordinatedTreeNode
  onClick: React.MouseEventHandler
  width: number
  height: number
  onDragStart: NodeDragHandler
  onDragProceed: NodeDragHandler
  onDragEnd: NodeDragHandler
}

const Node = forwardRef<
  SVGGElement, NodeProps
>(({ sentenceId, node, onClick, height, width, onDragStart, onDragProceed, onDragEnd }, forwardRef) => {
  const { id: nodeId, text } = node.data;
  const id = `${sentenceId}-${nodeId}`;
  const textWidth = getTextWidth(text);
  const textX = node.x - textWidth / 2, textY = node.y;
  const rectX = node.x - width / 2, rectY = node.y - height / 2;
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
      <rect x={rectX} y={rectY} height={height} width={width} fill="white"/>
      <text x={textX} y={textY}>
        {text}
      </text>
    </g>
  );
});

export default Node