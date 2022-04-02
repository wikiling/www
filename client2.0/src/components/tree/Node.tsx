import React, { forwardRef, useState } from 'react';
import { useForm } from 'react-hook-form';
import { getTextWidth } from 'utils/document';
import Menu from './Menu';
import { TreeNode } from './types';

type NodeProps = {
  node: TreeNode
  onClick: React.MouseEventHandler
  width: number
  height?: number
}

/*
    .append('rect')
    .classed('node', true)
    .attr('width', width)
    .attr('height', 25)
    .attr('x', function(d) {return d.x - width / 2})
    .attr('y', function(d) {return d.y - height / 2})
    .attr("fill", "white")
*/

const DEFUALT_HEIGHT = 25;

const Node = forwardRef<
  SVGGElement, NodeProps
>(({ node, onClick, height = DEFUALT_HEIGHT, width }, ref) => {
  const { text } = node.data;
  const textWidth = getTextWidth(text);
  const textX = node.x - textWidth / 2, textY = node.y;
  const rectX = node.x - width / 2, rectY = node.y - height / 2;

  return (
    <g ref={ref} onClick={onClick} className="node" data-id={node.data.id}>
      <rect x={rectX} y={rectY} height={height} width={width} fill="white"/>
      <text x={textX} y={textY}>
        {node.data.text}
      </text>
    </g>
  );
});

export default Node