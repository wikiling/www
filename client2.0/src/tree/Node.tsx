import React, { useState } from 'react';
import { useForm } from 'react-hook-form';
import { getTextWidth } from 'utils';
import Menu from './Menu';
import { TreeNode } from './types';

type NodeProps = {
  node: TreeNode
  onClick: React.MouseEventHandler
}

const Node: React.FC<NodeProps> = ({ node, onClick }) => {
  const { text } = node.data;
  const nodeWidth = getTextWidth(text);
  const [x, y] = [node.x - nodeWidth / 2, node.y];

  return (
    <text onClick={onClick} x={x} y={y} className="node">
      {node.data.text}
    </text>
  );
};

export default Node