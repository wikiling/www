import React, { useState } from 'react';
import { useForm } from 'react-hook-form';
import Menu from './Menu';
import { TreeNode } from './types';

type NodeProps = {
  node: TreeNode
  onClick: React.MouseEventHandler
}

const Node: React.FC<NodeProps> = ({ node, onClick }) => {
  return (
    <text onClick={onClick} x={node.x} y={node.y} className="node">
      {node.data.text}
    </text>
  );
};

export default Node