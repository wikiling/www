import React from 'react';

type NodeProps = {
  label: string
  x: string
  y: string
}

const Node: React.FC<NodeProps> = ({ x, y, label }) => {
  return (
    <text x={x} y={y}>{label}</text>
  )
};

export default Node