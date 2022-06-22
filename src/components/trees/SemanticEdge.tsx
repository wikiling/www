import React from 'react';
import { NODE_LINE_HEIGHT, SEM_NODE_HEIGHT } from './config';
import { CoordinatedTreeLink } from './types';

type EdgeProps = {
  link: CoordinatedTreeLink
}

const Edge: React.FC<EdgeProps> = ({ link }) => {
  const { source, target } = link;

  return (
    <line
      className="edge"
      x1={source.x}
      y1={source.y + NODE_LINE_HEIGHT * 1.5}
      x2={target.x}
      y2={target.y - NODE_LINE_HEIGHT * 1.5 }
      stroke="black"
    />
  )
};

export default Edge