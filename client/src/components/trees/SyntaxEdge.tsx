import React from 'react';
import { SYN_NODE_HEIGHT } from './config';
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
      y1={source.y + SYN_NODE_HEIGHT / 2}
      x2={target.x}
      y2={target.y - SYN_NODE_HEIGHT / 2}
      stroke="black"
    />
  )
};

export default Edge