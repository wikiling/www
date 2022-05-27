import React from 'react';
import { NODE_HEIGHT } from './config';
import { CoordinatedTreeLink } from './types';

type EdgeProps = {
  link: CoordinatedTreeLink
}

const Edge: React.FC<EdgeProps> = ({ link }) => {
  const { source, target } = link;
  console.log(target.data.id)
  return (
    <line
      className="edge"
      x1={source.x}
      y1={source.y + NODE_HEIGHT / 2}
      x2={target.x}
      y2={target.y - NODE_HEIGHT}
      stroke="black"
    />
  )
};

export default Edge