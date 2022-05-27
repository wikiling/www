import React from 'react';
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
      y1={source.y}
      x2={target.x}
      y2={target.y}
      stroke="black"
    />
  )
};

export default Edge