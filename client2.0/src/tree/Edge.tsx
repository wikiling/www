import React from 'react';

type EdgeProps = {
  x1: string
  y1: string
  x2: string
  y2: string
}

const Edge: React.FC<EdgeProps> = (edgeProps) => {
  return (
    <line stroke="black" {...edgeProps} />
  )
};

export default Edge