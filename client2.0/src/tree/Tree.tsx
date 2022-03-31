import React from 'react';
import { hierarchy, HierarchyPointNode, tree as d3Tree } from 'd3-hierarchy'
import { SyntaxTree } from 'types';
import Node from './Node';
import Edge from './Edge';

type TreeData = {
  text: string
  children?: TreeData[]
}

type TreeProps = {
  data: TreeData
};

const NODE_WIDTH = 30;
const NODE_HEIGHT = 75;
const NODE_SEP_Y = 16;
const NODE_SEP_X = 12;

const getTextWidth = (_: string) => {
  return 2;
}

const Tree: React.FC<TreeProps> = ({ data }) => {
  console.log(data)
  const hierarchicalData = hierarchy(data);

  const createTree = d3Tree<TreeData>()
    .nodeSize([
      NODE_WIDTH + NODE_SEP_X,
      NODE_HEIGHT + NODE_SEP_Y
    ])
    .separation((a: any, b: any) => {
      const halfWidthA = getTextWidth(a.data.text) / 2
      const halfWidthB = getTextWidth(b.data.text) / 2
      
      if (halfWidthA + halfWidthB > NODE_WIDTH + NODE_SEP_X) {
        return 1.5
      }
        
      return a.parent == b.parent ? 1 : 1.25;
    });

  const tree = createTree(hierarchicalData);
  const nodes = tree.descendants();
  const edges = tree.links();

  return (
    <svg>
      {nodes.map(node => (
        <Node></Node>
      ))}
      {edges.map(edge => {
        <Edge></Edge>
      })}
    </svg>
  )
};

export default Tree