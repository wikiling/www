import React, { useRef, useState } from 'react';
import './Tree.scss';
import { hierarchy, HierarchyPointNode, tree as d3Tree } from 'd3-hierarchy';
import { SyntaxTree, SyntaxTreeID } from 'types';
import Node from './Node';
import Edge from './Edge';
import { EditableNodeValues, TreeData, TreeNode } from './types';
import { getTextWidth } from 'utils';
import Menu from './Menu';
import EditableNode from './EditableNode';
import { useClickAway } from 'react-use';

type TreeProps = {
  data: TreeData
  onNodeAdd: (node: TreeNode) => void
  onNodeEdit: (values: EditableNodeValues) => void
  onNodeRemove: (nodeId: SyntaxTreeID) => void
};

const NODE_WIDTH = 30;
const NODE_HEIGHT = 75;
const NODE_SEP_Y = 16;
const NODE_SEP_X = 12;

type MenuCoordinates = {
  left: string
  top: string
}

const Tree: React.FC<TreeProps> = ({ data, onNodeAdd, onNodeEdit, onNodeRemove }) => {
  const [menuCoordinates, setMenuCoordinates] = useState<MenuCoordinates | null>(null);
  const [activeNode, setActiveNode] = useState<TreeNode | null>(null);
  const [nodeInEdit, setNodeInEdit] = useState<TreeNode | null>(null);
  const editableNodeRef = useRef<HTMLFormElement>(null);
  const hierarchicalData = hierarchy(data);

  const createTree = d3Tree<TreeData>()
    .nodeSize([
      NODE_WIDTH + NODE_SEP_X,
      NODE_HEIGHT + NODE_SEP_Y
    ])
    .separation((a, b) => {
      const halfWidthA = getTextWidth(a.data.text) / 2;
      const halfWidthB = getTextWidth(b.data.text) / 2;
      
      if (halfWidthA + halfWidthB > NODE_WIDTH + NODE_SEP_X) {
        return 1.5;
      }
        
      return a.parent === b.parent ? 1 : 1.25;
    });

  const tree = createTree(hierarchicalData);
  const nodes = tree.descendants();
  const links = tree.links();

  const onMenuEdit = () => {
    if (!activeNode) throw "No active node to edit!";

    setNodeInEdit(activeNode);
    setMenuCoordinates(null);
  }

  const onMenuRemove = () => {
    if (!activeNode) throw "No active node to remove!";

    onNodeRemove(activeNode.data.id);
    setMenuCoordinates(null);
  }

  const onEditableNodeSubmit = (values: EditableNodeValues) => {
    onNodeEdit(values);
    setNodeInEdit(null);
  }

  const onNodeClick = (e: React.MouseEvent, node: TreeNode) => {
    setActiveNode(node);
    setMenuCoordinates({
      left: `${e.clientX + 5}px`, top: `${e.clientY + 5}px`
    });
  };

  useClickAway(editableNodeRef, () => setNodeInEdit(null));

  return (
    <div className="tree">
      <svg width={1500} height={1000}>
        <g transform="translate(500,10)">
          {nodes.map(node => (
            node.data.id === nodeInEdit?.data.id
              ? <EditableNode onSubmit={onEditableNodeSubmit} node={node} key={`${node.data.id}-editable`} ref={editableNodeRef}/>
              : <Node onClick={(e) => onNodeClick(e, node)} node={node} key={node.data.id}/>
          ))}
          {links.map(link => (
            <Edge link={link} key={`${link.source.data.id}-${link.target.data.id}`}/>
          ))}
        </g>
      </svg>

      {activeNode && !!menuCoordinates && <Menu
        style={menuCoordinates}
        onAdd={() => onNodeAdd(activeNode)}
        onEdit={onMenuEdit}
        onRemove={onMenuRemove}/>}
    </div>
  )
};

export default Tree