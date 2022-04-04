import React, { useRef, useState } from 'react';
import './Tree.scss';
import { ID, Sentence, SyntaxTree, SyntaxTreeID } from 'types';
import Node from './Node';
import Edge from './Edge';
import { NodeDragHandler, EditableNodeValues, NodeDragEvent, TreeNodeData, CoordinatedTreeLink, CoordinatedTreeNode } from './types';
import { getTextDimensions } from 'utils/document';
import Menu from './Menu';
import EditableNode from './EditableNode';
import { useClickAway } from 'react-use';
import { D3DragEvent, drag } from 'd3-drag';
import { NODE_HEIGHT, NODE_WIDTH } from './config';
import { useEffect } from 'react';
import { computeLayout, translateTree } from './utils';
import { cloneDeep, isEqual } from 'lodash';

type TreeProps = {
  id: ID
  syntaxTree: SyntaxTree
  onNodeAdd: (node: SyntaxTreeID) => void
  onNodeEdit: (values: EditableNodeValues) => void
  onNodeRemove: (nodeId: SyntaxTreeID) => void
  onNodeMove: (nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => void
};

type MenuCoordinates = {
  left: string
  top: string
}

const Tree: React.FC<TreeProps> = ({ id, syntaxTree, onNodeAdd, onNodeEdit, onNodeRemove, onNodeMove }) => {
  const [menuCoordinates, setMenuCoordinates] = useState<MenuCoordinates | null>(null);
  const [coordinatedRootNode, setCoordinatedRootNode] = useState<CoordinatedTreeNode | null>(null);
  const [menuNode, setMenuNode] = useState<CoordinatedTreeNode | null>(null);
  const [editNode, setEditNode] = useState<CoordinatedTreeNode | null>(null);
  const [dragNode, setDragNode] = useState<CoordinatedTreeNode | null>(null);
  const editNodeRef = useRef<HTMLFormElement>(null);

  console.log('rendering...', coordinatedRootNode);

  const onMenuAdd = () => {
    if (!menuNode) throw "No active node to append to!";

    onNodeAdd(menuNode.data.id)
  }

  const onMenuEdit = () => {
    if (!menuNode) throw "No active node to edit!";

    setEditNode(menuNode);
  }

  const onMenuRemove = () => {
    if (!menuNode) throw "No active node to remove!";

    onNodeRemove(menuNode.data.id);
  }

  const onMenuActionSuccess = () => {
    setMenuCoordinates(null);
    setMenuNode(null);
  }

  const onEditableNodeSubmit = (values: EditableNodeValues) => {
    onNodeEdit(values);
    setEditNode(null);
  }

  const onNodeClick = (node: CoordinatedTreeNode, e: React.MouseEvent) => {
    setMenuNode(node);
    setMenuCoordinates({
      left: `${e.clientX + 5}px`, top: `${e.clientY + 5}px`
    });
  };

  const onNodeDragStart = (nodeId: SyntaxTreeID, event: NodeDragEvent) => {
    console.log(event)
    if (!coordinatedRootNode) throw `Unexpected: event ${event} without root;`;

    const node = coordinatedRootNode.find((node) => node.data.id === nodeId);

    if (!node) throw `Unexpected: event ${event} from unattached node (${nodeId});`;

    setDragNode(node);
  }

  const onNodeDragProceed = (nodeId: SyntaxTreeID, event: NodeDragEvent) => {
    if (!coordinatedRootNode) throw "Can't drag a tree without a root!";
  
    setCoordinatedRootNode((prev) => {
      if (!prev) return null;
  
      const newRoot = cloneDeep(prev);
      const node = newRoot.find((node) => node.data.id === nodeId);

      if (!node) return null;

      translateTree(node, event.dx, event.dy);
  
      return newRoot;
    });
  }

  const onNodeDragEnd = (nodeId: SyntaxTreeID, event: NodeDragEvent) => {
    setCoordinatedRootNode(
      computeLayout(syntaxTree)
    );
    setDragNode(null);
  }

  useClickAway(editNodeRef, () => setEditNode(null));

  useEffect(() => {
    setCoordinatedRootNode(
      computeLayout(syntaxTree)
    );
  }, [])

  return (
    <div className="tree">
      <svg width={1500} height={1000} data-id={id}>
        <g transform="translate(500,10)">
          {coordinatedRootNode?.links()
            .filter((link) => link.target.data.id !== dragNode?.data.id)
            .map(link => <Edge
              link={link}
              key={`${id}-${link.source.data.id}-${link.target.data.id}`}/>
          )}
          {coordinatedRootNode?.descendants().map(node => {
            const nodeId = node.data.id;

            return nodeId === editNode?.data.id
              ? <EditableNode
                  node={node}
                  onSubmit={onEditableNodeSubmit}
                  key={`${id}-${nodeId}-editable`}
                  ref={editNodeRef}/>
              : <Node
                  treeId={id}
                  node={node}
                  width={NODE_WIDTH}
                  height={NODE_HEIGHT}
                  onClick={(e) => onNodeClick(node, e)}
                  onDragStart={(e) => onNodeDragStart(nodeId, e)}
                  onDragProceed={(e) => onNodeDragProceed(nodeId, e)}
                  onDragEnd={(e) => onNodeDragEnd(nodeId, e)}
                  key={`${id}-${nodeId}`}/>
            })}
        </g>
      </svg>

      {menuNode && !!menuCoordinates && <Menu
        style={menuCoordinates}
        onAdd={onMenuAdd}
        onEdit={onMenuEdit}
        onRemove={onMenuRemove}
        onActionSuccess={onMenuActionSuccess}/>}
    </div>
  )
};

export default Tree