import React, { useRef, useState } from 'react';
import './Tree.scss';
import { SyntaxTree, SyntaxTreeID } from 'types';
import Node from './Node';
import Edge from './Edge';
import { NodeDragHandler, EditableNodeValues, NodeDragEvent, TreeData, CoordinatedTreeLink, CoordinatedTreeNode } from './types';
import { getTextWidth } from 'utils/document';
import Menu from './Menu';
import EditableNode from './EditableNode';
import { useClickAway } from 'react-use';
import { D3DragEvent } from 'd3-drag';
import { NODE_HEIGHT, NODE_WIDTH } from './config';
import { useEffect } from 'react';
import { computeLayout, replaceSubtree, translateSubtree } from './utils';

type TreeProps = {
  syntaxTree: SyntaxTree
  onNodeAdd: (node: SyntaxTreeID) => void
  onNodeEdit: (values: EditableNodeValues) => void
  onNodeRemove: (nodeId: SyntaxTreeID) => void
  onNodeDrag: (nodeId: SyntaxTreeID, dx: number, dy: number) => void
  onNodeDrop: (nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => void
};

type MenuCoordinates = {
  left: string
  top: string
}

const Tree: React.FC<TreeProps> = ({ syntaxTree, onNodeAdd, onNodeEdit, onNodeRemove, onNodeDrag, onNodeDrop }) => {
  const [menuCoordinates, setMenuCoordinates] = useState<MenuCoordinates | null>(null);
  const [activeNode, setActiveNode] = useState<CoordinatedTreeNode | null>(null);
  const [nodeInEdit, setNodeInEdit] = useState<CoordinatedTreeNode | null>(null);
  const [coordinatedRootNode, setCoordinatedRootNode] = useState<CoordinatedTreeNode | null>(null);
  const editableNodeRef = useRef<HTMLFormElement>(null);

  console.log('rendering...', coordinatedRootNode);

  const onMenuAdd = () => {
    if (!activeNode) throw "No active node to append to!";

    onNodeAdd(activeNode.data.id)
  }

  const onMenuEdit = () => {
    if (!activeNode) throw "No active node to edit!";

    setNodeInEdit(activeNode);
  }

  const onMenuRemove = () => {
    if (!activeNode) throw "No active node to remove!";

    onNodeRemove(activeNode.data.id);
  }

  const onMenuActionSuccess = () => {
    setMenuCoordinates(null);
    setActiveNode(null);
  }

  const onEditableNodeSubmit = (values: EditableNodeValues) => {
    onNodeEdit(values);
    setNodeInEdit(null);
  }

  const onNodeClick = (node: CoordinatedTreeNode, e: React.MouseEvent) => {
    setActiveNode(node);
    setMenuCoordinates({
      left: `${e.clientX + 5}px`, top: `${e.clientY + 5}px`
    });
  };

  const onNodeDragStart = (nodeId: SyntaxTreeID, event: NodeDragEvent) => {
    console.log(nodeId, event);
  }

  const onNodeDragProceed = (node: CoordinatedTreeNode, event: NodeDragEvent) => {
    if (!coordinatedRootNode) throw "Can't drag a tree without a root!";
  
    const translatedSubtree = translateSubtree(node, event.dx, event.dy);
    const newRoot = replaceSubtree(coordinatedRootNode, translatedSubtree);

    setCoordinatedRootNode(newRoot);

    const foo = newRoot.find((d) => d.data.id === node.data.id);
    console.log('new coords -->', foo?.x, foo?.y);
    console.log('diff?', newRoot === coordinatedRootNode)
  }

  const onNodeDragEnd = (nodeId: SyntaxTreeID, event: NodeDragEvent) => {
    console.log(nodeId, event);
  }

  useClickAway(editableNodeRef, () => setNodeInEdit(null));

  useEffect(() => {
    setCoordinatedRootNode(
      computeLayout(syntaxTree)
    );
  }, [])

  return (
    <div className="tree">
      <svg width={1500} height={1000}>
        <g transform="translate(500,10)">
          {coordinatedRootNode?.links().map(link => (
            <Edge link={link} key={`${link.source.data.id}-${link.target.data.id}`}/>
          ))}
          {coordinatedRootNode?.descendants().map(node => {
            const { id } = node.data;

            return id === nodeInEdit?.data.id
              ? <EditableNode
                  node={node}
                  onSubmit={onEditableNodeSubmit}
                  key={`${id}-editable`}
                  ref={editableNodeRef}/>
              : <Node
                  node={node}
                  width={NODE_WIDTH}
                  height={NODE_HEIGHT}
                  onClick={(e) => onNodeClick(node, e)}
                  onDragStart={(e) => onNodeDragStart(id, e)}
                  onDragProceed={(e) => onNodeDragProceed(node, e)}
                  onDragEnd={(e) => onNodeDragEnd(id, e)}
                  key={id}/>
            })}
        </g>
      </svg>

      {activeNode && !!menuCoordinates && <Menu
        style={menuCoordinates}
        onAdd={onMenuAdd}
        onEdit={onMenuEdit}
        onRemove={onMenuRemove}
        onActionSuccess={onMenuActionSuccess}/>}
    </div>
  )
};

export default Tree