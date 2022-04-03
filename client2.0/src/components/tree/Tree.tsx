import React, { useRef, useState } from 'react';
import './Tree.scss';
import { Sentence, SyntaxTree, SyntaxTreeID } from 'types';
import Node from './Node';
import Edge from './Edge';
import { NodeDragHandler, EditableNodeValues, NodeDragEvent, TreeData, CoordinatedTreeLink, CoordinatedTreeNode } from './types';
import { getTextWidth } from 'utils/document';
import Menu from './Menu';
import EditableNode from './EditableNode';
import { useClickAway } from 'react-use';
import { D3DragEvent, drag } from 'd3-drag';
import { NODE_HEIGHT, NODE_WIDTH } from './config';
import { useEffect } from 'react';
import { computeLayout, translateTree } from './utils';
import { cloneDeep, isEqual } from 'lodash';

type TreeProps = {
  sentence: Sentence
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

const Tree: React.FC<TreeProps> = ({ sentence, onNodeAdd, onNodeEdit, onNodeRemove, onNodeDrag, onNodeDrop }) => {
  const [menuCoordinates, setMenuCoordinates] = useState<MenuCoordinates | null>(null);
  const [menuNode, setMenuNode] = useState<CoordinatedTreeNode | null>(null);
  const [editNode, setEditNode] = useState<CoordinatedTreeNode | null>(null);
  const [coordinatedRootNode, setCoordinatedRootNode] = useState<CoordinatedTreeNode | null>(null);
  const editableNodeRef = useRef<HTMLFormElement>(null);

  // console.log('rendering...', sentence.content, coordinatedRootNode);

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
    console.log(nodeId, event);
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
    console.log(nodeId, event);
  }

  useClickAway(editableNodeRef, () => setEditNode(null));

  useEffect(() => {
    setCoordinatedRootNode(
      computeLayout(sentence.syntaxTree)
    );
  }, [])

  return (
    <div className="tree">
      <svg width={1500} height={1000} data-id={sentence.id}>
        <g transform="translate(500,10)">
          {coordinatedRootNode?.links().map(link => (
            <Edge link={link} key={`${sentence.id}-${link.source.data.id}-${link.target.data.id}`}/>
          ))}
          {coordinatedRootNode?.descendants().map(node => {
            const { id } = node.data;

            return id === editNode?.data.id
              ? <EditableNode
                  node={node}
                  onSubmit={onEditableNodeSubmit}
                  key={`${sentence.id}-${id}-editable`}
                  ref={editableNodeRef}/>
              : <Node
                  sentenceId={sentence.id}
                  node={node}
                  width={NODE_WIDTH}
                  height={NODE_HEIGHT}
                  onClick={(e) => onNodeClick(node, e)}
                  onDragStart={(e) => onNodeDragStart(id, e)}
                  onDragProceed={(e) => onNodeDragProceed(id, e)}
                  onDragEnd={(e) => onNodeDragEnd(id, e)}
                  key={`${sentence.id}-${id}`}/>
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