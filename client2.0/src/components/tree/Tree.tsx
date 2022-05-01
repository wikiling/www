import React, { useRef, useState } from 'react';
import './Tree.scss';
import { CoordinatedSyntaxTree, ID, SyntaxTreeID } from 'types';
import Node from './Node';
import Edge from './Edge';
import { EditableNodeValues, NodeDragEvent, CoordinatedTreeLink, CoordinatedTreeNode } from './types';
import Menu from './Menu';
import EditableNode from './EditableNode';
import { useClickAway } from 'react-use';
import { SubjectPosition } from 'd3-drag';
import { NODE_HEIGHT, NODE_WIDTH } from './config';
import { useEffect } from 'react';
import { computeLayout, translateTree } from './utils';
import { cloneDeep } from 'lodash';
import classNames from 'classnames';

type TreeProps = {
  id: ID
  syntaxTree: CoordinatedSyntaxTree
  onNodeAdd: (node: SyntaxTreeID) => void
  onNodeEdit: (values: EditableNodeValues) => void
  onNodeRemove: (nodeId: SyntaxTreeID) => void
  onNodeMove: (nodeId: SyntaxTreeID, targetParentId: SyntaxTreeID) => void
};

type MenuCoordinates = {
  left: string
  top: string
}

const DRAG_DROP_ADOPTION_MIN_DISTANCE = 50;

const isWithinAdoptionDistance = (a: SubjectPosition, b: SubjectPosition) => (
  Math.abs(a.x - b.x) < DRAG_DROP_ADOPTION_MIN_DISTANCE &&
  Math.abs(a.y - b.y) < DRAG_DROP_ADOPTION_MIN_DISTANCE 
);

const groupTransformTmpl = (translateX: number = 0) => `translate(${translateX}, 10)`;

const Tree: React.FC<TreeProps> = ({ id, syntaxTree, onNodeAdd, onNodeEdit, onNodeRemove, onNodeMove }) => {
  const rootRef = useRef<HTMLDivElement>(null);
  const [menuCoordinates, setMenuCoordinates] = useState<MenuCoordinates | null>(null);
  const [coordinatedRootNode, setCoordinatedRootNode] = useState<CoordinatedTreeNode | null>(null);
  const [menuNode, setMenuNode] = useState<CoordinatedTreeNode | null>(null);
  const [editNode, setEditNode] = useState<CoordinatedTreeNode | null>(null);
  const [dragNode, setDragNode] = useState<CoordinatedTreeNode | null>(null);
  const [potentialParentNode, setPotentialParentNode] = useState<CoordinatedTreeNode | null>(null);
  const editNodeRef = useRef<HTMLFormElement>(null);
  const [groupTransform, setGroupTransform] = useState<string>(groupTransformTmpl());

  const resize = () => {
    const newCoordinatedRootNode = computeLayout(syntaxTree);
    const groupTranslateX = rootRef.current ? (
      rootRef.current.getBoundingClientRect().width / 2
    ) : 0;

    setCoordinatedRootNode(newCoordinatedRootNode);
    setGroupTransform(
      groupTransformTmpl(groupTranslateX)
    );
  }

  const onMenuAdd = () => {
    if (!menuNode) throw new Error("No active node to append to!");

    onNodeAdd(menuNode.data.id);
  };

  const onMenuEdit = () => {
    if (!menuNode) throw new Error("No active node to edit!");

    setEditNode(menuNode);
  };

  const onMenuRemove = () => {
    if (!menuNode) throw new Error("No active node to remove!");

    onNodeRemove(menuNode.data.id);
  };

  const onMenuActionSuccess = () => {
    setMenuCoordinates(null);
    setMenuNode(null);
  };

  const onEditableNodeSubmit = (values: EditableNodeValues) => {
    onNodeEdit(values);
    setEditNode(null);
  };

  const onNodeClick = (node: CoordinatedTreeNode, e: React.MouseEvent) => {
    setMenuNode(node);
    setMenuCoordinates({
      left: `${e.clientX + 5}px`, top: `${e.clientY + 5}px`
    });
  };

  const onNodeDragProceed = (node: CoordinatedTreeNode, event: NodeDragEvent) => {
    if (!coordinatedRootNode) throw new Error("Can't drag a tree without a root!");

    // this would make sense to assign in the drag start callback,
    // but a drag start event doesn't guarantee a drag end event,
    // so it's possible then to have an orphaned drag node.
    setDragNode(node);
  
    // calculate new tree coordinates
    setCoordinatedRootNode((prev) => {
      if (!prev) return null;
  
      const newRoot = cloneDeep(prev);
      const newNode = newRoot.findById(node.data.id);

      if (!newNode) return null;

      translateTree(newNode, event.dx, event.dy);
  
      return newRoot;
    });

    // nominate a new parent if within distance
    const ppn = coordinatedRootNode.find(
      (n) => !node.isDescendant(n.data.id) && isWithinAdoptionDistance(n, event)
    );
    setPotentialParentNode(ppn ?? null);
    console.log('drag node proceed...');
  };

  const onNodeDragEnd = (nodeId: SyntaxTreeID, event: NodeDragEvent) => {
    setDragNode(null);

    if (potentialParentNode) {
      onNodeMove(nodeId, potentialParentNode.data.id);
    } else {
      resize();
    }
    
    setPotentialParentNode(null);
    console.log('drag node end...');
  };

  const linkIsGrounded = (link: CoordinatedTreeLink) => {
    console.log(dragNode);
    return link.target.data.id !== dragNode?.data.id;
  }

  useClickAway(editNodeRef, () => setEditNode(null));

  useEffect(resize, []);

  return (
    <div className="tree" ref={rootRef}>
      <svg width="100%" height={1000} data-id={id}>
        <g transform={groupTransform}>
          {coordinatedRootNode?.links()
            .filter(linkIsGrounded)
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
                  className={classNames({ "node--highlit": nodeId === potentialParentNode?.data.id })}
                  onClick={(e) => onNodeClick(node, e)}
                  onDragProceed={(e) => onNodeDragProceed(node, e)}
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