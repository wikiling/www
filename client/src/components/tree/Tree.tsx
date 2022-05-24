import React, { useRef, useState } from 'react';
import './Tree.scss';
import { CoordinatedSyntaxTree, ID, TreeID } from 'types';
import Node from './Node';
import Edge from './Edge';
import { EditableNodeValues, NodeDragEvent, CoordinatedTreeLink, CoordinatedTreeNode } from './types';
import Menu from './TreeMenu';
import EditableNode from './EditableNode';
import { useClickAway } from 'react-use';
import { SubjectPosition } from 'd3-drag';
import { NODE_HEIGHT, NODE_SEP_Y, NODE_WIDTH } from './config';
import { useEffect } from 'react';
import { computeLayout, translateTree } from './utils';
import { cloneDeep } from 'lodash';
import classNames from 'classnames';

type TreeProps = {
  id: ID
  syntaxTree: CoordinatedSyntaxTree
  onNodeAdd: (node: TreeID) => CoordinatedTreeNode | undefined
  onNodeEdit: (values: EditableNodeValues) => void
  onNodeRemove: (nodeId: TreeID) => void
  onNodeMove: (nodeId: TreeID, targetParentId: TreeID) => void
};

type MenuCoordinates = {
  left?: string
  top?: string
  visibility: 'visible' | 'hidden'
}

const defaultMenuCoordinates: MenuCoordinates = { visibility: 'hidden' };

const DRAG_DROP_ADOPTION_MIN_DISTANCE = 50;

const isWithinAdoptionDistance = (a: SubjectPosition, b: SubjectPosition) => (
  Math.abs(a.x - b.x) < DRAG_DROP_ADOPTION_MIN_DISTANCE &&
  Math.abs(a.y - b.y) < DRAG_DROP_ADOPTION_MIN_DISTANCE 
);

const groupTransformTmpl = (translateX: number = 0) => `translate(${translateX}, 10)`;

const Tree: React.FC<TreeProps> = ({ id, syntaxTree, onNodeAdd, onNodeEdit, onNodeRemove, onNodeMove }) => {
  const rootRef = useRef<HTMLDivElement>(null);
  const editNodeRef = useRef<SVGGElement>(null);
  const menuRef = useRef<HTMLDivElement>(null);
  const height = (syntaxTree.height + 1) * (NODE_HEIGHT + NODE_SEP_Y); 
  const [menuCoordinates, setMenuCoordinates] = useState<MenuCoordinates>(defaultMenuCoordinates);
  const [coordinatedRootNode, setCoordinatedRootNode] = useState<CoordinatedTreeNode | null>(null);
  const [menuNode, setMenuNode] = useState<CoordinatedTreeNode | null>(null);
  const [editNode, setEditNode] = useState<CoordinatedTreeNode | null>(null);
  const [dragNode, setDragNode] = useState<CoordinatedTreeNode | null>(null);
  const [potentialParentNode, setPotentialParentNode] = useState<CoordinatedTreeNode | null>(null);
 
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

  const closeMenu = () => {
    setMenuCoordinates(defaultMenuCoordinates);
    setMenuNode(null);
  };

  const handleMenuAdd = () => {
    if (!menuNode) throw new Error("No active node to append to!");

    onNodeAdd(menuNode.data.id);
  };

  const handleMenuEdit = () => {
    if (!menuNode) throw new Error("No active node to edit!");

    setEditNode(menuNode);
  };

  const handleMenuRemove = () => {
    if (!menuNode) throw new Error("No active node to remove!");

    onNodeRemove(menuNode.data.id);
  };

  const handleMenuActionSuccess = closeMenu;

  const handleEditableNodeSubmit = (values: EditableNodeValues) => {
    onNodeEdit(values);
    setEditNode(null);
  };

  const handleNodeClick = (node: CoordinatedTreeNode, e: React.MouseEvent) => {
    const rootWidth = rootRef.current?.offsetWidth;
    const menuDims = menuRef.current;
    const buffer = 10;

    if (!rootWidth) throw new Error("Can't click a tree without a root!");
    if (!menuDims) throw new Error("Can't click a tree without a menu!");
  
    setMenuNode(node);

    const left = (
      rootWidth / 2 +           // coordinate origin is center of top edge
      node.x +                  // orient to node
      buffer + NODE_WIDTH / 2   // account for width of node and apply a buffer
    );

    const top = (
      node.y -                                    // orient to node
      menuDims.offsetHeight / 2 + NODE_HEIGHT / 2 // center menu against node
    );

    setMenuCoordinates({
      left: `${left}px`,
      top: `${top}px`,
      visibility: 'visible'
    });
  };

  const handleNodeDragProceed = (node: CoordinatedTreeNode, event: NodeDragEvent) => {
    if (!coordinatedRootNode) throw new Error("Can't drag a tree without a root!");

    // this would make sense to assign on d3's drag start event,
    // but d3's drag start event doesn't guarantee a *drag end* event,
    // so it would be possible then to have an orphaned drag node.
    // there doesn't appear to be a downside to just doing this here.
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
  };

  const handleNodeDragEnd = (nodeId: TreeID) => {
    setDragNode(null);

    if (potentialParentNode) onNodeMove(nodeId, potentialParentNode.data.id);
    else resize();
    
    setPotentialParentNode(null);
  };

  const linkIsGrounded = (link: CoordinatedTreeLink) => {
    return link.target.data.id !== dragNode?.data.id;
  }

  useClickAway(
    // clickaway expects an HTMLElement, but works fine with our
    // svgforeignobject
    editNodeRef as unknown as React.RefObject<HTMLElement | null>,
    () => setEditNode(null)
  );
  useClickAway(menuRef, closeMenu);

  useEffect(resize, []);

  return (
    <div className="tree" ref={rootRef}>
      <svg width="100%" height={height} data-id={id}>
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
                  onSubmit={handleEditableNodeSubmit}
                  key={`${id}-${nodeId}-editable`}
                  ref={editNodeRef}/>
              : <Node
                  treeId={id}
                  node={node}
                  width={NODE_WIDTH}
                  height={NODE_HEIGHT}
                  className={classNames({ "node--highlit": nodeId === potentialParentNode?.data.id })}
                  onClick={(e) => handleNodeClick(node, e)}
                  onDragProceed={(e) => handleNodeDragProceed(node, e)}
                  onDragEnd={() => handleNodeDragEnd(nodeId)}
                  key={`${id}-${nodeId}`}/>
            })}
        </g>
      </svg>

      <Menu
        ref={menuRef}
        style={menuCoordinates}
        onAdd={handleMenuAdd}
        onEdit={handleMenuEdit}
        onRemove={handleMenuRemove}
        onActionSuccess={handleMenuActionSuccess}/>
    </div>
  )
};

export default Tree