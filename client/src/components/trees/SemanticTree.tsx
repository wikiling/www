import React, { useRef, useState } from 'react';
import './Tree.scss';
import { CoordinatedSemanticTree, ID, SemanticTree as SemanticTreeT } from 'types';
import SemanticEdge from './SemanticEdge';

import { SEM_NODE_HEIGHT, NODE_SEP_Y, NODE_WIDTH } from './config';
import { useEffect } from 'react';
import { computeLayout } from './utils';
import SemanticNode from './SemanticNode';
import { HierarchyPointNode } from 'd3-hierarchy';

type CoordinatedSemanticTreeNode = HierarchyPointNode<SemanticTreeT>

export type TreeProps = {
  id: ID
  tree: CoordinatedSemanticTree,
};

const groupTransformTmpl = (translateX: number = 0) => `translate(${translateX}, 40)`;

const SemanticTree: React.FC<TreeProps> = ({ id, tree  }) => {
  const rootRef = useRef<HTMLDivElement>(null);
  const height = (tree.height + 1) * (SEM_NODE_HEIGHT + NODE_SEP_Y); 
  const [coordinatedRootNode, setCoordinatedRootNode] = useState<CoordinatedSemanticTreeNode | null>(null);

  const [groupTransform, setGroupTransform] = useState<string>(groupTransformTmpl());

  const resize = () => {
    const newCoordinatedRootNode = computeLayout({ tree, getLabel: (node) => node.data.expr, nodeHeight: SEM_NODE_HEIGHT });
    const groupTranslateX = rootRef.current ? (
      rootRef.current.getBoundingClientRect().width / 2
    ) : 0;

    setCoordinatedRootNode(newCoordinatedRootNode);
    setGroupTransform(
      groupTransformTmpl(groupTranslateX)
    );
  };

  useEffect(resize, []);

  return (
    <div className="tree" ref={rootRef}>
      <svg width="100%" height={height} data-id={id}>
      <g transform={groupTransform}>
          {coordinatedRootNode?.links()
            .map(link => <SemanticEdge
              link={link}
              key={`${id}-${link.source.data.id}-${link.target.data.id}`}/>
          )}
          {coordinatedRootNode?.descendants().map(node =>
            <SemanticNode
              treeId={node.data.id}
              node={node}
              width={NODE_WIDTH}
              height={SEM_NODE_HEIGHT}
              key={`Sem-${id}-${node.data.id}`}
            />
          )}
        </g>
      </svg>
    </div>
  )
};

export default SemanticTree