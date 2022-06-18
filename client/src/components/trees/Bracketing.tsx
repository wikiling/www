import React from "react";
import "./Bracketing.scss";
import { CoordinatedTree } from "./types";

export type BracketingProps = {
  tree: CoordinatedTree
}

type BracketProps = {
  children: React.ReactNode;
  className?: string
}

const Bracket: React.FC<BracketProps> = ({ children, className }) => (
  <span className={className}>[{children}]</span>
);

const BracketNode = (node: CoordinatedTree) => {
  const { children, data: { label } } = node;
  return <Bracket>
    <span className="bracketing-bracket-node">{`${label.toLowerCase()} `}</span>
    {children?.map(bracket)}
  </Bracket>
};

const bracket = (tree: CoordinatedTree): React.ReactNode => {
  const { children, data: { label } } = tree;

  if (children?.length) return BracketNode(tree);
  
  return <span className="bracketing-bracket-leaf">{`${label} `}</span>;
};

const Bracketing: React.FC<BracketingProps> = ({ tree }) => {
  return <span className="bracketing">{bracket(tree)}</span>;
};

export default Bracketing;