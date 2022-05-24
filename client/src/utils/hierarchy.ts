import { hierarchy, IdentifiableHierarchyNode, IdentifiableNodeDatum } from "d3-hierarchy";
// @ts-ignore, FIXME: this gets around the global Node shadowing but sacrifices the typing
import { Node as d3Node } from "d3-hierarchy";

const ROOT_ID = "-1";

const mintNewChildId = (parent: IdentifiableHierarchyNode) => {
  const { id: parentId, children } = parent.data;

  if (children?.length) {
    const { id } = children[children.length - 1];
    return `${parentId === ROOT_ID ? "" : parentId}${parseInt(id[id.length - 1]) + 1}`;
  } else {
    return `${parentId}0`;
  }
};

d3Node.prototype.detach = function (node: IdentifiableHierarchyNode) {
  if (!node.parent) throw new Error("Can't remove the root of the tree!");

  // only child
  if (node.parent.children!.length === 1) {
    node.parent.data.children = undefined
  // full house
  } else {
    const nodeIdx = node.parent.children!.indexOf(node);
    node.parent.data.children!.splice(nodeIdx, 1)
  }
}

d3Node.prototype.attach = function (parent: IdentifiableHierarchyNode, nodeData: IdentifiableNodeDatum): string {
  nodeData.id = mintNewChildId(parent);

  if (!!parent.data.children) parent.data.children.push(nodeData);
  else parent.data.children = [nodeData];

  return nodeData.id;
}

d3Node.prototype.findById = function (id: string) {
  return this.find(
    (node: IdentifiableHierarchyNode) => {
      return node.data.id === id
    }
  )
}

d3Node.prototype.isDescendant = function (id: string) {
  return !!this.findById(id);
}

d3Node.prototype.width = function () {
  const leaves = this.leaves();

  if (!leaves.length) return 0;

  const first = leaves[0], last = leaves[leaves.length - 1];
  
  return last.x - first.x;
}

d3Node.prototype.isPreterminal = function () {
  const { children } = this;

  return children?.length === 1 && !children[0].children;
}

d3Node.prototype.parseString = function () {
  const { children } = this;

  if (this.isPreterminal()) {
    console.log(0, this.data.token)
    return `(${this.data.label} ${children[0].data.label})`;
  } else {
    console.log(1, this.data.token)
    return `(${this.data.label} ${children.map((node: IdentifiableHierarchyNode) => node.parseString()).join(' ')})`;
  }
}

export { hierarchy, mintNewChildId };

