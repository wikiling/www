import { hierarchy, IdentifiableHierarchyNode, IdentifiableNodeDatum, NodeSignatureFn } from "d3-hierarchy";
// @ts-ignore, FIXME: this gets around the global Node shadowing but sacrifices the typing
import { Node as d3Node } from "d3-hierarchy";

const ROOT_ID = "-1";

const getNewChildId = (parent: IdentifiableHierarchyNode) => {
  const { id: parentId, children } = parent.data;

  if (children?.length) {
    const { id } = children[children.length - 1];
    return `${parentId === ROOT_ID ? "" : parentId}${parseInt(id[id.length - 1]) + 1}`;
  } else {
    return `${parentId}0`;
  }
};

d3Node.prototype.detach = function(node: IdentifiableHierarchyNode) {
  if (!node.parent) throw "Can't remove the root of the tree!";

  // leaf
  if (node.parent.children!.length === 1) {
    node.parent.data.children = undefined
  // ancestor
  } else {
    const nodeIdx = node.parent.children!.indexOf(node);
    node.parent.data.children!.splice(nodeIdx, 1)
  }
}

d3Node.prototype.attach = function(parent: IdentifiableHierarchyNode, nodeData: IdentifiableNodeDatum) {
  nodeData.id = getNewChildId(parent);

  if (!!parent.data.children) parent.data.children.push(nodeData);
  else parent.data.children = [nodeData];
}

d3Node.prototype.findById = function(id: string) {
  return this.find(
    (node: IdentifiableHierarchyNode) => {
      return node.data.id === id
    }
  )
}

d3Node.prototype.isDescendant = function(id: string) {
  return !!this.findById(id);
}

export { hierarchy, getNewChildId };

