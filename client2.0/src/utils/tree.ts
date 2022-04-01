// @ts-ignore
import { HierarchyNode, Node, hierarchy } from "d3-hierarchy";

Node.prototype.remove = hierarchy.prototype.remove = function() {
}

export {
  hierarchy
}