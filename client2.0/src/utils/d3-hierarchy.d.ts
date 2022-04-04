import { HierarchyNode } from "d3-hierarchy";

declare module "d3-hierarchy" {
  export type IdentifiableNodeDatum = {
    id: string
    children?: IdentifiableNodeData[]
  }

  export type IdentifiableHierarchyNode = HierarchyNode<IdentifiableNodeDatum>

  export interface HierarchyNode<Datum extends IdentifiableNodeDatum> {
    detach(node: HierarchyNode<Datum>): void
    attach(parent: IdentifiableHierarchyNode, datum: IdentifiableNodeDatum)
    isDescendant(id: string): boolean
    findById(id: string): this | undefined
  }

  export function hierarchy<Datum extends IdentifiableNodeDatum>(
    data: Datum,
    children?: (d: Datum) => (Iterable<Datum> | null | undefined)
  ): HierarchyNode<Datum>;
}