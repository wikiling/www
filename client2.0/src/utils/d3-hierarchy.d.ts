import { HierarchyNode } from "d3-hierarchy";

declare module "d3-hierarchy" {
  export type IdentifiableNodeDatum = {
    id: string
    children?: IdentifiableNodeDatum[]
  }

  export type IdentifiableHierarchyNode = HierarchyNode<IdentifiableNodeDatum>

  export type NodeSignatureFn = (node: HierarchyNode<Datum>) => string

  export interface HierarchyNode<Datum extends IdentifiableNodeDatum> {
    detach(node: HierarchyNode<Datum>): void
    attach(parent: IdentifiableHierarchyNode, datum: IdentifiableNodeDatum)
    isDescendant(id: string): boolean
    findById(id: string): this | undefined
    width(): number
  }

  export function hierarchy<Datum extends IdentifiableNodeDatum>(
    data: Datum,
    children?: (d: Datum) => (Iterable<Datum> | null | undefined)
  ): HierarchyNode<Datum>;
}